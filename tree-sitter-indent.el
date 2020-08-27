;;; tree-sitter-indent.el --- Provide `line-indent-function' with a Tree-sitter backend using emacs-tree-sitter package  -*- lexical-binding: t; -*-

;; Copyright © 2020  Felipe Lema

;; Author: Felipe Lema <felipelema@mortemale.org>
;; Keywords: convenience, internal
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.10.0"))
;; URL: https://codeberg.org/FelipeLema/tree-sitter-indent.el
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use Tree-sitter as backend to source code indentation.
;;
;; Provide an `indent-line-function` using the emacs-tree-sitter package
;; Usage (for julia language):
;;
;; (require 'tree-sitter-indent)
;; (tree-sitter-require 'julia)
;;
;; (add-hook 'julia-mode-hook
;;           (lambda ()
;;             (tree-sitter-mode)
;; TODO is it necessary to set indent-region-function too?
;;             (setq-local indent-line-function #'tree-sitter-indent-line))
;;
;; The code in this package was based on Atom implementation of line indenting using
;; Tree-sitter at https://github.com/atom/atom/pull/18321/
;;
;; See Atom's "Creating a Grammat" page for therminology
;; https://flight-manual.atom.io/hacking-atom/sections/creating-a-grammar/

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'tree-sitter)

(defgroup tree-sitter-indent nil "Indent lines using Tree-sitter as backend"
  :group 'tree-sitter)

;;;; scopes per language
(defcustom tree-sitter-indent-julia-scopes
  ;; TODO cleanup this after thoroughly tested
  '((indent-all . ;; these nodes are always indented
                (class_body
                 binary-expression))
    (indent-rest . ;; if parent node is one of this and node is not first → indent
                 (assignment_expression))
    (indent-body . ;; if parent node is one of this and current node is in middle → indent
                 (
                  compound_expression ;; begin … end
                  function_definition ;; function … end
                  if_statement
                  while_statement))

    (paren-indent . ;; if parent node is one of these → indent to paren opener
                  (argument_list ;; arguments of a function call
                   ))
    (multi-line-text . ;; if node is one of this, then don't modify the indent
                     ;; this is basically a peaceful way out by saying "this looks like something
                     ;; that cannot be indented using AST, so best I leave it as-is"
                     (triple_string))
    (outdent . ;; these nodes always outdent (1 shift in opposite direction)
             (else_clause)))
  "Scopes for indenting in Julia."
  :type 'sexp)

;;;; Private functions
(defun tree-sitter-indent--node-is-indent-all (node scopes)
  "TODO: document"
  (let-alist scopes
    (member (ts-node-type node)
            .indent-all)))

(defun tree-sitter-indent--node-is-indent-rest (node scopes)
  "TODO: document"
  (let-alist scopes
    (member (ts-node-type node)
            .indent-rest)))

(defun tree-sitter-indent--node-is-indent-body (node scopes)
  "TODO: document"
  (let-alist scopes
    (member (ts-node-type node)
            .indent-body)))

(defun tree-sitter-indent--node-is-multi-line-text (node scope)
  "TODO: document"
  (let-alist scope
    (member (ts-node-type node)
            .multi-line-text)))

(defun tree-sitter-indent--bounded-position (position)
  "Return POSITION, with the guarantee that it will be within buffer range.

TODO: find out why tree-sitter reports out-of-bound-bytes."
  (max (point-min)
       (min (point-max) position)))

(defun tree-sitter-indent--highest-node-at-position (position)
  "Get the node at buffer POSITION that's at the highest level.

POSITION is a byte position in buffer like \\(point-min\\).

TODO: test this"
  (save-excursion
    (goto-char position)
    ;; maybe implement this as a cl-loop
    (let* ((current-node (tree-sitter-node-at-point)))
      ;; move upwards until we either don't have aparent node
      ;; or we moved out of line
      (while (and
	      current-node
	      (when-let* ((parent-node (ts-get-parent current-node)))
                (when (and ;; parent and current share same position
                       (eq (ts-node-start-byte parent-node)
                           (ts-node-start-byte current-node)))
		  ;; move upwards to the parent node
		  (setq current-node parent-node)))))
      current-node)))

(defun tree-sitter-indent--parentwise-path (node)
  "Get list of nodes by moving parent-wise starting at NODE.

The last element in returned path is NODE."
  (let ((next-parent-node (ts-get-parent node))
        (path
         (list node)))
    (while next-parent-node
      ;; collect
      (setq path
            (append (list next-parent-node)
                    path))
      ;; move to next iteration
      (setq next-parent-node (ts-get-parent next-parent-node)))
    path))

(defun tree-sitter-indent--get-buffer-scopes ()
  "Get scopes by reading the name of `major-mode'.

E.g. julia-mode → tree-sitter-indent-julia-scopes."
  (thread-last major-mode
    (symbol-name)
    (replace-regexp-in-string (rx "-mode") "")
    (format "tree-sitter-indent-%s-scopes")
    (intern)
    (symbol-value)))

(defun tree-sitter-indent--node-is-paren-indent (node scopes)
  (let-alist scopes
    (member (ts-node-type node)
            .paren-indent)))

(cl-defun tree-sitter-indent--indents-in-path (parentwise-path scopes original-column)
  "Map PARENTWISE-PATH into indent instructions.

Each element of the returned list is one of the following

no-indent                         nothing to add to current column
indent                            add one indent to current column
outdent                           subtract one indent to current column
\(paren-indent . COLUMN)  match   parent's parent opener column
\(preserve . ORIGINAL-COLUMN)     preserve the column that was before

What is checked to add an indent:
- A node bolongs into the \"indent\" group in SCOPES
- Deterimen what group the node's parent belongs to, and whether the node
is in a middle position.
- A node belongs to the \"outdent\" group in SCOPES
- A node belongs to the \"paren-indent\" group in SCOPES"
  (let ((last-node
         (seq-elt
          parentwise-path
          (-
           (length parentwise-path)
           1))))
    (thread-last parentwise-path
      (seq-map
       (lambda (current-node)
         (let* ((previous-node
                 (ts-get-prev-sibling current-node))
                (next-node
                 (ts-get-next-sibling current-node))
                (parent-node
                 (ts-get-parent current-node))
                (current-node-is-rest
                 previous-node)
                (current-node-is-middle-node
                 (and current-node-is-rest next-node))
                (current-node-must-indent
                 (tree-sitter-indent--node-is-indent-all
                  current-node scopes))
                (current-node-must-outdent
                 (and
                  (eq last-node current-node)
                  (tree-sitter-indent--node-is-outdent current-node
                                                       scopes))))
           (cond
            ((and parent-node
                  (tree-sitter-indent--node-is-paren-indent parent-node
                                                            scopes))
             (let* ((paren-opener
                     (ts-node-start-byte parent-node))
                    (paren-point
                     (save-excursion
                       (goto-char paren-opener)
                       (point)))
                    (beginning-of-line-point
                     (save-excursion
                       (goto-char paren-opener)
                       (beginning-of-line 1)
                       (point)))
                    (paren-indenting-column
                     (+ 1
                        (- paren-point beginning-of-line-point))))
               `(paren-indent ,paren-indenting-column)))
            ((or current-node-must-indent
                 (and parent-node
                      current-node-is-rest
                      (tree-sitter-indent--node-is-indent-rest
                       parent-node scopes))
                 (and parent-node
                      current-node-is-middle-node
                      (tree-sitter-indent--node-is-indent-body
                       parent-node scopes)))
             (if current-node-must-outdent
                 'no-indent ;; if it's an outdent, cancel
               'indent))
            (current-node-must-outdent
             'outdent)
            ((tree-sitter-indent--node-is-multi-line-text current-node
                                                          scopes)
             `(preserve . ,original-column))
            (t
             'no-indent))))))))

(defun tree-sitter-indent--node-is-outdent (node scopes)
  "Return non-nil if NODE outdents per SCOPES.

NODE is tested if it belongs into the \"outdent\" group in SCOPES."
  (let-alist scopes
    (member (ts-node-type node)
            .outdent)))

(defun tree-sitter-indent--updated-column (indent-offset column indent)
  "Return COLUMN after added indent instructions per INDENT.

INDENT is one of `tree-sitter-indent--indents-in-path'.

If \"1 indent\" is to be applied, then returned value is INDENT-OFFSET + INDENT."
  (pcase indent
    (`no-indent
     column)
    (`indent
     (+ column indent-offset))
    (`outdent
     (- column indent-offset))
    (`(paren-indent ,paren-column)
     paren-column)
    (`(preserve . ,original-column)
     original-column)
    (_
     (error "Unexpected indent instruction: %s" indent))))

(cl-defun tree-sitter-indent--indent-column (current-buffer-indent-offset
                                             original-column
                                             position)
  "Return the column the first non-whitespace char at POSITION should indent to.

Collect indent instruction per AST with `tree-sitter-indent--indents-in-path', then
apply instructions with `tree-sitter-indent--updated-column' using CURRENT-BUFFER-INDENT-OFFSET as step.

See `tree-sitter-indent-line'."
  (save-excursion
    ;; go to first non-whitespace character
    (back-to-indentation)
    (let* ((scopes
            (tree-sitter-indent--get-buffer-scopes))
           (indenting-node (tree-sitter-indent--highest-node-at-position
                            position))
           (parentwise-path (tree-sitter-indent--parentwise-path indenting-node))
           (indents-in-path
            (tree-sitter-indent--indents-in-path parentwise-path
                                                 scopes
                                                 original-column)))
      (seq-reduce (apply-partially
                   'tree-sitter-indent--updated-column
                   current-buffer-indent-offset)
                  indents-in-path
                  0 ;; start at column 0
                  ))))

;;;; Public API

;;;###autoload
(defun tree-sitter-indent-line ()
  "Use Tree-sitter as backend to indent current line.

Use in buffer with

(setq-local indent-line-function #'tree-sitter-indent-line)."
  (let ((current-buffer-indent-offset
         (thread-last major-mode
           (symbol-name)
           (replace-regexp-in-string (rx "-mode") "")
           (format "%s-indent-offset")
           (intern)
           (symbol-value)))
        (original-column
         (-
          (save-excursion
            (beginning-of-line 1)
            (point))
          (save-excursion
            (back-to-indentation)
            (point)))))
    (indent-line-to
     (tree-sitter-indent--indent-column current-buffer-indent-offset
                                        original-column
                                        (point)))))

(provide 'tree-sitter-indent)
;;; tree-sitter-indent.el ends here
