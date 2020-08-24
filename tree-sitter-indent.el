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
  '((indent . ;; these nodes are always indented
            (class_body
             argument_list ;; arguments of a function call
             compound_expression ;; begin … end (TODO: is begin and end also indented?)
             ))
    (indent-rest . ;; if parent node is one of this and current node is in middle → indent
                 (;; member esp, assignment exp
                  ;; TODO … =  bleh
                  ;; TODO … = (bleh)
                  if_statement
                  while_statement
                  ))
    (outdent . ;; these nodes always outdent (1 shift in opposite direction)
             (else
              )))
  "Scopes for indenting in Julia.")

;;;; Private functions
(defun tree-sitter-indent--node-is-indent-rest (node scopes)
  "TODO: document"
  (let-alist scopes
    (member (ts-node-type node)
            .indent-rest)))

(defun tree-sitter-indent--node-is-indent (node scopes)
  "TODO: document"
  (let-alist scopes
    (member (ts-node-type node)
            .indent)))

(defun tree-sitter-indent--node-is-indent-rest-or-block (node scope)
  "TODO: document"
  (let-alist scope
    (member (ts-node-type node)
            .indent-rest-or-block)))

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
	      (when-let* ((parent-node (ts-get-parent current-node))
                          (_same-position
                           (and
                            (eq (ts-node-start-byte parent-node)
                                (ts-node-start-byte current-node)))))
		;; move upwards to the parent node
		(setq current-node parent-node))))
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
    (s-replace-regexp (rx "-mode") "")
    (format "tree-sitter-indent-%s-scopes")
    (intern)
    (symbol-value)))

(cl-defun tree-sitter-indent--indents-in-path (parentwise-path scopes)
  "Add 1 or 0 as indent in each node in (PARENTWISE-PATH.

What is checked to add an indent:
- A node bolongs into the \"indent\" group in SCOPES
- Deterimen what group the node's parent belongs to, and whether the node
is in a middle position."
  (thread-last parentwise-path
    (seq-map
     (lambda (current-node)
       (let* ((previous-node
               (ts-get-prev-sibling current-node))
              (next-node
               (ts-get-next-sibling current-node))
              (parent-node
               (ts-get-parent current-node))
              (current-node-is-middle-node
               (and previous-node next-node))
              (current-node-must-indent
               (tree-sitter-indent--node-is-indent
                current-node scopes)))
         (if (or current-node-must-indent
                 (and current-node-is-middle-node
                      parent-node
                      (tree-sitter-indent--node-is-indent-rest
                       parent-node scopes)))
             1
           0))))))

(defun tree-sitter-indent--outdents-in-path (parentwise-path scopes)
  "Subtract 1 or 0 as indent in each node in (PARENTWISE-PATH.

Each node is tested if it belongs into the \"outdent\" group in SCOPES."
  (thread-last parentwise-path
    (seq-map
     (lambda (node)
       (if (let-alist scopes
             (member (ts-node-type node)
                     .outdent))
           -1
         0)))))

(cl-defun tree-sitter-indent--indent-count (&optional
                                            (position (point)))
  "Number of shifts the line at POSITION should have as indent.

The actual visible indent is supposed to end up as
returned count × major-mode-indent-offset

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
                                                 scopes))
           (outdents-in-path
            (tree-sitter-indent--outdents-in-path parentwise-path
                                                  scopes)))
      (+
       (cl-reduce '+ indents-in-path)
       (cl-reduce '+ outdents-in-path)))))

;;;; Public API

;;;###autoload
(defun tree-sitter-indent-line ()
  "Use Tree-sitter as backend to indent current line.

Use in buffer with

(setq-local indent-line-function #'tree-sitter-indent-line)."
  (let ((current-buffer-indent-offset
         (thread-last major-mode
           (symbol-name)
           (s-replace-regexp (rx "-mode") "")
           (format "%s-indent-offset")
           (intern)
           (symbol-value))))
    (indent-line-to
     (* current-buffer-indent-offset
        (tree-sitter-indent--indent-count)))))

(provide 'tree-sitter-indent)
;;; tree-sitter-indent.el ends here
