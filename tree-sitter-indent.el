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
  '((indent-all . ;; these nodes are always indented
                (class_body
                 binary_expression))
    (indent-rest . ;; if parent node is one of this and node is not first → indent
                 (assignment_expression
                  export_statement
                  import_statement))
    (indent-body . ;; if parent node is one of this and current node is in middle → indent
                 (compound_expression ;; begin … end
                  do_clause
                  for_statement
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

(defcustom tree-sitter-indent-rust-scopes
  '((indent-all . ;; these nodes are always indented
                (where_clause))
    (indent-rest . ;; if parent node is one of this and node is not first → indent
                 (function_item
                  for_expression))
    (indent-body . ;; if parent node is one of this and current node is in middle → indent
                 (let_declaration))

    (paren-indent . ;; if parent node is one of these → indent to paren opener
                  (parameters))
    (align-char-to . ;; chaining char → node types we move parentwise to find the first chaining char
                   ((?. . (call_expression field_expression))))
    (aligned-siblings . ;; siblings (nodes with same parent) should be aligned to the first child
                      (where_predicate))

    (multi-line-text . ;; if node is one of this, then don't modify the indent
                     ;; this is basically a peaceful way out by saying "this looks like something
                     ;; that cannot be indented using AST, so best I leave it as-is"
                     ())
    (outdent . ;; these nodes always outdent (1 shift in opposite direction)
             ("}")))
  "Scopes for indenting in Julia."
  :type 'sexp)

;;;; Private functions
(defun tree-sitter-indent--node-is-indent-all (node scopes)
  "Non-nil if NODE type is in indent-all group.

Nodes in this group will be always +1 indentend.

SCOPES is supposed to come from `tree-sitter-indent--get-buffer-scopes'."
  (let-alist scopes
    (member (ts-node-type node)
            .indent-all)))

(defun tree-sitter-indent--node-is-indent-rest (node scopes)
  "Non-nil if NODE type is in indent-rest group.

Nodes in this group will +1 indentend if they are a non-first child of
parent node.

SCOPES is supposed to come from `tree-sitter-indent--get-buffer-scopes'."
  (let-alist scopes
    (member (ts-node-type node)
            .indent-rest)))

(defun tree-sitter-indent--node-is-indent-body (node scopes)
  "Non-nil if NODE type is in indent-body group.

Nodes in this group will +1 indentend if they are both a non-first child of
and non-last child of parent node.

SCOPES is supposed to come from `tree-sitter-indent--get-buffer-scopes'."
  (let-alist scopes
    (member (ts-node-type node)
            .indent-body)))

(defun tree-sitter-indent--node-is-multi-line-text (node scopes)
  "Non-nil if NODE type is in indent-rest group.

Nodes in this group will keep their current indentation

SCOPES is supposed to come from `tree-sitter-indent--get-buffer-scopes'."
  (let-alist scopes
    (member (ts-node-type node)
            .multi-line-text)))

(defun tree-sitter-indent--node-is-aligned-sibling (node scopes)
  "Non-nil if NODE type is in aligned-siblings group.

Nodes in this group will be aligned to the column of the first sibling.

SCOPES is supposed to come from `tree-sitter-indent--get-buffer-scopes'."
  (let-alist scopes
    (member (ts-node-type node)
            .aligned-siblings)))

(defun tree-sitter-indent--highest-node-at-position (position)
  "Get the node at buffer POSITION that's at the highest level.

POSITION is a byte position in buffer like \\(point-min\\)."
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
  "Non-nil if NODE type is in paren-indent group.

Child nodes in this group will be indentend to the paren opener column.

SCOPES is supposed to come from `tree-sitter-indent--get-buffer-scopes'."
  (let-alist scopes
    (member (ts-node-type node)
            .paren-indent)))

(defun tree-sitter-indent--chain-column (current-node align-char-to-alist parentwise-path)
  "When node is in a chain call, return column to align each call.

CURRENT-NODE current node being indented
ALIGN-CHAR-TO-ALIST char → group of node types we can move within when searching
for the first chain char.
This group is supposed to contain all node types conformed by a chain.
PARENTWISE-PATH nodes from CURRENT-NODE to tree root (\"document\")

Returns a column to indent to or nil if no such column can / should be applied.

Reads text from current buffer."
  (let ((first-character-for-current-node
         (string-to-char
          (ts-node-text current-node))))
    (when-let* ((last-parent-belongs-to
                 (alist-get first-character-for-current-node
                            align-char-to-alist))
                (last-parent-belonging-to
                 (thread-last parentwise-path
                   (reverse) ;; path starts at (ts-parent current-node)
                   (cdr) ;; skip current-node
                   ;; walk within allowed boundaries
                   (seq-take-while
                    (lambda (node)
                      (member (ts-node-type node)
                              last-parent-belongs-to)))
                   (seq-first)))
                (first-char-position-within-last-parent-node
                 ;; naive search, could be updated later
                 ;; this may detect wrong column-char with something like ⎡a(should().ignore().this)\n.b()\n.c()⎦
                 (save-excursion
                   (goto-char
                    (ts-node-start-byte last-parent-belonging-to))
                   (search-forward-regexp
                    (regexp-quote
                     (char-to-string
                      first-character-for-current-node))
                    (ts-node-end-byte current-node)
                    t)
                   (- (point) 1)))
                (end-of-parent-line-pos
                 (save-excursion
                   (goto-char
                    (ts-node-start-byte last-parent-belonging-to))
                   (line-end-position))))
      (when (and (numberp first-char-position-within-last-parent-node)
                 ;; char is within parent line
                 (or (< first-char-position-within-last-parent-node
                        end-of-parent-line-pos)
                     ;; char is the first in its line
                     (eq first-char-position-within-last-parent-node
                         (save-excursion
                           (goto-char
                            first-char-position-within-last-parent-node)
                           (back-to-indentation)
                           (point)))))
        ;; indent to column, which is (char-pos - line-begin-pos)
        (save-excursion
          (goto-char first-char-position-within-last-parent-node)
          (- first-char-position-within-last-parent-node
             (line-beginning-position)))))))

(defun tree-sitter-indent--first-sibling-column (current-node scopes parent-node)
  "Column position for CURRENT-NODE's first sibling.

If CURRENT-NODE belongs to the aligned-siblings group, will look up the first
sibling in same group \\(running through PARENT-NODE's children) and return
its column.

SCOPES is used to test whether CURRENT-NODE belongs to the aligned-siblings group."
  (when (and parent-node
             (tree-sitter-indent--node-is-aligned-sibling
              current-node scopes))
    (when-let* ((current-node-type
                 (ts-node-type current-node))
                (first-sibling
                 (cl-loop for ith-sibling = (ts-get-nth-child parent-node 0)
                          then (ts-get-next-sibling ith-sibling)
                          while (not (null ith-sibling))
                          if (equal current-node-type
                                    (ts-node-type ith-sibling))
                          return ith-sibling
                          end))
                (first-sibling-position
                 (ts-node-start-byte first-sibling))
                )
      (when (not (ts-node-eq current-node first-sibling))
        (save-excursion
          (goto-char first-sibling-position)
          (- first-sibling-position
             (line-beginning-position)))))))

;;; Extensible indentation instructions
;; Leverage eieio's dispatch to have an extensible framework to sort out an indent
;; given a certain context (information at a node position)

(defclass tree-sitter-indent--indent-from-context ()
  ((parentwise-path)
   (scopes)
   (original-column))
  "Get indentation command (an instruction `tree-sitter-indent--updated-column' can understand)
from current context.
If you want to add a new indent from a specific context, inherit from this class and define
a method for `tree-sitter-indent--indent-at-context'. In the inherited class, add
information from context that you need and in the method make the calculations if
such indentation order applies.

To change type priority (what type has higher priority) you should change the inheritance
tree, using this class as root.

Higher priority types are further from this class. The highest priority class accumulates all
information from each of the types."
  )

(cl-defgeneric tree-sitter-indent--step (obj)
  "Move OBJ to the next node."
  (set-slot-value obj current-node
                  (ts-get-next-sibling current-node))
  obj)

(cl-defgeneric tree-sitter-indent--indent-at-context (obj)
  "Each defmethod will be called on obj until one spits results

TODO: improve above text"
  )

;;;; no indent
(defclass tree-sitter-indent--no-indent-from-context (tree-sitter-indent--indent-from-context)
  ()
  "No indent from context, nothing to indent")

(cl-defmethod tree-sitter-indent--indent-at-context ((obj tree-sitter-indent--no-indent-from-context))
  'no-indent)

;;;; multi-line
(defclass tree-sitter-indent--multi-line (tree-sitter-indent--no-indent-from-context)
  ()
  "Current node is multi-line and we should preserve the original column")

(cl-defmethod tree-sitter-indent--indent-at-context ((obj tree-sitter-indent--no-indent-from-context))
  (with-slots (current-node scopes original-column) obj

    (when (let-alist scopes
            (member (ts-node-type current-node)
                    .multi-line-text))
      `(preserve . ,original-column))))

;;;; outdent-or-indent
(error "WIP: ↓ define new slots")
(defclass tree-sitter-indent--outdent-or-indent (tree-sitter-indent--indent-at-context)
  ()
  "Current node is multi-line and we should preserve the original column")

(cl-defmethod tree-sitter-indent--indent-at-context ((obj tree-sitter-indent--outdent-or-indent))
  (with-slots (current-node scopes original-column) obj
    (when (or current-node-must-indent
              (and parent-node
                   current-node-is-rest
                   (tree-sitter-indent--node-is-indent-rest
                    parent-node scopes))
              (and parent-node
                   current-node-is-middle-node
                   (tree-sitter-indent--node-is-indent-body
                    parent-node scopes))))
    ))

(cl-defun tree-sitter-indent--indents-in-path (parentwise-path scopes original-column)
  "Meh"
  (cl-loop indent-from-context = (tree-sitter-indent--indent-from-context)
           then (tree-sitter-indent--step indent-from-context)
           while (oref indent-context current-node)
           collect (tree-sitter-indent--indent-at-context indent-context)))



(cl-defun tree-sitter-indent--indents-in-path (parentwise-path scopes original-column)
  "Map PARENTWISE-PATH into indent instructions.

Each element of the returned list is one of the following

no-indent                         nothing to add to current column
indent                            add one indent to current column
outdent                           subtract one indent to current column
\(column-indent . COLUMN)         match parent's parent opener column
\(preserve . ORIGINAL-COLUMN)     preserve the column that was before

What is checked to add an indent:
- A node bolongs into the \"indent\" group in SCOPES
- Deterimen what group the node's parent belongs to, and whether the node
is in a middle position.
- A node belongs to the \"outdent\" group in SCOPES
- A node belongs to the \"column-indent\" group in SCOPES"
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
                                                       scopes)))
                (chain-column
                 (tree-sitter-indent--chain-column
                  current-node
                  (let-alist scopes
                    .align-char-to)
                  parentwise-path))
                (sibling-column
                 (tree-sitter-indent--first-sibling-column
                  current-node
                  scopes
                  parent-node)))
           (cond
            ((numberp chain-column)
             `(column-indent ,chain-column))
            ((numberp sibling-column)
             `(column-indent ,sibling-column))
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
               `(column-indent ,paren-indenting-column)))

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
    (`(column-indent ,paren-column)
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

See `tree-sitter-indent-line'.  ORIGINAL-COLUMN is forwarded to
`tree-sitter-indent--indents-in-path'"
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

Use in buffer like so:

\(setq-local indent-line-function #'tree-sitter-indent-line)."
  (let* ((current-buffer-indent-offset
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
             (point))))
         (new-column
          (tree-sitter-indent--indent-column current-buffer-indent-offset
                                             original-column
                                             (point))))
    (when (numberp new-column)
      (indent-line-to new-column))
    new-column))

(defun tree-sitter-debug-indent-line ()
  "Call `tree-sitter-indent-line' while printing useful info."
  (let* ((line-str (thing-at-point 'line))
         (position (point))
         (indenting-node (tree-sitter-indent--highest-node-at-position
                          position))
         (parentwise-path (tree-sitter-indent--parentwise-path indenting-node))
         (readable-parentwise-path
          (seq-map 'ts-node-type parentwise-path))
         (tree-sitter-tree-before (ts-tree-to-sexp tree-sitter-tree))
         (column
          (tree-sitter-indent-line)))
    (message "tree-sitter-indent: Indented ⎡%s⎦ to ⎡%s⎦ (col %d) because of parentwise path of ⎡%s⎦ (while looking at ⎡%s⎦ & when tree is ⎡%s⎦)"
             line-str
             (thing-at-point 'line)
             column
             readable-parentwise-path
             (ts-node-type indenting-node)
             tree-sitter-tree-before)))

(provide 'tree-sitter-indent)
;;; tree-sitter-indent.el ends here
