;;; tree-sitter-indent-tests.el --- tests for tree-sitter-indent.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Felipe Lema

;; Author: Felipe Lema <felipel@debian>
;; Keywords:

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

;;

;;; Code:
(require 'buttercup)
(require 'julia-mode)
(require 'dash)
(require 's)
(tree-sitter-require 'julia)
(require 'tree-sitter-indent)

;;;; helpers
;; http://www.modernemacs.com/post/testing-emacs/
(defun tree-sitter-indent-tests--unindent (code-text)
  "Return CODE-TEXT with all indent from it."
  (->> code-text
       (s-split "\n")
       (--map
        (s-replace-regexp (rx line-start (* (any space)))
                          "" it))
       (s-join "\n")))

(defvar tree-sitter-indent-tests--current-major-mode
  nil
  "Current major mode to use for indenting function.")

(buttercup-define-matcher :is-tree-sitter-indented (code-text)
  (let* ((original-text (s-trim (funcall code-text)))
         (text-no-indent (tree-sitter-indent-tests--unindent original-text))
         (tree-sitter-indented-text
          (with-temp-buffer
            ;; setup artificial buffer
            (insert text-no-indent)
            (funcall tree-sitter-indent-tests--current-major-mode)

            ;; setup indent using tree-sitter
            (tree-sitter-mode)
            (setq-local indent-line-function #'tree-sitter-indent-line)
            (indent-region-line-by-line (point-min) (point-max))

            ;; get buffer string
            (buffer-substring-no-properties (point-min) (point-max)))))
    (if (s-equals? original-text
                   tree-sitter-indented-text)
        t
      `(nil . ,(format "Expected indented text to be \n%s\nbut it was indented to \n%s\n"
                       original-text
                       tree-sitter-indented-text)))))
;;;; Julia
;;; These were taken from https://github.com/JuliaEditorSupport/julia-emacs/blob/master/julia-mode-tests.el



(describe "Julia"
  (before-all
    (setq julia-indent-offset 4
          tree-sitter-indent-tests--current-major-mode 'julia-mode))
  ;; Tests here were taken from julia-mode-tests.el
  (it "if"
    (expect
     "
if foo
    bar
end" :is-tree-sitter-indented))
  (it "else"
    (expect
     "
if foo
    bar
else
    baz
end" :is-tree-sitter-indented)
    )
  (it "toplevel"
    (expect
     "
foo()
bar()" :is-tree-sitter-indented))
  (it "nested if"
    (expect
     "
if foo
    if bar
        bar
    end
end"
     :is-tree-sitter-indented))
  ;;  (it "module keyword should not indent"
  ;;    (expect
  ;;     "
  ;;module
  ;;begin # ← are we supposed to use `begin` after declaring a module?
  ;;    a = 1
  ;;end
  ;;end"
  ;;
  ;;     :is-tree-sitter-indented))
  (it "module keyword should not outdent"
    (expect
     "
begin
    module
    foo
    end
end"
     :is-tree-sitter-indented))
  (it "function bodies indent"
    (expect
     "
function foo()
    bar
end"
     :is-tree-sitter-indented))
  (it "begin keyword"
    (expect
     "
@async begin
    bar
end"
     :is-tree-sitter-indented))
  (it "paren"
    (expect "
foobar(bar,
       baz)"
            :is-tree-sitter-indented))
  ;; TODO julia--test-indent-paren-space
  ;; TODO julia--test-indent-paren-newline
  (it "equals"
    (expect
     "
foo() =
    bar"
     :is-tree-sitter-indented))

  (it "operator"
    ;; TODO ↓ does not parse
    ;;    (expect
    ;;     "
    ;;foo() |>
    ;;    bar |>
    ;;    baz
    ;;qux"
    ;;     :is-tree-sitter-indented)
    ;;
    (expect
     "x \\
    y \\
    z"
     :is-tree-sitter-indented))
  (it "ignores blank lines"
    (expect
     "
if foo

    bar
end"
     :is-tree-sitter-indented))
  (it "comment equal"
    (expect
     "
# a =
# b =
c" :is-tree-sitter-indented ))
  (it "leading paren"
    (expect
     "
\(1)"
     :is-tree-sitter-indented))
  ;; TODO julia--test-top-level-following-paren-indent
  (it "multi-line strings"
    ;; this is actually a translation of julia--test-indentation-of-multi-line-strings
    ;; since we cannot read within a string comment
    "a = \"\"\"
    description
begin
    foo
bar
end
\"\"\"" :is-tree-sitter-indented)
  ;; TODO julia--test-indent-of-end-in-brackets has double begin (indented the same)
  (it "after commented keyword"
    (expect
     "# if foo
a = 1"
     :is-tree-sitter-indented)
    )

  )
;; TODO https://github.com/JuliaEditorSupport/julia-emacs/issues/11

(provide 'tree-sitter-indent-tests)
;;; tree-sitter-indent-tests.el ends here
