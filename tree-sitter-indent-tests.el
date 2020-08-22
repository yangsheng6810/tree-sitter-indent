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

;;;; Julia
;;; These were taken from https://github.com/JuliaEditorSupport/julia-emacs/blob/master/julia-mode-tests.el

(defsubst tree-sitter-indent-tests--indent-julia (code)
  (with-temp-buffer
    (let ((julia-indent-offset 4)
          (indent-region-function 'lisp-indent-region))
      (insert code)
      (julia-mode)
      (tree-sitter-mode)
      (setq-local indent-line-function #'tree-sitter-indent-line)
      (indent-region (point-min) (point-max))
      (buffer-substring-no-properties (point-min) (point-max)))))

(describe "Julia"
  ;; Tests here were taken from julia-mode-tests.el
  (it "if"
    (expect
     "
if foo
bar
end"
     :to-equal
     (tree-sitter-indent-tests--indent-julia
      "
if foo
    bar
end")))
  (it "else"
    (expect
     "
if foo
    bar
else
baz
end"
     :to-equal
     (tree-sitter-indent-tests--indent-julia
      "
if foo
    bar
else
    baz
end")
     )
    )
  )
;; TODO https://github.com/JuliaEditorSupport/julia-emacs/issues/11

(provide 'tree-sitter-indent-tests)
;;; tree-sitter-indent-tests.el ends here
