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
(require 'rust-mode)
(require 'dash)
(require 's)
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
         (indent-tabs-mode nil)
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
      `(nil . ,(format "\n🔴 Expected indented text to be \n%s\nbut it was indented to \n%s\n"
                       original-text
                       tree-sitter-indented-text)))))
;;;; Julia
(tree-sitter-require 'julia)
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
  (it "paren space"
    (display-warning 'buttercup  "space-within-a-parentheses treatment in Julia is not implemented"))
  (it "paren newlin"
    (display-warning 'buttercup "newline-within-a-parentheses treatment in Julia is not implemented"))
  (it "equals"
    (expect
     "
foo() =
    bar"
     :is-tree-sitter-indented))

  (it "operator"
    (display-warning 'buttercup "See https://github.com/tree-sitter/tree-sitter-julia/issues/9")
    ;;    (expect
    ;;     "
    ;;foo() |>
    ;;    bar |>
    ;;    baz
    ;;qux"
    ;;     :is-tree-sitter-indented)
    ;;
    )
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
  (it "after commented end"
    (expect
     "if foo
    a = 1
    #end
    b = 1
end"
     :is-tree-sitter-indented))
  (it "import, export, using"
    (expect
     "
export bar, baz,
    quux"
     :is-tree-sitter-indented)
    (expect
     "using Foo: bar ,
    baz,
    quux
notpartofit"
     :is-tree-sitter-indented)

    )
  (it "anonymous function"
    (display-warning 'buttercup "See https://github.com/tree-sitter/tree-sitter-julia/issues/12")
    ;;    (expect
    ;;     "function f(x)
    ;;    function(y)
    ;;        x+y
    ;;    end
    ;;end"
    ;;     :is-tree-sitter-indented)
    )
  (it "backlash indent"
    (expect
     "(\\)
1
(:\\)
1"
     :is-tree-sitter-indented))
  (it "ignore :end as block ending"
    (display-warning 'buttercup "See https://github.com/tree-sitter/tree-sitter-julia/issues/11")
    ;;    (expect
    ;;     "if a == :end
    ;;    r = 1
    ;;end"
    ;;     :is-tree-sitter-indented)
    (expect
     "if a == a[end-4:end]
    r = 1
end"
     :is-tree-sitter-indented))
  (it "indent hanging"
    (expect
     "
f(x) =
    x*
    x"
     :is-tree-sitter-indented)
    (display-warning 'buttercup "See https://github.com/tree-sitter/tree-sitter-julia/issues/9")
    ;;    (expect
    ;;     "
    ;;a = \"#\" |>
    ;;identity"
    ;;     :is-tree-sitter-indented)
    (expect
     "
a = \"#\" # |>
identity"
     :is-tree-sitter-indented))
  (it "issue 11"
    (expect
     "
function1(a, b, c
          d, e, f)"
     :is-tree-sitter-indented)
    (expect
     "
function2(
          a, b, c
          d, e, f)
" :is-tree-sitter-indented)
    (display-warning 'buttercup "See https://github.com/tree-sitter/tree-sitter-julia/issues/10")
    ;;     (expect
    ;;      "
    ;; for i in Float64[1, 2, 3, 4
    ;;                  5, 6, 7, 8]
    ;; end
    ;; " :is-tree-sitter-indented)
    (display-warning 'buttercup "See https://github.com/tree-sitter/tree-sitter-julia/issues/10")
    ;;    (expect
    ;;     "
    ;;for i in Float64[
    ;;                 1, 2, 3, 4
    ;;                 5, 6, 7, 8]
    ;;end
    ;;" :is-tree-sitter-indented)
    (display-warning 'buttercup "See https://github.com/tree-sitter/tree-sitter-julia/issues/12")
    ;;    (expect
    ;;     "
    ;;a = function3(function ()
    ;;              return 1
    ;;              end)
    ;;" :is-tree-sitter-indented)
    (display-warning 'buttercup "See https://github.com/tree-sitter/tree-sitter-julia/issues/12")
    ;;    (expect
    ;;     "
    ;;a = function4(
    ;;              function ()
    ;;              return 1
    ;;              end)
    ;;" :is-tree-sitter-indented)
    )
  (it "issue 11 comment"
    ;; https://github.com/JuliaEditorSupport/julia-emacs/issues/11#issuecomment-638907475
    "
hl = Highlighter((h,data,i,j)->begin
    id = div(i-1, 3)
    sim_id = id*num_cols + j
    if sims_status[sim_id] == 1
        return crayon\"black bg:green\"
    else
        return crayon\"bg:yellow\"
    end
"
    :is-tree-sitter-indented)
  (it "issue 127"
    ;; https://github.com/JuliaEditorSupport/julia-emacs/issues/127#issue-622145320
    ;; this was actually altered so that the inside of begin … end indent according
    ;; to the open parentheses
    (expect
     "
map(x->begin
        if a = 2
            b
        end
    end, v)
"
     :is-tree-sitter-indented
     ))
  (it "issue 111"
    ;; https://github.com/JuliaEditorSupport/julia-emacs/issues/111#issue-586904814
    ;; this was actually altered so that the inside of (…) will match the parentheses column
    (expect
     "
f(
  map(1:3) do x
      x
  end
  )
"
     :is-tree-sitter-indented
     ))
  (it "multi-line λ"
    ;; https://github.com/JuliaEditorSupport/julia-emacs/issues/73
    (expect
     "
lines=[]
Channel(c->begin
            for l in lines
                for w in split(l)
                    put!(c, w)
                end
                @show l
            end
        end)"
     :is-tree-sitter-indented)
    (expect
     "
Channel(
        c->begin
              if true
                  push!(c, \"true\")
              end
        end)
"
     :is-tree-sitter-indented)
    (expect
     "
Channel(c->begin
              if true
                  push!(c, \"true\")
              end
        end)
"
     :is-tree-sitter-indented)

    )
  )

;;;; rust
(tree-sitter-require 'rust)
(describe "Rust"
  (before-all
    (setq rust-indent-offset 4
          tree-sitter-indent-tests--current-major-mode 'rust-mode))
  (it "issue 126"
    ;; https://github.com/rust-lang/rust-mode/issues/126
    (expect
     "
fn main() {
    for x in foo.iter()
                .map(|x| x * 2) {
        let y = 22; // I expect this to be aligned with the `for`!
    }
}
" :is-tree-sitter-indented))
  (it "issue 299"
    ;; https://github.com/rust-lang/rust-mode/issues/299
    (expect
     "
impl <A, D> MyTrait<A, D> for YourType
    where A: TraitB + TraitC,
          D: TraitE + TraitF {}

#[test]
//
"
     :is-tree-sitter-indented)))

(provide 'tree-sitter-indent-tests)
;;; tree-sitter-indent-tests.el ends here
