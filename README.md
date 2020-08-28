THIS PACKAGE IS CURRENTLY IN α
You can try it to use it (all tests pass), but it has not been thoroughly tested by end users
So it's currently targeted to developers

# tree-sitter-indent.el

Use Tree-sitter as backend to source code indentation.

Provide an `indent-line-function` that resolves the indentation by looking at the syntax tree using the emacs-tree-sitter package.

## Installation

First, install emacs-tree-sitter. I recommend [using `straight.el`](https://ubolonton.github.io/emacs-tree-sitter/installation/#installing-with-straight-dot-el)

```elisp
(straight-use-package
 '(tree-sitter :host github
               :repo "ubolonton/emacs-tree-sitter"
               :files ("lisp/*.el")))

(straight-use-package
 '(tree-sitter-langs :host github
                     :repo "ubolonton/emacs-tree-sitter"
                     :files ("langs/*.el" "langs/queries")))

;; Load the language bundle

(require 'tree-sitter-langs)

```

Then, install `tree-sitter-indent`. Recommended way is using `straight.el` (no (M)ELPA package currently available).

```elisp
(straight-use-package
 '(tree-sitter-indent :type git
                      :repo "https://codeberg.org/FelipeLema/tree-sitter-indent.el.git"
                      :branch "main"
                      :files ("tree-sitter-indent.el")))
```

## Usage

So far, only julia language can be indented. Add the following to your `init.el`

```elisp
;; install tree-sitter
;; …

;; install tree-sitter-indent
;; …

(tree-sitter-require 'julia)
(require 'tree-sitter-indent)

(add-hook 'julia-mode-hook
          (lambda ()
            (tree-sitter-mode)
            (setq-local indent-line-function #'tree-sitter-indent-line))
```

## Adding languages or fixing current

TODO

### Running tests

To run tests, execute the following
```zsh
make
```
This will byte-compile elisp files and run tests. Byte-compiler warnings will be treated as errors, so make sure to cover them.

Internally, `make` will fetch all dependencies using `straight.el` and run.

TODO: expand
