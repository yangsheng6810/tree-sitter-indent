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

So far, only rust language can be indented. Add the following to your `init.el`

```elisp
;; install tree-sitter
;; …

;; install tree-sitter-indent
;; …

(tree-sitter-require 'rust)
(require 'tree-sitter-indent)

(add-hook 'rust-mode-hook #'tree-sitter-indent-mode)
```

## Adding languages or fixing current

Currently, julia is work-in-progress as the Tree-sitter implementation is not done. Rust was actually the second language being worked on, which involved a more mature development, so you may want to check those commits.

The best way to work through a language is to add the targets you want to get ("I want this code to be indented like this") as a unit test. And then add code as needed.

PRs are welcome.

### Running tests

To run tests, execute the following
```zsh
make
```
This will byte-compile elisp files and run tests. Byte-compiler warnings will be treated as errors, so make sure to cover them.

Internally, `make` will fetch all dependencies using `straight.el` and run.

### Code of Conduct

Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms.
