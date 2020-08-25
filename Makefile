# taken from https://git.launchpad.net/global-tags.el
# (which was actually based on https://github.com/abo-abo/tiny/blob/master/Makefile & http://sachachua.com/blog/2015/02/continuous-integration-code-coverage-emacs-packages-travis-coveralls/
EMACS ?= emacs
BEMACS = $(EMACS) -Q -batch

ELISP_SOURCES=$(wildcard *.el)
ELISP_BYTECOMPILED=$(patsubst %.el,%.elc,$(ELISP_SOURCES))

all: bytec test

test:
	LC_ALL=C $(BEMACS) \
	       -l setup-tests.el \
	       -l tree-sitter-indent.el \
	       -l tree-sitter-indent-tests.el \
	       -f buttercup-run

bytec: $(ELISP_BYTECOMPILED)


%.elc: %.el
	LC_ALL=C $(BEMACS) \
	       -l setup-tests.el \
		-L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $<

.PHONY:	all test
