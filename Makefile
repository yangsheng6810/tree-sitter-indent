# taken from https://git.launchpad.net/global-tags.el
# (which was actually based on https://github.com/abo-abo/tiny/blob/master/Makefile & http://sachachua.com/blog/2015/02/continuous-integration-code-coverage-emacs-packages-travis-coveralls/
EMACS ?= emacs
BEMACS = $(EMACS) -Q -batch

all: bytec test

test:
	LC_ALL=C $(BEMACS) \
	       -l setup-tests.el \
	       -l tree-sitter-indent.el \
	       -l tree-sitter-indent-tests.el \
	       -f buttercup-run
bytec:
	LC_ALL=C $(BEMACS) --eval '(byte-recompile-directory "./")'


.PHONY:	all test
