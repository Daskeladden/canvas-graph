# canvas-graph -- graphs of boxes and arrows on an Emacs 32 canvas,
# built on canvas-diagram.

EMACS   ?= emacs
DIAGRAM ?= ../canvas-diagram
KEYS    ?= ../canvas-keys

.PHONY: test module clean

test: module
	$(EMACS) -Q --batch -L . -L $(DIAGRAM) -L $(KEYS) -L tests \
	  -l tests/canvas-graph-tests.el \
	  --eval '(ert-run-tests-batch-and-exit)'

# The cairo and pango module lives with canvas-diagram.
module:
	$(MAKE) -C $(DIAGRAM)

clean:
	rm -f *.elc tests/*.elc
