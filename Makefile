# canvas-graph -- graphs of boxes and arrows on an Emacs 32 canvas,
# built on canvas-diagram.

EMACS   ?= emacs
DIAGRAM ?= ../canvas-diagram
KEYS    ?= ../canvas-keys

.PHONY: test module clean

test: module
	$(EMACS) -Q --batch -L . -L $(DIAGRAM) -L $(KEYS) -L tests \
	  -l tests/canvas-graph-tests.el \
	  -l tests/canvas-graph-dot-tests.el \
	  --eval '(ert-run-tests-batch-and-exit)'

# The cairo and pango module lives with canvas-diagram.
module:
	$(MAKE) -C $(DIAGRAM)

clean:
	rm -f *.elc tests/*.elc

# The layouts of a graph shown in a real window, with the real keys, on a
# private display.
.PHONY: layout-acceptance
layout-acceptance: module
	timeout 120 xvfb-run -a -s "-screen 0 1400x900x24" $(EMACS) -Q \
	  --eval '(setq load-prefer-newer t)' -L . -L $(DIAGRAM) -L $(KEYS) -L tests \
	  -l tests/canvas-graph-layout-acceptance.el --eval '(canvas-graph-layout-acceptance-run)'

# The background of a frame in a real theme, written with the real
# command, on a private display.
.PHONY: tint-acceptance
tint-acceptance: module
	timeout 120 xvfb-run -a -s "-screen 0 1400x900x24" $(EMACS) -Q \
	  --eval '(setq load-prefer-newer t)' -L . -L $(DIAGRAM) -L $(KEYS) -L tests \
	  -l tests/canvas-graph-tint-acceptance.el --eval '(canvas-graph-tint-acceptance-run)'

# The dot layout of graphviz in a real window, on a private display.  A
# test that needs the program dot skips itself without it.
.PHONY: dot-acceptance
dot-acceptance: module
	timeout 180 xvfb-run -a -s "-screen 0 1400x900x24" $(EMACS) -Q \
	  --eval '(setq load-prefer-newer t)' -L . -L $(DIAGRAM) -L $(KEYS) -L tests \
	  -l tests/canvas-graph-dot-acceptance.el --eval '(canvas-graph-dot-acceptance-run)'
