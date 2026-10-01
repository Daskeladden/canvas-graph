;;; canvas-graph-tests.el --- tests -*- lexical-binding: t -*-
(require 'ert)
(require 'cl-lib)
(require 'lisp-mnt)
(require 'package)
(require 'canvas-graph)

;; The canvas, the boxes, the keys and the mouse are canvas-diagram's,
;; tested there.  These tests cover the graph: building it from a spec,
;; its layouts, its arrows and labels, and how the keyboard walks it.

;;;; Helpers

(defconst canvas-graph-test--sample
  '(:name "state" :type "state_t" :start "IDLE"
    :nodes (("IDLE") ("RUN") ("WAIT_ACK") ("ERROR") ("FINISH") ("LOST"))
    :edges (("IDLE" "RUN" "start = '1'")
            ("RUN" "WAIT_ACK" "done = '1'")
            ("RUN" "ERROR" "err = '1'")
            ("RUN" "IDLE" "abort = '1'")
            ("WAIT_ACK" "FINISH" "count <= 3")
            ("ERROR" "IDLE")
            ("LOST" "IDLE")
            ("FINISH" "IDLE")))
  "A state machine, with a state nobody reaches.")

(defun canvas-graph-test--spec (nodes edges)
  "A spec of NODES, names, and EDGES, (FROM TO [LABEL]) each."
  (list :name "graph" :type "graph_t" :nodes (mapcar #'list nodes) :edges edges))

(defun canvas-graph-test--canvas (w h)
  "A fresh W x H canvas spec."
  (list 'image :type 'canvas :id (make-symbol "test-canvas")
        :data-width w :data-height h))

(defmacro canvas-graph-test--with-context (var w h &rest body)
  "Run BODY with VAR bound to a context on a fresh W x H canvas."
  (declare (indent 3))
  `(let ((,var (canvas-cairo-context (canvas-graph-test--canvas ,w ,h))))
     (unwind-protect (progn ,@body)
       (canvas-cairo-destroy ,var))))

(defmacro canvas-graph-test--rendering (&rest body)
  "Run BODY with fixed font, colours and layout, so pixels are predictable."
  `(let ((canvas-diagram-font "Sans 12px")
         (canvas-diagram-margin 10)
         (canvas-diagram-colors '(:background "white" :node "blue"
                                  :text "black" :edge "red"))
         (canvas-diagram-palettes '(("derived") ("test" "blue" "green" "yellow" "cyan" "magenta")))
         (canvas-diagram-palette "test")
         (canvas-diagram-shape 'square)
         (canvas-diagram-spacing 'normal)
         (canvas-diagram-family nil)
         (canvas-diagram-show-kinds nil)
         (canvas-diagram-show-legend nil)
         (canvas-diagram-show-icons nil)
         (canvas-diagram-paper nil)
         (canvas-graph-layout 'layered)
         (canvas-graph-fold-common t)
         (canvas-graph-label-width 220)
         (canvas-graph-show-labels nil))
     ,@body))

(defun canvas-graph-test--laid-out (spec ctx &optional diagram)
  "A graph diagram, DIAGRAM or a plain one, built from SPEC and laid out on CTX."
  (let ((diagram (or diagram (canvas-graph-diagram))))
    (setf (canvas-diagram-spec diagram) spec
          (canvas-diagram-model diagram) (canvas-diagram--call diagram :build diagram spec)
          (canvas-diagram-nodes diagram) (canvas-graph--lay-out diagram ctx))
    diagram))

(defmacro canvas-graph-test--in-buffer (spec size &rest body)
  "Run BODY in a diagram buffer showing SPEC on a canvas of SIZE."
  (declare (indent 2))
  `(canvas-graph-test--rendering
    (with-temp-buffer
      (canvas-graph-mode)
      (canvas-diagram-adopt (canvas-graph-diagram) ,spec)
      (plist-put (cdr canvas-diagram--canvas) :data-width (car ,size))
      (plist-put (cdr canvas-diagram--canvas) :data-height (cdr ,size))
      (unwind-protect (progn ,@body)
        (canvas-diagram--release)))))

(defun canvas-graph-test--node (label)
  "The node called LABEL in this buffer's graph."
  (canvas-graph-node-named (canvas-diagram-current-model) label))

(defun canvas-graph-test--selected ()
  "The label of the selected node."
  (canvas-diagram-node-label (canvas-diagram-selected)))

(defun canvas-graph-test--reddish-p (argb)
  "Whether ARGB is red ink, however thinly laid over white."
  (and (= (logand (ash argb -16) #xFF) #xFF)
       (< (logand (ash argb -8) #xFF) #x80)
       (< (logand argb #xFF) #x80)))

(defun canvas-graph-test--dark-p (argb)
  "Whether ARGB is ink: every channel below a quarter of full."
  (and (< (logand (ash argb -16) #xFF) #x40)
       (< (logand (ash argb -8) #xFF) #x40)
       (< (logand argb #xFF) #x40)))

(defun canvas-graph-test--png-p (file)
  "Whether FILE starts with the PNG signature."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 8)
    (equal (buffer-string) "\x89PNG\r\n\x1a\n")))

(defun canvas-graph-test--apart-p (a b)
  "Whether rects A and B, (X Y W H), do not overlap."
  (or (<= (+ (nth 0 a) (nth 2 a)) (nth 0 b)) (<= (+ (nth 0 b) (nth 2 b)) (nth 0 a))
      (<= (+ (nth 1 a) (nth 3 a)) (nth 1 b)) (<= (+ (nth 1 b) (nth 3 b)) (nth 1 a))))

;;;; Building the graph

(ert-deftest canvas-graph-build-measures-depth-from-the-start ()
  ;; GIVEN the sample's spec, whose start is its first node
  ;; WHEN the graph is built
  ;; THEN the start is that node, each node's depth is its distance from
  ;;      it, the unreachable one has none, AND both are kinds
  (let* ((graph (canvas-graph-build canvas-graph-test--sample))
         (nodes (canvas-graph-nodes graph)))
    (should (eq (canvas-graph-start graph) (car nodes)))
    (should (equal (canvas-graph-name graph) "state"))
    (should (equal (canvas-graph-type graph) "state_t"))
    (should (equal (mapcar #'canvas-graph-node-depth nodes) '(0 1 2 2 3 nil)))
    (should (equal (canvas-diagram-node-kind (car nodes)) "start"))
    (should (equal (canvas-diagram-node-kind (car (last nodes))) "unreachable"))
    (should-not (canvas-diagram-node-kind (cadr nodes)))
    (should (canvas-diagram-node-p (car nodes)))))

(ert-deftest canvas-graph-build-merges-edges-and-links-nodes ()
  ;; GIVEN two edges to the same target under different labels, and a self loop
  ;; WHEN the graph is built
  ;; THEN there is one edge with both labels, the nodes know their edges in
  ;;      and out, AND the loop is an edge from a node to itself
  (let* ((graph (canvas-graph-build
                 (canvas-graph-test--spec '("A" "B") '(("A" "B" "p") ("A" "B" "q") ("A" "A" "w") ("B" "A")))))
         (edges (canvas-graph-edges graph))
         (a (canvas-graph-node-named graph "A"))
         (b (canvas-graph-node-named graph "B")))
    (should (= (length edges) 3))
    (let ((ab (car edges)))
      (should (eq (canvas-graph-edge-from ab) a))
      (should (eq (canvas-graph-edge-to ab) b))
      (should (equal (canvas-graph-edge-labels ab) '("p" "q"))))
    (should (= (length (canvas-graph-node-out a)) 2))
    (should (= (length (canvas-graph-node-in a)) 2))
    (should (cl-some #'canvas-graph--loop-p edges))))

(ert-deftest canvas-graph-a-spec-is-checked-before-it-is-built ()
  ;; GIVEN specs with an edge to an unknown node, a start that names no
  ;;       node, a key a spec has not, a node option a node has not, and
  ;;       no nodes at all; one carrying data of its reader's; and one
  ;;       without a start, whose nodes have places
  ;; WHEN each is built
  ;; THEN each of the first five is an error, not a graph drawn wrong, the
  ;;      reader's data is let be, AND the last starts at its first node,
  ;;      which keeps its place
  (should-error (canvas-graph-build (canvas-graph-test--spec '("A") '(("A" "B")))))
  (should-error (canvas-graph-build (append '(:start "Z") (canvas-graph-test--spec '("A") nil))))
  (should-error (canvas-graph-build '(:name "g" :nodes (("A")) :states (("A")))))
  (should-error (canvas-graph-build '(:name "g" :nodes (("A" :colour "red")))))
  (should-error (canvas-graph-build '(:name "g" :nodes nil :edges (("A" "B")))))
  (should (canvas-graph-build '(:name "g" :nodes (("A")) :data (:kind mealy :anything "the reader keeps"))))
  (let ((graph (canvas-graph-build '(:name "g" :nodes (("A" :pos 7) ("B")) :edges (("A" "B"))))))
    (should (equal (canvas-diagram-node-label (canvas-graph-start graph)) "A"))
    (should (= (canvas-diagram-node-pos (canvas-graph-start graph)) 7))))

(ert-deftest canvas-graph-a-node-can-show-a-label-of-its-own ()
  ;; GIVEN a spec naming its nodes by ids, two of them labelled alike
  ;; WHEN the graph is built and a diagram of it keys and restores a node
  ;; THEN the edges join the nodes by name, each node shows its label, the
  ;;      two alike stay two, a node without a label shows its name, the
  ;;      header and the card speak of labels, AND the diagram keys a node
  ;;      by its name and finds it again by that
  (let* ((graph (canvas-graph-build '(:name "g" :nodes (("a" :label "Retry") ("b" :label "Retry") ("c"))
                                      :edges (("a" "b" "again") ("b" "c")))))
         (a (canvas-graph-node-named graph "a"))
         (b (canvas-graph-node-named graph "b"))
         (diagram (canvas-graph-diagram)))
    (should (= (length (canvas-graph-nodes graph)) 3))
    (should (equal (mapcar #'canvas-diagram-node-label (canvas-graph-nodes graph)) '("Retry" "Retry" "c")))
    (should (equal (canvas-graph-node-id a) "a"))
    (should (eq (canvas-graph-edge-to (car (canvas-graph-node-out a))) b))
    (should (equal (canvas-graph-header graph b) "g › Retry · 1 out, 1 in"))
    (should (equal (canvas-graph-card graph a) '("Retry" "g" "→ Retry  when again")))
    (setf (canvas-diagram-model diagram) graph)
    (should (equal (canvas-diagram--call diagram :node-key diagram b) "b"))
    (should (eq (canvas-diagram--call diagram :restore diagram "b") b))))

(ert-deftest canvas-graph-a-reader-writes-a-draft-and-gets-a-spec ()
  ;; GIVEN a reader noting a start, a node with a label and then a better
  ;;       one, edges that meet their nodes first, one from any node, and a
  ;;       second start
  ;; WHEN the draft is made a spec
  ;; THEN the nodes come in the order first met, at the place first met,
  ;;      with the last label given, the edges in order, the first start,
  ;;      the name and type given, AND the spec builds
  (let ((draft (canvas-graph-draft-create)))
    (canvas-graph-draft-start-at draft "s" 1)
    (canvas-graph-draft-node draft "a" "first" 5)
    (canvas-graph-draft-edge draft "s" "a" "go" 9)
    (canvas-graph-draft-edge draft "a" "b" nil 12)
    (canvas-graph-draft-node draft "a" "Alpha" 20)
    (canvas-graph-draft-edge draft nil "s" "reset" 30)
    (canvas-graph-draft-start-at draft "b" 40)
    (let ((spec (canvas-graph-draft-spec draft "flow" "chart")))
      (should (equal spec '(:name "flow" :type "chart" :start "s"
                            :nodes (("s" :pos 1) ("a" :label "Alpha" :pos 5) ("b" :pos 12))
                            :edges (("s" "a" "go" 9) ("a" "b" nil 12) (nil "s" "reset" 30)))))
      (should (canvas-graph-build spec)))))

(ert-deftest canvas-graph-a-state-machine-s-pseudo-states-become-its-start-and-end ()
  ;; GIVEN transitions from and to the pseudo-states, nil, at the top and
  ;;       inside a composite state, and between two states
  ;; WHEN they are noted in a draft
  ;; THEN the first from the initial one at the top makes the start, one
  ;;      inside a composite leads from the composite, one to the final one
  ;;      at the top leads to a node labelled end, one inside is left out,
  ;;      AND the rest are edges as written
  (let ((draft (canvas-graph-draft-create)))
    (canvas-graph-draft-transition draft nil "Idle" nil 1)
    (canvas-graph-draft-transition draft "Idle" "Busy" "go" 2)
    (canvas-graph-draft-transition draft nil "Inner" nil 3 "Busy")
    (canvas-graph-draft-transition draft "Inner" nil nil 4 "Busy")
    (canvas-graph-draft-transition draft "Busy" nil "done" 5)
    (should (equal (canvas-graph-draft-spec draft "m")
                   '(:name "m" :start "Idle"
                     :nodes (("Idle" :pos 1) ("Busy" :pos 2) ("Inner" :pos 3) ("[*]" :label "end" :pos 5))
                     :edges (("Idle" "Busy" "go" 2) ("Busy" "Inner" nil 3) ("Busy" "[*]" "done" 5)))))))

(ert-deftest canvas-graph-a-style-speaks-for-its-reader ()
  ;; GIVEN a style calling the start reset and an edge from anywhere any
  ;;       state, marking labels up in bold and cutting them to their
  ;;       first word, and the sample with an edge from any node added
  ;; WHEN a graph is built with it, and one without
  ;; THEN the styled start is of kind reset and heads the legend, the
  ;;      marker says any state, an arrow's label is cut and a label
  ;;      measured as marked up; the plain graph says start and any node
  ;;      and marks up only what pango would misread, AND a style key
  ;;      nobody knows is an error
  (let* ((style (list :start "reset" :any "any state"
                      :markup (lambda (text) (concat "<b>" (canvas-diagram-markup-escape text) "</b>"))
                      :shorten (lambda (text) (car (split-string text " ")))))
         (spec (plist-put (copy-sequence canvas-graph-test--sample) :edges
                          (cons '(nil "LOST" "hdr = '1'") (plist-get canvas-graph-test--sample :edges))))
         (styled (canvas-graph-build spec style))
         (plain (canvas-graph-build spec))
         (group (car (canvas-graph--common-groups (progn (canvas-graph--mark-common styled) styled)))))
    (should (equal (canvas-diagram-node-kind (canvas-graph-start styled)) "reset"))
    (should (equal (car (car (canvas-graph--legend-entries styled))) "reset"))
    (should (equal (canvas-graph--marker-label styled group) "any state · hdr"))
    (should (equal (canvas-graph--label styled (cadr (canvas-graph-edges styled))) "start"))
    (should (equal (canvas-graph--markup styled "a < b") "<b>a &lt; b</b>"))
    (should (equal (canvas-diagram-node-kind (canvas-graph-start plain)) "start"))
    (should (equal (car (car (canvas-graph--legend-entries plain))) "start"))
    (canvas-graph--mark-common plain)
    (should (equal (canvas-graph--marker-label plain (car (canvas-graph--common-groups plain)))
                   "any node · hdr = '1'"))
    (should (equal (canvas-graph--markup plain "a < b") "a &lt; b"))
    (should-error (canvas-graph-build spec '(:colour "red")))))

(ert-deftest canvas-graph-labels-go-where-the-reader-would-have-them ()
  ;; GIVEN the setting as it comes, a graph of the default style, and one
  ;;       whose style keeps its labels for the panel
  ;; WHEN the labels shown are asked for on auto and on every other value,
  ;;      and both graphs are laid out on auto
  ;; THEN auto is the default, on auto the default style labels every
  ;;      arrow and the other only the selected node's, through the panel;
  ;;      any other value wins over both, AND an export labels every arrow
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((spec (canvas-graph-test--spec '("A" "B") '(("A" "B" "go"))))
            (plain (canvas-graph-build spec))
            (panel-style '(:labels selected))
            (panel (canvas-graph-build spec panel-style))
            (canvas-graph-show-labels 'auto)
            (laid-out (lambda (diagram) (car (canvas-graph-edges (canvas-diagram-model (canvas-graph-test--laid-out spec ctx diagram)))))))
       (should (eq (eval (car (get 'canvas-graph-show-labels 'standard-value))) 'auto))
       (should (eq (canvas-graph--labels plain) 'all))
       (should (eq (canvas-graph--labels panel) 'selected))
       (dolist (value '(selected all nil))
         (let ((canvas-graph-show-labels value))
           (should (eq (canvas-graph--labels plain) value))
           (should (eq (canvas-graph--labels panel) value))))
       (should (canvas-graph-edge-label-rect (funcall laid-out nil)))
       (should-not (canvas-graph-edge-label-rect
                    (funcall laid-out (canvas-graph-diagram (list :build (lambda (_diagram spec) (canvas-graph-build spec panel-style)))))))
       (should (eq (canvas-graph--exported-labels) 'all))))))

(ert-deftest canvas-graph-long-labels-are-cut-for-the-card-to-keep ()
  ;; GIVEN an edge under four long labels
  ;; WHEN its label and its card line are made
  ;; THEN the label lists three, one per line, each cut at the limit with
  ;;      an ellipsis, then says how many more, AND the card has all four
  (let* ((long "a_condition_that_is_long = '1' and another_long_one = '0' and a_third_one_still = '1'")
         (graph (canvas-graph-build
                 (canvas-graph-test--spec '("A" "B")
                                          (list (list "A" "B" long) (list "A" "B" (concat "or_" long))
                                                (list "A" "B" (concat "yet_" long)) (list "A" "B" "last = '1'")))))
         (edge (car (canvas-graph-edges graph)))
         (canvas-graph-label-max 60)
         (canvas-graph-label-lines 3)
         (lines (split-string (canvas-graph--label graph edge) "\n")))
    (should (= (length lines) 4))
    (dolist (line (butlast lines))
      (should (<= (length line) 61))
      (should (string-suffix-p "…" line)))
    (should (equal (car (last lines)) "… and 1 more"))
    (should (string-prefix-p (substring long 0 30) (car lines)))
    (let ((card (nth 2 (canvas-graph-card graph (canvas-graph-node-named graph "A")))))
      (should (string-search "yet_" card))
      (should (string-search "last = '1'" card)))))

;;;; The layouts

(ert-deftest canvas-graph-ring-puts-the-nodes-on-a-circle ()
  ;; GIVEN six nodes with no two-way edges and no loops
  ;; WHEN they are laid out as a ring
  ;; THEN every centre is the same distance from their common centre,
  ;;      no two boxes overlap, the drawing starts at the origin, AND the
  ;;      nodes come back in declaration order
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((canvas-graph-layout 'ring)
            (diagram (canvas-graph-test--laid-out
                      (canvas-graph-test--spec '("A" "B" "C" "D" "E" "F")
                                               '(("A" "B") ("B" "C") ("C" "D") ("D" "E") ("E" "F") ("F" "A")))
                      ctx))
            (nodes (canvas-diagram-nodes diagram))
            (cx (/ (apply #'+ (mapcar #'canvas-diagram-middle-x nodes)) 6.0))
            (cy (/ (apply #'+ (mapcar #'canvas-diagram-middle-y nodes)) 6.0))
            (radii (mapcar (lambda (n) (sqrt (+ (expt (- (canvas-diagram-middle-x n) cx) 2)
                                                (expt (- (canvas-diagram-middle-y n) cy) 2))))
                           nodes)))
       (should (equal (mapcar #'canvas-diagram-node-label nodes) '("A" "B" "C" "D" "E" "F")))
       (dolist (r radii) (should (< (abs (- r (car radii))) 0.5)))
       (should (> (car radii) 40))
       (should (equal (canvas-diagram-slack diagram) '(0 . 0)))
       (should (< (apply #'min (mapcar #'canvas-diagram-node-x nodes)) 0.5))
       (should (< (apply #'min (mapcar #'canvas-diagram-node-y nodes)) 0.5))
       (cl-loop for (a . rest) on nodes
                do (dolist (b rest)
                     (should (or (>= (canvas-diagram-node-x b) (+ (canvas-diagram-node-x a) (canvas-diagram-node-w a)))
                                 (>= (canvas-diagram-node-x a) (+ (canvas-diagram-node-x b) (canvas-diagram-node-w b)))
                                 (>= (canvas-diagram-node-y b) (+ (canvas-diagram-node-y a) (canvas-diagram-node-h a)))
                                 (>= (canvas-diagram-node-y a) (+ (canvas-diagram-node-y b) (canvas-diagram-node-h b)))))))))))

(ert-deftest canvas-graph-layered-puts-each-depth-in-a-row ()
  ;; GIVEN the sample
  ;; WHEN it is laid out in layers
  ;; THEN y grows with depth, nodes of one depth share a row and sit
  ;;      apart in it, the unreachable node is in the last row,
  ;;      AND the nodes come back row by row
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((diagram (canvas-graph-test--laid-out canvas-graph-test--sample ctx))
            (graph (canvas-diagram-model diagram))
            (at (lambda (name) (canvas-graph-node-named graph name))))
       (should (< (canvas-diagram-middle-y (funcall at "IDLE")) (canvas-diagram-middle-y (funcall at "RUN"))
                  (canvas-diagram-middle-y (funcall at "WAIT_ACK")) (canvas-diagram-middle-y (funcall at "FINISH"))
                  (canvas-diagram-middle-y (funcall at "LOST"))))
       (should (= (canvas-diagram-middle-y (funcall at "WAIT_ACK")) (canvas-diagram-middle-y (funcall at "ERROR"))))
       (should (>= (canvas-diagram-node-x (funcall at "ERROR"))
                   (+ (canvas-diagram-node-x (funcall at "WAIT_ACK")) (canvas-diagram-node-w (funcall at "WAIT_ACK")))))
       (should (equal (mapcar #'canvas-diagram-node-label (canvas-diagram-nodes diagram))
                      '("IDLE" "RUN" "WAIT_ACK" "ERROR" "FINISH" "LOST")))))))

(ert-deftest canvas-graph-rows-are-ordered-under-their-parents ()
  ;; GIVEN two branches whose leaves are declared crosswise
  ;; WHEN they are laid out in layers
  ;; THEN each leaf sits under its parent, so the arrows do not cross
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let ((diagram (canvas-graph-test--laid-out
                     (canvas-graph-test--spec '("A" "B" "C" "E" "D") '(("A" "B") ("A" "C") ("B" "D") ("C" "E"))) ctx)))
       (should (equal (mapcar #'canvas-diagram-node-label (canvas-diagram-nodes diagram)) '("A" "B" "C" "D" "E")))))))

(ert-deftest canvas-graph-rows-stay-gap-y-apart-whatever-the-labels ()
  ;; GIVEN two nodes joined by an edge with a long label, labels shown
  ;; WHEN they are laid out in layers
  ;; THEN the rows are the plain gap apart: labels sit beside the arrows and
  ;;      take no room from the layout
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((canvas-graph-show-labels 'all)
            (diagram (canvas-graph-test--laid-out
                      (canvas-graph-test--spec '("A" "B") '(("A" "B" "a rather long condition indeed"))) ctx))
            (a (car (canvas-diagram-nodes diagram)))
            (b (cadr (canvas-diagram-nodes diagram))))
       (should (= (- (canvas-diagram-node-y b) (+ (canvas-diagram-node-y a) (canvas-diagram-node-h a)))
                  canvas-graph-gap-y))))))

(ert-deftest canvas-graph-labels-of-a-fan-sit-at-its-far-end ()
  ;; GIVEN a node fanning out to two, both leading into one
  ;; WHEN each label's place along its arrow is asked for
  ;; THEN the fan-out labels sit nearer their targets, the fan-in ones
  ;;      nearer their sources, AND a lone edge's halfway
  (let* ((graph (canvas-graph-build
                 (canvas-graph-test--spec '("A" "B" "C" "D" "E")
                                          '(("A" "B") ("A" "C") ("B" "D") ("C" "D") ("D" "E")))))
         (edges (canvas-graph-edges graph))
         (at (lambda (i) (canvas-graph--label-parameter (nth i edges)))))
    (should (= (funcall at 0) 0.7))
    (should (= (funcall at 1) 0.7))
    (should (= (funcall at 2) 0.3))
    (should (= (funcall at 3) 0.3))
    (should (= (funcall at 4) 0.5))))

(ert-deftest canvas-graph-a-label-moves-off-a-box ()
  ;; GIVEN a back edge whose straight line runs between two boxes of the
  ;;       row it crosses, too narrow a gap for its label
  ;; WHEN its label is placed
  ;; THEN the label lies on no box, having moved along or beside the arrow
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((canvas-graph-show-labels 'all)
            (diagram (canvas-graph-test--laid-out
                      (canvas-graph-test--spec '("A" "a wide state with a long name" "B" "C")
                                               '(("A" "a wide state with a long name") ("A" "B") ("B" "C")
                                                 ("C" "A" "a fairly long condition text")))
                      ctx))
            (graph (canvas-diagram-model diagram))
            (back (car (last (canvas-graph-edges graph))))
            (rect (canvas-graph--label-rect ctx back graph "Sans 12px")))
       (pcase-let ((`(,x ,y ,w ,h) rect))
         (dolist (node (canvas-graph-nodes graph))
           (should (or (<= (+ x w) (canvas-diagram-node-x node))
                       (>= x (+ (canvas-diagram-node-x node) (canvas-diagram-node-w node)))
                       (<= (+ y h) (canvas-diagram-node-y node))
                       (>= y (+ (canvas-diagram-node-y node) (canvas-diagram-node-h node)))))))))))

(ert-deftest canvas-graph-labels-lie-clear-of-boxes-and-each-other ()
  ;; GIVEN the sample, with its fan out of RUN and its arrows back to IDLE
  ;; WHEN it is laid out with labels shown, in rows and on a ring
  ;; THEN no label lies on a box AND no two labels lie on each other
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (dolist (layout '(layered ring))
       (let* ((canvas-graph-show-labels 'all)
              (canvas-graph-layout layout)
              (diagram (canvas-graph-test--laid-out canvas-graph-test--sample ctx))
              (graph (canvas-diagram-model diagram))
              (rects (delq nil (mapcar (lambda (e) (canvas-graph--label-rect ctx e graph "Sans 12px"))
                                       (canvas-graph-edges graph)))))
         (should (>= (length rects) 5))
         (dolist (rect rects)
           (should (canvas-graph--clear-p rect (canvas-graph-nodes graph))))
         (cl-loop for (a . rest) on rects
                  do (dolist (b rest)
                       (should (canvas-graph-test--apart-p a b)))))))))

(ert-deftest canvas-graph-labels-in-a-row-gap-sit-side-by-side-and-the-arrows-bend-to-them ()
  ;; GIVEN a node fanning out to two under long labels, the targets
  ;;       side by side below it
  ;; WHEN the rows are laid out with labels shown
  ;; THEN both labels sit in the gap between the rows, apart, AND each
  ;;      arrow passes through the middle of its label
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((canvas-graph-show-labels 'all)
            (diagram (canvas-graph-test--laid-out
                      (canvas-graph-test--spec '("A" "B" "C")
                                               '(("A" "B" "quite a long condition here") ("A" "C" "and another long one there")))
                      ctx))
            (graph (canvas-diagram-model diagram))
            (edges (canvas-graph-edges graph))
            (a (canvas-graph-node-named graph "A"))
            (b (canvas-graph-node-named graph "B"))
            (rects (mapcar (lambda (e) (canvas-graph--label-rect ctx e graph "Sans 12px")) edges)))
       (dolist (rect rects)
         (should (>= (nth 1 rect) (+ (canvas-diagram-node-y a) (canvas-diagram-node-h a))))
         (should (<= (+ (nth 1 rect) (nth 3 rect)) (canvas-diagram-node-y b))))
       (should (canvas-graph-test--apart-p (nth 0 rects) (nth 1 rects)))
       (cl-loop for edge in edges for rect in rects
                do (pcase-let ((`(,x . ,y) (canvas-graph--point-on (canvas-graph--geometry edge graph) 0.5)))
                     (should (< (abs (- x (+ (nth 0 rect) (/ (nth 2 rect) 2.0)))) 1.0))
                     (should (< (abs (- y (+ (nth 1 rect) (/ (nth 3 rect) 2.0)))) 1.0))))))))

(ert-deftest canvas-graph-labels-of-bowed-arrows-sit-beside-them ()
  ;; GIVEN a column of three nodes with arrows both ways between the first
  ;;       two, one on to the third, and one from the third back to the
  ;;       first, all labelled
  ;; WHEN their labels are placed
  ;; THEN the two-way pair's labels share the row gap side by side, their
  ;;      arrows bending apart through them; the far arrow's label sits
  ;;      wholly to one side of the column, beside its bow; AND the plain
  ;;      arrow's label sits on the column
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((canvas-graph-show-labels 'all)
            (diagram (canvas-graph-test--laid-out
                      (canvas-graph-test--spec '("A" "B" "C") '(("A" "B" "down") ("B" "A" "up") ("B" "C" "on") ("C" "A" "back"))) ctx))
            (graph (canvas-diagram-model diagram))
            (edges (canvas-graph-edges graph))
            (column (canvas-diagram-middle-x (canvas-graph-node-named graph "A")))
            (rect (lambda (i) (canvas-graph--label-rect ctx (nth i edges) graph "Sans 12px")))
            (side (lambda (r) (cond ((< (+ (nth 0 r) (nth 2 r)) column) 'left)
                                    ((> (nth 0 r) column) 'right)
                                    (t 'across)))))
       (should (canvas-graph-test--apart-p (funcall rect 0) (funcall rect 1)))
       (should (< (* (- (car (canvas-graph-edge-control (nth 0 edges))) column)
                     (- (car (canvas-graph-edge-control (nth 1 edges))) column))
                  0))
       (should (memq (funcall side (funcall rect 3)) '(left right)))
       (should-not (canvas-graph-edge-control (nth 3 edges)))
       (should-not (= (canvas-graph-edge-bow (nth 3 edges)) 0))
       (should (eq (funcall side (funcall rect 2)) 'across))))))

(ert-deftest canvas-graph-same-row-arrows-get-room-for-their-labels ()
  ;; GIVEN two nodes in one row joined by an arrow with a label taller than
  ;;       the boxes, nodes in the rows above and below them, and a
  ;;       labelled arrow into the row below
  ;; WHEN the rows are laid out with labels shown, then hidden
  ;; THEN the two stand far enough apart for the label, which sits between
  ;;      them on the row, the rows above and below keep clear of it, the
  ;;      arrow below still has its label, AND without labels the two are
  ;;      the plain gap apart
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((tall "a rather long condition between the siblings that takes several lines to say in full")
            (spec (canvas-graph-test--spec '("A" "B" "C" "D")
                                           (list '("A" "B" "go") '("A" "C") (list "B" "C" tall) '("C" "D" "on"))))
            (gap (lambda (diagram)
                   (let ((b (nth 1 (canvas-diagram-nodes diagram))) (c (nth 2 (canvas-diagram-nodes diagram))))
                     (- (canvas-diagram-node-x c) (+ (canvas-diagram-node-x b) (canvas-diagram-node-w b)))))))
       (let* ((canvas-graph-show-labels 'all)
              (diagram (canvas-graph-test--laid-out spec ctx))
              (graph (canvas-diagram-model diagram))
              (a (nth 0 (canvas-diagram-nodes diagram)))
              (b (nth 1 (canvas-diagram-nodes diagram)))
              (c (nth 2 (canvas-diagram-nodes diagram)))
              (d (nth 3 (canvas-diagram-nodes diagram)))
              (edges (canvas-graph-edges graph))
              (rect (canvas-graph--label-rect ctx (nth 2 edges) graph "Sans 12px")))
         (should (> (nth 3 rect) (canvas-diagram-node-h b)))
         (should (>= (funcall gap diagram)
                     (+ 16 (car (canvas-graph--label-size ctx graph (canvas-graph--label graph (nth 2 edges)) "Sans 12px")))))
         (should (>= (nth 0 rect) (+ (canvas-diagram-node-x b) (canvas-diagram-node-w b))))
         (should (<= (+ (nth 0 rect) (nth 2 rect)) (canvas-diagram-node-x c)))
         (should (< (abs (- (+ (nth 1 rect) (/ (nth 3 rect) 2.0)) (canvas-diagram-middle-y b))) 1.0))
         (should (<= (+ (canvas-diagram-node-y a) (canvas-diagram-node-h a)) (nth 1 rect)))
         (should (>= (canvas-diagram-node-y d) (+ (nth 1 rect) (nth 3 rect))))
         (should (canvas-graph--label-rect ctx (nth 3 edges) graph "Sans 12px"))
         (let ((rects (delq nil (mapcar #'canvas-graph-edge-label-rect edges))))
           (cl-loop for (x . rest) on rects
                    do (dolist (y rest) (should (canvas-graph-test--apart-p x y))))))
       (should (= (funcall gap (canvas-graph-test--laid-out spec ctx)) canvas-graph-gap-x))))))

(ert-deftest canvas-graph-a-crowded-label-moves-out-to-the-side ()
  ;; GIVEN a graph whose far arrow's label finds no free place near its bow
  ;;       because the row gap there is full of packed labels
  ;; WHEN the labels are placed
  ;; THEN the far arrow's label still lies on no box and no other label
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((canvas-graph-show-labels 'all)
            (wide "a condition wide enough to fill the gap between the rows on its own")
            (diagram (canvas-graph-test--laid-out
                      (canvas-graph-test--spec '("A" "B" "C" "D")
                                               (list '("A" "B" "go") (list "B" "C" wide) (list "B" "D" wide)
                                                     (list "C" "A" "back again to the start")
                                                     (list "D" "A" "and back again to the start")))
                      ctx))
            (graph (canvas-diagram-model diagram))
            (rects (delq nil (mapcar #'canvas-graph-edge-label-rect (canvas-graph-edges graph)))))
       (should (= (length rects) 5))
       (dolist (rect rects)
         (should (canvas-graph--clear-p rect (canvas-graph-nodes graph))))
       (cl-loop for (a . rest) on rects
                do (dolist (b rest)
                     (should (canvas-graph-test--apart-p a b))))))))

(ert-deftest canvas-graph-long-labels-wrap ()
  ;; GIVEN a short label and one far wider than a label may be
  ;; WHEN their patches are measured
  ;; THEN the long one is no wider than the limit and taller than the short one
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((canvas-graph-show-labels 'all)
            (long "a condition that goes on and on and on, with many words in it, and more")
            (diagram (canvas-graph-test--laid-out
                      (canvas-graph-test--spec '("A" "B" "C") (list '("A" "B" "go") (list "B" "C" long))) ctx))
            (graph (canvas-diagram-model diagram))
            (edges (canvas-graph-edges graph))
            (short-rect (canvas-graph--label-rect ctx (nth 0 edges) graph "Sans 12px"))
            (long-rect (canvas-graph--label-rect ctx (nth 1 edges) graph "Sans 12px")))
       (should (<= (nth 2 long-rect) (+ canvas-graph-label-width 9)))
       (should (> (nth 3 long-rect) (nth 3 short-rect)))))))

(ert-deftest canvas-graph-rows-part-for-a-tall-label ()
  ;; GIVEN an edge whose wrapped label is taller than the row gap allows
  ;; WHEN the rows are laid out with labels shown
  ;; THEN those two rows are further apart than the plain gap, AND a pair
  ;;      of rows with a short label between them is the plain gap apart
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((canvas-graph-show-labels 'all)
            (tall "a condition that goes on and on and on, with many words in it, and more, and yet more words to wrap onto a third line")
            (diagram (canvas-graph-test--laid-out
                      (canvas-graph-test--spec '("A" "B" "C") (list (list "A" "B" tall) '("B" "C" "go"))) ctx))
            (nodes (canvas-diagram-nodes diagram))
            (gap (lambda (a b) (- (canvas-diagram-node-y b) (+ (canvas-diagram-node-y a) (canvas-diagram-node-h a))))))
       (should (> (funcall gap (nth 0 nodes) (nth 1 nodes)) canvas-graph-gap-y))
       (should (= (funcall gap (nth 1 nodes) (nth 2 nodes)) canvas-graph-gap-y))))))

(ert-deftest canvas-graph-what-is-drawn-fits-the-drawing ()
  ;; GIVEN a graph with a self loop and a long label, and one whose arrow
  ;;       must bow round a box
  ;; WHEN each is laid out
  ;; THEN every point of every arrow and label lies within the drawing,
  ;;      AND the drawing has slack only on the sides something reaches past
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let ((canvas-graph-show-labels 'all))
       (dolist (spec (list (canvas-graph-test--spec '("A" "B") '(("A" "A" "a long condition on the loop") ("A" "B")))
                           (canvas-graph-test--spec '("A" "B" "C") '(("A" "B") ("B" "C") ("C" "A")))))
         (let* ((diagram (canvas-graph-test--laid-out spec ctx))
                (graph (canvas-diagram-model diagram))
                (size (canvas-diagram--map-size diagram)))
           ;; the loop and the bow reach sideways, nothing reaches above or below
           (should (or (> (car (canvas-diagram-slack diagram)) 0)
                       (> (apply #'min (mapcar #'canvas-diagram-node-x (canvas-diagram-nodes diagram))) 0)))
           (should (= (cdr (canvas-diagram-slack diagram)) 0))
           (should (= (apply #'min (mapcar #'canvas-diagram-node-y (canvas-diagram-nodes diagram))) 0))
           (pcase-dolist (`(,x . ,y) (canvas-graph--drawn-points graph ctx))
             (should (>= x 0))
             (should (>= y 0))
             (should (<= (+ x canvas-diagram-margin) (car size)))
             (should (<= (+ y canvas-diagram-margin) (cdr size))))))
       ;; AND a plain chain reaches past nothing
       (let ((chain (canvas-graph-test--laid-out (canvas-graph-test--spec '("A" "B") '(("A" "B"))) ctx)))
         (should (equal (canvas-diagram-slack chain) '(0 . 0)))
         (should (= (apply #'min (mapcar #'canvas-diagram-node-x (canvas-diagram-nodes chain))) 0)))))))

(ert-deftest canvas-graph-edges-leave-and-enter-at-the-border ()
  ;; GIVEN two boxes side by side, level
  ;; WHEN the point where a line from one's centre towards the other leaves it is asked for
  ;; THEN it lies on the facing side, at the middle
  (let ((a (canvas-diagram-node-create :label "A" :x 0 :y 0 :w 40 :h 20))
        (b (canvas-diagram-node-create :label "B" :x 100 :y 0 :w 40 :h 20)))
    (should (equal (canvas-graph--border-point a (cons (canvas-diagram-middle-x b) (canvas-diagram-middle-y b)))
                   '(40.0 . 10.0)))
    (should (equal (canvas-graph--border-point b (cons (canvas-diagram-middle-x a) (canvas-diagram-middle-y a)))
                   '(100.0 . 10.0)))
    ;; AND straight up leaves through the top
    (should (equal (canvas-graph--border-point a '(20.0 . -50.0)) '(20.0 . 0.0)))))

(ert-deftest canvas-graph-two-way-edges-bow-to-opposite-sides ()
  ;; GIVEN a graph with edges both ways between two nodes, and one way
  ;;       to a third, all in one column
  ;; WHEN their geometry is made
  ;; THEN the two-way pair's control points lie left and right of the
  ;;      column, AND the one-way edge runs straight down it
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((diagram (canvas-graph-test--laid-out
                      (canvas-graph-test--spec '("A" "B" "C") '(("A" "B") ("B" "A") ("B" "C"))) ctx))
            (graph (canvas-diagram-model diagram))
            (edges (canvas-graph-edges graph))
            (column (canvas-diagram-middle-x (canvas-graph-node-named graph "A")))
            (control-x (lambda (edge) (nth 2 (canvas-graph--geometry edge graph)))))
       (should (< (* (- (funcall control-x (nth 0 edges)) column) (- (funcall control-x (nth 1 edges)) column)) 0))
       (should (< (abs (- (funcall control-x (nth 2 edges)) column)) 0.001))))))

(ert-deftest canvas-graph-an-edge-that-would-cross-a-box-bows-round-it ()
  ;; GIVEN three nodes in a row, the last leading back to the first
  ;; WHEN the graph is laid out in layers
  ;; THEN the back edge bows, the forward edges run straight, AND its
  ;;      curve passes through no box
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((diagram (canvas-graph-test--laid-out
                      (canvas-graph-test--spec '("A" "B" "C") '(("A" "B") ("B" "C") ("C" "A"))) ctx))
            (graph (canvas-diagram-model diagram))
            (edges (canvas-graph-edges graph))
            (b (canvas-graph-node-named graph "B")))
       (should (= (canvas-graph-edge-bow (nth 0 edges)) 0))
       (should (= (canvas-graph-edge-bow (nth 1 edges)) 0))
       (should-not (= (canvas-graph-edge-bow (nth 2 edges)) 0))
       (should (= (canvas-graph--crossings (nth 2 edges) (canvas-graph-edge-bow (nth 2 edges)) (list b)) 0))
       (should (> (canvas-graph--crossings (nth 2 edges) 0 (list b)) 0))
       ;; AND the drawing has room on the side the bow went
       (should (or (> (car (canvas-diagram-slack diagram)) 0)
                   (> (apply #'min (mapcar #'canvas-diagram-node-x (canvas-diagram-nodes diagram))) 0)))))))

(ert-deftest canvas-graph-common-edges-fold-into-one-marker ()
  ;; GIVEN a chain of five nodes, four of which abort to the first under
  ;;       one label
  ;; WHEN the graph is laid out with folding on, then off
  ;; THEN those four edges are common and drawn as one any-node marker
  ;;      left of the target, the chain's edges are not, the target's card
  ;;      lists them as one line, AND with folding off nothing is common
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((canvas-graph-show-labels 'all)
            (spec (canvas-graph-test--spec '("A" "B" "C" "D" "E")
                                           '(("A" "B" "go") ("B" "C") ("C" "D") ("D" "E")
                                             ("B" "A" "abort = '1'") ("C" "A" "abort = '1'")
                                             ("D" "A" "abort = '1'") ("E" "A" "abort = '1'"))))
            (diagram (canvas-graph-test--laid-out spec ctx))
            (graph (canvas-diagram-model diagram))
            (edges (canvas-graph-edges graph))
            (a (canvas-graph-node-named graph "A")))
       (should (equal (mapcar #'canvas-graph-edge-common edges) '(nil nil nil nil t t t t)))
       (let ((groups (canvas-graph--common-groups graph)))
         (should (= (length groups) 1))
         (should (eq (car (car groups)) a))
         (should (= (length (cddr (car groups))) 4)))
       (should (cl-some (lambda (p) (< (car p) (canvas-diagram-node-x a))) (canvas-graph--drawn-points graph ctx)))
       (should (equal (nth 2 (canvas-graph-card graph a))
                      "→ B  when go\n← any node (4)  when abort = '1'"))
       (let ((canvas-graph-fold-common nil))
         (should (cl-notany #'canvas-graph-edge-common
                            (canvas-graph-edges (canvas-diagram-model (canvas-graph-test--laid-out spec ctx))))))))))

(ert-deftest canvas-graph-a-few-returns-are-not-folded ()
  ;; GIVEN the sample, where three of five other nodes return to IDLE unlabelled
  ;; WHEN it is laid out
  ;; THEN no edge is common: fewer than two thirds of the nodes share the way
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (should (cl-notany #'canvas-graph-edge-common
                        (canvas-graph-edges
                         (canvas-diagram-model (canvas-graph-test--laid-out canvas-graph-test--sample ctx))))))))

;;;; The panel of the selected node's labels

(ert-deftest canvas-graph-selected-labels-follow-the-keyboard ()
  ;; GIVEN the sample laid out showing the selected node's labels only
  ;; WHEN the layout is made, and the keyboard is put on RUN
  ;; THEN no arrow carries a label and the rows are the plain gap apart;
  ;;      RUN's arrows out and then in are numbered, the panel lists each
  ;;      number with its way and label, AND after a redraw the panel
  ;;      stands in the view's corner with ink in it and a tag on the canvas
  (canvas-graph-test--in-buffer canvas-graph-test--sample '(700 . 500)
    (let ((canvas-graph-show-labels 'selected)
          (canvas-diagram-colors (append '(:selection "yellow") canvas-diagram-colors)))
      (canvas-diagram-relayout)
      (let* ((graph (canvas-diagram-current-model))
             (run (canvas-graph-test--node "RUN"))
             (idle (canvas-graph-test--node "IDLE"))
             (tags (canvas-graph--tags graph run)))
        (should (cl-notany #'canvas-graph-edge-label-rect (canvas-graph-edges graph)))
        (should (= (- (canvas-diagram-node-y run) (+ (canvas-diagram-node-y idle) (canvas-diagram-node-h idle)))
                   canvas-graph-gap-y))
        (should (equal (mapcar #'car tags) '(1 2 3 4)))
        (should (equal (canvas-graph--panel-lines graph run)
                       '("1 → WAIT_ACK  when done = '1'" "2 → ERROR  when err = '1'"
                         "3 → IDLE  when abort = '1'" "4 ← IDLE  when start = '1'")))
        (canvas-diagram--select run)
        (pcase-let* ((inner (canvas-graph--panel-inner canvas-diagram--diagram '(700 . 500) '(0 . 0) 1.0))
                     (`(,x ,y ,w ,h) (canvas-graph--panel-rect canvas-diagram--context "Sans 12px" '(700 . 500)
                                                               (canvas-graph--panel-markup graph run) inner)))
          (should (= w (+ inner (* 2 canvas-diagram-padding))))
          (should (= (+ x w) 690))
          (should (= y 10))
          (should (cl-loop for px from x below (+ x w) by 2
                           thereis (cl-loop for py from y below (+ y h) by 2
                                            thereis (canvas-graph-test--dark-p
                                                     (canvas-cairo-pixel canvas-diagram--context px py))))))
        ;; AND the first tag sits on the arrow to WAIT_ACK as a disc: the
        ;;     arrow, in the selection colour, shows just outside it and is
        ;;     covered just inside it, beyond the digit
        (let* ((edge (car (canvas-graph--about run)))
               (curve (canvas-graph--geometry edge graph))
               (k (canvas-graph--label-parameter edge))
               (centre (cdr (car tags)))
               (ahead (canvas-graph--point-on curve (+ k 0.01)))
               (len (sqrt (+ (expt (- (car ahead) (car centre)) 2) (expt (- (cdr ahead) (cdr centre)) 2))))
               (ux (/ (- (car ahead) (car centre)) len))
               (uy (/ (- (cdr ahead) (cdr centre)) len))
               (at (lambda (d) (canvas-cairo-pixel canvas-diagram--context
                                                   (+ 10 (round (+ (car centre) (* d ux))))
                                                   (+ 10 (round (+ (cdr centre) (* d uy))))))))
          (should (= (funcall at 6.5) #xFFFFFFFF))
          (should (= (funcall at -6.5) #xFFFFFFFF))
          (should-not (= (funcall at 12) #xFFFFFFFF)))))))

(defun canvas-graph-test--fake-face-hex (face &rest _)
  "A colour of its own per font-lock face, to tell them apart in markup."
  (pcase face
    ('font-lock-keyword-face "#010101")
    ('font-lock-type-face "#050505")
    (_ "#090909")))

(ert-deftest canvas-graph-panel-markup-shows-what-it-lists ()
  ;; GIVEN faces with colours of their own, and the sample with WAIT_ACK selected
  ;; WHEN the panel's markup is made
  ;; THEN the number is bold in the selection colour, the node at the far
  ;;      end bold in the type colour, when in the keyword colour, the
  ;;      label marked up by the style, its <= escaped, AND every line measures
  (cl-letf (((symbol-function 'canvas-diagram-face-hex) #'canvas-graph-test--fake-face-hex))
    (canvas-graph-test--rendering
     (canvas-graph-test--with-context ctx 4 4
       (let* ((canvas-diagram-colors (append '(:selection "yellow") canvas-diagram-colors))
              (graph (canvas-graph-build canvas-graph-test--sample))
              (lines (canvas-graph--panel-markup graph (canvas-graph-node-named graph "WAIT_ACK")))
              (first (car lines)))
         (should (= (length lines) 2))
         (should (string-search "<span weight=\"bold\" foreground=\"#ffff00\">1</span>" first))
         (should (string-search "<span weight=\"bold\" foreground=\"#050505\">FINISH</span>" first))
         (should (string-search "foreground=\"#010101\">when</span> count &lt;= 3" first))
         (dolist (line lines)
           (should (canvas-cairo-markup-size ctx line "Sans 12px"))))))))

(ert-deftest canvas-graph-the-panel-scrolls-when-taller-than-the-view ()
  ;; GIVEN a node with many long labels shown in a short view
  ;; WHEN the panel is drawn, scrolled down twice, up once, then the
  ;;      keyboard moves to another node
  ;; THEN the panel is cut to the view with room to spare and knows how far
  ;;      it can scroll; scrolling moves by steps and stops at the end; and a
  ;;      new selection starts at the top again
  (canvas-graph-test--in-buffer
      (canvas-graph-test--spec '("A" "B" "C" "D")
                               (list '("A" "B" "a condition that goes on and on, with many words in it, to wrap")
                                     '("A" "C" "another condition that goes on and on, with many words in it")
                                     '("A" "D" "a third condition that goes on and on, with many more words")
                                     '("B" "A" "back") '("C" "A" "back again") '("D" "A" "and back")))
      '(500 . 120)
    (let ((canvas-graph-show-labels 'selected)
          (canvas-graph-panel-width 200))
      (canvas-diagram-relayout)
      (let ((a (canvas-graph-test--node "A")))
        (canvas-diagram--select a)
        (should (> canvas-graph--panel-overflow 0))
        (should (= canvas-graph--panel-scroll 0))
        (pcase-let ((`(,_ ,y ,_ ,h) (canvas-graph--panel-box canvas-diagram--context "Sans 12px" '(500 . 120)
                                                             (canvas-graph--panel-markup (canvas-diagram-current-model) a) 200)))
          (should (<= (+ y h) 110)))
        (canvas-graph-panel-down)
        (should (= canvas-graph--panel-scroll 60))
        (dotimes (_ 30) (canvas-graph-panel-down))
        (should (= canvas-graph--panel-scroll canvas-graph--panel-overflow))
        (canvas-graph-panel-up)
        (should (= canvas-graph--panel-scroll (- canvas-graph--panel-overflow 60)))
        (canvas-diagram--select (canvas-graph-test--node "B"))
        (should (= canvas-graph--panel-scroll 0))
        (should (= canvas-graph--panel-overflow 0))))))

(ert-deftest canvas-graph-the-panel-width-follows-the-room-beside-the-drawing ()
  ;; GIVEN the panel width on auto and a drawing five nodes wide
  ;; WHEN the panel's text width is asked for in views of several widths,
  ;;      scrolled and zoomed
  ;; THEN it is the room right of the drawing less a margin, no more than
  ;;      three fifths of the view and no less than 200, following the
  ;;      scroll and the zoom, AND a set width is taken as it is
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((canvas-graph-panel-width 'auto)
            (diagram (canvas-graph-test--laid-out
                      (canvas-graph-test--spec '("A" "state_one" "state_two" "state_three" "state_four" "state_five")
                                               '(("A" "state_one") ("A" "state_two") ("A" "state_three") ("A" "state_four") ("A" "state_five")))
                      ctx))
            (right (+ canvas-diagram-margin (car (canvas-diagram--bounds (canvas-diagram-nodes diagram)))))
            (room (lambda (view offset zoom) (- (car view) (round (- (+ canvas-diagram-margin (* zoom (- right canvas-diagram-margin))) offset)) 30))))
       (should (> right 300))
       (should (= (canvas-graph--panel-inner diagram '(700 . 500) '(0 . 0) 1.0)
                  (max 200 (min 420 (funcall room '(700 . 500) 0 1.0)))))
       (should (= (canvas-graph--panel-inner diagram '(700 . 500) '(200 . 0) 1.0)
                  (max 200 (min 420 (funcall room '(700 . 500) 200 1.0)))))
       (should (> (canvas-graph--panel-inner diagram '(700 . 500) '(200 . 0) 1.0)
                  (canvas-graph--panel-inner diagram '(700 . 500) '(0 . 0) 1.0)))
       (should (= (canvas-graph--panel-inner diagram '(2000 . 500) '(0 . 0) 1.0) 1200))
       (should (= (canvas-graph--panel-inner diagram '(400 . 500) '(0 . 0) 4.0) 200))
       (let ((canvas-graph-panel-width 320))
         (should (= (canvas-graph--panel-inner diagram '(2000 . 500) '(0 . 0) 1.0) 320)))))))

(ert-deftest canvas-graph-the-panel-width-is-adjustable ()
  ;; GIVEN the panel width on auto, last drawn 412 wide
  ;; WHEN it is widened twice and narrowed once, then narrowed far
  ;; THEN it becomes a fixed width from the drawn one, moves by steps,
  ;;      stops at its floor, AND the panel's box follows up to the view's width
  (canvas-graph-test--with-context ctx 4 4
    (let ((canvas-graph-panel-width 'auto)
          (canvas-graph--panel-inner-drawn 412))
      (canvas-graph-panel-wider)
      (canvas-graph-panel-wider)
      (should (= canvas-graph-panel-width 492))
      (canvas-graph-panel-narrower)
      (should (= canvas-graph-panel-width 452))
      (should (= (nth 2 (canvas-graph--panel-rect ctx "Sans 12px" '(1000 . 500) '("1 → A") 452))
                 (+ 452 (* 2 canvas-diagram-padding))))
      (should (< (nth 2 (canvas-graph--panel-rect ctx "Sans 12px" '(300 . 500) '("1 → A") 452)) 300))
      (dotimes (_ 20) (canvas-graph-panel-narrower))
      (should (= canvas-graph-panel-width 200)))))

(ert-deftest canvas-graph-the-panel-lists-any-node-ways-too ()
  ;; GIVEN a node reached from any node and leading on
  ;; WHEN its tags and panel are made
  ;; THEN the way out is tagged and listed, then the way in, then the
  ;;      any-node way in, whose tag sits at its marker
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((canvas-graph-show-labels 'selected)
            (diagram (canvas-graph-test--laid-out
                      '(:name "st" :start "A" :nodes (("A") ("B") ("C"))
                        :edges ((nil "B" "hdr = '1'") ("A" "B" "go") ("B" "C" "on") ("C" "A")))
                      ctx))
            (graph (canvas-diagram-model diagram))
            (b (canvas-graph-node-named graph "B")))
       (should (equal (canvas-graph--panel-lines graph b)
                      '("1 → C  when on" "2 ← A  when go" "3 ← any node  when hdr = '1'")))
       (should (equal (mapcar #'car (canvas-graph--tags graph b)) '(1 2 3)))
       (should (< (car (cdr (nth 2 (canvas-graph--tags graph b)))) (canvas-diagram-node-x b)))))))

(defconst canvas-graph-test--aborts
  '(:name "st" :start "A" :nodes (("A") ("B") ("C") ("D"))
    :edges (("A" "B" "go") ("B" "C" "on") ("C" "D" "more")
            ("B" "A" "abort") ("C" "A" "abort") ("D" "A" "abort")))
  "A graph whose three other nodes all go back to the start on abort.")

(ert-deftest canvas-graph-the-panel-names-the-far-end-of-a-folded-way ()
  ;; GIVEN a graph whose three other nodes all go back to the start on
  ;;       abort, folded into one any-node marker
  ;; WHEN the panel is made for one of those nodes, and for the start
  ;; THEN the folded way out is listed as going to the start, by name, AND
  ;;      the start's panel lists it as from any node
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((canvas-graph-show-labels 'selected)
            (graph (canvas-diagram-model (canvas-graph-test--laid-out canvas-graph-test--aborts ctx))))
       (should (equal (canvas-graph--panel-lines graph (canvas-graph-node-named graph "C"))
                      '("1 → D  when more" "2 ← B  when on" "3 → A  when abort")))
       (should (equal (canvas-graph--panel-lines graph (canvas-graph-node-named graph "A"))
                      '("1 → B  when go" "2 ← any node  when abort")))))))

;;;; Routing a folded way round the boxes

(defun canvas-graph-test--arrows-crossed (corners graph)
  "How many times the way along CORNERS crosses GRAPH's drawn arrows."
  (cl-loop for edge in (canvas-graph-edges graph)
           when (canvas-graph--drawn-p edge)
           sum (canvas-graph--polyline-crossings corners (canvas-graph--curve-points edge))))

(defun canvas-graph-test--route-crosses (corners nodes)
  "The boxes of NODES the way along CORNERS runs through."
  (cl-remove-if-not
   (lambda (node)
     (cl-loop for (a b) on corners while b
              thereis (cl-loop for k from 1 to 19
                               thereis (canvas-graph--inside-p
                                        (canvas-graph--toward a b (* k (/ (canvas-graph--distance a b) 20.0)))
                                        node 0))))
   nodes))

(ert-deftest canvas-graph-trail-routes-a-folded-way-round-the-boxes ()
  ;; GIVEN a layered graph whose three other nodes go back to the start
  ;;       on abort, folded into one marker, the middle one below the
  ;;       others, with the keyboard on it
  ;; WHEN the graph is drawn with the selected node's labels
  ;; THEN its folded way goes round the boxes, out of its top, left past
  ;;      them all and onto the marker's dot, running through no box, in
  ;;      the selection colour, its tag halfway along the longest leg, the
  ;;      drawing having room for it, AND on the start the tag of the
  ;;      way in stays left of the dot
  (canvas-graph-test--in-buffer canvas-graph-test--aborts '(500 . 400)
    (let* ((canvas-diagram-colors (append '(:selection "yellow") canvas-diagram-colors))
           (graph (canvas-diagram-model canvas-diagram--diagram))
           (nodes (canvas-graph-nodes graph))
           (a (canvas-graph-test--node "A"))
           (c (canvas-graph-test--node "C"))
           (folded (car (canvas-graph--folded-out c)))
           (corners (canvas-graph-edge-route folded))
           (dot (canvas-graph--marker-geometry a 0))
           (tag (canvas-graph--route-tag-point corners))
           (leftmost (apply #'min (mapcar #'canvas-diagram-node-x nodes))))
      (should (eq (canvas-graph-edge-to folded) a))
      (should (equal (car corners) (cons (canvas-diagram-middle-x c) (canvas-diagram-node-y c))))
      (should (cl-some (lambda (p) (< (car p) leftmost)) corners))
      (should (< (canvas-graph--distance (car (last corners)) (cons (car dot) (cadr dot))) 6))
      (should (equal (canvas-graph-test--route-crosses (cdr corners) nodes) nil))
      (should (>= (car tag) (- canvas-graph--tag-radius)))
      (canvas-diagram--select c)
      (let ((on (canvas-graph--toward (nth 2 corners) (nth 3 corners) 20)))
        (should (= (canvas-cairo-pixel canvas-diagram--context (+ 10 (round (car on))) (+ 10 (round (cdr on))))
                   #xFFFFFF00)))
      (should (equal (cdr (nth 2 (canvas-graph--tags graph c))) tag))
      (should (equal (cdr (nth 1 (canvas-graph--tags graph a))) (cons (- (car dot) 14) (cadr dot))))
      (let ((canvas-graph-layout 'ring))
        (canvas-graph--route-folded graph)
        (should (= (length (canvas-graph-edge-route folded)) 2))))))

(ert-deftest canvas-graph-the-grid-router-goes-round-boxes-and-past-arrows ()
  ;; GIVEN a three by three grid, a start at its top left and a goal at its
  ;;       top right, first with an arrow cutting the top two lines in
  ;;       the middle, then with a box over the middle of them instead
  ;; WHEN the way is routed
  ;; THEN it detours along the bottom line rather than cross the arrow, as
  ;;      the detour costs less than a crossing, likewise round the box,
  ;;      AND with nothing in the way it runs straight along the top
  (let ((arrow (canvas-graph-grid-create :xs '(0.0 50.0 100.0) :ys '(0.0 20.0 40.0)
                                         :arrows (let ((points (list (cons 50 -10) (cons 50 30))))
                                                   (list (cons (canvas-graph--bounds-of points) points)))))
        (box (canvas-graph-grid-create :xs '(0.0 50.0 100.0) :ys '(0.0 20.0 40.0)
                                       :boxes '((20 -10 60 40))))
        (clear (canvas-graph-grid-create :xs '(0.0 50.0 100.0) :ys '(0.0 20.0 40.0))))
    (dolist (grid (list arrow box))
      (should (equal (canvas-graph--corners-of (canvas-graph--route-on grid '((0 . 0)) '((100 . 0))))
                     '((0.0 . 0.0) (0.0 . 40.0) (100.0 . 40.0) (100.0 . 0.0)))))
    (should (equal (canvas-graph--corners-of (canvas-graph--route-on clear '((0 . 0)) '((100 . 0))))
                   '((0.0 . 0.0) (100.0 . 0.0))))))

(ert-deftest canvas-graph-folded-ways-cross-no-arrow-when-a-way-round-exists ()
  ;; GIVEN a layered graph, the start over two branches of two nodes
  ;;       each, every other node aborting to the start
  ;; WHEN the folded ways of the two lower nodes are routed
  ;; THEN each leaves the top or bottom of its box, runs through no box
  ;;      and crosses no arrow, though the straight way to the marker
  ;;      would have
  (canvas-graph-test--in-buffer
      '(:name "st" :start "A" :nodes (("A") ("B") ("C") ("D") ("E"))
        :edges (("A" "B" "go") ("A" "E" "alt") ("B" "C" "on") ("E" "D" "on")
                ("B" "A" "abort") ("C" "A" "abort") ("D" "A" "abort") ("E" "A" "abort")))
      '(600 . 400)
    (let* ((graph (canvas-diagram-model canvas-diagram--diagram))
           (nodes (canvas-graph-nodes graph)))
      (dolist (name '("C" "D"))
        (let* ((node (canvas-graph-test--node name))
               (edge (car (canvas-graph--folded-out node)))
               (route (canvas-graph-edge-route edge))
               (straight (canvas-graph--straight-route edge graph)))
          (should (= (car (car route)) (canvas-diagram-middle-x node)))
          (should (member (cdr (car route)) (list (canvas-diagram-node-y node)
                                                  (+ (canvas-diagram-node-y node) (canvas-diagram-node-h node)))))
          (should (equal (canvas-graph-test--route-crosses (cdr route) nodes) nil))
          (should (= (canvas-graph-test--arrows-crossed route graph) 0))
          (should (or (canvas-graph-test--route-crosses straight nodes)
                      (> (canvas-graph-test--arrows-crossed straight graph) 0))))))))

;;;; Drawing

(ert-deftest canvas-graph-render-draws-arrows-into-the-target ()
  ;; GIVEN two nodes in rows with one edge between them, labels off
  ;; WHEN the graph is rendered
  ;; THEN the gap between the boxes carries the edge colour in the middle,
  ;;      AND the arrowhead, solid, sits just before the target's border
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 400 300
     (let* ((diagram (canvas-graph-test--laid-out (canvas-graph-test--spec '("A" "B") '(("A" "B" "go"))) ctx))
            (graph (canvas-diagram-model diagram))
            (a (canvas-graph-node-named graph "A"))
            (b (canvas-graph-node-named graph "B"))
            (col (+ 10 (floor (canvas-diagram-middle-x a))))
            (mid (+ 10 (round (/ (+ (+ (canvas-diagram-node-y a) (canvas-diagram-node-h a)) (canvas-diagram-node-y b)) 2.0)))))
       (canvas-diagram--render diagram ctx '(400 . 300))
       (should (canvas-graph-test--reddish-p (canvas-cairo-pixel ctx col mid)))
       (should (= (canvas-cairo-pixel ctx col (+ 10 (round (canvas-diagram-node-y b)) -4)) #xFFFF0000))
       (should (canvas-graph-test--reddish-p (canvas-cairo-pixel ctx (- col 1) (+ 10 (round (canvas-diagram-node-y b)) -4))))
       (should (= (canvas-cairo-pixel ctx (- col 20) mid) #xFFFFFFFF))
       ;; AND laid out with labels on, the label's ink sits near the middle of the edge
       (let* ((canvas-graph-show-labels 'all)
              (diagram (canvas-graph-test--laid-out (canvas-graph-test--spec '("A" "B") '(("A" "B" "go"))) ctx)))
         (canvas-diagram--render diagram ctx '(400 . 300))
         (should (cl-loop for px from (- col 20) to (+ col 20)
                          thereis (cl-loop for py from (- mid 14) to (+ mid 14)
                                           thereis (canvas-graph-test--dark-p (canvas-cairo-pixel ctx px py))))))))))

(defun canvas-graph-test--patch-pixels (ctx diagram edge)
  "The pixels of EDGE's label patch on CTX, drawn with a ten pixel margin."
  (pcase-let ((`(,x ,y ,w ,h) (canvas-graph--label-rect ctx edge (canvas-diagram-model diagram) "Sans 12px")))
    (append (canvas-cairo-pixels ctx (+ 10 x) (+ 10 y) w h) nil)))

(defun canvas-graph-test--patch-rim (ctx diagram edge col)
  "The pixels in column COL of EDGE's label patch between its top and
bottom edges and its text, three at either end, where only the patch
itself is painted."
  (pcase-let ((`(,_ ,y ,_ ,h) (canvas-graph--label-rect ctx edge (canvas-diagram-model diagram) "Sans 12px")))
    (mapcar (lambda (dy) (canvas-cairo-pixel ctx col (+ 10 y dy)))
            (list 1 2 3 (- h 2) (- h 3) (- h 4)))))

(ert-deftest canvas-graph-label-patch-hides-the-line-beneath ()
  ;; GIVEN two nodes joined by a labelled edge, labels on
  ;; WHEN the graph is rendered
  ;; THEN where the red line enters and leaves the label's patch, the
  ;;      patch is plain background, nothing showing through,
  ;;      AND the line is there just outside the patch
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 400 300
     (let* ((canvas-graph-show-labels 'all)
            (diagram (canvas-graph-test--laid-out (canvas-graph-test--spec '("A" "B") '(("A" "B" "go"))) ctx))
            (edge (car (canvas-graph-edges (canvas-diagram-model diagram))))
            (a (canvas-graph-node-named (canvas-diagram-model diagram) "A"))
            (col (+ 10 (floor (canvas-diagram-middle-x a)))))
       (canvas-diagram--render diagram ctx '(400 . 300))
       (should (cl-every (lambda (px) (= px #xFFFFFFFF)) (canvas-graph-test--patch-rim ctx diagram edge col)))
       (pcase-let ((`(,_ ,y ,_ ,_) (canvas-graph--label-rect ctx edge (canvas-diagram-model diagram) "Sans 12px")))
         (should (canvas-graph-test--reddish-p (canvas-cairo-pixel ctx col (+ 10 y -3)))))))))

(ert-deftest canvas-graph-labels-read-over-the-trail ()
  ;; GIVEN the keyboard on A, whose edge to B is labelled, with a selection
  ;;       colour configured and labels on
  ;; WHEN the graph is drawn, the trail out of A in that colour
  ;; THEN the label's patch carries none of it, AND the trail runs up to the patch
  (canvas-graph-test--in-buffer (canvas-graph-test--spec '("A" "B") '(("A" "B" "go"))) '(400 . 300)
    (let* ((canvas-diagram-colors (append '(:selection "yellow") canvas-diagram-colors))
           (canvas-graph-show-labels 'all)
           (a (canvas-graph-test--node "A"))
           (edge (car (canvas-graph-node-out a)))
           (col (+ 10 (floor (canvas-diagram-middle-x a)))))
      (canvas-diagram-relayout)
      (canvas-diagram--select a)
      (should (cl-notany (lambda (px) (= px #xFFFFFF00))
                         (canvas-graph-test--patch-pixels canvas-diagram--context canvas-diagram--diagram edge)))
      (pcase-let ((`(,_ ,y ,_ ,_) (canvas-graph--label-rect canvas-diagram--context edge
                                                           (canvas-diagram-current-model) "Sans 12px")))
        (should (= (canvas-cairo-pixel canvas-diagram--context col (+ 10 y -3)) #xFFFFFF00))))))

(ert-deftest canvas-graph-render-draws-a-self-loop-beside-the-box ()
  ;; GIVEN a node with an edge to itself
  ;; WHEN the graph is rendered
  ;; THEN the row through the box's middle carries the edge colour
  ;;      somewhere just right of the box, within the loop's reach
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 300 200
     (let* ((diagram (canvas-graph-test--laid-out (canvas-graph-test--spec '("A") '(("A" "A"))) ctx))
            (a (car (canvas-diagram-nodes diagram)))
            (right (+ 10 (round (+ (canvas-diagram-node-x a) (canvas-diagram-node-w a)))))
            (cy (+ 10 (floor (canvas-diagram-middle-y a)))))
       (canvas-diagram--render diagram ctx '(300 . 200))
       (should (cl-loop for px from (1+ right) to (+ right canvas-graph--loop-height 2)
                        thereis (canvas-graph-test--reddish-p (canvas-cairo-pixel ctx px cy))))))))

(ert-deftest canvas-graph-nodes-are-coloured-by-depth-and-listed-in-the-legend ()
  ;; GIVEN the sample built and coloured from a palette
  ;; WHEN the nodes' colours and the legend are asked for
  ;; THEN the start has the palette's first colour, a node two steps away
  ;;      the third, the unreachable one the node colour, AND the legend
  ;;      has a row per depth, then the unreachable kind
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((diagram (canvas-graph-test--laid-out canvas-graph-test--sample ctx))
            (graph (canvas-diagram-model diagram))
            (rgb (lambda (name) (canvas-diagram--node-rgb diagram (canvas-graph-node-named graph name)))))
       (should (equal (funcall rgb "IDLE") '(0.0 0.0 1.0)))
       (should (equal (funcall rgb "ERROR") '(1.0 1.0 0.0)))
       (should (equal (funcall rgb "LOST") '(0.0 0.0 1.0)))
       (let ((canvas-diagram-show-kinds t))
         (should (equal (mapcar #'car (canvas-diagram--legend-entries diagram))
                        '("start" "1 step" "2 steps" "3 steps" "unreachable"))))))))

(ert-deftest canvas-graph-export-shows-every-label ()
  ;; GIVEN the labels set to the selected node's, and to none
  ;; WHEN the setting an export draws with is asked for
  ;; THEN a selection setting exports all of them, none stays none
  (let ((canvas-graph-show-labels 'selected))
    (should (eq (canvas-graph--exported-labels) 'all)))
  (let ((canvas-graph-show-labels nil))
    (should-not (canvas-graph--exported-labels))))

;;;; The buffer and the keyboard

(ert-deftest canvas-graph-mode-shows-a-graph-with-the-keyboard-on-the-start ()
  ;; GIVEN a diagram buffer of the sample
  ;; THEN the mode derives from the diagram mode, the keyboard starts on the
  ;;      start node, AND the header line names the graph and the node
  (canvas-graph-test--in-buffer canvas-graph-test--sample '(600 . 400)
    (should (derived-mode-p 'canvas-diagram-mode))
    (should (equal (canvas-graph-test--selected) "IDLE"))
    (should (equal (canvas-diagram--header) "state › IDLE · start · 1 out, 4 in"))
    (canvas-diagram--select (canvas-graph-test--node "LOST"))
    (should (equal (canvas-diagram--header) "state › LOST · unreachable · 1 out, 0 in"))))

(ert-deftest canvas-graph-moves-follow-the-edges ()
  ;; GIVEN the sample with the keyboard on the start
  ;; WHEN it moves in, in, out, along its depth, to the branch and the next
  ;; THEN in follows the first edge out, out the first in, the depth moves
  ;;      stay in the row, branch is the start, next branch the next row,
  ;;      AND last is the last node
  (canvas-graph-test--in-buffer canvas-graph-test--sample '(600 . 400)
    (canvas-diagram-move-in)
    (should (equal (canvas-graph-test--selected) "RUN"))
    (canvas-diagram-move-in)
    (should (equal (canvas-graph-test--selected) "WAIT_ACK"))
    (canvas-diagram-move-next-at-depth)
    (should (equal (canvas-graph-test--selected) "ERROR"))
    (canvas-diagram-move-next-at-depth)
    (should (equal (canvas-graph-test--selected) "ERROR"))
    (canvas-diagram-move-previous-at-depth)
    (should (equal (canvas-graph-test--selected) "WAIT_ACK"))
    (canvas-diagram-move-out)
    (should (equal (canvas-graph-test--selected) "RUN"))
    (canvas-diagram-move-next-branch)
    (should (equal (canvas-graph-test--selected) "WAIT_ACK"))
    (canvas-diagram-move-branch)
    (should (equal (canvas-graph-test--selected) "IDLE"))
    (canvas-diagram-move-last)
    (should (equal (canvas-graph-test--selected) "LOST"))
    (canvas-diagram-move-next)
    (should (equal (canvas-graph-test--selected) "LOST"))
    (canvas-diagram-move-first)
    (should (equal (canvas-graph-test--selected) "IDLE"))))

(ert-deftest canvas-graph-siblings-share-a-source ()
  ;; GIVEN the keyboard on a node first reached from RUN, as another is;
  ;;       RUN leads back to IDLE too, but IDLE was there before it
  ;; WHEN it moves to the next sibling twice and back
  ;; THEN it walks the nodes RUN first reaches, and holds at the end
  (canvas-graph-test--in-buffer canvas-graph-test--sample '(600 . 400)
    (canvas-diagram--select (canvas-graph-test--node "WAIT_ACK"))
    (canvas-diagram-move-next-sibling)
    (should (equal (canvas-graph-test--selected) "ERROR"))
    (canvas-diagram-move-next-sibling)
    (should (equal (canvas-graph-test--selected) "ERROR"))
    (canvas-diagram-move-previous-sibling)
    (should (equal (canvas-graph-test--selected) "WAIT_ACK"))))

(ert-deftest canvas-graph-card-lists-the-edges ()
  ;; GIVEN the keyboard on RUN, and a graph with no type
  ;; WHEN their cards are asked for
  ;; THEN the title is the node, the path the graph and its type, the body
  ;;      lists the edges out with their labels, then those in, AND a
  ;;      graph with no type gives its name alone
  (canvas-graph-test--in-buffer canvas-graph-test--sample '(600 . 400)
    (should (equal (canvas-diagram-card-text canvas-diagram--diagram (canvas-graph-test--node "RUN"))
                   '("RUN" "state : state_t"
                     "→ WAIT_ACK  when done = '1'\n→ ERROR  when err = '1'\n→ IDLE  when abort = '1'\n← IDLE  when start = '1'")))
    (should (equal (nth 2 (canvas-diagram-card-text canvas-diagram--diagram (canvas-graph-test--node "LOST")))
                   "→ IDLE")))
  (let ((graph (canvas-graph-build '(:name "g" :nodes (("A"))))))
    (should (equal (canvas-graph-card graph (canvas-graph-start graph)) '("A" "g" "No edges.")))))

(ert-deftest canvas-graph-copying-a-node-copies-its-card ()
  ;; GIVEN the keyboard on RUN, then a package drawing a card of its own
  ;; WHEN RUN's content, what copying it copies, is asked for each time
  ;; THEN it is the card's title and body, AND then the package's card
  (canvas-graph-test--in-buffer canvas-graph-test--sample '(600 . 400)
    (let ((run (canvas-graph-test--node "RUN")))
      (should (equal (canvas-diagram-node-content canvas-diagram--diagram run)
                     (list "RUN" (nth 2 (canvas-diagram-card-text canvas-diagram--diagram run)))))
      (setf (canvas-diagram-callbacks canvas-diagram--diagram)
            (append (list :card (lambda (_d node) (list (canvas-diagram-node-label node) "path" "own body")))
                    (canvas-diagram-callbacks canvas-diagram--diagram)))
      (should (equal (canvas-diagram-node-content canvas-diagram--diagram run) '("RUN" "own body"))))))

(ert-deftest canvas-graph-trail-marks-the-edges-out ()
  ;; GIVEN the keyboard on A, which leads to B below it, with a selection colour configured
  ;; WHEN the graph is drawn
  ;; THEN the edge from A to B carries the selection colour
  (canvas-graph-test--in-buffer (canvas-graph-test--spec '("A" "B") '(("A" "B"))) '(400 . 300)
    (let* ((canvas-diagram-colors (append '(:selection "yellow") canvas-diagram-colors))
           (a (canvas-graph-test--node "A"))
           (b (canvas-graph-test--node "B"))
           (col (+ 10 (floor (canvas-diagram-middle-x a))))
           (mid (+ 10 (round (/ (+ (+ (canvas-diagram-node-y a) (canvas-diagram-node-h a)) (canvas-diagram-node-y b)) 2.0)))))
      (canvas-diagram--select a)
      (should (= (canvas-cairo-pixel canvas-diagram--context col mid) #xFFFFFF00)))))

(ert-deftest canvas-graph-changing-the-layout-keeps-the-keyboard ()
  ;; GIVEN the keyboard on ERROR in a layered drawing, no labels shown
  ;; WHEN the layout is cycled to a ring and the labels toggled round
  ;; THEN the keyboard is still on ERROR, the layout changed, AND the labels
  ;;      went from none to the selected node's, to the reader's own, all
  ;;      of them, as auto, then to none and round again
  (canvas-graph-test--in-buffer canvas-graph-test--sample '(600 . 400)
    (canvas-diagram--select (canvas-graph-test--node "ERROR"))
    (canvas-graph-cycle-layout)
    (should (eq canvas-graph-layout 'ring))
    (should (equal (canvas-graph-test--selected) "ERROR"))
    (should (equal (cl-loop repeat 4 collect (progn (canvas-graph-toggle-labels) canvas-graph-show-labels))
                   '(selected auto nil selected)))))

(ert-deftest canvas-graph-each-toggle-of-the-labels-shows-something-new ()
  ;; GIVEN a reader that keeps its labels for the panel, and one that puts
  ;;       them on the arrows
  ;; WHEN the labels are toggled from each way of showing them
  ;; THEN each toggle shows the next way after the one shown, the reader's
  ;;      own way reached as auto, AND so no toggle leaves the drawing as it was
  (should (equal (mapcar (lambda (shown) (canvas-graph--next-labels shown 'selected)) '(selected all nil))
                 '(all nil auto)))
  (should (equal (mapcar (lambda (shown) (canvas-graph--next-labels shown 'all)) '(all nil selected))
                 '(nil selected auto))))

(ert-deftest canvas-graph-keys-and-menu-have-the-layout-and-the-labels ()
  ;; GIVEN the mode map and the menu
  ;; THEN every graph setting key is in both with its command, the shared
  ;;      keys are inherited, AND the menu is the diagram's
  (pcase-dolist (`(,key . ,command) canvas-graph-setting-keys)
    (let ((suffix (transient-get-suffix 'canvas-graph-menu key)))
      (should suffix)
      (should (eq (plist-get (cdr suffix) :command) command))
      (should (eq (lookup-key canvas-graph-mode-map (kbd key)) command))))
  (should (eq (lookup-key canvas-graph-mode-map (kbd "L")) #'canvas-graph-cycle-layout))
  (should (eq (lookup-key canvas-graph-mode-map (kbd "t")) #'canvas-graph-toggle-labels))
  (should (eq (lookup-key canvas-graph-mode-map (kbd "n")) #'canvas-diagram-move-next))
  (should (eq (lookup-key canvas-graph-mode-map (kbd "SPC")) #'canvas-keys-menu))
  (should (transient-get-suffix 'canvas-graph-menu "+"))
  (should (transient-get-suffix 'canvas-graph-menu "B"))
  (should (eq (plist-get canvas-graph-callbacks :menu) 'canvas-graph-menu)))

(canvas-graph-define-menu canvas-graph-test--menu
  "A menu a package on the graph might define."
  ["Mine" ("x" ignore :transient t :description "mine")])

(ert-deftest canvas-graph-a-package-on-it-puts-its-own-in-front ()
  ;; GIVEN a diagram made with a header of a package's own, and a menu
  ;;       defined with a group of its own
  ;; THEN the diagram's header is the package's and the rest the graph's,
  ;;      AND the menu has the package's group, the graph's and the shared ones
  (let ((diagram (canvas-graph-diagram (list :header (lambda (_diagram _node) "mine")))))
    (should (equal (canvas-diagram--call diagram :header diagram nil) "mine"))
    (should (eq (plist-get (canvas-diagram-callbacks diagram) :layout) #'canvas-graph--lay-out)))
  (should (transient-get-suffix 'canvas-graph-test--menu "x"))
  (should (transient-get-suffix 'canvas-graph-test--menu "L"))
  (should (transient-get-suffix 'canvas-graph-test--menu "+")))

;;;; Showing and exporting

(ert-deftest canvas-graph-show-puts-a-spec-in-a-graph-buffer ()
  ;; GIVEN a spec
  ;; WHEN it is shown, with a buffer name and without
  ;; THEN a graph diagram of it is shown in the graph mode, in the buffer
  ;;      named or in *canvas-graph*
  (let (shown)
    (cl-letf (((symbol-function 'canvas-diagram-show)
               (lambda (name mode diagram spec &optional source)
                 (push (list name mode (plist-get (canvas-diagram-callbacks diagram) :layout) spec source) shown))))
      (canvas-graph-show canvas-graph-test--sample)
      (canvas-graph-show canvas-graph-test--sample "*mine*"))
    (should (equal shown (list (list "*mine*" #'canvas-graph-mode #'canvas-graph--lay-out canvas-graph-test--sample nil)
                               (list "*canvas-graph*" #'canvas-graph-mode #'canvas-graph--lay-out canvas-graph-test--sample nil))))))

(ert-deftest canvas-graph-follow-shows-what-a-reader-finds-and-reads-again ()
  ;; GIVEN a reader that finds a spec in a buffer, and one that finds none
  ;; WHEN a graph is shown following the buffer with each
  ;; THEN the first is shown in the graph mode from the spec it read, the
  ;;      buffer followed with that reader to read it again, callbacks
  ;;      given going first, AND the second is a user error
  (let ((source (generate-new-buffer " canvas-graph-test-source"))
        (read (lambda (_buffer) canvas-graph-test--sample))
        shown)
    (unwind-protect
        (cl-letf (((symbol-function 'canvas-diagram-show)
                   (lambda (name mode diagram spec &optional followed)
                     (let ((callbacks (canvas-diagram-callbacks diagram)))
                       (setq shown (list name mode (plist-get callbacks :read-source)
                                         (plist-get callbacks :header) (plist-get callbacks :layout)
                                         spec followed))))))
          (canvas-graph-follow source read "*mine*" (list :header #'ignore))
          (should (equal shown (list "*mine*" #'canvas-graph-mode read #'ignore #'canvas-graph--lay-out
                                     canvas-graph-test--sample source)))
          (should-error (canvas-graph-follow source #'ignore) :type 'user-error))
      (kill-buffer source))))

(ert-deftest canvas-graph-export-writes-a-png ()
  ;; GIVEN the sample and the demo
  ;; WHEN each is exported
  ;; THEN a PNG of some size is written, with no buffer involved, AND the
  ;;      demo has a folded edge, a loop and a node nothing reaches to show
  (let ((png (make-temp-file "canvas-graph-test" nil ".png"))
        (canvas-diagram-font "Sans 12px"))
    (unwind-protect
        (progn
          (should (equal (canvas-graph-export canvas-graph-test--sample png) png))
          (should (canvas-graph-test--png-p png))
          (should (> (file-attribute-size (file-attributes png)) 500))
          (delete-file png)
          (canvas-graph-export canvas-graph--demo png)
          (should (canvas-graph-test--png-p png)))
      (delete-file png)))
  (let ((demo (canvas-graph-build canvas-graph--demo)))
    (should (cl-some #'canvas-graph--from-any-p (canvas-graph-edges demo)))
    (should (cl-some #'canvas-graph--loop-p (canvas-graph-edges demo)))
    (should (cl-some (lambda (n) (null (canvas-graph-node-depth n))) (canvas-graph-nodes demo)))))

;;;; The package

(defun canvas-graph-test--library (name)
  "The source file of library NAME."
  (let ((file (locate-library (concat name ".el"))))
    (should file)
    file))

(defun canvas-graph-test--info (name)
  "Parse the package headers of library NAME."
  (with-temp-buffer
    (insert-file-contents (canvas-graph-test--library name))
    (package-buffer-info)))

(ert-deftest canvas-graph-package-headers-describe-the-package ()
  ;; GIVEN the library as it is published
  ;; WHEN package.el reads its headers
  ;; THEN it finds the name, a version, a one-line summary and a URL
  (let ((info (canvas-graph-test--info "canvas-graph")))
    (should (equal (package-desc-name info) 'canvas-graph))
    (should (version-list-<= '(0 1) (package-desc-version info)))
    (should-not (equal (package-desc-summary info) "No description available."))
    (should (string-prefix-p "https://" (cdr (assq :url (package-desc-extras info)))))))

(ert-deftest canvas-graph-package-requires-emacs-and-canvas-diagram ()
  ;; GIVEN the library, which cannot run without a canvas or the shared package
  ;; WHEN its requirements are read
  ;; THEN both are declared, AND the Emacs it asks for is one this very
  ;;      Emacs satisfies -- a requirement no built Emacs meets would keep
  ;;      package.el from ever installing it
  (let* ((reqs (package-desc-reqs (canvas-graph-test--info "canvas-graph")))
         (wanted (cadr (assq 'emacs reqs))))
    (should (version-list-<= '(32) wanted))
    (should (version-list-<= wanted (version-to-list emacs-version)))
    (should (assq 'canvas-diagram reqs))))

(ert-deftest canvas-graph-package-names-its-author-and-licence ()
  ;; GIVEN the library, published under the GPL alongside the other
  ;;       canvas packages
  ;; WHEN its headers and leading comments are read
  ;; THEN it names an author, holds the copyright the way they all do,
  ;;      AND carries a Commentary section and the licence notice
  (with-temp-buffer
    (insert-file-contents (canvas-graph-test--library "canvas-graph"))
    (should (string-match-p "[^ ]" (or (lm-header "author") "")))
    (should (save-excursion
              (goto-char (point-min))
              (re-search-forward "^;; Copyright (C) [0-9-]+ canvas-graph contributors$" nil t)))
    (should (lm-commentary-start))
    (should (save-excursion (re-search-forward "GNU General Public License" nil t)))))

(ert-deftest canvas-graph-licence-file-is-the-whole-gpl ()
  ;; GIVEN a package that says it is GPL-3.0-or-later
  ;; WHEN the LICENSE file beside it is read
  ;; THEN it is the licence itself, not a summary pointing elsewhere
  (let ((license (expand-file-name
                  "LICENSE" (file-name-directory (canvas-graph-test--library "canvas-graph")))))
    (should (file-readable-p license))
    (with-temp-buffer
      (insert-file-contents license)
      (should (save-excursion (re-search-forward "TERMS AND CONDITIONS" nil t)))
      (should (save-excursion (re-search-forward "Version 3, 29 June 2007" nil t))))))

;;;; Nodes with rows, and edges that join one

(ert-deftest canvas-graph-a-node-may-hold-rows-and-an-edge-may-name-one ()
  ;; GIVEN a spec whose nodes hold rows, with two edges between the same
  ;;       two nodes, each naming rows of its own
  ;; WHEN it is built
  ;; THEN the nodes keep their rows, AND the two edges stay apart, because
  ;;      they join different rows, where edges with no rows would have
  ;;      become one
  (let* ((spec (list :name "ports" :nodes '(("fifo" :rows ("clk" "data" "full"))
                                            ("dut" :rows ("tick" "out")))
                     :edges '(("dut" "fifo" "a" nil :from-row "tick" :to-row "clk")
                              ("dut" "fifo" "b" nil :from-row "out" :to-row "data"))))
         (graph (canvas-graph-build spec))
         (edges (canvas-graph-edges graph)))
    (should (equal (canvas-diagram-node-rows (canvas-graph-node-named graph "fifo")) '("clk" "data" "full")))
    (should (= (length edges) 2))
    (should (equal (mapcar #'canvas-graph-edge-from-row edges) '("tick" "out")))
    (should (equal (mapcar #'canvas-graph-edge-to-row edges) '("clk" "data")))))

(ert-deftest canvas-graph-a-draft-writes-down-the-rows-of-a-node ()
  ;; GIVEN a draft with a node given three rows, one of them twice
  ;; WHEN its spec is asked for
  ;; THEN the node carries those rows in the order they were given, each
  ;;      once, AND a node given none carries no rows at all
  (let ((draft (canvas-graph-draft-create)))
    (canvas-graph-draft-node draft "fifo" "Fifo" 10)
    (canvas-graph-draft-row draft "fifo" "clk")
    (canvas-graph-draft-row draft "fifo" "data")
    (canvas-graph-draft-row draft "fifo" "clk")
    (canvas-graph-draft-node draft "plain")
    (let ((spec (canvas-graph-draft-spec draft "ports")))
      (should (equal (plist-get (cdr (assoc "fifo" (plist-get spec :nodes))) :rows) '("clk" "data")))
      (should-not (plist-member (cdr (assoc "plain" (plist-get spec :nodes))) :rows)))))

(ert-deftest canvas-graph-edges-that-join-rows-are-never-folded-away ()
  ;; GIVEN three edges between two nodes, each joining a row of its own,
  ;;       which without rows would be folded into an any-node marker
  ;; WHEN the graph is built
  ;; THEN none of them is common, because each lands in a place of its own
  (let* ((spec (list :name "ports"
                     :nodes '(("dut" :rows ("clk" "wr" "data"))
                              ("fifo" :rows ("clk" "wr" "data")))
                     :edges '(("dut" "fifo" nil nil :from-row "clk" :to-row "clk")
                              ("dut" "fifo" nil nil :from-row "wr" :to-row "wr")
                              ("dut" "fifo" nil nil :from-row "data" :to-row "data"))))
         (graph (canvas-graph-build spec)))
    (should-not (cl-some #'canvas-graph-edge-common (canvas-graph-edges graph)))))

(ert-deftest canvas-graph-an-edge-of-rows-lands-on-the-row-it-names ()
  ;; GIVEN a graph of two nodes of rows, laid out, with an edge from one
  ;;       row to another
  ;; WHEN its arrow is measured
  ;; THEN each end lies at the height of the row it names, not at the
  ;;      middle of the box, AND an edge that names no row still leaves
  ;;      from the border as before
  (canvas-graph-test--rendering
   (canvas-graph-test--with-context ctx 4 4
     (let* ((spec (list :name "ports" :nodes '(("a" :rows ("one" "two" "three"))
                                               ("b" :rows ("first" "second" "third")))
                        :edges '(("a" "b" nil nil :from-row "three" :to-row "first")
                                 ("a" "b" "plain"))))
            (diagram (canvas-graph-test--laid-out spec ctx))
            (graph (canvas-diagram-model diagram))
            (rowed (car (canvas-graph-edges graph)))
            (plain (cadr (canvas-graph-edges graph))))
       (pcase-let ((`(,_ ,y0 ,_ ,_ ,_ ,y1) (canvas-graph--curve rowed 0))
                   (`(,_ ,py0 ,_ ,_ ,_ ,_) (canvas-graph--curve plain 0)))
         (should (< (abs (- y0 (cdr (canvas-diagram-row-anchor (canvas-graph-edge-from rowed) "three" 'right)))) 1))
         (should (< (abs (- y1 (cdr (canvas-diagram-row-anchor (canvas-graph-edge-to rowed) "first" 'left)))) 1))
         (should (/= y0 py0)))))))

(ert-deftest canvas-graph-is-the-layout-of-its-kind ()
  ;; GIVEN canvas-graph loaded
  ;; THEN readers draw a graph, a spec of kind graph, with it: it follows
  ;;      the source AND exports a picture
  (should (equal (alist-get 'graph canvas-diagram-layouts)
                 '(:follow canvas-graph-follow :export canvas-graph-export))))

;;; canvas-graph-tests.el ends here
