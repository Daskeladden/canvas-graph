;;; canvas-graph-layout-acceptance.el --- labels beside their arrows -*- lexical-binding: t -*-

;; These tests show a graph with `canvas-graph-show' in a real window and
;; press the real key `L', so they need a display.  `make
;; layout-acceptance' runs them on a private Xvfb display.  They read
;; where the layout put each box, arrow and label from the diagram of the
;; buffer, which is what the canvas draws.

(require 'ert)
(require 'cl-lib)
(require 'transient)
(require 'canvas-graph)

;;;; Helpers

(defconst canvas-graph-layout-acceptance--name "*graph-layout-acceptance*"
  "The name of the buffer that shows the graph of a test.")

(defmacro canvas-graph-layout-acceptance--with-graph (spec &rest body)
  "Show SPEC in a window with the default layout, select it and run BODY.
A layout chosen with `L' in BODY does not outlive the test."
  (declare (indent 1))
  `(let ((canvas-graph-layout (default-toplevel-value 'canvas-graph-layout)))
     (delete-other-windows)
     (let ((buffer (canvas-graph-show ,spec canvas-graph-layout-acceptance--name)))
       (unwind-protect
           (progn
             (select-window (or (get-buffer-window buffer) (error "The graph has no window")))
             (redisplay t)
             ,@body)
         (kill-buffer buffer)))))

(defun canvas-graph-layout-acceptance--press (key)
  "Press KEY in the window of the graph."
  (execute-kbd-macro (kbd key))
  (redisplay t))

(defun canvas-graph-layout-acceptance--menu-layout ()
  "What the menu says on the line of the key `L'."
  ;; The menu's line is (CLASS . PLIST), with a level before the class in
  ;; some versions of transient.
  (let* ((suffix (transient-get-suffix 'canvas-graph-menu "L"))
         (description (plist-get (cl-member-if #'keywordp suffix) :description)))
    (substring-no-properties (if (functionp description) (funcall description) description))))

(defun canvas-graph-layout-acceptance--graph ()
  "The graph that the selected window shows."
  (canvas-diagram-model canvas-diagram--diagram))

(defun canvas-graph-layout-acceptance--node (id)
  "The box of the state ID."
  (or (cl-find id (canvas-graph-nodes (canvas-graph-layout-acceptance--graph))
               :key #'canvas-graph-node-id :test #'equal)
      (error "No state %s" id)))

(defun canvas-graph-layout-acceptance--edge (from to)
  "The arrow from the state FROM to the state TO."
  (or (cl-find-if (lambda (edge)
                    (and (equal (canvas-graph-node-id (canvas-graph-edge-from edge)) from)
                         (equal (canvas-graph-node-id (canvas-graph-edge-to edge)) to)))
                  (canvas-graph-edges (canvas-graph-layout-acceptance--graph)))
      (error "No arrow from %s to %s" from to)))

(defun canvas-graph-layout-acceptance--label (edge)
  "The patch of EDGE's label, (X Y W H)."
  (or (canvas-graph-edge-label-rect edge) (error "The arrow has no label")))

(defun canvas-graph-layout-acceptance--points (edge)
  "41 points along EDGE's arrow as it is drawn."
  (let ((ks (mapcar (lambda (i) (/ i 40.0)) (number-sequence 0 40))))
    (if (canvas-graph--loop-p edge)
        (pcase-let ((`(,x0 ,y0 ,c1x ,c1y ,c2x ,c2y ,x1 ,y1)
                     (canvas-graph--loop-geometry (canvas-graph-edge-from edge))))
          (mapcar (lambda (k)
                    (let ((m (- 1 k)))
                      (cons (+ (* m m m x0) (* 3 m m k c1x) (* 3 m k k c2x) (* k k k x1))
                            (+ (* m m m y0) (* 3 m m k c1y) (* 3 m k k c2y) (* k k k y1)))))
                  ks))
      (let ((curve (canvas-graph--geometry edge (canvas-graph-layout-acceptance--graph))))
        (mapcar (lambda (k) (canvas-graph--point-on curve k)) ks)))))

(defun canvas-graph-layout-acceptance--middle (edge)
  "The point halfway along EDGE's arrow."
  (nth 20 (canvas-graph-layout-acceptance--points edge)))

(defun canvas-graph-layout-acceptance--straight-p (edge)
  "Whether EDGE's arrow runs straight from box to box."
  (pcase-let ((`(,x0 ,y0 ,cx ,cy ,x1 ,y1)
               (canvas-graph--geometry edge (canvas-graph-layout-acceptance--graph))))
    (let ((len (max 0.001 (sqrt (+ (expt (- x1 x0) 2) (expt (- y1 y0) 2))))))
      (< (/ (abs (- (* (- x1 x0) (- y0 cy)) (* (- x0 cx) (- y1 y0)))) len) 0.5))))

(defun canvas-graph-layout-acceptance--inside-p (point rect)
  "Whether POINT lies inside RECT, (X Y W H)."
  (pcase-let ((`(,x ,y ,w ,h) rect))
    (and (< x (car point) (+ x w)) (< y (cdr point) (+ y h)))))

(defun canvas-graph-layout-acceptance--distance (point rect)
  "Pixels from POINT to the nearest point of RECT, (X Y W H)."
  (pcase-let ((`(,x ,y ,w ,h) rect))
    (let ((dx (max (- x (car point)) 0 (- (car point) (+ x w))))
          (dy (max (- y (cdr point)) 0 (- (cdr point) (+ y h)))))
      (sqrt (+ (* dx dx) (* dy dy))))))

(defun canvas-graph-layout-acceptance--labelled-edges ()
  "The arrows of the graph that have a label."
  (cl-remove-if-not #'canvas-graph-edge-label-rect
                    (canvas-graph-edges (canvas-graph-layout-acceptance--graph))))

(defun canvas-graph-layout-acceptance--under-a-label-p (edge)
  "Whether some part of EDGE's arrow runs under a label."
  (let ((rects (mapcar #'canvas-graph-edge-label-rect
                       (canvas-graph-layout-acceptance--labelled-edges))))
    (cl-some (lambda (point)
               (cl-some (lambda (rect) (canvas-graph-layout-acceptance--inside-p point rect)) rects))
             (canvas-graph-layout-acceptance--points edge))))

(defun canvas-graph-layout-acceptance--beside-p (edge)
  "Whether EDGE's label sits beside its own arrow: no more than 12 pixels
from it."
  (let ((rect (canvas-graph-layout-acceptance--label edge)))
    (<= (apply #'min (mapcar (lambda (p) (canvas-graph-layout-acceptance--distance p rect))
                             (canvas-graph-layout-acceptance--points edge)))
        12)))

(defun canvas-graph-layout-acceptance--bottom (node)
  "The y of the bottom of NODE's box."
  (+ (canvas-diagram-node-y node) (canvas-diagram-node-h node)))

(defun canvas-graph-layout-acceptance--right (id)
  "The x of the right side of the box of the state ID."
  (let ((node (canvas-graph-layout-acceptance--node id)))
    (+ (canvas-diagram-node-x node) (canvas-diagram-node-w node))))

(defun canvas-graph-layout-acceptance--rows-apart (upper lower)
  "Pixels between the box of the state UPPER and the box of LOWER below it."
  (- (canvas-diagram-node-y (canvas-graph-layout-acceptance--node lower))
     (canvas-graph-layout-acceptance--bottom (canvas-graph-layout-acceptance--node upper))))

(defun canvas-graph-layout-acceptance--clearance (from to)
  "Pixels between the label of the arrow from FROM down to TO and the
nearer of the two boxes, above and below it."
  (pcase-let ((`(,_ ,y ,_ ,h) (canvas-graph-layout-acceptance--label
                               (canvas-graph-layout-acceptance--edge from to))))
    (min (- y (canvas-graph-layout-acceptance--bottom (canvas-graph-layout-acceptance--node from)))
         (- (canvas-diagram-node-y (canvas-graph-layout-acceptance--node to)) (+ y h)))))

;;;; The coffee machine

(defconst canvas-graph-layout-acceptance--machine
  '(:name "coffee machine" :start "idle"
    :nodes (("idle") ("heating") ("ready"))
    :edges (("idle" "heating" "power on")
            ("heating" "heating" "still cold")
            ("heating" "ready" "water hot")
            ("heating" "ready" "pressure up")
            ("heating" "ready" "light on")))
  "Three states one below the other: labels between the rows, one of them
three lines long, and a self loop.")

(defconst canvas-graph-layout-acceptance--two-states
  '(:name "kettle" :start "off" :nodes (("off") ("on")) :edges (("off" "on")))
  "Two states joined by an arrow without a label.")

(defconst canvas-graph-layout-acceptance--both-ways
  '(:name "brewer" :start "idle"
    :nodes (("idle") ("brewing"))
    :edges (("idle" "brewing" "button pressed")
            ("brewing" "idle" "cup full")))
  "Two states with an arrow each way between them.")

(defconst canvas-graph-layout-acceptance--side-by-side
  '(:name "grinder" :start "idle"
    :nodes (("idle") ("grinding") ("heating"))
    :edges (("idle" "grinding") ("idle" "heating")
            ("grinding" "heating" "beans ground")))
  "Two states in one row, joined by a labelled arrow.")

;;;; The scenarios

(ert-deftest canvas-graph-layout-acceptance-labels-beside-their-arrows ()
  ;; GIVEN a coffee machine whose arrows between the rows have labels, one
  ;; of them three lines long, and a self loop with a label
  ;; WHEN it is shown with the default settings
  ;; THEN each arrow between the rows runs straight from box to box
  ;; AND no arrow runs under any label
  ;; AND each label sits beside its own arrow
  ;; AND each label between the rows stays at least 22 pixels clear of the
  ;; boxes above and below it
  ;; AND the label of the self loop sits right of the loop
  (canvas-graph-layout-acceptance--with-graph canvas-graph-layout-acceptance--machine
    (let ((between (list (canvas-graph-layout-acceptance--edge "idle" "heating")
                         (canvas-graph-layout-acceptance--edge "heating" "ready")))
          (loop (canvas-graph-layout-acceptance--edge "heating" "heating")))
      (should (cl-every #'canvas-graph-layout-acceptance--straight-p between))
      (should-not (cl-some #'canvas-graph-layout-acceptance--under-a-label-p
                           (cons loop between)))
      (should (cl-every #'canvas-graph-layout-acceptance--beside-p between))
      (should (>= (canvas-graph-layout-acceptance--clearance "idle" "heating") 22))
      (should (>= (canvas-graph-layout-acceptance--clearance "heating" "ready") 22))
      (should (> (nth 0 (canvas-graph-layout-acceptance--label loop))
                 (+ (canvas-graph-layout-acceptance--right "heating")
                    canvas-graph--loop-height))))))

(ert-deftest canvas-graph-layout-acceptance-rows-at-least-64-apart ()
  ;; GIVEN two states joined by an arrow without a label
  ;; WHEN they are shown with the default settings
  ;; THEN their rows are at least 64 pixels apart
  (canvas-graph-layout-acceptance--with-graph canvas-graph-layout-acceptance--two-states
    (should (>= (canvas-graph-layout-acceptance--rows-apart "off" "on") 64))))

(ert-deftest canvas-graph-layout-acceptance-two-way-arrows-side-by-side ()
  ;; GIVEN two states with a labelled arrow each way between them
  ;; WHEN they are shown with the default settings
  ;; THEN both arrows run straight and side by side, 20 pixels apart
  ;; AND each label sits on the outer side of its own arrow
  (canvas-graph-layout-acceptance--with-graph canvas-graph-layout-acceptance--both-ways
    (let* ((there (canvas-graph-layout-acceptance--edge "idle" "brewing"))
           (back (canvas-graph-layout-acceptance--edge "brewing" "idle"))
           (pair (sort (list there back)
                       (lambda (a b) (< (car (canvas-graph-layout-acceptance--middle a))
                                        (car (canvas-graph-layout-acceptance--middle b))))))
           (left (car pair))
           (right (cadr pair)))
      (should (canvas-graph-layout-acceptance--straight-p there))
      (should (canvas-graph-layout-acceptance--straight-p back))
      (should (< (abs (- (- (car (canvas-graph-layout-acceptance--middle right))
                            (car (canvas-graph-layout-acceptance--middle left)))
                         20))
                 1))
      (pcase-let ((`(,lx ,_ ,lw ,_) (canvas-graph-layout-acceptance--label left))
                  (`(,rx ,_ ,_ ,_) (canvas-graph-layout-acceptance--label right)))
        (should (<= (+ lx lw) (car (canvas-graph-layout-acceptance--middle left))))
        (should (>= rx (car (canvas-graph-layout-acceptance--middle right))))))))

(ert-deftest canvas-graph-layout-acceptance-label-above-an-arrow-within-a-row ()
  ;; GIVEN two states side by side in one row, joined by a labelled arrow
  ;; WHEN they are shown with the default settings
  ;; THEN the label sits above the arrow, over its middle, and not on it
  (canvas-graph-layout-acceptance--with-graph canvas-graph-layout-acceptance--side-by-side
    (let* ((edge (canvas-graph-layout-acceptance--edge "grinding" "heating"))
           (middle (canvas-graph-layout-acceptance--middle edge)))
      (pcase-let ((`(,x ,y ,w ,h) (canvas-graph-layout-acceptance--label edge)))
        (should (<= (+ y h) (cdr middle)))
        (should (< x (car middle) (+ x w)))
        (should-not (canvas-graph-layout-acceptance--under-a-label-p edge))))))

(ert-deftest canvas-graph-layout-acceptance-l-goes-round-four-layouts ()
  ;; GIVEN a graph shown with the default settings
  ;; THEN the menu names the layout layered
  ;; WHEN the user presses L
  ;; THEN the menu names compact
  ;; WHEN the user presses L again
  ;; THEN the menu names dot
  ;; WHEN the user presses L a third time
  ;; THEN the menu names ring
  ;; WHEN the user presses L a fourth time
  ;; THEN the menu names layered again
  (canvas-graph-layout-acceptance--with-graph canvas-graph-layout-acceptance--machine
    (should (string-match-p "\\_<layered\\_>" (canvas-graph-layout-acceptance--menu-layout)))
    (canvas-graph-layout-acceptance--press "L")
    (should (string-match-p "\\_<compact\\_>" (canvas-graph-layout-acceptance--menu-layout)))
    (canvas-graph-layout-acceptance--press "L")
    (should (string-match-p "\\_<dot\\_>" (canvas-graph-layout-acceptance--menu-layout)))
    (canvas-graph-layout-acceptance--press "L")
    (should (string-match-p "\\_<ring\\_>" (canvas-graph-layout-acceptance--menu-layout)))
    (canvas-graph-layout-acceptance--press "L")
    (should (string-match-p "\\_<layered\\_>" (canvas-graph-layout-acceptance--menu-layout)))))

(ert-deftest canvas-graph-layout-acceptance-compact-is-the-old-look ()
  ;; GIVEN two states joined by an arrow with a one-line label
  ;; WHEN the user presses L once, for the compact layout
  ;; THEN the label sits on its arrow, which bends through the middle of
  ;; the label, as before
  ;; AND the rows are 48 pixels apart, as before
  (canvas-graph-layout-acceptance--with-graph
      '(:name "kettle" :start "off" :nodes (("off") ("on")) :edges (("off" "on" "switch")))
    (canvas-graph-layout-acceptance--press "L")
    (pcase-let* ((edge (canvas-graph-layout-acceptance--edge "off" "on"))
                 (`(,x ,y ,w ,h) (canvas-graph-layout-acceptance--label edge))
                 (middle (canvas-graph-layout-acceptance--middle edge)))
      (should (< (abs (- (car middle) (+ x (/ w 2.0)))) 2))
      (should (< (abs (- (cdr middle) (+ y (/ h 2.0)))) 2))
      (should (= (canvas-graph-layout-acceptance--rows-apart "off" "on") 48)))))

;;;; Running them

(defun canvas-graph-layout-acceptance-run ()
  "Run these tests, show the result on the standard error stream and
leave Emacs."
  (let ((stats (ert-run-tests-batch "^canvas-graph-layout-acceptance-")))
    (princ (with-current-buffer "*Messages*" (buffer-string)) #'external-debugging-output)
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(provide 'canvas-graph-layout-acceptance)
;;; canvas-graph-layout-acceptance.el ends here
