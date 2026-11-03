;;; canvas-graph-dot-acceptance.el --- the dot layout of graphviz -*- lexical-binding: t -*-

;; These tests show a graph with `canvas-graph-show' in a real window in
;; the layout `dot', which runs the program dot of graphviz, and read
;; where the boxes, labels, arrows and frames came to stand.  A test
;; that needs dot skips itself when dot is not installed.  `make
;; dot-acceptance' runs them on a private Xvfb display.

(require 'ert)
(require 'cl-lib)
(require 'canvas-graph)
(require 'canvas-graph-tint-acceptance)

(defvar canvas-graph-dot-program)

;;;; Helpers

(defconst canvas-graph-dot-acceptance--name "*graph-dot-acceptance*"
  "The name of the buffer that shows the graph of a test.")

(defmacro canvas-graph-dot-acceptance--with-graph (layout spec &rest body)
  "Show SPEC in a window in LAYOUT, select it and run BODY."
  (declare (indent 2))
  `(let ((canvas-graph-layout ,layout))
     (delete-other-windows)
     (let ((buffer (canvas-graph-show ,spec canvas-graph-dot-acceptance--name)))
       (unwind-protect
           (progn
             (select-window (or (get-buffer-window buffer) (error "The graph has no window")))
             (redisplay t)
             ,@body)
         (kill-buffer buffer)))))

(defun canvas-graph-dot-acceptance--need-dot ()
  "Skip the test when graphviz is not installed."
  ;; `skip-unless' is a macro of the body of a test only.
  (unless (executable-find "dot")
    (ert-skip "graphviz is not installed")))

(defun canvas-graph-dot-acceptance--graph ()
  "The graph that the selected window shows."
  (canvas-diagram-model canvas-diagram--diagram))

(defun canvas-graph-dot-acceptance--node (id)
  "The box of the state ID."
  (or (canvas-graph-node-named (canvas-graph-dot-acceptance--graph) id)
      (error "No state %s" id)))

(defun canvas-graph-dot-acceptance--box (id)
  "The box of the state ID, (X Y W H)."
  (canvas-graph--box (canvas-graph-dot-acceptance--node id)))

(defun canvas-graph-dot-acceptance--boxes ()
  "Each box of the graph: (ID X Y W H)."
  (mapcar (lambda (node) (cons (canvas-graph-node-id node) (canvas-graph--box node)))
          (canvas-graph-nodes (canvas-graph-dot-acceptance--graph))))

(defun canvas-graph-dot-acceptance--edge (from to)
  "The arrow from the state FROM to the state TO."
  (or (cl-find-if (lambda (edge)
                    (and (equal (canvas-graph-node-id (canvas-graph-edge-from edge)) from)
                         (equal (canvas-graph-node-id (canvas-graph-edge-to edge)) to)))
                  (canvas-graph-edges (canvas-graph-dot-acceptance--graph)))
      (error "No arrow from %s to %s" from to)))

(defun canvas-graph-dot-acceptance--arrows ()
  "The arrows of the graph, loops aside."
  (cl-remove-if (lambda (e) (or (canvas-graph--loop-p e) (not (canvas-graph--drawn-p e))))
                (canvas-graph-edges (canvas-graph-dot-acceptance--graph))))

(defun canvas-graph-dot-acceptance--points (edge)
  "Points along EDGE's arrow as it is drawn, from its start to its tip."
  (canvas-graph--curve-points edge))

(defun canvas-graph-dot-acceptance--inside-p (point rect)
  "Whether POINT lies inside RECT, (X Y W H)."
  (pcase-let ((`(,x ,y ,w ,h) rect))
    (and (< x (car point) (+ x w)) (< y (cdr point) (+ y h)))))

(defun canvas-graph-dot-acceptance--distance (point rect)
  "Pixels from POINT to the nearest point of RECT, (X Y W H)."
  (pcase-let ((`(,x ,y ,w ,h) rect))
    (let ((dx (max (- x (car point)) 0 (- (car point) (+ x w))))
          (dy (max (- y (cdr point)) 0 (- (cdr point) (+ y h)))))
      (sqrt (+ (* dx dx) (* dy dy))))))

(defun canvas-graph-dot-acceptance--arrow-distance (edge rect)
  "Pixels from EDGE's arrow to RECT."
  (apply #'min (mapcar (lambda (p) (canvas-graph-dot-acceptance--distance p rect))
                       (canvas-graph-dot-acceptance--points edge))))

(defun canvas-graph-dot-acceptance--on-border-p (point rect)
  "Whether POINT lies on the border of RECT, (X Y W H), within 1.5 pixels."
  (pcase-let ((`(,x ,y ,w ,h) rect))
    (and (<= (- x 1.5) (car point) (+ x w 1.5)) (<= (- y 1.5) (cdr point) (+ y h 1.5))
         (or (< (abs (- (car point) x)) 1.5) (< (abs (- (car point) (+ x w))) 1.5)
             (< (abs (- (cdr point) y)) 1.5) (< (abs (- (cdr point) (+ y h))) 1.5)))))

(defun canvas-graph-dot-acceptance--overlap-p (a b)
  "Whether the rects A and B, (X Y W H), overlap."
  (not (or (<= (+ (nth 0 a) (nth 2 a)) (nth 0 b)) (<= (+ (nth 0 b) (nth 2 b)) (nth 0 a))
           (<= (+ (nth 1 a) (nth 3 a)) (nth 1 b)) (<= (+ (nth 1 b) (nth 3 b)) (nth 1 a)))))

(defun canvas-graph-dot-acceptance--holds-p (outer inner)
  "Whether the rect OUTER holds the whole of the rect INNER."
  (and (<= (nth 0 outer) (nth 0 inner)) (<= (nth 1 outer) (nth 1 inner))
       (<= (+ (nth 0 inner) (nth 2 inner)) (+ (nth 0 outer) (nth 2 outer)))
       (<= (+ (nth 1 inner) (nth 3 inner)) (+ (nth 1 outer) (nth 3 outer)))))

(defun canvas-graph-dot-acceptance--frames-of (group)
  "The rects of the frames of the group GROUP, by its id."
  (let ((graph (canvas-graph-dot-acceptance--graph)))
    (mapcar (lambda (frame) (canvas-graph--frame-rect frame graph))
            (cl-remove-if-not (lambda (frame) (equal (canvas-graph-group-id (canvas-graph-frame-group frame)) group))
                              (canvas-graph-frames graph)))))

(defun canvas-graph-dot-acceptance--sizes (layout spec)
  "The sizes of the boxes of SPEC in LAYOUT: ((ID W . H)...)."
  (canvas-graph-dot-acceptance--with-graph layout spec
    (mapcar (lambda (node) (cons (canvas-graph-node-id node)
                                 (cons (canvas-diagram-node-w node) (canvas-diagram-node-h node))))
            (canvas-graph-nodes (canvas-graph-dot-acceptance--graph)))))

(defun canvas-graph-dot-acceptance--ink-p (ctx x y)
  "Whether the picture on CTX has the ink of a line at X Y, not the
background."
  (let* ((argb (canvas-cairo-pixel ctx (round x) (round y)))
         (rgb (mapcar (lambda (shift) (/ (logand (ash argb (- shift)) #xFF) 255.0)) '(16 8 0)))
         (background (canvas-diagram-color :background)))
    (> (apply #'+ (cl-mapcar (lambda (a b) (abs (- a b))) rgb background)) 0.3)))

(defun canvas-graph-dot-acceptance--head-points (edge)
  "Two points inside a well turned arrowhead of EDGE, in the picture: 6
pixels back from its tip along the line that comes in, and 2 pixels to
either side, well inside the 3 pixels the head is wide there."
  (let* ((points (reverse (canvas-graph-dot-acceptance--points edge)))
         (tip (car points))
         (back (or (cl-find-if (lambda (p) (>= (sqrt (+ (expt (- (car p) (car tip)) 2) (expt (- (cdr p) (cdr tip)) 2))) 10))
                               points)
                   (car (last points))))
         (len (sqrt (+ (expt (- (car tip) (car back)) 2) (expt (- (cdr tip) (cdr back)) 2))))
         (dx (/ (- (car tip) (car back)) len)) (dy (/ (- (cdr tip) (cdr back)) len))
         (m canvas-diagram-margin))
    (list (cons (+ m (car tip) (* -6 dx) (* 2 (- dy))) (+ m (cdr tip) (* -6 dy) (* 2 dx)))
          (cons (+ m (car tip) (* -6 dx) (* -2 (- dy))) (+ m (cdr tip) (* -6 dy) (* -2 dx))))))

;;;; The graphs

(defconst canvas-graph-dot-acceptance--kitchen
  '(:name "kitchen" :start "order"
    :nodes (("order") ("pay") ("brew") ("serve"))
    :edges (("order" "pay") ("pay" "brew") ("brew" "serve"))
    :groups (("counter" :title "Counter" :nodes ("order" "pay" "serve"))))
  "A coffee order whose counter takes the order and the money, and serves
the cup that the machine brews.  In the layered layout the counter falls
into two frames, as the machine stands between its boxes.")

(defconst canvas-graph-dot-acceptance--machine
  '(:name "machine" :start "idle"
    :nodes (("idle") ("grinding") ("brewing") ("cleaning") ("ready"))
    :edges (("idle" "grinding" "button")
            ("grinding" "grinding" "more beans")
            ("grinding" "brewing" "beans ground")
            ("brewing" "ready" "cup full")
            ("ready" "idle" "cup taken")
            ("brewing" "cleaning" "scale found")
            ("cleaning" "brewing" "descaled")))
  "A coffee machine with a self loop, an arrow each way between two
states and a long arrow back to the start, every arrow labelled.")

(defconst canvas-graph-dot-acceptance--forward
  '(("idle" . "grinding") ("grinding" . "brewing") ("brewing" . "ready") ("brewing" . "cleaning"))
  "The arrows of the coffee machine that lead on, not back.")

;;;; The scenarios

(ert-deftest canvas-graph-dot-acceptance-a-group-is-one-cluster ()
  ;; GIVEN a coffee order whose counter group holds the order, the payment
  ;;       and the serving, with the brewing between them
  ;; WHEN it is shown in the dot layout
  ;; THEN the counter has one frame, which holds the boxes of the order,
  ;;      the payment and the serving
  ;; AND the box of the brewing lies outside that frame
  (canvas-graph-dot-acceptance--need-dot)
  (canvas-graph-dot-acceptance--with-graph 'dot canvas-graph-dot-acceptance--kitchen
    (let ((frames (canvas-graph-dot-acceptance--frames-of "counter")))
      (should (= (length frames) 1))
      (dolist (id '("order" "pay" "serve"))
        (should (canvas-graph-dot-acceptance--holds-p (car frames) (canvas-graph-dot-acceptance--box id))))
      (should-not (canvas-graph-dot-acceptance--overlap-p (car frames) (canvas-graph-dot-acceptance--box "brew"))))))

(ert-deftest canvas-graph-dot-acceptance-each-label-beside-its-own-arrow ()
  ;; GIVEN a coffee machine with a self loop, an arrow each way between
  ;;       two states and a long arrow back to the start, every arrow
  ;;       labelled
  ;; WHEN it is shown in the dot layout
  ;; THEN each label of an arrow lies within 12 pixels of its own arrow,
  ;;      nearer to it than to any other arrow
  ;; AND no arrow runs under any label
  (canvas-graph-dot-acceptance--need-dot)
  (canvas-graph-dot-acceptance--with-graph 'dot canvas-graph-dot-acceptance--machine
    (let ((arrows (canvas-graph-dot-acceptance--arrows)))
      (dolist (edge arrows)
        (let ((rect (or (canvas-graph-edge-label-rect edge) (error "An arrow has no label"))))
          (should (<= (canvas-graph-dot-acceptance--arrow-distance edge rect) 12))
          (dolist (other (remq edge arrows))
            (should (< (canvas-graph-dot-acceptance--arrow-distance edge rect)
                       (canvas-graph-dot-acceptance--arrow-distance other rect))))
          (dolist (other arrows)
            (should-not (cl-some (lambda (p) (canvas-graph-dot-acceptance--inside-p p rect))
                                 (canvas-graph-dot-acceptance--points other)))))))))

(ert-deftest canvas-graph-dot-acceptance-arrows-meet-their-boxes-head-on ()
  ;; GIVEN the coffee machine
  ;; WHEN it is shown in the dot layout and the user writes its picture
  ;; THEN each arrow starts on the border of its first box and ends on the
  ;;      border of its last box
  ;; AND each arrowhead points along the last piece of its line
  (canvas-graph-dot-acceptance--need-dot)
  (canvas-graph-dot-acceptance--with-graph 'dot canvas-graph-dot-acceptance--machine
    (let ((ctx (canvas-graph-tint-acceptance--picture)))
      (dolist (edge (canvas-graph-dot-acceptance--arrows))
        (let ((points (canvas-graph-dot-acceptance--points edge)))
          (should (canvas-graph-dot-acceptance--on-border-p
                   (car points) (canvas-graph--box (canvas-graph-edge-from edge))))
          (should (canvas-graph-dot-acceptance--on-border-p
                   (car (last points)) (canvas-graph--box (canvas-graph-edge-to edge))))
          (dolist (p (canvas-graph-dot-acceptance--head-points edge))
            (should (canvas-graph-dot-acceptance--ink-p ctx (car p) (cdr p)))))))))

(ert-deftest canvas-graph-dot-acceptance-boxes-stand-where-dot-ranks-them ()
  ;; GIVEN the coffee machine
  ;; WHEN it is shown in the dot layout
  ;; THEN the start stands at the top, and each arrow that leads on, not
  ;;      back, points down
  ;; AND each box has the size it has in the layered layout
  ;; AND no two boxes overlap
  (canvas-graph-dot-acceptance--need-dot)
  (let ((sizes (canvas-graph-dot-acceptance--sizes 'layered canvas-graph-dot-acceptance--machine)))
    (canvas-graph-dot-acceptance--with-graph 'dot canvas-graph-dot-acceptance--machine
      (let ((boxes (canvas-graph-dot-acceptance--boxes)))
        (should (= (nth 2 (assoc "idle" boxes)) (apply #'min (mapcar (lambda (b) (nth 2 b)) boxes))))
        (pcase-dolist (`(,from . ,to) canvas-graph-dot-acceptance--forward)
          (should (> (nth 2 (assoc to boxes)) (nth 2 (assoc from boxes)))))
        (pcase-dolist (`(,id ,_ ,_ ,w ,h) boxes)
          (should (equal (cons w h) (cdr (assoc id sizes)))))
        (cl-loop for (a . rest) on boxes
                 do (dolist (b rest)
                      (should-not (canvas-graph-dot-acceptance--overlap-p (cdr a) (cdr b)))))))))

(ert-deftest canvas-graph-dot-acceptance-without-graphviz-the-graph-is-layered ()
  ;; GIVEN a graphviz program that is not installed
  ;; WHEN the user picks the dot layout
  ;; THEN the boxes stand where the layered layout puts them
  ;; AND the echo area says that graphviz is missing
  (let ((layered (canvas-graph-dot-acceptance--with-graph 'layered canvas-graph-dot-acceptance--machine
                   (canvas-graph-dot-acceptance--boxes)))
        (canvas-graph-dot-program "canvas-graph-no-such-dot"))
    (canvas-graph-dot-acceptance--with-graph 'dot canvas-graph-dot-acceptance--machine
      (should (equal (canvas-graph-dot-acceptance--boxes) layered))
      (should (string-match-p "graphviz" (or (current-message) ""))))))

(ert-deftest canvas-graph-dot-acceptance-a-stuck-graphviz-does-not-freeze-emacs ()
  ;; GIVEN a graphviz program that never answers
  ;; WHEN the user picks the dot layout
  ;; THEN Emacs answers again within 5 seconds
  ;; AND the boxes stand where the layered layout puts them
  ;; AND the echo area says that graphviz did not answer
  (let ((script (make-temp-file "stuck-dot" nil ".sh" "#!/bin/sh\nsleep 60\n")))
    (unwind-protect
        (let ((layered (canvas-graph-dot-acceptance--with-graph 'layered canvas-graph-dot-acceptance--machine
                         (canvas-graph-dot-acceptance--boxes)))
              (canvas-graph-dot-program script)
              (start (float-time)))
          (set-file-modes script #o755)
          (canvas-graph-dot-acceptance--with-graph 'dot canvas-graph-dot-acceptance--machine
            (with-timeout (5 (error "Emacs waited for graphviz"))
              (while (not (string-match-p "graphviz" (or (current-message) "")))
                (with-timeout (0.2) (read-event))))
            (should (< (- (float-time) start) 5))
            (should (equal (canvas-graph-dot-acceptance--boxes) layered))))
      (delete-file script))))

;;;; Running them

(defun canvas-graph-dot-acceptance-run ()
  "Run these tests, show the result on the standard error stream and
leave Emacs."
  (let ((stats (ert-run-tests-batch "^canvas-graph-dot-acceptance-")))
    (princ (with-current-buffer "*Messages*" (buffer-string)) #'external-debugging-output)
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(provide 'canvas-graph-dot-acceptance)
;;; canvas-graph-dot-acceptance.el ends here
