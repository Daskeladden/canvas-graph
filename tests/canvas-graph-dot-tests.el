;;; canvas-graph-dot-tests.el --- tests of the dot layout -*- lexical-binding: t -*-
(require 'ert)
(require 'cl-lib)
(require 'canvas-graph)
(require 'canvas-graph-dot)

;; The layout `dot' writes a graph in the DOT language, runs the program
;; dot of graphviz on it and reads the places back.  The tests that run
;; dot skip themselves when it is not installed.

;;;; Writing the graph

(ert-deftest canvas-graph-dot-writes-boxes-of-their-measured-size ()
  ;; GIVEN two boxes, 72 by 36 and 144 by 18 pixels, joined by an arrow
  ;; WHEN the graph is written for dot
  ;; THEN each box is a node of that size in inches, fixed in size and
  ;;      with no text of its own, AND the arrow joins them under its id
  (let ((text (canvas-graph-dot--text '((0 72 36) (1 144 18)) '((0 0 1 nil)) nil)))
    (should (string-match-p "^  node \\[shape=box, fixedsize=true, label=\"\"\\];$" text))
    (should (string-match-p "^  n0 \\[width=1\\.0000, height=0\\.5000\\];$" text))
    (should (string-match-p "^  n1 \\[width=2\\.0000, height=0\\.2500\\];$" text))
    (should (string-match-p "^  n0 -> n1 \\[id=e0\\];$" text))))

(ert-deftest canvas-graph-dot-writes-the-boxes-in-the-order-given ()
  ;; GIVEN three boxes given in the order 2, 0, 1
  ;; WHEN the graph is written for dot
  ;; THEN the nodes come in that order, so that dot starts from the first
  (let ((text (canvas-graph-dot--text '((2 10 10) (0 10 10) (1 10 10)) nil nil)))
    (should (< (string-match "^  n2 " text) (string-match "^  n0 " text) (string-match "^  n1 " text)))))

(ert-deftest canvas-graph-dot-gives-a-label-its-measured-size ()
  ;; GIVEN an arrow whose label takes 50 by 22 pixels
  ;; WHEN the graph is written for dot
  ;; THEN the arrow has a label of exactly that size, with no text
  (let ((text (canvas-graph-dot--text '((0 10 10) (1 10 10)) '((0 0 1 (50 . 22))) nil)))
    (should (string-match-p
             (regexp-quote "n0 -> n1 [id=e0, label=<<TABLE FIXEDSIZE=\"TRUE\" WIDTH=\"50\" HEIGHT=\"22\" BORDER=\"0\" CELLPADDING=\"0\" CELLSPACING=\"0\"><TR><TD></TD></TR></TABLE>>];")
             text))))

(ert-deftest canvas-graph-dot-writes-a-group-as-a-cluster ()
  ;; GIVEN a group of the first and the third box, whose title takes 40
  ;;       by 16 pixels
  ;; WHEN the graph is written for dot
  ;; THEN a cluster holds those two nodes, with a label of the size of the
  ;;      title at its top left
  (let ((text (canvas-graph-dot--text '((0 10 10) (1 10 10) (2 10 10)) nil '((0 (0 2) (40 . 16))))))
    (should (string-match-p "^  subgraph cluster_0 {$" text))
    (should (string-match-p "^    labelloc=t; labeljust=l;$" text))
    (should (string-match-p (regexp-quote "WIDTH=\"40\" HEIGHT=\"16\"") text))
    (should (string-match-p "^    n0; n2;$" text))))

(ert-deftest canvas-graph-dot-ranks-the-whole-graph-at-once ()
  ;; GIVEN a graph written for dot
  ;; THEN dot ranks all its boxes together, also those in groups, so the
  ;;      start stays at the top and the flow reads top down
  (should (string-match-p "newrank=true" (canvas-graph-dot--text '((0 10 10)) nil nil))))

;;;; Running dot

(defun canvas-graph-dot-test--need-dot ()
  "Skip the test when graphviz is not installed."
  ;; `skip-unless' is a macro of the body of a test only.
  (unless (executable-find "dot")
    (ert-skip "graphviz is not installed")))

(defmacro canvas-graph-dot-test--with-script (var body &rest forms)
  "Run FORMS with VAR the file name of a shell script whose text after
its first line is BODY, and with the dot program set to it and no
answer kept from before."
  (declare (indent 2))
  `(let ((,var (make-temp-file "canvas-graph-dot" nil ".sh" (concat "#!/bin/sh\n" ,body))))
     (unwind-protect
         (progn
           (set-file-modes ,var #o755)
           (let ((canvas-graph-dot-program ,var))
             (clrhash canvas-graph-dot--answers)
             ,@forms))
       (delete-file ,var))))

(ert-deftest canvas-graph-dot-answers-with-the-places-in-json ()
  ;; GIVEN graphviz and a graph of two boxes
  ;; WHEN dot is run on it
  ;; THEN the answer is JSON that names both nodes
  (canvas-graph-dot-test--need-dot)
  (let ((canvas-graph-dot-program "dot"))
    (clrhash canvas-graph-dot--answers)
    (pcase-let ((`(,kind ,json) (canvas-graph-dot--run (canvas-graph-dot--text '((0 72 36) (1 72 36)) '((0 0 1 nil)) nil))))
      (should (eq kind :ok))
      (should (string-match-p "\"name\": \"n0\"" json))
      (should (string-match-p "\"name\": \"n1\"" json)))))

(ert-deftest canvas-graph-dot-without-the-program-says-it-is-missing ()
  ;; GIVEN a dot program that is not installed
  ;; WHEN dot is run
  ;; THEN the answer is that the program is missing
  (let ((canvas-graph-dot-program "canvas-graph-no-such-dot"))
    (clrhash canvas-graph-dot--answers)
    (should (equal (canvas-graph-dot--run "digraph g {}") '(:missing)))))

(ert-deftest canvas-graph-dot-a-failing-program-gives-its-error ()
  ;; GIVEN a dot program that writes an error and fails
  ;; WHEN dot is run
  ;; THEN the answer is the error it wrote
  (canvas-graph-dot-test--with-script program "cat >/dev/null; echo 'syntax error in line 1' >&2; exit 1\n"
    (should (equal (canvas-graph-dot--run "digraph g {}") '(:error "syntax error in line 1")))))

(ert-deftest canvas-graph-dot-a-stuck-program-is-given-up ()
  ;; GIVEN a dot program that never answers
  ;; WHEN dot is run
  ;; THEN the answer is that it timed out, within a second of the limit
  (canvas-graph-dot-test--with-script program "sleep 60\n"
    (let ((start (float-time)))
      (should (equal (canvas-graph-dot--run "digraph g {}") '(:timeout)))
      (should (< (- (float-time) start) (1+ canvas-graph-dot--time-limit))))))

(ert-deftest canvas-graph-dot-runs-once-for-a-graph-that-did-not-change ()
  ;; GIVEN a dot program that counts its runs
  ;; WHEN dot is run twice on one graph, and then on another
  ;; THEN the program ran once for each graph, and both runs of the first
  ;;      graph got the same answer
  (let ((count (make-temp-file "canvas-graph-dot-count")))
    (unwind-protect
        (canvas-graph-dot-test--with-script program (format "echo run >> %s; cat >/dev/null; echo '{}'\n" count)
          (let ((first (canvas-graph-dot--run "digraph a {}")))
            (should (equal (canvas-graph-dot--run "digraph a {}") first))
            (canvas-graph-dot--run "digraph b {}")
            (should (= (length (split-string (with-temp-buffer (insert-file-contents count) (buffer-string)) "\n" t))
                       2))))
      (delete-file count))))

(ert-deftest canvas-graph-dot-keeps-a-gap-between-a-label-and-its-arrow ()
  ;; GIVEN an arrow whose label patch takes 50 by 22 pixels
  ;; WHEN the arrow is given to dot
  ;; THEN dot gets a label wider by a gap on each side, so the patch,
  ;;      drawn in its middle, keeps clear of the arrow beside it
  (let* ((graph (canvas-graph-build '(:name "g" :start "a" :nodes (("a") ("b")) :edges (("a" "b" "go")))))
         (edge (car (canvas-graph-dot--edges graph '((0 50 . 22))))))
    (should (equal (nth 3 edge) (cons (+ 50 (* 2 (canvas-diagram-spaced canvas-graph-dot--label-gap))) 22)))))

;;;; Reading the answer

(defconst canvas-graph-dot-test--answer
  "{\"bb\": \"0,0,136,267\",
 \"objects\": [
  {\"_gvid\": 0, \"name\": \"cluster_0\", \"bb\": \"8,77,114,259\", \"lp\": \"36,247\"},
  {\"_gvid\": 1, \"name\": \"n0\", \"pos\": \"61,209\", \"width\": \"1\", \"height\": \"0.5\"},
  {\"_gvid\": 2, \"name\": \"n1\", \"pos\": \"52,103\", \"width\": \"1\", \"height\": \"0.5\"},
  {\"_gvid\": 3, \"name\": \"n2\", \"pos\": \"100,18\", \"width\": \"1\", \"height\": \"0.5\"}],
 \"edges\": [
  {\"_gvid\": 0, \"tail\": 1, \"head\": 2, \"id\": \"e0\", \"lp\": \"83,156\",
   \"pos\": \"e,53.488,121.2 59.514,190.83 58.113,174.64 55.992,150.13 54.36,131.27\"},
  {\"_gvid\": 1, \"tail\": 2, \"head\": 2, \"id\": \"e3\",
   \"pos\": \"e,88.186,95.194 88.186,110.81 103.24,111.34 116,108.74 116,103 116,98.604 108.52,96.048 98.258,95.333\"},
  {\"_gvid\": 2, \"tail\": 2, \"head\": 3, \"id\": \"e1\",
   \"pos\": \"e,90.067,36.176 61.946,84.802 68.575,73.34 77.412,58.058 84.902,45.106\"}]}"
  "Part of the answer of dot 2.43.0 for three boxes, the first two in a
group, with a labelled arrow, a plain one and a self loop.")

(defun canvas-graph-dot-test--near-p (a b)
  "Whether the points or numbers A and B are within a thousandth."
  (if (consp a)
      (and (< (abs (- (car a) (car b))) 0.001) (< (abs (- (cdr a) (cdr b))) 0.001))
    (< (abs (- a b)) 0.001)))

(ert-deftest canvas-graph-dot-reads-the-middle-of-each-box-top-down ()
  ;; GIVEN the answer of dot for three boxes in a drawing 267 points high
  ;; WHEN it is read
  ;; THEN each box has its middle, measured from the top, by its index
  (let ((places (plist-get (canvas-graph-dot--read canvas-graph-dot-test--answer) :nodes)))
    (should (canvas-graph-dot-test--near-p (alist-get 0 places) '(61.0 . 58.0)))
    (should (canvas-graph-dot-test--near-p (alist-get 1 places) '(52.0 . 164.0)))
    (should (canvas-graph-dot-test--near-p (alist-get 2 places) '(100.0 . 249.0)))))

(ert-deftest canvas-graph-dot-reads-the-frame-of-each-cluster ()
  ;; GIVEN the answer of dot with one cluster
  ;; WHEN it is read
  ;; THEN the cluster has its frame, (X Y W H) from the top left
  (let* ((frames (plist-get (canvas-graph-dot--read canvas-graph-dot-test--answer) :clusters))
         (frame (alist-get 0 frames)))
    (should (equal (length frames) 1))
    (should (cl-every #'canvas-graph-dot-test--near-p frame '(8.0 8.0 106.0 182.0)))))

(ert-deftest canvas-graph-dot-reads-the-curve-the-tip-and-the-label-of-an-arrow ()
  ;; GIVEN the answer of dot, its arrows listed in another order than
  ;;       they were written, the first arrow labelled
  ;; WHEN it is read
  ;; THEN each arrow is found by its id, with the points of its curve, the
  ;;      tip of its arrowhead and the middle of its label, from the top
  (let* ((edges (plist-get (canvas-graph-dot--read canvas-graph-dot-test--answer) :edges))
         (first (alist-get 0 edges))
         (plain (alist-get 1 edges)))
    (should (equal (length edges) 3))
    (should (cl-every #'canvas-graph-dot-test--near-p (plist-get first :points)
                      '((59.514 . 76.17) (58.113 . 92.36) (55.992 . 116.87) (54.36 . 135.73))))
    (should (canvas-graph-dot-test--near-p (plist-get first :tip) '(53.488 . 145.8)))
    (should (canvas-graph-dot-test--near-p (plist-get first :label) '(83.0 . 111.0)))
    (should-not (plist-get plain :label))
    (should (= (length (plist-get (alist-get 3 edges) :points)) 7))))

;;;; Putting the places on the graph

(defun canvas-graph-dot-test--graph ()
  "Three boxes of 72 by 36 pixels, the first two in a group, joined as in
`canvas-graph-dot-test--answer', laid out by that answer."
  (let ((graph (canvas-graph-build '(:name "g" :start "a"
                                     :nodes (("a") ("b") ("c"))
                                     :edges (("a" "b" "go") ("b" "c") ("c" "a") ("b" "b"))
                                     :groups (("g" :nodes ("a" "b")))))))
    (dolist (node (canvas-graph-nodes graph))
      (setf (canvas-diagram-node-w node) 72 (canvas-diagram-node-h node) 36))
    (canvas-graph-dot--apply graph (canvas-graph-dot--read canvas-graph-dot-test--answer) '((0 50 . 22)))
    graph))

(defun canvas-graph-dot-test--edge (graph i)
  "The I-th edge of GRAPH."
  (nth i (canvas-graph-edges graph)))

(ert-deftest canvas-graph-dot-puts-each-box-round-its-middle ()
  ;; GIVEN three boxes of 72 by 36 pixels and the answer of dot for them
  ;; WHEN the answer is put on the graph
  ;; THEN each box stands round the middle dot gave it, at its own size
  (let ((graph (canvas-graph-dot-test--graph)))
    (should (equal (mapcar (lambda (n) (list (canvas-diagram-node-x n) (canvas-diagram-node-y n)
                                             (canvas-diagram-node-w n) (canvas-diagram-node-h n)))
                           (canvas-graph-nodes graph))
                   '((25.0 40.0 72 36) (16.0 146.0 72 36) (64.0 231.0 72 36))))))

(ert-deftest canvas-graph-dot-puts-a-label-round-its-middle ()
  ;; GIVEN the answer of dot with a label of 50 by 22 pixels on the first
  ;;       arrow, and no label on the second
  ;; WHEN the answer is put on the graph
  ;; THEN the first label stands round the middle dot gave it, and the
  ;;      second arrow has no label
  (let ((graph (canvas-graph-dot-test--graph)))
    (should (equal (canvas-graph-edge-label-rect (canvas-graph-dot-test--edge graph 0)) '(58 100 50 22)))
    (should-not (canvas-graph-edge-label-rect (canvas-graph-dot-test--edge graph 1)))))

(ert-deftest canvas-graph-dot-frames-a-group-as-its-cluster ()
  ;; GIVEN the answer of dot with the group of the first two boxes as a
  ;;       cluster
  ;; WHEN the answer is put on the graph
  ;; THEN the group has one frame round its two boxes, where dot drew the
  ;;      cluster
  (let* ((graph (canvas-graph-dot-test--graph))
         (frames (canvas-graph-frames graph)))
    (should (= (length frames) 1))
    (should (equal (mapcar #'canvas-graph-node-id (canvas-graph-frame-nodes (car frames))) '("a" "b")))
    (should (equal (canvas-graph--frame-rect (car frames) graph) '(8.0 8.0 106.0 182.0)))))

(ert-deftest canvas-graph-dot-draws-an-arrow-along-its-curve-to-its-tip ()
  ;; GIVEN the answer of dot for the first arrow
  ;; WHEN the answer is put on the graph
  ;; THEN the points of the arrow run from the start of its curve to the
  ;;      tip of its arrowhead
  (let* ((graph (canvas-graph-dot-test--graph))
         (points (canvas-graph--curve-points (canvas-graph-dot-test--edge graph 0))))
    (should (canvas-graph-dot-test--near-p (car points) '(59.514 . 76.17)))
    (should (canvas-graph-dot-test--near-p (car (last points)) '(53.488 . 145.8)))))

(ert-deftest canvas-graph-dot-moves-curves-and-frames-with-the-boxes ()
  ;; GIVEN a graph laid out by the answer of dot
  ;; WHEN the whole drawing moves 10 pixels right and 5 down
  ;; THEN the curves of the arrows and the frames move with it
  (let ((graph (canvas-graph-dot-test--graph)))
    (canvas-graph--shift graph 10 5)
    (should (canvas-graph-dot-test--near-p (car (canvas-graph--curve-points (canvas-graph-dot-test--edge graph 0)))
                                           '(69.514 . 81.17)))
    (should (equal (canvas-graph--frame-rect (car (canvas-graph-frames graph)) graph) '(18.0 13.0 106.0 182.0)))))

;;;; The layout

(defconst canvas-graph-dot-test--pair
  '(:name "pair" :start "a"
    :nodes (("a") ("b") ("c"))
    :edges (("a" "b" "go") ("b" "a" "back") ("b" "c"))
    :groups (("g" :nodes ("a" "b"))))
  "Two boxes in a group with an arrow each way, and a third below.")

(defun canvas-graph-dot-test--laid-out (spec layout)
  "The graph of SPEC laid out in LAYOUT, every label shown."
  (let* ((canvas-graph-layout layout)
         (canvas-graph-show-labels 'all)
         (canvas-diagram-font "Sans 12px")
         (ctx (canvas-cairo-context (list 'image :type 'canvas :id (make-symbol "dot-test")
                                          :data-width 4 :data-height 4)))
         (diagram (canvas-graph-diagram)))
    (unwind-protect
        (progn
          (setf (canvas-diagram-spec diagram) spec
                (canvas-diagram-model diagram) (canvas-diagram--call diagram :build diagram spec)
                (canvas-diagram-nodes diagram) (canvas-graph--lay-out diagram ctx))
          (canvas-diagram-model diagram))
      (canvas-cairo-destroy ctx))))

(defun canvas-graph-dot-test--places (graph)
  "Where the boxes of GRAPH stand: (ID X Y) each."
  (mapcar (lambda (n) (list (canvas-graph-node-id n) (canvas-diagram-node-x n) (canvas-diagram-node-y n)))
          (canvas-graph-nodes graph)))

(defun canvas-graph-dot-test--messages-about (text since)
  "How many lines of *Messages* after the position SINCE hold TEXT."
  (with-current-buffer "*Messages*"
    (cl-count-if (lambda (line) (string-match-p (regexp-quote text) line))
                 (split-string (buffer-substring since (point-max)) "\n"))))

(ert-deftest canvas-graph-dot-lays-out-a-graph-with-graphviz ()
  ;; GIVEN graphviz and two boxes in a group with an arrow each way, and a
  ;;       third box below
  ;; WHEN the graph is laid out in the dot layout
  ;; THEN each arrow has the curve of dot, the labelled one its label, the
  ;;      group one frame placed by dot, and the first box stands above
  ;;      the third
  (canvas-graph-dot-test--need-dot)
  (let ((canvas-graph-dot-program "dot"))
    (clrhash canvas-graph-dot--answers)
    (let* ((graph (canvas-graph-dot-test--laid-out canvas-graph-dot-test--pair 'dot))
           (edges (canvas-graph-edges graph)))
      (should (cl-every #'canvas-graph-edge-spline edges))
      (should (canvas-graph-edge-label-rect (car edges)))
      (should (= (length (canvas-graph-frames graph)) 1))
      (should (canvas-graph-frame-rect (car (canvas-graph-frames graph))))
      (should (< (canvas-diagram-node-y (canvas-graph-node-named graph "a"))
                 (canvas-diagram-node-y (canvas-graph-node-named graph "c")))))))

(ert-deftest canvas-graph-dot-without-graphviz-lays-out-as-layered ()
  ;; GIVEN a dot program that is not installed
  ;; WHEN the graph is laid out in the dot layout, twice
  ;; THEN the boxes stand where the layered layout puts them
  ;; AND the echo area said once that graphviz is missing
  (let ((layered (canvas-graph-dot-test--places (canvas-graph-dot-test--laid-out canvas-graph-dot-test--pair 'layered)))
        (canvas-graph-dot-program "canvas-graph-no-such-dot")
        (canvas-graph-dot--told-missing nil)
        (since (with-current-buffer "*Messages*" (point-max))))
    (clrhash canvas-graph-dot--answers)
    (should (equal (canvas-graph-dot-test--places (canvas-graph-dot-test--laid-out canvas-graph-dot-test--pair 'dot))
                   layered))
    (canvas-graph-dot-test--laid-out canvas-graph-dot-test--pair 'dot)
    (should (= (canvas-graph-dot-test--messages-about "graphviz is missing" since) 1))))

(ert-deftest canvas-graph-dot-a-failed-dot-draws-its-arrows-as-layered-does ()
  ;; GIVEN a dot program that is not installed, and an arrow each way
  ;;       between two boxes
  ;; WHEN the graph is laid out in the dot layout
  ;; THEN each arrow is drawn on the curve the layered layout gives it
  (let ((layered (mapcar #'canvas-graph--drawn-curve-points
                         (canvas-graph-edges (canvas-graph-dot-test--laid-out canvas-graph-dot-test--pair 'layered))))
        (canvas-graph-dot-program "canvas-graph-no-such-dot"))
    (clrhash canvas-graph-dot--answers)
    (cl-loop for edge in (canvas-graph-edges (canvas-graph-dot-test--laid-out canvas-graph-dot-test--pair 'dot))
             for want in layered
             do (let ((points (canvas-graph--curve-points edge)))
                  (should (canvas-graph-dot-test--near-p (car points) (car want)))
                  (should (canvas-graph-dot-test--near-p (nth 5 points) (nth 5 want)))
                  (should (canvas-graph-dot-test--near-p (car (last points)) (car (last want))))))))

(provide 'canvas-graph-dot-tests)
;;; canvas-graph-dot-tests.el ends here
