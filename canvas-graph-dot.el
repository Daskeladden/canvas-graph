;;; canvas-graph-dot.el --- The dot layout of canvas-graph -*- lexical-binding: t -*-

;; Copyright (C) 2026 canvas-graph contributors

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The layout `dot' of canvas-graph: graphviz places the boxes, the
;; labels, the curves of the arrows and the frames of the groups, and
;; canvas-graph draws them in its own look.  The graph goes to the
;; program dot in the DOT language, and its places come back as JSON.
;; The program runs on its own: the graphviz library is under the
;; Eclipse Public License 1.0, which cannot be combined with the GPL.

;;; Code:

(require 'cl-lib)
(require 'canvas-diagram)
(require 'canvas-graph)

(defvar canvas-graph-gap-x)
(defvar canvas-graph-gap-y)
(defvar canvas-graph-dot-program)

(defconst canvas-graph-dot--time-limit 3
  "Seconds dot may take before it is given up and the graph is drawn as
the layered layout draws it.")

;;;; Writing the graph in the DOT language

;; A node is called n and its place in the graph, so that no name of the
;; spec needs quoting.  dot measures in points, 72 to the inch; one
;; pixel of the canvas is one point.

(defun canvas-graph-dot--inches (pixels)
  "PIXELS in inches, as dot reads a size."
  (format "%.4f" (/ pixels 72.0)))

(defun canvas-graph-dot--box-label (size)
  "An empty HTML-like label of SIZE, (W . H) in pixels, for dot to make
room for and to place."
  (format "<<TABLE FIXEDSIZE=\"TRUE\" WIDTH=\"%d\" HEIGHT=\"%d\" BORDER=\"0\" CELLPADDING=\"0\" CELLSPACING=\"0\"><TR><TD></TD></TR></TABLE>>"
          (ceiling (car size)) (ceiling (cdr size))))

(defun canvas-graph-dot--node-line (node)
  "The line of NODE, (INDEX W H)."
  (pcase-let ((`(,index ,w ,h) node))
    (format "  n%d [width=%s, height=%s];\n" index (canvas-graph-dot--inches w) (canvas-graph-dot--inches h))))

(defun canvas-graph-dot--edge-line (edge)
  "The line of EDGE, (INDEX FROM TO LABEL-SIZE), its label nil for none.
The id e and the index find the arrow again in the answer, which lists
the arrows in an order of its own."
  (pcase-let ((`(,index ,from ,to ,size) edge))
    (if size
        (format "  n%d -> n%d [id=e%d, label=%s];\n" from to index (canvas-graph-dot--box-label size))
      (format "  n%d -> n%d [id=e%d];\n" from to index))))

(defun canvas-graph-dot--cluster (group)
  "The cluster of GROUP, (INDEX MEMBERS TITLE-SIZE), its members the
indices of its nodes."
  (pcase-let ((`(,index ,members ,title) group))
    (concat (format "  subgraph cluster_%d {\n" index)
            "    labelloc=t; labeljust=l;\n"
            (format "    margin=%d;\n" (canvas-diagram-spaced 8))
            (format "    label=%s;\n" (canvas-graph-dot--box-label title))
            "    " (mapconcat (lambda (i) (format "n%d;" i)) members " ") "\n"
            "  }\n")))

(defun canvas-graph-dot--graph-line ()
  "The settings of the whole graph: top down, its gaps from the settings
of canvas-graph.  With newrank, dot ranks the boxes of the groups with
all the others, not each group apart, so the start stays at the top."
  (format "  graph [rankdir=TB, newrank=true, nodesep=%s, ranksep=%s];\n"
          (canvas-graph-dot--inches (canvas-diagram-spaced canvas-graph-gap-x))
          (canvas-graph-dot--inches (canvas-diagram-spaced canvas-graph-gap-y))))

(defun canvas-graph-dot--text (nodes edges groups)
  "The graph in the DOT language: NODES, (INDEX W H) each, in the order
dot is to meet them; EDGES, (INDEX FROM TO LABEL-SIZE) each; GROUPS,
\(INDEX MEMBERS TITLE-SIZE) each.  Sizes are in pixels."
  (concat "digraph g {\n"
          (canvas-graph-dot--graph-line)
          "  node [shape=box, fixedsize=true, label=\"\"];\n"
          (mapconcat #'canvas-graph-dot--node-line nodes "")
          (mapconcat #'canvas-graph-dot--cluster groups "")
          (mapconcat #'canvas-graph-dot--edge-line edges "")
          "}\n"))

;;;; Reading the answer

;; dot measures from the bottom left, the canvas from the top left, so
;; each height is turned over within the height of the whole drawing.

(defun canvas-graph-dot--numbers (text)
  "The numbers in TEXT, a list of them with commas between."
  (mapcar #'string-to-number (split-string text "," t)))

(defun canvas-graph-dot--point (text height)
  "The point of TEXT, \"X,Y\", from the top of a drawing HEIGHT high."
  (pcase-let ((`(,x ,y) (canvas-graph-dot--numbers text)))
    (cons (float x) (- height y))))

(defun canvas-graph-dot--index (name prefix)
  "The index in NAME after PREFIX, such as 3 of n3, or nil."
  (when (string-match (concat "\\`" (regexp-quote prefix) "\\([0-9]+\\)\\'") name)
    (string-to-number (match-string 1 name))))

(defun canvas-graph-dot--read-nodes (objects height)
  "The middle of each node of OBJECTS, (INDEX . (X . Y)) each."
  (cl-loop for object in objects
           for index = (canvas-graph-dot--index (alist-get 'name object) "n")
           when index
           collect (cons index (canvas-graph-dot--point (alist-get 'pos object) height))))

(defun canvas-graph-dot--read-clusters (objects height)
  "The frame of each cluster of OBJECTS, (INDEX X Y W H) each."
  (cl-loop for object in objects
           for index = (canvas-graph-dot--index (alist-get 'name object) "cluster_")
           when index
           collect (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (canvas-graph-dot--numbers (alist-get 'bb object))))
                     (list index (float x1) (float (- height y2)) (float (- x2 x1)) (float (- y2 y1))))))

(defun canvas-graph-dot--curve (pos height)
  "The curve of POS, the place of an arrow: (:points POINTS :tip TIP),
its points those of the cubic pieces from the first box to the base of
the arrowhead, and TIP the tip of the arrowhead."
  (let (points tip)
    (dolist (word (split-string pos " " t))
      (cond ((string-prefix-p "e," word) (setq tip (canvas-graph-dot--point (substring word 2) height)))
            ((string-prefix-p "s," word))
            (t (push (canvas-graph-dot--point word height) points))))
    (list :points (nreverse points) :tip tip)))

(defun canvas-graph-dot--read-edges (edges height)
  "Each arrow of EDGES by its index: (INDEX :points POINTS :tip TIP
:label MIDDLE), the middle of its label nil for none."
  (cl-loop for edge in edges
           for index = (canvas-graph-dot--index (or (alist-get 'id edge) "") "e")
           when index
           collect (cons index (append (canvas-graph-dot--curve (alist-get 'pos edge) height)
                                       (list :label (when-let* ((lp (alist-get 'lp edge)))
                                                      (canvas-graph-dot--point lp height)))))))

(defun canvas-graph-dot--read (json)
  "The places in JSON, the answer of dot: (:nodes NODES :clusters
CLUSTERS :edges EDGES), each by its index, measured from the top left."
  (let* ((answer (json-parse-string json :object-type 'alist :array-type 'list :null-object nil))
         (height (nth 3 (canvas-graph-dot--numbers (alist-get 'bb answer))))
         (objects (alist-get 'objects answer)))
    (list :nodes (canvas-graph-dot--read-nodes objects height)
          :clusters (canvas-graph-dot--read-clusters objects height)
          :edges (canvas-graph-dot--read-edges (alist-get 'edges answer) height))))

;;;; Putting the places on the graph

(defun canvas-graph-dot--place-boxes (graph middles)
  "Put each box of GRAPH round its middle in MIDDLES, (INDEX . (X . Y))
each, the index its place among the nodes."
  (cl-loop for node in (canvas-graph-nodes graph) for i from 0
           do (when-let* ((middle (alist-get i middles)))
                (setf (canvas-diagram-node-x node) (- (car middle) (/ (canvas-diagram-node-w node) 2.0))
                      (canvas-diagram-node-y node) (- (cdr middle) (/ (canvas-diagram-node-h node) 2.0))))))

(defun canvas-graph-dot--label-rect (middle size)
  "The patch of a label of SIZE, (W . H), round MIDDLE, (X Y W H) in
whole pixels."
  (list (floor (- (car middle) (/ (car size) 2.0))) (floor (- (cdr middle) (/ (cdr size) 2.0)))
        (car size) (cdr size)))

(defun canvas-graph-dot--place-edges (graph curves sizes)
  "Give each edge of GRAPH its curve in CURVES, (INDEX :points :tip
:label) each, and its label, of its size in SIZES, (INDEX . (W . H))
each, round the middle dot gave it."
  (cl-loop for edge in (canvas-graph-edges graph) for i from 0
           do (when-let* ((curve (alist-get i curves)))
                (setf (canvas-graph-edge-spline edge)
                      (list :points (plist-get curve :points) :tip (plist-get curve :tip)))
                (when-let* ((size (alist-get i sizes)) (middle (plist-get curve :label)))
                  (setf (canvas-graph-edge-label-rect edge) (canvas-graph-dot--label-rect middle size))))))

(defun canvas-graph-dot--place-frames (graph clusters)
  "Frame each group of GRAPH whose cluster is in CLUSTERS, (INDEX X Y W
H) each, where dot drew the cluster."
  (setf (canvas-graph-frames graph)
        (cl-loop for (i . rect) in clusters
                 for group = (nth i (canvas-graph-groups graph))
                 collect (canvas-graph-frame-create :group group :nodes (canvas-graph-group-nodes group)
                                                    :rect rect))))

(defun canvas-graph-dot--apply (graph places sizes)
  "Put PLACES, the answer of dot as `canvas-graph-dot--read' gives it, on
GRAPH: its boxes, its curves, the labels of SIZES, (INDEX . (W . H))
each, and its frames."
  (canvas-graph-dot--place-boxes graph (plist-get places :nodes))
  (canvas-graph-dot--place-edges graph (plist-get places :edges) sizes)
  (canvas-graph-dot--place-frames graph (plist-get places :clusters)))

;;;; The layout

(defvar canvas-graph-dot--told-missing nil
  "Whether the echo area said already that graphviz is missing.")

(defun canvas-graph-dot--label-sizes (graph ctx font)
  "The size of the patch of each label of GRAPH's arrows, measured on CTX
in FONT, when every label is shown: (INDEX . (W . H)) each."
  (when (canvas-graph--all-labels-p graph)
    (cl-loop for edge in (canvas-graph-edges graph) for i from 0
             for text = (and (canvas-graph--drawn-p edge) (canvas-graph--label graph edge))
             when text
             collect (let ((patch (canvas-graph--patch 0 0 (canvas-graph--label-size ctx graph text font))))
                       (cons i (cons (nth 2 patch) (nth 3 patch)))))))

(defun canvas-graph-dot--nodes (graph)
  "GRAPH's boxes for dot, (INDEX W H) each, the start first, so that dot
puts it at the top."
  (let* ((nodes (canvas-graph-nodes graph))
         (start (canvas-graph-start graph))
         (rows (cl-loop for node in nodes for i from 0
                        collect (list i (canvas-diagram-node-w node) (canvas-diagram-node-h node)))))
    (if-let* ((first (and start (cl-position start nodes))))
        (cons (nth first rows) (remq (nth first rows) rows))
      rows)))

(defconst canvas-graph-dot--label-gap 6
  "Pixels between a label and its arrow.  dot puts a label right against
the side of its arrow, so it gets the label this much wider on each
side, and the patch is drawn in the middle.")

(defun canvas-graph-dot--room-for (size)
  "The room for a label patch of SIZE, (W . H), that dot gets: wider by
the gap on each side."
  (and size (cons (+ (car size) (* 2 (canvas-diagram-spaced canvas-graph-dot--label-gap))) (cdr size))))

(defun canvas-graph-dot--edges (graph sizes)
  "GRAPH's arrows for dot, (INDEX FROM TO LABEL-SIZE) each, the room for
the label patches of SIZES."
  (let ((nodes (canvas-graph-nodes graph)))
    (cl-loop for edge in (canvas-graph-edges graph) for i from 0
             when (canvas-graph--drawn-p edge)
             collect (list i (cl-position (canvas-graph-edge-from edge) nodes)
                           (cl-position (canvas-graph-edge-to edge) nodes)
                           (canvas-graph-dot--room-for (alist-get i sizes))))))

(defun canvas-graph-dot--groups (graph ctx font)
  "GRAPH's groups for dot, (INDEX MEMBERS TITLE-SIZE) each, the titles
measured on CTX in FONT."
  (let ((nodes (canvas-graph-nodes graph))
        (pad canvas-graph--frame-pad))
    (cl-loop for group in (canvas-graph-groups graph) for i from 0
             collect (pcase-let ((`(,w . ,h) (canvas-cairo-text-size ctx (canvas-graph-group-title group) font)))
                       (list i (mapcar (lambda (n) (cl-position n nodes)) (canvas-graph-group-nodes group))
                             (cons (+ w (* 2 pad)) h))))))

(defun canvas-graph-dot--usable-p (answer)
  "Whether ANSWER of dot can be drawn; else say in the echo area why the
graph is drawn as the layered layout draws it."
  (pcase answer
    (`(:ok ,_) t)
    (`(:missing)
     (unless canvas-graph-dot--told-missing
       (setq canvas-graph-dot--told-missing t)
       (message "canvas-graph: graphviz is missing, as there is no program %s, so the dot layout draws as layered"
                canvas-graph-dot-program))
     nil)
    (`(:error ,text)
     (message "canvas-graph: graphviz failed (%s), so the dot layout draws as layered" text)
     nil)
    (`(:timeout)
     (message "canvas-graph: graphviz did not answer within %d seconds, so the dot layout draws as layered"
              canvas-graph-dot--time-limit)
     nil)))

(defun canvas-graph-dot--title-frames (graph ctx font)
  "Give each frame of GRAPH the title of its group, measured on CTX in
FONT."
  (setf (canvas-graph-title-h graph) (cdr (canvas-cairo-text-size ctx "Ag" font)))
  (dolist (frame (canvas-graph-frames graph))
    (canvas-graph--title-frame frame nil ctx font)))

(defun canvas-graph-dot--reading-order (nodes)
  "NODES top down, and left to right within a height."
  (sort (copy-sequence nodes)
        (lambda (a b) (or (< (canvas-diagram-node-y a) (canvas-diagram-node-y b))
                          (and (= (canvas-diagram-node-y a) (canvas-diagram-node-y b))
                               (< (canvas-diagram-node-x a) (canvas-diagram-node-x b)))))))

(defun canvas-graph-dot--finish (diagram graph ctx font)
  "Title the frames of GRAPH, route its folded edges and give DIAGRAM
room for what reaches past the boxes, on CTX in FONT."
  (canvas-graph-dot--title-frames graph ctx font)
  (canvas-graph--route-folded graph)
  (pcase-let ((`(,left ,top ,right ,bottom) (canvas-graph--overshoot graph ctx)))
    (setf (canvas-diagram-slack diagram) (cons right bottom))
    (canvas-graph--normalize graph left top)))

(defun canvas-graph-dot-lay-out (diagram ctx)
  "Lay DIAGRAM's graph out on CTX with graphviz: the nodes in reading
order, or nil when graphviz cannot, as the echo area then says."
  (let* ((graph (canvas-diagram-model diagram))
         (nodes (canvas-graph-nodes graph))
         (measure (canvas-diagram-measure ctx))
         (font (canvas-diagram-font)))
    (canvas-graph--mark-common graph)
    (canvas-graph--forget-placing graph)
    (dolist (node nodes)
      (canvas-diagram-size-node diagram node measure))
    (let* ((sizes (canvas-graph-dot--label-sizes graph ctx font))
           (answer (canvas-graph-dot--run (canvas-graph-dot--text (canvas-graph-dot--nodes graph)
                                                                  (canvas-graph-dot--edges graph sizes)
                                                                  (canvas-graph-dot--groups graph ctx font)))))
      (when (canvas-graph-dot--usable-p answer)
        (canvas-graph-dot--apply graph (canvas-graph-dot--read (cadr answer)) sizes)
        (canvas-graph-dot--finish diagram graph ctx font)
        (canvas-graph-dot--reading-order nodes)))))

;;;; Running dot

(defvar canvas-graph-dot--answers (make-hash-table :test 'equal)
  "The answers of dot kept for the graphs it laid out, by the program
and the text of the graph.")

(defconst canvas-graph-dot--answers-kept 32
  "How many answers of dot are kept before all are let go.")

(defun canvas-graph-dot--run (text)
  "Lay TEXT, a graph in the DOT language, out with dot: (:ok JSON), or
\(:missing) when the program is not installed, (:error MESSAGE) when it
failed, or (:timeout) when it took longer than the limit.  An answer is
kept, so a graph that did not change is not laid out again."
  (let ((key (list canvas-graph-dot-program text)))
    (or (gethash key canvas-graph-dot--answers)
        (progn
          (when (>= (hash-table-count canvas-graph-dot--answers) canvas-graph-dot--answers-kept)
            (clrhash canvas-graph-dot--answers))
          (puthash key (canvas-graph-dot--answer text) canvas-graph-dot--answers)))))

(defun canvas-graph-dot--answer (text)
  "The answer of dot for TEXT, as `canvas-graph-dot--run' gives it."
  (if-let* ((program (executable-find canvas-graph-dot-program)))
      (canvas-graph-dot--run-program program text)
    (list :missing)))

(defun canvas-graph-dot--run-program (program text)
  "Run PROGRAM, dot, on TEXT and wait for it up to the time limit."
  (let* ((out (generate-new-buffer " *canvas-graph-dot*"))
         (err (generate-new-buffer " *canvas-graph-dot-errors*"))
         ;; A pipe of its own for the errors, whose quiet sentinel writes
         ;; no "finished" line into them.
         (errors (make-pipe-process :name "canvas-graph-dot-errors" :buffer err
                                    :noquery t :sentinel #'ignore))
         (process (make-process :name "canvas-graph-dot" :buffer out :stderr errors
                                :command (list program "-Tjson") :connection-type 'pipe
                                :noquery t :sentinel #'ignore)))
    (unwind-protect
        (progn
          (process-send-string process text)
          (process-send-eof process)
          (canvas-graph-dot--wait process err)
          (canvas-graph-dot--outcome process out err))
      (when (process-live-p process)
        (delete-process process))
      (when-let* ((errors (get-buffer-process err)))
        (delete-process errors))
      (kill-buffer out)
      (kill-buffer err))))

(defun canvas-graph-dot--wait (process err)
  "Wait for PROCESS to end, up to the time limit, and take in what it
wrote to ERR, the buffer of its errors."
  (let ((deadline (+ (float-time) canvas-graph-dot--time-limit)))
    (while (and (process-live-p process) (< (float-time) deadline))
      (accept-process-output process 0.05))
    (when-let* ((errors (get-buffer-process err)))
      (while (accept-process-output errors 0.05)))))

(defun canvas-graph-dot--outcome (process out err)
  "The answer of PROCESS, dot, from the buffers OUT and ERR."
  (cond ((process-live-p process) (list :timeout))
        ((zerop (process-exit-status process))
         (list :ok (with-current-buffer out (buffer-string))))
        (t (list :error (string-trim (with-current-buffer err (buffer-string)))))))

(provide 'canvas-graph-dot)
;;; canvas-graph-dot.el ends here
