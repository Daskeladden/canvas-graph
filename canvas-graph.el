;;; canvas-graph.el --- Graphs of boxes and arrows on an Emacs canvas -*- lexical-binding: t -*-

;; Copyright (C) 2026 canvas-graph contributors

;; Author: Daskeladden
;; Version: 0.1.0
;; Package-Requires: ((emacs "32.0.50") (canvas-diagram "0.1.0"))
;; Keywords: multimedia, tools, convenience
;; URL: https://github.com/Daskeladden/canvas-graph

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

;; Draws a directed graph on an Emacs 32 canvas, on top of
;; canvas-diagram, which holds everything a diagram of boxes shares:
;; the canvas, the boxes, the keys, the mouse, the minimap, the source.
;; This package is the graph: nodes and the labelled edges between them,
;; built from a spec and measured from a start node.  The nodes sit on a
;; ring or in layers by their distance from the start, as graphviz lays
;; a graph out; arrows join them, bowed round the boxes, labelled on the
;; arrow or in a panel beside the drawing; the edges most nodes share
;; are folded into one marker, and a node's way to it is routed round
;; the boxes.
;;
;; A reader turns a source into a spec: canvas-vhdl-fsm reads the state
;; machines of VHDL into one.  A style says how the reader's domain
;; speaks: what the start node is called, how a label is marked up and
;; how it is shortened.  A package on the graph puts callbacks of its
;; own in front of the graph's, and a menu group of its own before the
;; graph's.
;;
;; This is a prototype.

;;; Code:

(require 'cl-lib)
(require 'canvas-diagram)
(require 'canvas-diagram-reader)

(defgroup canvas-graph nil
  "Graphs of boxes and arrows on a canvas."
  :group 'canvas-diagram)

(defcustom canvas-graph-layout 'layered
  "How the nodes are placed: in `layered' rows by their distance from
the start node, top down, as graphviz lays a graph out, or on a
`ring' in declaration order."
  :type '(choice (const layered) (const ring)))

(defcustom canvas-graph-show-labels 'auto
  "Which arrows are labelled: `auto', as the graph's reader would have
it; those in and out of the `selected' node, tagged as the keyboard
moves and listed in a panel beside the drawing, for labels that run
long; `all' of them, laid out with the boxes; or none.  An export shows
them all."
  :type '(choice (const auto) (const selected) (const all) (const nil)))

(defcustom canvas-graph-gap-x 28
  "Pixels between nodes side by side in a row; twice that around the ring."
  :type 'integer)

(defcustom canvas-graph-gap-y 48
  "Pixels between rows: room for an arrow and the label beside it."
  :type 'integer)

(defcustom canvas-graph-bow 0.18
  "How far an arrow bows out when another comes back the other way,
as a fraction of its length."
  :type 'number)

(defcustom canvas-graph-label-width 260
  "Pixels a label may be wide before it wraps onto more lines; a name
longer than that is not broken."
  :type 'integer)

(defcustom canvas-graph-fold-common t
  "Whether edges most nodes share, to one target under one label, an
abort say, are drawn once as an any-node marker at the target rather
than as an arrow from each."
  :type 'boolean)

(defcustom canvas-graph-label-max 90
  "Characters a label may run to on an arrow before it is cut with an
ellipsis.  The card has the whole of it."
  :type 'integer)

(defcustom canvas-graph-panel-width 'auto
  "Pixels of text the panel of the selected node's labels holds on a
line, or `auto': the room beside the drawing, between 200 pixels and
three fifths of the window.  `canvas-graph-panel-wider' and `-narrower'
set a width by hand."
  :type '(choice (const auto) integer))

(defcustom canvas-graph-label-lines 3
  "How many of an edge's labels its arrow lists, one per line, before
saying how many more there are."
  :type 'integer)

(defcustom canvas-graph-common-min 3
  "How many nodes must share an edge for it to be folded; two thirds of
the other nodes must, too."
  :type 'integer)

(defconst canvas-graph--marker-gap 26
  "Pixels from the any-node marker's dot to the box it points at.")

(defconst canvas-graph--loop-height 26
  "Pixels a self loop rises above its box.")

(defconst canvas-graph--arrow-length 9
  "Pixels from an arrowhead's tip to its base.")

(defconst canvas-graph--arrow-width 4.5
  "Pixels from an arrowhead's spine to either barb.")

(dolist (kind '(("start" . "dark orange") ("unreachable" . "red")))
  (unless (assoc (car kind) canvas-diagram-extra-kinds)
    (push kind canvas-diagram-extra-kinds)))

(dolist (icon '(("start" . "material:play") ("unreachable" . "material:link-off")))
  (unless (assoc (car icon) canvas-diagram-extra-kind-icons)
    (push icon canvas-diagram-extra-kind-icons)))

;;;; The style

(defconst canvas-graph--default-style
  (list :start "start" :any "any node"
        :markup #'canvas-diagram-markup-escape :shorten #'identity
        :labels 'all)
  "How a graph speaks when its reader says nothing: the kind of its start
node, what an edge from any node comes from, how a label is marked up
for pango, how it is shortened on an arrow, and which arrows are
labelled when `canvas-graph-show-labels' leaves it to the reader.")

(defun canvas-graph--check-style (style)
  "STYLE, a plist of the keys `canvas-graph--default-style' has; a key it
has not, or labels shown in no way the setting knows, is an error."
  (cl-loop for (key _) on style by #'cddr
           unless (plist-member canvas-graph--default-style key)
           do (error "canvas-graph: no style %S" key))
  (unless (memq (plist-get style :labels) '(selected all nil))
    (error "canvas-graph: labels shown as %S" (plist-get style :labels)))
  style)

(defun canvas-graph--styled (graph key)
  "GRAPH's word, or function, for KEY: its reader's, else the default."
  (plist-get (canvas-graph-style graph) key))

(defun canvas-graph--markup (graph text)
  "TEXT, a label of GRAPH, as pango markup in GRAPH's style."
  (funcall (canvas-graph--styled graph :markup) text))

(defun canvas-graph--shorten (graph text)
  "TEXT, a label of GRAPH, as short as GRAPH's style has it on an arrow."
  (funcall (canvas-graph--styled graph :shorten) text))

;;;; The graph

(cl-defstruct (canvas-graph-node (:include canvas-diagram-node)
                                 (:constructor canvas-graph-node-create))
  "A node: its ID, the name the spec knows it by, its label shown unless
it has one of its own; its DEPTH, steps from the start node, nil when
unreachable; the PARENT that first reaches it; its edges OUT and IN."
  id depth parent out in)

(cl-defstruct (canvas-graph-edge (:constructor canvas-graph-edge-create))
  "An edge FROM a node TO a node, under any of its LABELS, first seen at
POS in the source.  BOW is how far its arrow bows out, as a fraction
of its length, positive to its right; COMMON whether it is one of many
alike that are drawn once; LABEL-RECT where its label sits, (X Y W H),
or nil for none shown; CONTROL a point, (X . Y), the arrow bends
through, its label's middle, or nil; ROUTE the corners of its way to
the any-node marker it is folded into, (X . Y) each, or nil when it is
drawn.  The layout sets them.  FROM-ROW and TO-ROW name the rows of the
two boxes the arrow joins, when it joins rows rather than whole boxes."
  from to labels pos (bow 0) common label-rect control route from-row to-row)

(cl-defstruct (canvas-graph (:constructor canvas-graph-create))
  "A graph: its NAME and TYPE, its NODES in declaration order, the START
node among them, its EDGES in the spec's order, and the STYLE its
reader speaks in."
  name type nodes start edges style)

(defun canvas-graph-node-named (graph name)
  "The node of GRAPH its spec calls NAME, or nil."
  (cl-find name (canvas-graph-nodes graph) :key #'canvas-graph-node-id :test #'equal))

(defun canvas-graph--loop-p (edge)
  "Whether EDGE leads from a node back to itself."
  (eq (canvas-graph-edge-from edge) (canvas-graph-edge-to edge)))

(defun canvas-graph--from-any-p (edge)
  "Whether EDGE comes from any node."
  (null (canvas-graph-edge-from edge)))

;;;; Building the graph

(defconst canvas-graph--spec-keys '(:name :type :start :nodes :edges :data)
  "What a spec may say.  :data is its reader's own, kept with the spec for
the reader's callbacks and let be here.")

(defun canvas-graph--check-spec (spec)
  "SPEC, or an error saying what is wrong with it: a key a spec has not,
or no nodes."
  (cl-loop for (key _) on spec by #'cddr
           unless (memq key canvas-graph--spec-keys)
           do (error "canvas-graph: no spec key %S" key))
  (unless (plist-get spec :nodes)
    (error "canvas-graph: %s has no nodes" (plist-get spec :name)))
  spec)

(defun canvas-graph--node-of (spec)
  "The node SPEC, (NAME [:label LABEL] [:pos POS]), describes, showing
LABEL, else NAME; another option is an error."
  (pcase-let ((`(,name . ,options) spec))
    (cl-loop for (key _) on options by #'cddr
             unless (memq key '(:label :pos :rows))
             do (error "canvas-graph: node %s has no option %S" name key))
    (canvas-graph-node-create :id name :label (or (plist-get options :label) name)
                              :pos (plist-get options :pos)
                              :rows (plist-get options :rows))))

(defun canvas-graph--named-in (nodes name)
  "The node of NODES its spec calls NAME; there being none is an error."
  (or (cl-find name nodes :key #'canvas-graph-node-id :test #'equal)
      (error "canvas-graph: no node %s" name)))

(defun canvas-graph--edges (nodes specs)
  "Edges for SPECS, (FROM TO [LABEL [POS [:from-row ROW] [:to-row ROW]]])
each, between NODES: one for each pair of nodes and pair of rows,
holding every label seen; FROM nil is any node.  Two edges between one
pair of nodes stay apart when they join different rows, since each lands
in a place of its own.  An edge to or from no node, or naming a row its
node has not, is an error."
  (let (edges)
    (pcase-dolist (`(,from ,to ,label ,pos . ,options) specs)
      (let* ((a (and from (canvas-graph--named-in nodes from)))
             (b (canvas-graph--named-in nodes to))
             (from-row (canvas-graph--checked-row a (plist-get options :from-row)))
             (to-row (canvas-graph--checked-row b (plist-get options :to-row)))
             (edge (cl-find-if (lambda (e) (and (eq (canvas-graph-edge-from e) a)
                                                (eq (canvas-graph-edge-to e) b)
                                                (equal (canvas-graph-edge-from-row e) from-row)
                                                (equal (canvas-graph-edge-to-row e) to-row)))
                               edges)))
        (if edge
            (when (and label (not (member label (canvas-graph-edge-labels edge))))
              (setf (canvas-graph-edge-labels edge)
                    (append (canvas-graph-edge-labels edge) (list label))))
          (push (canvas-graph-edge-create :from a :to b :pos pos :labels (and label (list label))
                                          :from-row from-row :to-row to-row)
                edges))))
    (nreverse edges)))

(defun canvas-graph--checked-row (node row)
  "ROW, when NODE holds it; a row NODE has not is an error, and nil is no
row at all."
  (when row
    (unless (member row (canvas-diagram-node-rows node))
      (error "canvas-graph: %s holds no row %S" (canvas-graph-node-id node) row))
    row))

(defun canvas-graph--connect (edges)
  "Tell each node of EDGES which edges leave it and which arrive."
  (dolist (edge edges)
    (let ((from (canvas-graph-edge-from edge)) (to (canvas-graph-edge-to edge)))
      (when from
        (setf (canvas-graph-node-out from) (append (canvas-graph-node-out from) (list edge))))
      (setf (canvas-graph-node-in to) (append (canvas-graph-node-in to) (list edge))))))

(defun canvas-graph--measure-depths (start edges)
  "Give every node reachable from START its distance from it and the
node that first reaches it, breadth first.  A node an edge of EDGES
from any node leads to is one step away."
  (setf (canvas-graph-node-depth start) 0)
  (let ((queue (list start)))
    (dolist (edge edges)
      (let ((to (canvas-graph-edge-to edge)))
        (when (and (canvas-graph--from-any-p edge) (not (canvas-graph-node-depth to)))
          (setf (canvas-graph-node-depth to) 1
                (canvas-graph-node-parent to) start)
          (setq queue (append queue (list to))))))
    (while queue
      (let ((node (pop queue)))
        (dolist (edge (canvas-graph-node-out node))
          (let ((next (canvas-graph-edge-to edge)))
            (unless (canvas-graph-node-depth next)
              (setf (canvas-graph-node-depth next) (1+ (canvas-graph-node-depth node))
                    (canvas-graph-node-parent next) node)
              (setq queue (append queue (list next))))))))))

(defun canvas-graph--mark-kinds (nodes start word)
  "Give START the kind WORD, and the unreachable ones of NODES theirs."
  (dolist (node nodes)
    (setf (canvas-diagram-node-kind node)
          (cond ((eq node start) word)
                ((null (canvas-graph-node-depth node)) "unreachable")))))

(defun canvas-graph--start-of (spec nodes)
  "The node of NODES SPEC starts at: the one it names, else the first."
  (if-let* ((label (plist-get spec :start)))
      (canvas-graph--named-in nodes label)
    (car nodes)))

(defun canvas-graph-build (spec &optional style)
  "The graph SPEC describes, speaking STYLE, measured from its start node.
SPEC is (:name NAME [:type TYPE] [:start NODE] :nodes ((NODE [:label
LABEL] [:pos POS])...) :edges ((FROM TO [LABEL [POS]])...)): a node is
known by its name, NODE, and shows LABEL when it has one; FROM nil is
any node, and without a start the first node is.  A reader may add
:data, anything of its own for its callbacks, which the graph lets be.
STYLE is a plist of the keys `canvas-graph--default-style' has, the
reader's own for them."
  (canvas-graph--check-spec spec)
  (let* ((style (append (canvas-graph--check-style style) canvas-graph--default-style))
         (nodes (mapcar #'canvas-graph--node-of (plist-get spec :nodes)))
         (edges (canvas-graph--edges nodes (plist-get spec :edges)))
         (start (canvas-graph--start-of spec nodes)))
    (canvas-graph--connect edges)
    (canvas-graph--measure-depths start edges)
    (canvas-graph--mark-kinds nodes start (plist-get style :start))
    (canvas-graph-create :name (plist-get spec :name) :type (plist-get spec :type)
                         :nodes nodes :start start :edges edges :style style)))

;;;; Writing a spec down

(cl-defstruct (canvas-graph-draft (:constructor canvas-graph-draft-create))
  "A graph a reader is writing down, to become a spec: its NODES, names
newest first, their LABELS, PLACES and ROWS by name, its EDGES, (FROM TO
LABEL POS) each, newest first, and its START."
  (nodes nil) (labels (make-hash-table :test #'equal)) (places (make-hash-table :test #'equal))
  (rows (make-hash-table :test #'equal)) (edges nil) (start nil))

(defun canvas-graph-draft-row (draft name row &optional pos)
  "Note that the node NAME of DRAFT, met at POS, holds ROW, a line of text
under its label.  The rows keep the order they were given, and a row
given twice is held once.  Return ROW."
  (canvas-graph-draft-node draft name nil pos)
  (let ((rows (gethash name (canvas-graph-draft-rows draft))))
    (unless (member row rows)
      (puthash name (append rows (list row)) (canvas-graph-draft-rows draft))))
  row)

(defun canvas-graph-draft-node (draft name &optional label pos)
  "Note the node NAME in DRAFT, placed at POS when first met, showing
LABEL when one is given, the last given winning.  Return NAME."
  (when (eq (gethash name (canvas-graph-draft-places draft) 'unmet) 'unmet)
    (push name (canvas-graph-draft-nodes draft))
    (puthash name pos (canvas-graph-draft-places draft)))
  (when label
    (puthash name label (canvas-graph-draft-labels draft)))
  name)

(defun canvas-graph-draft-edge (draft from to &optional label pos)
  "Note in DRAFT an edge from FROM, nil for any node, to TO under LABEL,
at POS, and its nodes with it."
  (when from
    (canvas-graph-draft-node draft from nil pos))
  (canvas-graph-draft-node draft to nil pos)
  (push (list from to label pos) (canvas-graph-draft-edges draft)))

(defun canvas-graph-draft-start-at (draft name &optional pos)
  "Make the node NAME, met at POS, DRAFT's start, unless it has one."
  (canvas-graph-draft-node draft name nil pos)
  (unless (canvas-graph-draft-start draft)
    (setf (canvas-graph-draft-start draft) name)))

(defconst canvas-graph--final "[*]"
  "The name of the node a state machine's final pseudo-state becomes.")

(defun canvas-graph-draft-transition (draft from to &optional label pos inside)
  "Note in DRAFT a state machine's transition FROM to TO under LABEL, at
POS, written INSIDE a composite state, or at the top when nil.  FROM or
TO nil is the initial or final pseudo-state, `[*]' in mermaid and
PlantUML.  From the initial one, TO is the start at the top, and the
composite leads to TO inside one.  To the final one, FROM leads to a
node labelled end at the top, and nothing is noted inside a composite."
  (cond ((and (null from) inside) (canvas-graph-draft-edge draft inside to label pos))
        ((null from) (canvas-graph-draft-start-at draft to pos))
        ((and (null to) inside) (canvas-graph-draft-node draft from nil pos))
        ((null to)
         (canvas-graph-draft-node draft canvas-graph--final "end" pos)
         (canvas-graph-draft-edge draft from canvas-graph--final label pos))
        (t (canvas-graph-draft-edge draft from to label pos))))

(defun canvas-graph--draft-node-spec (draft name)
  "The spec of DRAFT's node NAME: (NAME [:label LABEL] [:pos POS] [:rows
ROWS])."
  (let ((label (gethash name (canvas-graph-draft-labels draft)))
        (pos (gethash name (canvas-graph-draft-places draft)))
        (rows (gethash name (canvas-graph-draft-rows draft))))
    (append (list name) (and label (list :label label)) (and pos (list :pos pos))
            (and rows (list :rows rows)))))

(defun canvas-graph-draft-spec (draft name &optional type)
  "The spec DRAFT has written down, of the graph NAME, of TYPE if given."
  (append (list :name name)
          (and type (list :type type))
          (and (canvas-graph-draft-start draft) (list :start (canvas-graph-draft-start draft)))
          (list :nodes (mapcar (lambda (node) (canvas-graph--draft-node-spec draft node))
                               (reverse (canvas-graph-draft-nodes draft)))
                :edges (reverse (canvas-graph-draft-edges draft)))))

;;;; Common edges

(defun canvas-graph--common-groups (graph)
  "The common edges of GRAPH grouped: (TARGET LABELS EDGE...) each, in
order of first appearance."
  (let (groups)
    (dolist (edge (canvas-graph-edges graph))
      (when (canvas-graph-edge-common edge)
        (let ((group (cl-find-if (lambda (g) (and (eq (car g) (canvas-graph-edge-to edge))
                                                  (equal (cadr g) (canvas-graph-edge-labels edge))))
                                 groups)))
          (if group
              (setcdr (last group) (list edge))
            (push (list (canvas-graph-edge-to edge) (canvas-graph-edge-labels edge) edge) groups)))))
    (nreverse groups)))

(defun canvas-graph--alike (edge edges)
  "The edges of EDGES to EDGE's target under EDGE's labels, loops aside."
  (cl-remove-if-not (lambda (e) (and (not (canvas-graph--loop-p e))
                                     (eq (canvas-graph-edge-to e) (canvas-graph-edge-to edge))
                                     (equal (canvas-graph-edge-labels e) (canvas-graph-edge-labels edge))))
                    edges))

(defun canvas-graph--mark-common (graph)
  "Mark the edges of GRAPH that are common: those from any node, and
those to one target under one label from at least
`canvas-graph-common-min' nodes and two thirds of the others.  Only the
first when folding is off, and never one that joins rows."
  (let ((edges (canvas-graph-edges graph))
        (others (1- (length (canvas-graph-nodes graph)))))
    (dolist (edge edges)
      (setf (canvas-graph-edge-common edge)
            (or (canvas-graph--from-any-p edge)
                (and canvas-graph-fold-common
                     (not (canvas-graph--loop-p edge))
                     ;; An arrow that joins rows lands in a place of its
                     ;; own, and folding it would throw that away.
                     (not (canvas-graph-edge-from-row edge))
                     (not (canvas-graph-edge-to-row edge))
                     (let ((alike (length (canvas-graph--alike edge edges))))
                       (and (>= alike canvas-graph-common-min)
                            (>= (* 3 alike) (* 2 others))))))))))

(defun canvas-graph--drawn-p (edge)
  "Whether EDGE is drawn as an arrow of its own."
  (not (canvas-graph-edge-common edge)))

;;;; The layouts

(defun canvas-graph--place-ring (nodes gap)
  "Put NODES on a ring, clockwise from the top, in order, GAP apart.
The ring is wide enough for the boxes not to touch."
  (let* ((n (length nodes))
         (widest (apply #'max (mapcar #'canvas-diagram-node-w nodes)))
         (tallest (apply #'max (mapcar #'canvas-diagram-node-h nodes)))
         (radius (max (/ (* n (+ widest gap)) (* 2 float-pi)) (+ tallest gap))))
    (cl-loop for node in nodes for i from 0
             do (let ((angle (+ (- (/ float-pi 2)) (/ (* 2 float-pi i) n))))
                  (setf (canvas-diagram-node-x node) (- (* radius (cos angle)) (/ (canvas-diagram-node-w node) 2.0))
                        (canvas-diagram-node-y node) (- (* radius (sin angle)) (/ (canvas-diagram-node-h node) 2.0)))))))

(defun canvas-graph--layers (nodes)
  "NODES by depth, shallowest first, the unreachable ones in a last row."
  (let* ((depths (delq nil (mapcar #'canvas-graph-node-depth nodes)))
         (beyond (1+ (apply #'max -1 depths)))
         (layers (make-vector (1+ beyond) nil)))
    (dolist (node nodes)
      (let ((d (or (canvas-graph-node-depth node) beyond)))
        (aset layers d (append (aref layers d) (list node)))))
    (cl-remove-if #'null (append layers nil))))

(defun canvas-graph--barycentre (node previous)
  "The mean place in PREVIOUS, a row, of the nodes there that lead to
NODE, the one that first reached it among them; past the end when none
does, so such nodes keep to the right."
  (let ((places (delq nil (mapcar (lambda (s) (cl-position s previous))
                                  (cons (canvas-graph-node-parent node)
                                        (canvas-graph--sources node))))))
    (if places
        (/ (apply #'+ places) (float (length places)))
      (length previous))))

(defun canvas-graph--order-row (row previous)
  "ROW's nodes ordered under the nodes of PREVIOUS that lead to them, so
fewer arrows cross; ties keep declaration order."
  (if previous
      (sort (copy-sequence row)
            (lambda (a b) (< (canvas-graph--barycentre a previous)
                             (canvas-graph--barycentre b previous))))
    row))

(defun canvas-graph--ordered-rows (nodes)
  "The rows of NODES, top down, each ordered under the row above."
  (let (rows previous)
    (dolist (row (canvas-graph--layers nodes))
      (let ((ordered (canvas-graph--order-row row previous)))
        (push ordered rows)
        (setq previous ordered)))
    (nreverse rows)))

(defun canvas-graph--between-p (edge row next)
  "Whether EDGE runs between ROW and NEXT, either way."
  (let ((from (canvas-graph-edge-from edge)) (to (canvas-graph-edge-to edge)))
    (or (and (memq from row) (memq to next))
        (and (memq from next) (memq to row)))))

(defun canvas-graph--row-gap (row next gap-y graph label-size)
  "Pixels between ROW and NEXT: GAP-Y, or more when a label of an arrow
between them, sized by LABEL-SIZE, needs it; and room besides for the
labels of arrows within either row that stand taller than their boxes."
  (+ (apply #'max gap-y
            (mapcar (lambda (edge)
                      (if-let* (((canvas-graph--between-p edge row next))
                                (size (funcall label-size edge)))
                          (+ 12 (cdr size))
                        0))
                    (canvas-graph-edges graph)))
     (canvas-graph--overhang row graph label-size)
     (canvas-graph--overhang next graph label-size)))

(defun canvas-graph--within (row graph)
  "The drawn arrows of GRAPH between two nodes of ROW."
  (cl-loop for (a . rest) on row
           append (cl-loop for b in rest append (canvas-graph--joining a b graph))))

(defun canvas-graph--overhang (row graph label-size)
  "How far the label of an arrow within ROW, on the row's middle, sticks
out above and below the row's boxes; 0 when none does."
  (let ((tallest (apply #'max (mapcar #'canvas-diagram-node-h row))))
    (apply #'max 0
           (mapcar (lambda (edge)
                     (if-let* ((size (funcall label-size edge)))
                         (+ 6 (ceiling (/ (- (+ (cdr size) 5) tallest) 2.0)))
                       0))
                   (canvas-graph--within row graph)))))

(defun canvas-graph--place-row-labels (row graph label-size)
  "Give the labelled arrows between two nodes of ROW their labels, on the
arrow between the boxes, several stacked."
  (cl-loop for (a . rest) on row
           do (dolist (b rest)
                (let* ((edges (cl-remove-if-not label-size (canvas-graph--joining a b graph)))
                       (sizes (mapcar label-size edges))
                       (total (+ (apply #'+ (mapcar (lambda (s) (+ (cdr s) 5)) sizes)) (* 4 (max 0 (1- (length edges))))))
                       (cx (/ (+ (+ (canvas-diagram-node-x a) (canvas-diagram-node-w a)) (canvas-diagram-node-x b)) 2.0))
                       (y (- (canvas-diagram-middle-y a) (/ total 2.0))))
                  (cl-loop for edge in edges for size in sizes
                           do (setf (canvas-graph-edge-label-rect edge)
                                    (canvas-graph--centred (cons cx (+ y (/ (+ (cdr size) 5) 2.0))) size))
                           (cl-incf y (+ (cdr size) 5 4)))))))

(defun canvas-graph--joining (a b graph)
  "The drawn arrows of GRAPH between the nodes A and B, either way."
  (cl-remove-if-not (lambda (e) (and (canvas-graph--drawn-p e)
                                     (or (and (eq (canvas-graph-edge-from e) a) (eq (canvas-graph-edge-to e) b))
                                         (and (eq (canvas-graph-edge-from e) b) (eq (canvas-graph-edge-to e) a)))))
                    (canvas-graph-edges graph)))

(defun canvas-graph--row-gaps (row gap-x graph label-size)
  "The gaps between ROW's nodes in turn: GAP-X, or room for the label of
an arrow between two of them side by side."
  (cl-loop for (a b) on row while b
           collect (apply #'max gap-x
                          (mapcar (lambda (e) (if-let* ((size (funcall label-size e))) (+ 16 (car size)) 0))
                                  (canvas-graph--joining a b graph)))))

(defun canvas-graph--row-width (row gaps)
  "Width of ROW's boxes side by side with GAPS between them."
  (+ (apply #'+ (mapcar #'canvas-diagram-node-w row)) (apply #'+ gaps)))

(defun canvas-graph--place-row (row y widest gaps)
  "Put ROW's boxes side by side from Y down, GAPS apart, centred on WIDEST;
the row's height."
  (let ((x (/ (- widest (canvas-graph--row-width row gaps)) 2.0))
        (tallest (apply #'max (mapcar #'canvas-diagram-node-h row))))
    (cl-loop for node in row for gap in (append gaps '(0))
             do (setf (canvas-diagram-node-x node) x
                      (canvas-diagram-node-y node) (+ y (/ (- tallest (canvas-diagram-node-h node)) 2.0)))
             (cl-incf x (+ (canvas-diagram-node-w node) gap)))
    tallest))

(defun canvas-graph--gap-labels (row next graph label-size)
  "The labelled arrows between ROW and NEXT with their labels' sizes and
the x their labels would like, under the middle of the arrow: (EDGE
SIZE X) each, left to right."
  (sort (cl-loop for edge in (canvas-graph-edges graph)
                 for size = (and (canvas-graph--drawn-p edge)
                                 (not (canvas-graph--loop-p edge))
                                 (canvas-graph--between-p edge row next)
                                 (funcall label-size edge))
                 when size
                 collect (list edge size
                               (/ (+ (canvas-diagram-middle-x (canvas-graph-edge-from edge))
                                     (canvas-diagram-middle-x (canvas-graph-edge-to edge)))
                                  2.0)))
        (lambda (a b) (< (nth 2 a) (nth 2 b)))))

(defun canvas-graph--pack (items)
  "Left edges for ITEMS, (WIDTH . WANTED-MIDDLE) each in order, laid side
by side six apart, each as near its wanted middle as the ones before
allow, the row then moved back so it strays as little as it can."
  (let ((right nil) (lefts nil) (strayed 0))
    (pcase-dolist (`(,w . ,x) items)
      (let ((left (max (- x (/ w 2.0)) (if right (+ right 6) most-negative-fixnum))))
        (push left lefts)
        (cl-incf strayed (- left (- x (/ w 2.0))))
        (setq right (+ left w))))
    (let ((back (/ strayed (float (max 1 (length items))))))
      (mapcar (lambda (left) (- left back)) (nreverse lefts)))))

(defun canvas-graph--place-gap-labels (row next top bottom graph label-size)
  "Give the labelled arrows between ROW and NEXT their labels, side by side
in the gap from TOP to BOTTOM, and have each arrow bend through the
middle of its label."
  (let* ((items (canvas-graph--gap-labels row next graph label-size))
         (lefts (canvas-graph--pack (mapcar (lambda (i) (cons (+ (car (nth 1 i)) 9) (nth 2 i))) items)))
         (cy (/ (+ top bottom) 2.0)))
    (cl-loop for (edge size) in items for left in lefts
             do (let ((cx (+ left (/ (+ (car size) 9) 2.0))))
                  (setf (canvas-graph-edge-label-rect edge) (canvas-graph--centred (cons cx cy) size)
                        (canvas-graph-edge-control edge) (cons cx cy))))))

(defun canvas-graph--place-rows (rows gap-x gap-y graph label-size)
  "Put ROWS one below the other, each centred on the widest, its nodes
GAP-X apart or their labels' worth, and GAP-Y or a tall label's height
below the row before, the labels of the arrows between two rows side by
side in the gap."
  (let* ((gaps (mapcar (lambda (row) (canvas-graph--row-gaps row gap-x graph label-size)) rows))
         (widest (apply #'max (cl-mapcar #'canvas-graph--row-width rows gaps)))
         (y 0))
    (cl-loop for (row next) on rows for (row-gaps next-gaps) on gaps
             do (let ((tallest (canvas-graph--place-row row y widest row-gaps)))
                  (canvas-graph--place-row-labels row graph label-size)
                  (when next
                    (let* ((bottom (+ y tallest))
                           (next-y (+ bottom (canvas-graph--row-gap row next gap-y graph label-size))))
                      ;; The next row's boxes must stand before the arrows to them bend.
                      (canvas-graph--place-row next next-y widest next-gaps)
                      ;; The labels between the rows keep clear of those on the rows.
                      (canvas-graph--place-gap-labels row next
                                                      (+ bottom (canvas-graph--overhang row graph label-size))
                                                      (- next-y (canvas-graph--overhang next graph label-size))
                                                      graph label-size)
                      (setq y next-y)))))))

(defun canvas-graph--two-way-p (edge)
  "Whether another drawn edge runs back the way EDGE runs."
  (and (canvas-graph--drawn-p edge)
       (not (canvas-graph--loop-p edge))
       (cl-some (lambda (back) (and (canvas-graph--drawn-p back)
                                    (eq (canvas-graph-edge-to back) (canvas-graph-edge-from edge))))
                (canvas-graph-node-out (canvas-graph-edge-to edge)))))

(defun canvas-graph--inside-p (point node room)
  "Whether POINT, (X . Y), lies within ROOM pixels of NODE's box."
  (and (<= (- (canvas-diagram-node-x node) room) (car point)
           (+ (canvas-diagram-node-x node) (canvas-diagram-node-w node) room))
       (<= (- (canvas-diagram-node-y node) room) (cdr point)
           (+ (canvas-diagram-node-y node) (canvas-diagram-node-h node) room))))

(defun canvas-graph--point-on (curve k)
  "The point K of the way, 0 to 1, along CURVE, (X0 Y0 CX CY X1 Y1)."
  (pcase-let ((`(,x0 ,y0 ,cx ,cy ,x1 ,y1) curve))
    (let ((a (* (- 1 k) (- 1 k))) (b (* 2 k (- 1 k))) (c (* k k)))
      (cons (+ (* a x0) (* b cx) (* c x1))
            (+ (* a y0) (* b cy) (* c y1))))))

(defun canvas-graph--cubic-points (cubic)
  "Eleven points along CUBIC, (X0 Y0 C1X C1Y C2X C2Y X1 Y1)."
  (pcase-let ((`(,x0 ,y0 ,c1x ,c1y ,c2x ,c2y ,x1 ,y1) cubic))
    (mapcar (lambda (i)
              (let* ((k (/ i 10.0)) (m (- 1 k))
                     (a (* m m m)) (b (* 3 m m k)) (c (* 3 m k k)) (d (* k k k)))
                (cons (+ (* a x0) (* b c1x) (* c c2x) (* d x1))
                      (+ (* a y0) (* b c1y) (* c c2y) (* d y1)))))
            (number-sequence 0 10))))

(defun canvas-graph--crossings (edge bow others)
  "How many of OTHERS' boxes EDGE's arrow, bowed by BOW, runs through."
  (let ((curve (canvas-graph--curve edge bow)))
    (cl-count-if (lambda (node)
                   (cl-some (lambda (k) (canvas-graph--inside-p (canvas-graph--point-on curve k) node 4))
                            '(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)))
                 others)))

(defun canvas-graph--clearest-bow (edge others)
  "The smallest bow, none first and right before left, that takes EDGE's
arrow past every box of OTHERS; failing that, the one that crosses fewest."
  (let* ((bow canvas-graph-bow)
         (candidates (cons 0 (cl-loop for k from 1 to 6 collect (* k bow) collect (* (- k) bow))))
         (crossings (lambda (b) (canvas-graph--crossings edge b others))))
    (or (cl-find-if (lambda (b) (zerop (funcall crossings b))) candidates)
        (cl-reduce (lambda (best b) (if (< (funcall crossings b) (funcall crossings best)) b best))
                   candidates))))

(defun canvas-graph--choose-bow (edge nodes)
  "The bow EDGE takes among NODES: none when it is not drawn as an arrow
or loops; to its right when another comes back the other way; else
none, unless a straight arrow would run through a box, when it bows
just enough to clear it."
  (cond ((or (canvas-graph--loop-p edge) (not (canvas-graph--drawn-p edge))
             (canvas-graph-edge-control edge))
         0)
        ((canvas-graph--two-way-p edge) canvas-graph-bow)
        (t (canvas-graph--clearest-bow
            edge (cl-set-difference nodes (list (canvas-graph-edge-from edge)
                                                (canvas-graph-edge-to edge)))))))

(defun canvas-graph--choose-bows (graph)
  "Decide how far each arrow of GRAPH bows, now that its boxes are placed."
  (dolist (edge (canvas-graph-edges graph))
    (setf (canvas-graph-edge-bow edge)
          (canvas-graph--choose-bow edge (canvas-graph-nodes graph)))))

(defun canvas-graph--curve-points (edge)
  "Points along EDGE's arrow as it will be drawn, a loop's included."
  (if (canvas-graph--loop-p edge)
      (canvas-graph--cubic-points (canvas-graph--loop-geometry (canvas-graph-edge-from edge)))
    (let ((curve (canvas-graph--curve edge (canvas-graph-edge-bow edge))))
      (mapcar (lambda (i) (canvas-graph--point-on curve (/ i 10.0))) (number-sequence 0 10)))))

(defun canvas-graph--rect-corners (rect)
  "The two corners of RECT, (X Y W H), or nil for none."
  (pcase rect
    (`(,x ,y ,w ,h) (list (cons x y) (cons (+ x w) (+ y h))))))

(defun canvas-graph--drawn-points (graph ctx)
  "Points of everything drawn for GRAPH besides its boxes: the arrows,
the any-node markers, the corners of the labels when shown, and the
ways round the boxes a selected node's folded edges take."
  (let ((font (canvas-diagram-font)))
    (append
     (cl-loop for edge in (canvas-graph-edges graph)
              when (canvas-graph--drawn-p edge)
              append (canvas-graph--curve-points edge)
              and append (canvas-graph--rect-corners (canvas-graph-edge-label-rect edge)))
     (cl-loop for (i . group) in (canvas-graph--marker-groups graph)
              append (pcase-let ((`(,dx ,dy ,_ ,_) (canvas-graph--marker-geometry (car group) i)))
                       (list (cons (- dx 4) (- dy 4)) (cons (- dx 4) (+ dy 4))))
              append (when (canvas-graph--all-labels-p graph)
                       (canvas-graph--rect-corners (canvas-graph--marker-label-rect ctx graph group i font))))
     (cl-loop for edge in (canvas-graph--folded-edges graph)
              append (canvas-graph--route-points (canvas-graph-edge-route edge))))))

(defun canvas-graph--overshoot (graph ctx)
  "How far what is drawn for GRAPH reaches past its boxes, in whole
pixels: (LEFT TOP RIGHT BOTTOM), each 0 when nothing does."
  (let* ((nodes (canvas-graph-nodes graph))
         (bounds (canvas-diagram--bounds nodes))
         (left (apply #'min (mapcar #'canvas-diagram-node-x nodes)))
         (top (apply #'min (mapcar #'canvas-diagram-node-y nodes)))
         (over (list 0 0 0 0)))
    (pcase-dolist (`(,x . ,y) (canvas-graph--drawn-points graph ctx))
      (setf (nth 0 over) (max (nth 0 over) (- left x))
            (nth 1 over) (max (nth 1 over) (- top y))
            (nth 2 over) (max (nth 2 over) (- x (car bounds)))
            (nth 3 over) (max (nth 3 over) (- y (cdr bounds)))))
    (mapcar #'ceiling over)))

(defun canvas-graph--shift (graph dx dy)
  "Move every box of GRAPH, every label placed and every way routed, by DX DY."
  (dolist (node (canvas-graph-nodes graph))
    (setf (canvas-diagram-node-x node) (+ (canvas-diagram-node-x node) dx)
          (canvas-diagram-node-y node) (+ (canvas-diagram-node-y node) dy)))
  (dolist (edge (canvas-graph-edges graph))
    (when-let* ((rect (canvas-graph-edge-label-rect edge)))
      (setf (canvas-graph-edge-label-rect edge)
            (list (+ (nth 0 rect) dx) (+ (nth 1 rect) dy) (nth 2 rect) (nth 3 rect))))
    (when-let* ((through (canvas-graph-edge-control edge)))
      (setf (canvas-graph-edge-control edge) (cons (+ (car through) dx) (+ (cdr through) dy))))
    (when-let* ((route (canvas-graph-edge-route edge)))
      (setf (canvas-graph-edge-route edge)
            (mapcar (lambda (p) (cons (+ (car p) dx) (+ (cdr p) dy))) route)))))

(defun canvas-graph--normalize (graph left top)
  "Shift GRAPH so that its leftmost box sits LEFT from the origin and its
topmost TOP."
  (let ((nodes (canvas-graph-nodes graph)))
    (canvas-graph--shift graph
                         (- left (apply #'min (mapcar #'canvas-diagram-node-x nodes)))
                         (- top (apply #'min (mapcar #'canvas-diagram-node-y nodes))))))

(defun canvas-graph--labels (graph)
  "Which of GRAPH's arrows are labelled: as `canvas-graph-show-labels'
says, or on auto as GRAPH's reader would have it."
  (if (eq canvas-graph-show-labels 'auto)
      (canvas-graph--styled graph :labels)
    canvas-graph-show-labels))

(defun canvas-graph--all-labels-p (graph)
  "Whether every arrow of GRAPH is labelled, so the labels take part in
the layout."
  (eq (canvas-graph--labels graph) 'all))

(defun canvas-graph--label-sizer (ctx graph font)
  "A function giving the (W . H) of an edge's label of GRAPH on CTX in
FONT, or nil when it has none or labels are not laid out."
  (lambda (edge)
    (when-let* (((and (canvas-graph--all-labels-p graph) (canvas-graph--drawn-p edge)))
                (text (canvas-graph--label graph edge)))
      (canvas-graph--label-size ctx graph text font))))

(defun canvas-graph--forget-placing (graph)
  "Take away every label, bend and route GRAPH's last layout gave its edges."
  (dolist (edge (canvas-graph-edges graph))
    (setf (canvas-graph-edge-label-rect edge) nil
          (canvas-graph-edge-control edge) nil
          (canvas-graph-edge-route edge) nil)))

(defun canvas-graph--place-boxes (graph rows ctx)
  "Place GRAPH's boxes on CTX: in ROWS, or on a ring without them."
  (let ((gap-x (canvas-diagram-spaced canvas-graph-gap-x)))
    (if rows
        (canvas-graph--place-rows rows gap-x (canvas-diagram-spaced canvas-graph-gap-y)
                                  graph (canvas-graph--label-sizer ctx graph (canvas-diagram-font)))
      (canvas-graph--place-ring (canvas-graph-nodes graph) (* 2 gap-x)))))

(defun canvas-graph--lay-out (diagram ctx)
  "Lay DIAGRAM's graph out on CTX; the nodes in reading order.
The drawing gets room on each side for whatever the arrows and labels
reach past the boxes, and no more."
  (let* ((graph (canvas-diagram-model diagram))
         (nodes (canvas-graph-nodes graph))
         (measure (canvas-diagram-measure ctx))
         (rows (and (eq canvas-graph-layout 'layered) (canvas-graph--ordered-rows nodes))))
    (canvas-graph--mark-common graph)
    (canvas-graph--forget-placing graph)
    (dolist (node nodes)
      (canvas-diagram-size-node diagram node measure))
    (canvas-graph--place-boxes graph rows ctx)
    (canvas-graph--choose-bows graph)
    (canvas-graph--place-labels graph ctx (canvas-diagram-font))
    (canvas-graph--route-folded graph)
    (pcase-let ((`(,left ,top ,right ,bottom) (canvas-graph--overshoot graph ctx)))
      (setf (canvas-diagram-slack diagram) (cons right bottom))
      (canvas-graph--normalize graph left top))
    (if rows (apply #'append rows) nodes)))

;;;; Arrows

(defun canvas-graph--border-point (node toward)
  "Where the line from NODE's centre TOWARD a point (X . Y) leaves its box."
  (let* ((cx (canvas-diagram-middle-x node))
         (cy (canvas-diagram-middle-y node))
         (dx (- (car toward) cx))
         (dy (- (cdr toward) cy))
         (k (min (if (zerop dx) 1.0e9 (/ (canvas-diagram-node-w node) 2.0 (abs dx)))
                 (if (zerop dy) 1.0e9 (/ (canvas-diagram-node-h node) 2.0 (abs dy))))))
    (cons (+ cx (* k dx)) (+ cy (* k dy)))))

(defun canvas-graph--control (ax ay bx by bow)
  "The control point of an arrow from (AX, AY) to (BX, BY) bowed by BOW,
a fraction of its length, positive to its right: (X . Y)."
  (cons (+ (/ (+ ax bx) 2.0) (* bow (- by ay)))
        (+ (/ (+ ay by) 2.0) (* bow (- ax bx)))))

(defun canvas-graph--end-point (node row other control)
  "Where an arrow meets NODE: the place of ROW on the side facing OTHER,
or, for no row, the point on its border towards CONTROL."
  (if row
      (canvas-diagram-row-anchor node row (if (< (canvas-diagram-middle-x other)
                                                 (canvas-diagram-middle-x node))
                                              'left 'right))
    (canvas-graph--border-point node control)))

(defun canvas-graph--curve (edge bow)
  "EDGE's arrow bowed by BOW: (X0 Y0 CX CY X1 Y1), from the border of its
source, past a control point, to the border of its target."
  (let* ((from (canvas-graph-edge-from edge))
         (to (canvas-graph-edge-to edge))
         (control (canvas-graph--control (canvas-diagram-middle-x from) (canvas-diagram-middle-y from)
                                         (canvas-diagram-middle-x to) (canvas-diagram-middle-y to) bow))
         (p0 (canvas-graph--end-point from (canvas-graph-edge-from-row edge) to control))
         (p1 (canvas-graph--end-point to (canvas-graph-edge-to-row edge) from control)))
    (list (car p0) (cdr p0) (car control) (cdr control) (car p1) (cdr p1))))

(defun canvas-graph--folded-group (edge graph)
  "The any-node group of GRAPH that EDGE is folded into, with its place
among its target's: (I . GROUP)."
  (or (cl-find-if (lambda (ig) (memq edge (cddr (cdr ig)))) (canvas-graph--marker-groups graph))
      (error "canvas-graph: %s is not folded" (canvas-graph-edge-labels edge))))

(defconst canvas-graph--tag-radius 8
  "Radius of the disc a tag's number sits in.")

;;;;; Routing a folded way round the boxes

;; An edge folded into an any-node marker has no arrow of its own; when
;; its node is selected the trail shows its way to the marker.  That way
;; is routed orthogonally on a visibility grid, as graphviz's ortho
;; splines are: a grid line runs a margin out from every side of every
;; box, plus lines through the way's ends; a step between two
;; neighbouring lines is barred when it runs through a box; and the
;; cheapest path from the source's top or bottom to the marker's dot is
;; found with A*, a step costing its length and every arrow it crosses,
;; a turn a little more, so the way goes round the drawing rather than
;; through it when it can.

(defconst canvas-graph--route-margin 16
  "Pixels a folded way keeps from the boxes it goes round.")

(defconst canvas-graph--bend-cost 20
  "What a turn costs a folded way, in pixels of length.")

(defconst canvas-graph--crossing-cost 150
  "What crossing an arrow costs a folded way, in pixels of length.")

(defcustom canvas-graph-route-turns 'rounded
  "How a folded way turns its corners: `rounded' or `sharp'."
  :type '(choice (const rounded) (const sharp)))

(cl-defstruct (canvas-graph-grid (:constructor canvas-graph-grid-create))
  "The grid a folded way is routed on: its lines' XS and YS, sorted; the
BOXES it goes round, (X Y W H) each; the ARROWS it crosses at a cost,
each (BOUNDS . POINTS); the STEPS from each point and the COSTS of the
steps found so far."
  xs ys boxes arrows
  (steps (make-hash-table :test #'equal)) (costs (make-hash-table :test #'equal)))

(defun canvas-graph--distance (a b)
  "How far the point A, (X . Y), is from B."
  (sqrt (+ (expt (- (car b) (car a)) 2) (expt (- (cdr b) (cdr a)) 2))))

(defun canvas-graph--toward (a b d)
  "The point D along the way from A to B, (X . Y) each."
  (let ((len (max 0.001 (canvas-graph--distance a b))))
    (cons (+ (car a) (* d (/ (- (car b) (car a)) len)))
          (+ (cdr a) (* d (/ (- (cdr b) (cdr a)) len))))))

(defun canvas-graph--segments-cross-p (a b c d)
  "Whether the segments A-B and C-D, points (X . Y), cross or touch,
unless they lie along one line."
  (cl-flet ((side (p q r) (- (* (- (car q) (car p)) (- (cdr r) (cdr p)))
                             (* (- (cdr q) (cdr p)) (- (car r) (car p))))))
    (let ((c-side (side a b c)) (d-side (side a b d)))
      (and (<= (* c-side d-side) 0)
           (not (and (zerop c-side) (zerop d-side)))
           (<= (* (side c d a) (side c d b)) 0)))))

(defun canvas-graph--bounds-of (points)
  "The box round POINTS: (LEFT TOP RIGHT BOTTOM)."
  (list (apply #'min (mapcar #'car points)) (apply #'min (mapcar #'cdr points))
        (apply #'max (mapcar #'car points)) (apply #'max (mapcar #'cdr points))))

(defun canvas-graph--bounds-meet-p (a b)
  "Whether the boxes A and B, (LEFT TOP RIGHT BOTTOM) each, overlap or touch."
  (and (<= (nth 0 a) (nth 2 b)) (<= (nth 0 b) (nth 2 a))
       (<= (nth 1 a) (nth 3 b)) (<= (nth 1 b) (nth 3 a))))

(defun canvas-graph--polyline-crossings (corners points)
  "How many times the way along CORNERS crosses the line through POINTS."
  (cl-loop for (a b) on corners while b
           sum (cl-loop for (c d) on points while d
                        count (canvas-graph--segments-cross-p a b c d))))

(defun canvas-graph--box (node)
  "NODE's box: (X Y W H)."
  (list (canvas-diagram-node-x node) (canvas-diagram-node-y node)
        (canvas-diagram-node-w node) (canvas-diagram-node-h node)))

(defun canvas-graph--in-box-p (point box)
  "Whether POINT, (X . Y), lies strictly inside BOX, (X Y W H)."
  (pcase-let ((`(,x ,y ,w ,h) box))
    (and (< x (car point) (+ x w)) (< y (cdr point) (+ y h)))))

(defun canvas-graph--in-some-box-p (point boxes)
  "Whether POINT lies strictly inside one of BOXES."
  (cl-some (lambda (box) (canvas-graph--in-box-p point box)) boxes))

(defun canvas-graph--float-point (point)
  "POINT, (X . Y), with both floats, as the grid's lines are."
  (cons (float (car point)) (float (cdr point))))

(defun canvas-graph--sorted-lines (numbers)
  "NUMBERS as grid lines: floats, each once, ascending."
  (sort (cl-remove-duplicates (mapcar #'float numbers) :test #'=) #'<))

(defun canvas-graph--folded-edges (graph)
  "The edges of GRAPH folded into any-node markers, the ones from a node."
  (cl-remove-if (lambda (e) (or (canvas-graph--drawn-p e) (canvas-graph--from-any-p e)))
                (canvas-graph-edges graph)))

(defun canvas-graph--marker-dot (edge graph)
  "The dot of the any-node marker EDGE is folded into in GRAPH, (X . Y)."
  (pcase-let* ((`(,i . ,group) (canvas-graph--folded-group edge graph))
               (`(,dx ,dy ,_ ,_) (canvas-graph--marker-geometry (car group) i)))
    (cons dx dy)))

(defun canvas-graph--box-exits (node)
  "Where a way may leave NODE's box: a margin above the middle of its top
and below the middle of its bottom, each with the point on the box it
comes from: ((EXIT . ON-BOX)...)."
  (let ((m canvas-graph--route-margin)
        (mx (canvas-diagram-middle-x node))
        (top (canvas-diagram-node-y node))
        (bottom (+ (canvas-diagram-node-y node) (canvas-diagram-node-h node))))
    (list (cons (cons mx (- top m)) (cons mx top))
          (cons (cons mx (+ bottom m)) (cons mx bottom)))))

(defun canvas-graph--dot-approaches (dot)
  "Where a way may come at DOT, a marker's dot, from: a margin left of
it, above it and below it."
  (let ((m canvas-graph--route-margin))
    (list (cons (- (car dot) m) (cdr dot))
          (cons (car dot) (- (cdr dot) m))
          (cons (car dot) (+ (cdr dot) m)))))

(defun canvas-graph--grid (graph folded)
  "The routing grid over GRAPH's boxes for its FOLDED edges: a line a
margin out from every side of every box, and lines through the ways'
ends."
  (let* ((nodes (canvas-graph-nodes graph))
         (m canvas-graph--route-margin)
         (ends (append (cl-loop for e in folded append (mapcar #'car (canvas-graph--box-exits (canvas-graph-edge-from e))))
                       (cl-loop for e in folded append (canvas-graph--dot-approaches (canvas-graph--marker-dot e graph))))))
    (canvas-graph-grid-create
     :xs (canvas-graph--sorted-lines
          (append (mapcar #'car ends)
                  (cl-loop for s in nodes collect (- (canvas-diagram-node-x s) m)
                           collect (+ (canvas-diagram-node-x s) (canvas-diagram-node-w s) m))))
     :ys (canvas-graph--sorted-lines
          (append (mapcar #'cdr ends)
                  (cl-loop for s in nodes collect (- (canvas-diagram-node-y s) m)
                           collect (+ (canvas-diagram-node-y s) (canvas-diagram-node-h s) m))))
     :boxes (mapcar #'canvas-graph--box nodes)
     :arrows (cl-loop for edge in (canvas-graph-edges graph)
                      when (canvas-graph--drawn-p edge)
                      collect (let ((points (canvas-graph--curve-points edge)))
                                (cons (canvas-graph--bounds-of points) points))))))

(defun canvas-graph--next-line (lines v before)
  "The line of LINES, ascending, just BEFORE V, or just after it; nil at the end."
  (if before
      (cl-find-if (lambda (l) (< l v)) lines :from-end t)
    (cl-find-if (lambda (l) (> l v)) lines)))

(defun canvas-graph--step-barred-p (grid a b)
  "Whether the step from A to B on GRID runs into a box: B or the middle
of the step lies in one."
  (let ((boxes (canvas-graph-grid-boxes grid)))
    (or (canvas-graph--in-some-box-p b boxes)
        (canvas-graph--in-some-box-p (cons (/ (+ (car a) (car b)) 2.0) (/ (+ (cdr a) (cdr b)) 2.0)) boxes))))

(defun canvas-graph--find-steps (grid point)
  "The steps from POINT on GRID, (DIRECTION . POINT) each: to the next
line left, right, up and down, those into a box left out."
  (let ((xs (canvas-graph-grid-xs grid)) (ys (canvas-graph-grid-ys grid)))
    (cl-remove-if (lambda (step) (canvas-graph--step-barred-p grid point (cdr step)))
                  (delq nil
                        (list (when-let* ((x (canvas-graph--next-line xs (car point) t))) (cons 'left (cons x (cdr point))))
                              (when-let* ((x (canvas-graph--next-line xs (car point) nil))) (cons 'right (cons x (cdr point))))
                              (when-let* ((y (canvas-graph--next-line ys (cdr point) t))) (cons 'up (cons (car point) y)))
                              (when-let* ((y (canvas-graph--next-line ys (cdr point) nil))) (cons 'down (cons (car point) y))))))))

(defun canvas-graph--grid-steps (grid point)
  "The steps from POINT on GRID, found once and kept."
  (let ((steps (canvas-graph-grid-steps grid)))
    (or (gethash point steps)
        (puthash point (or (canvas-graph--find-steps grid point) 'none) steps))))

(defun canvas-graph--step-crossings (grid a b)
  "How many arrows of GRID the step from A to B crosses, only those whose
bounds it meets being tried."
  (let ((bounds (canvas-graph--bounds-of (list a b))))
    (cl-loop for (arrow-bounds . points) in (canvas-graph-grid-arrows grid)
             when (canvas-graph--bounds-meet-p bounds arrow-bounds)
             sum (canvas-graph--polyline-crossings (list a b) points))))

(defun canvas-graph--step-cost (grid a b)
  "What the step from A to B on GRID costs: its length, and every arrow
it crosses."
  (let ((costs (canvas-graph-grid-costs grid)))
    (or (gethash (cons a b) costs)
        (puthash (cons a b)
                 (+ (canvas-graph--distance a b)
                    (* canvas-graph--crossing-cost (canvas-graph--step-crossings grid a b)))
                 costs))))

(defun canvas-graph--to-go (point goals)
  "The least a way from POINT to one of GOALS can cost: the nearest's
distance along the grid."
  (apply #'min (mapcar (lambda (g) (+ (abs (- (car g) (car point))) (abs (- (cdr g) (cdr point))))) goals)))

(defun canvas-graph--enqueue (queue item)
  "QUEUE, (PRIORITY . REST) entries ascending, with ITEM spliced into its
place; QUEUE is changed."
  (if (or (null queue) (< (car item) (car (car queue))))
      (cons item queue)
    (let ((tail queue))
      (while (and (cdr tail) (<= (car (cadr tail)) (car item)))
        (setq tail (cdr tail)))
      (setcdr tail (cons item (cdr tail)))
      queue)))

(defun canvas-graph--path-back (state from)
  "The points from a start to STATE, (POINT . DIRECTION), following FROM."
  (let (points)
    (while state
      (push (car state) points)
      (setq state (gethash state from)))
    points))

(defun canvas-graph--corners-of (points)
  "POINTS with each one on a straight run between its neighbours left out."
  (if (< (length points) 3)
      points
    (let ((kept (list (car points))))
      (cl-loop for (a b c) on points while c
               unless (or (and (= (car a) (car b)) (= (car b) (car c)))
                          (and (= (cdr a) (cdr b)) (= (cdr b) (cdr c))))
               do (push b kept))
      (nreverse (cons (car (last points)) kept)))))

(defun canvas-graph--route-on (grid starts goals)
  "The cheapest way on GRID from one of STARTS to one of GOALS, points, as
its corners; nil when nothing joins them.  A step costs its length and
the arrows it crosses, a turn `canvas-graph--bend-cost' more."
  (let ((best (make-hash-table :test #'equal))
        (from (make-hash-table :test #'equal))
        (queue nil)
        (found nil))
    (setq starts (mapcar #'canvas-graph--float-point starts)
          goals (mapcar #'canvas-graph--float-point goals))
    (dolist (start starts)
      (puthash (cons start nil) 0 best)
      (setq queue (canvas-graph--enqueue queue (list (canvas-graph--to-go start goals) 0 (cons start nil)))))
    (while (and queue (not found))
      (pcase-let ((`(,_ ,cost ,state) (pop queue)))
        (when (<= cost (gethash state best 1.0e+INF))
          (if (member (car state) goals)
              (setq found state)
            (pcase-dolist (`(,direction . ,next) (let ((steps (canvas-graph--grid-steps grid (car state))))
                                                    (and (listp steps) steps)))
              (let* ((next-state (cons next direction))
                     (next-cost (+ cost (canvas-graph--step-cost grid (car state) next)
                                   (if (and (cdr state) (not (eq (cdr state) direction))) canvas-graph--bend-cost 0))))
                (when (< next-cost (gethash next-state best 1.0e+INF))
                  (puthash next-state next-cost best)
                  (puthash next-state state from)
                  (setq queue (canvas-graph--enqueue
                               queue (list (+ next-cost (canvas-graph--to-go next goals)) next-cost next-state))))))))))
    (and found (canvas-graph--path-back found from))))

(defun canvas-graph--straight-route (edge graph)
  "EDGE's way straight from its source's border to the rim of the dot of
the marker it is folded into in GRAPH: two corners."
  (let* ((dot (canvas-graph--marker-dot edge graph))
         (start (canvas-graph--border-point (canvas-graph-edge-from edge) dot)))
    (list start (canvas-graph--toward dot start 5))))

(defun canvas-graph--grid-route (edge graph grid)
  "EDGE's way round the boxes of GRAPH on GRID, as its corners: from the
top or bottom of its source, whichever comes cheaper, to the rim of
its marker's dot, from the left, above or below; straight when the grid
joins neither."
  (let* ((dot (canvas-graph--marker-dot edge graph))
         (exits (cl-remove-if (lambda (e) (canvas-graph--in-some-box-p (car e) (canvas-graph-grid-boxes grid)))
                              (canvas-graph--box-exits (canvas-graph-edge-from edge))))
         (goals (cl-remove-if (lambda (p) (canvas-graph--in-some-box-p p (canvas-graph-grid-boxes grid)))
                              (canvas-graph--dot-approaches dot)))
         (way (and exits goals (canvas-graph--route-on grid (mapcar #'car exits) goals))))
    (if way
        (canvas-graph--corners-of
         (append (list (cdr (assoc (car way) (mapcar (lambda (e) (cons (canvas-graph--float-point (car e)) (cdr e))) exits))))
                 way (list (canvas-graph--toward dot (car (last way)) 5))))
      (canvas-graph--straight-route edge graph))))

(defun canvas-graph--route-folded (graph)
  "Give every folded edge of GRAPH its way to its marker: round the boxes
in the layered layout, straight on a ring."
  (let* ((folded (canvas-graph--folded-edges graph))
         (grid (and folded (eq canvas-graph-layout 'layered) (canvas-graph--grid graph folded))))
    (dolist (edge folded)
      (setf (canvas-graph-edge-route edge)
            (if grid
                (canvas-graph--grid-route edge graph grid)
              (canvas-graph--straight-route edge graph))))))

(defun canvas-graph--route-tag-point (corners)
  "Where a tag on the way along CORNERS sits: halfway along its longest leg."
  (let ((best nil) (longest -1))
    (cl-loop for (a b) on corners
             while b
             do (let ((d (canvas-graph--distance a b)))
                  (when (> d longest)
                    (setq longest d
                          best (canvas-graph--toward a b (/ d 2.0))))))
    best))

(defun canvas-graph--route-points (corners)
  "The points a way along CORNERS reaches: its corners and the disc of
its tag."
  (let ((tag (canvas-graph--route-tag-point corners))
        (r canvas-graph--tag-radius))
    (append corners (list (cons (- (car tag) r) (- (cdr tag) r)) (cons (+ (car tag) r) (+ (cdr tag) r))))))

;;;; Drawing the arrows

(defun canvas-graph--curve-through (edge point)
  "EDGE's arrow bent to pass through POINT halfway: (X0 Y0 CX CY X1 Y1)."
  (let* ((p0 (canvas-graph--border-point (canvas-graph-edge-from edge) point))
         (p1 (canvas-graph--border-point (canvas-graph-edge-to edge) point)))
    (list (car p0) (cdr p0)
          (- (* 2 (car point)) (/ (+ (car p0) (car p1)) 2.0))
          (- (* 2 (cdr point)) (/ (+ (cdr p0) (cdr p1)) 2.0))
          (car p1) (cdr p1))))

(defun canvas-graph--geometry (edge _graph)
  "EDGE's arrow as the layout shaped it, through its label or bowed:
\(X0 Y0 CX CY X1 Y1)."
  (if-let* ((through (canvas-graph-edge-control edge)))
      (canvas-graph--curve-through edge through)
    (canvas-graph--curve edge (canvas-graph-edge-bow edge))))

(defun canvas-graph--loop-geometry (node)
  "A self loop's cubic off NODE's right side: (X0 Y0 C1X C1Y C2X C2Y X1 Y1).
It leaves above the middle, swings out `canvas-graph--loop-height' and
comes back below."
  (let ((right (+ (canvas-diagram-node-x node) (canvas-diagram-node-w node)))
        (cy (canvas-diagram-middle-y node))
        (reach (* canvas-graph--loop-height (/ 4.0 3.0))))
    (list right (- cy 8) (+ right reach) (- cy 24) (+ right reach) (+ cy 24) right (+ cy 8))))

(defun canvas-graph--marker-geometry (target i)
  "The any-node marker into TARGET, the I-th of its markers from 0:
\(DOT-X DOT-Y TIP-X TIP-Y), a dot left of the box and the arrow's tip
on its side."
  (let ((cy (+ (canvas-diagram-middle-y target) (* 14 i))))
    (list (- (canvas-diagram-node-x target) canvas-graph--marker-gap) cy
          (canvas-diagram-node-x target) cy)))

(defun canvas-graph--arrowhead (ctx x y from-x from-y)
  "Fill an arrowhead with its tip at X Y, coming from FROM-X FROM-Y."
  (let* ((dx (- x from-x)) (dy (- y from-y))
         (len (max 0.001 (sqrt (+ (* dx dx) (* dy dy)))))
         (ux (/ dx len)) (uy (/ dy len))
         (bx (- x (* canvas-graph--arrow-length ux)))
         (by (- y (* canvas-graph--arrow-length uy)))
         (w canvas-graph--arrow-width))
    (canvas-cairo-new-path ctx)
    (canvas-cairo-move-to ctx x y)
    (canvas-cairo-line-to ctx (- bx (* w uy)) (+ by (* w ux)))
    (canvas-cairo-line-to ctx (+ bx (* w uy)) (- by (* w ux)))
    (canvas-cairo-close-path ctx)
    (canvas-cairo-fill ctx)))

(defun canvas-graph--stroke-edge (ctx edge graph)
  "Stroke EDGE of GRAPH on CTX in the current colour and width, with its
arrowhead."
  (if (canvas-graph--loop-p edge)
      (pcase-let ((`(,x0 ,y0 ,c1x ,c1y ,c2x ,c2y ,x1 ,y1)
                   (canvas-graph--loop-geometry (canvas-graph-edge-from edge))))
        (canvas-cairo-new-path ctx)
        (canvas-cairo-move-to ctx x0 y0)
        (canvas-cairo-curve-to ctx c1x c1y c2x c2y x1 y1)
        (canvas-cairo-stroke ctx)
        (canvas-graph--arrowhead ctx x1 y1 c2x c2y))
    (canvas-graph--stroke-curve ctx (canvas-graph--geometry edge graph))))

(defun canvas-graph--stroke-curve (ctx curve)
  "Stroke CURVE, (X0 Y0 CX CY X1 Y1), on CTX in the current colour and
width, with its arrowhead at the end."
  (pcase-let ((`(,x0 ,y0 ,cx ,cy ,x1 ,y1) curve))
    (canvas-cairo-new-path ctx)
    (canvas-cairo-move-to ctx x0 y0)
    ;; A quadratic through the control point, as a cubic.
    (canvas-cairo-curve-to ctx
                           (+ x0 (* (/ 2.0 3) (- cx x0))) (+ y0 (* (/ 2.0 3) (- cy y0)))
                           (+ x1 (* (/ 2.0 3) (- cx x1))) (+ y1 (* (/ 2.0 3) (- cy y1)))
                           x1 y1)
    (canvas-cairo-stroke ctx)
    (canvas-graph--arrowhead ctx x1 y1 cx cy)))

(defconst canvas-graph--route-radius 10
  "Pixels a folded way's turns are rounded by, when they are.")

(defun canvas-graph--turn (ctx a b c)
  "Draw the way from A round the corner at B toward C on CTX: sharp, or
rounded `canvas-graph--route-radius' along each leg, or half a short
one, as `canvas-graph-route-turns' says."
  (if (eq canvas-graph-route-turns 'sharp)
      (canvas-cairo-line-to ctx (car b) (cdr b))
    (let* ((r canvas-graph--route-radius)
           (in (canvas-graph--toward b a (min r (/ (canvas-graph--distance a b) 2.0))))
           (out (canvas-graph--toward b c (min r (/ (canvas-graph--distance b c) 2.0)))))
      (canvas-cairo-line-to ctx (car in) (cdr in))
      (canvas-cairo-curve-to ctx
                             (+ (car in) (* (/ 2.0 3) (- (car b) (car in)))) (+ (cdr in) (* (/ 2.0 3) (- (cdr b) (cdr in))))
                             (+ (car out) (* (/ 2.0 3) (- (car b) (car out)))) (+ (cdr out) (* (/ 2.0 3) (- (cdr b) (cdr out))))
                             (car out) (cdr out)))))

(defun canvas-graph--stroke-route (ctx corners)
  "Stroke the way along CORNERS, (X . Y) each, on CTX in the current
colour and width, its turns rounded, with an arrowhead at the end."
  (let ((end (car (last corners)))
        (before (car (last corners 2))))
    (canvas-cairo-new-path ctx)
    (canvas-cairo-move-to ctx (car (car corners)) (cdr (car corners)))
    (cl-loop for (a b c) on corners
             while c
             do (canvas-graph--turn ctx a b c))
    (canvas-cairo-line-to ctx (car end) (cdr end))
    (canvas-cairo-stroke ctx)
    (canvas-graph--arrowhead ctx (car end) (cdr end) (car before) (cdr before))))

(defun canvas-graph--draw-marker (ctx target i)
  "Draw the I-th any-node marker into TARGET on CTX: a dot and a short arrow."
  (pcase-let ((`(,dx ,dy ,tx ,ty) (canvas-graph--marker-geometry target i)))
    (canvas-cairo-new-path ctx)
    (canvas-cairo-arc ctx dx dy 4 0 (* 2 float-pi))
    (canvas-cairo-fill ctx)
    (canvas-cairo-new-path ctx)
    (canvas-cairo-move-to ctx (+ dx 4) dy)
    (canvas-cairo-line-to ctx tx ty)
    (canvas-cairo-stroke ctx)
    (canvas-graph--arrowhead ctx tx ty dx dy)))

;;;; Placing the labels

(defun canvas-graph--drawn-out (node)
  "The edges out of NODE drawn as arrows, loops aside."
  (cl-remove-if (lambda (e) (or (canvas-graph--loop-p e) (not (canvas-graph--drawn-p e))))
                (canvas-graph-node-out node)))

(defun canvas-graph--drawn-in (node)
  "The edges into NODE drawn as arrows, loops aside."
  (cl-remove-if (lambda (e) (or (canvas-graph--loop-p e) (not (canvas-graph--drawn-p e))))
                (canvas-graph-node-in node)))

(defun canvas-graph--label-parameter (edge)
  "How far along EDGE its label sits, 0 to 1: nearer the target when the
source fans out more than the target fans in, so the labels of the fan
spread with its arrows; nearer the source the other way round; else
halfway, as on an arrow that bows or spans rows."
  (let ((out (canvas-graph--drawn-out (canvas-graph-edge-from edge)))
        (in (canvas-graph--drawn-in (canvas-graph-edge-to edge))))
    (cond ((or (canvas-graph--two-way-p edge) (canvas-graph--far-p edge)) 0.5)
          ((> (length out) (length in)) 0.7)
          ((< (length out) (length in)) 0.3)
          (t 0.5))))

(defun canvas-graph--far-p (edge)
  "Whether EDGE spans more than one row: its ends more than a step apart,
or one of them unreachable."
  (let ((a (canvas-graph-node-depth (canvas-graph-edge-from edge)))
        (b (canvas-graph-node-depth (canvas-graph-edge-to edge))))
    (or (null a) (null b) (> (abs (- a b)) 1))))

(defun canvas-graph--beside (curve point bow size &optional extra)
  "POINT on CURVE moved out to the side the arrow bows, BOW, far enough
for a label of SIZE, (W . H), centred there to clear the arrow, and
EXTRA pixels further."
  (pcase-let* ((`(,x0 ,y0 ,_ ,_ ,x1 ,y1) curve)
               (dx (- x1 x0)) (dy (- y1 y0))
               (len (max 0.001 (sqrt (+ (* dx dx) (* dy dy)))))
               (sign (if (< bow 0) -1 1))
               (nx (/ (* sign dy) len)) (ny (/ (* sign (- dx)) len))
               (reach (+ 4 (or extra 0) (* (abs nx) (/ (car size) 2.0)) (* (abs ny) (/ (cdr size) 2.0)))))
    (cons (+ (car point) (* nx reach)) (+ (cdr point) (* ny reach)))))

(defun canvas-graph--label-candidates (edge graph size)
  "Where EDGE's label of SIZE might go, its middle, best first: on the
arrow at its place, or beside a bowed arrow on the side it bows; then
the other side, then further along the arrow, on it and beside it, and
at last further out to either side, as far as the label is wide."
  (let* ((curve (canvas-graph--geometry edge graph))
         (bow (canvas-graph-edge-bow edge))
         (k (canvas-graph--label-parameter edge))
         (side (if (zerop bow) 1 bow))
         (on (lambda (k) (canvas-graph--point-on curve k)))
         (beside (lambda (k side extra) (canvas-graph--beside curve (funcall on k) side size extra))))
    (append
     (if (zerop bow)
         (list (funcall on k) (funcall on 0.5) (funcall on 0.3) (funcall on 0.7))
       (list (funcall beside k side 0) (funcall beside k (- side) 0) (funcall on k)))
     (cl-loop for extra in (list 0 (/ (car size) 2.0) (car size))
              append (cl-loop for k in (list k 0.3 0.7 0.15 0.85)
                              collect (funcall beside k side extra)
                              collect (funcall beside k (- side) extra)))
     (list (funcall on 0.2) (funcall on 0.8)))))

(defun canvas-graph--apart-p (a b)
  "Whether the rects A and B, (X Y W H), do not overlap."
  (or (<= (+ (nth 0 a) (nth 2 a)) (nth 0 b)) (<= (+ (nth 0 b) (nth 2 b)) (nth 0 a))
      (<= (+ (nth 1 a) (nth 3 a)) (nth 1 b)) (<= (+ (nth 1 b) (nth 3 b)) (nth 1 a))))

(defun canvas-graph--clear-p (rect nodes &optional taken)
  "Whether RECT, (X Y W H), lies on no box of NODES and none of TAKEN, rects."
  (and (cl-notany (lambda (s) (not (canvas-graph--apart-p rect (canvas-graph--box s)))) nodes)
       (cl-every (lambda (other) (canvas-graph--apart-p rect other)) taken)))

(defun canvas-graph--centred (point size)
  "The patch for a label of SIZE whose middle is POINT."
  (canvas-graph--patch (- (car point) (/ (car size) 2.0)) (- (cdr point) (/ (cdr size) 2.0)) size))

(defun canvas-graph--loop-label-rect (edge size)
  "The patch for EDGE's label, a loop's, of SIZE: right of the loop."
  (let ((node (canvas-graph-edge-from edge)))
    (canvas-graph--patch (+ (canvas-diagram-node-x node) (canvas-diagram-node-w node) canvas-graph--loop-height 6)
                         (- (canvas-diagram-middle-y node) (/ (cdr size) 2.0))
                         size)))

(defun canvas-graph--overlap (a b)
  "The area rects A and B, (X Y W H), share."
  (* (max 0 (- (min (+ (nth 0 a) (nth 2 a)) (+ (nth 0 b) (nth 2 b))) (max (nth 0 a) (nth 0 b))))
     (max 0 (- (min (+ (nth 1 a) (nth 3 a)) (+ (nth 1 b) (nth 3 b))) (max (nth 1 a) (nth 1 b))))))

(defun canvas-graph--crowding (rect nodes taken)
  "How much of RECT lies on the boxes of NODES and the rects TAKEN, as area."
  (+ (apply #'+ (mapcar (lambda (s) (canvas-graph--overlap rect (canvas-graph--box s))) nodes))
     (apply #'+ (mapcar (lambda (other) (canvas-graph--overlap rect other)) taken))))

(defun canvas-graph--place-label (edge graph size taken)
  "The patch for EDGE's label of SIZE: the first of the places its arrow
offers that lies on no box and none of TAKEN, the patches placed
already; the least crowded place when none is clear."
  (if (canvas-graph--loop-p edge)
      (canvas-graph--loop-label-rect edge size)
    (let* ((nodes (canvas-graph-nodes graph))
           (rects (mapcar (lambda (p) (canvas-graph--centred p size))
                          (canvas-graph--label-candidates edge graph size))))
      (or (cl-find-if (lambda (r) (canvas-graph--clear-p r nodes taken)) rects)
          (cl-reduce (lambda (best r) (if (< (canvas-graph--crowding r nodes taken)
                                             (canvas-graph--crowding best nodes taken))
                                          r best))
                     rects)))))

(defun canvas-graph--place-labels (graph ctx font)
  "Give every drawn, labelled edge of GRAPH not yet placed by the rows its
label's patch on CTX in FONT, each clear of the boxes and of the labels
placed before it as far as can be; none when the labels are hidden."
  (let ((taken (append (delq nil (mapcar #'canvas-graph-edge-label-rect (canvas-graph-edges graph)))
                       (canvas-graph--marker-rects graph ctx font))))
    (dolist (edge (canvas-graph-edges graph))
      (unless (canvas-graph-edge-label-rect edge)
        (setf (canvas-graph-edge-label-rect edge)
              (when-let* (((and (canvas-graph--all-labels-p graph) (canvas-graph--drawn-p edge)))
                          (text (canvas-graph--label graph edge))
                          (rect (canvas-graph--place-label edge graph (canvas-graph--label-size ctx graph text font) taken)))
                (push rect taken)
                rect))))))

;;;; What the labels say

(defun canvas-graph--labels-text (edge)
  "EDGE's labels as one text, any of which is its; nil for none."
  (when (canvas-graph-edge-labels edge)
    (mapconcat #'identity (canvas-graph-edge-labels edge) " | ")))

(defun canvas-graph--cap (text)
  "TEXT cut at `canvas-graph-label-max' characters with an ellipsis."
  (if (> (length text) canvas-graph-label-max)
      (concat (string-trim-right (substring text 0 (1- canvas-graph-label-max))) "…")
    text))

(defun canvas-graph--label-point (edge graph)
  "Where EDGE's label or tag sits in GRAPH: at its place along the arrow,
or right of its loop."
  (if (canvas-graph--loop-p edge)
      (let ((node (canvas-graph-edge-from edge)))
        (cons (+ (canvas-diagram-node-x node) (canvas-diagram-node-w node) canvas-graph--loop-height 6)
              (canvas-diagram-middle-y node)))
    (canvas-graph--point-on (canvas-graph--geometry edge graph)
                            (canvas-graph--label-parameter edge))))

(defun canvas-graph--label (graph edge)
  "What EDGE's label says on its arrow in GRAPH: its labels, one per line,
shortened in GRAPH's style and each cut when long, the first few and
then how many more; nil for none."
  (when-let* ((labels (canvas-graph-edge-labels edge)))
    (let ((shown (seq-take labels canvas-graph-label-lines))
          (more (- (length labels) canvas-graph-label-lines)))
      (concat (mapconcat (lambda (l) (canvas-graph--cap (canvas-graph--shorten graph l))) shown "\n")
              (if (> more 0) (format "\n… and %d more" more) "")))))

(defun canvas-graph--label-size (ctx graph text font)
  "(W . H) of TEXT, a label of GRAPH marked up in its style, set in FONT
on CTX, wrapped at `canvas-graph-label-width'."
  (canvas-cairo-markup-size ctx (canvas-graph--markup graph text) font canvas-graph-label-width))

(defun canvas-graph--patch (x y size)
  "The patch a label of SIZE, (W . H), sits on with its top-left at X Y:
\(X Y W H) in whole pixels, so no half-covered rim lets a line through."
  (list (floor (- x 4)) (floor (- y 2)) (ceiling (+ (car size) 9)) (ceiling (+ (cdr size) 5))))

(defun canvas-graph--label-rect (_ctx edge _graph _font)
  "The patch EDGE's label sits on, (X Y W H) in drawing pixels, as the
layout placed it, or nil when none is shown."
  (canvas-graph-edge-label-rect edge))

(defun canvas-graph--marker-label (graph group)
  "What the any-node marker of GROUP, (TARGET LABELS EDGE...), says in GRAPH."
  (let ((any (canvas-graph--styled graph :any)))
    (if (cadr group)
        (concat any " · " (canvas-graph--cap (canvas-graph--shorten graph (mapconcat #'identity (cadr group) " | "))))
      any)))

(defun canvas-graph--marker-label-rect (ctx graph group i font)
  "The patch the label of GROUP's marker in GRAPH sits on, left of its
dot, the I-th marker of its target."
  (pcase-let* ((text (canvas-graph--marker-label graph group))
               (size (canvas-graph--label-size ctx graph text font))
               (`(,dx ,dy ,_ ,_) (canvas-graph--marker-geometry (car group) i)))
    (canvas-graph--patch (- dx 8 (car size)) (- dy (/ (cdr size) 2.0)) size)))

(defun canvas-graph--marker-groups (graph)
  "GRAPH's any-node groups each with its place among its target's: (I . GROUP)."
  (let (seen)
    (mapcar (lambda (group)
              (let ((i (cl-count (car group) seen)))
                (push (car group) seen)
                (cons i group)))
            (canvas-graph--common-groups graph))))

(defun canvas-graph--marker-rects (graph ctx font)
  "The patches of the any-node markers' labels of GRAPH, when all are shown."
  (when (canvas-graph--all-labels-p graph)
    (mapcar (lambda (ig) (canvas-graph--marker-label-rect ctx graph (cdr ig) (car ig) font))
            (canvas-graph--marker-groups graph))))

(defun canvas-graph--draw-patch (ctx graph rect text font)
  "Write TEXT, a label of GRAPH marked up in its style, on RECT, an opaque
patch of background, so no line runs through the words."
  (pcase-let ((`(,x ,y ,w ,h) rect))
    (canvas-cairo-rectangle ctx x y w h)
    (canvas-diagram-set-rgb ctx (canvas-diagram-color :background))
    (canvas-cairo-fill ctx)
    (canvas-diagram-set-rgb ctx (canvas-diagram-color :text))
    (canvas-cairo-markup ctx (+ x 4) (+ y 2) (canvas-graph--markup graph text) font canvas-graph-label-width)))

(defun canvas-graph--draw-label (ctx edge graph font)
  "Write EDGE's labels at their place on its arrow on CTX."
  (when-let* ((text (canvas-graph--label graph edge))
              (rect (canvas-graph--label-rect ctx edge graph font)))
    (canvas-graph--draw-patch ctx graph rect text font)))

(defun canvas-graph--draw-common (ctx graph font)
  "Draw the any-node markers of GRAPH on CTX, with their labels when all
labels are shown."
  (pcase-dolist (`(,i . ,group) (canvas-graph--marker-groups graph))
    (canvas-diagram-set-rgb ctx (canvas-diagram-color :edge))
    (canvas-graph--draw-marker ctx (car group) i)
    (when (canvas-graph--all-labels-p graph)
      (canvas-graph--draw-patch ctx graph (canvas-graph--marker-label-rect ctx graph group i font)
                                (canvas-graph--marker-label graph group) font))))

(defun canvas-graph--draw-edges (diagram ctx)
  "Draw every arrow of DIAGRAM's graph on CTX, then their labels, then the
any-node markers."
  (let* ((graph (canvas-diagram-model diagram))
         (font (canvas-diagram-font))
         (drawn (cl-remove-if-not #'canvas-graph--drawn-p (canvas-graph-edges graph))))
    (canvas-diagram-set-rgb ctx (canvas-diagram-color :edge))
    (canvas-cairo-set-line-width ctx 1.5)
    (dolist (edge drawn)
      (canvas-graph--stroke-edge ctx edge graph))
    (when (canvas-graph--all-labels-p graph)
      (dolist (edge drawn)
        (canvas-graph--draw-label ctx edge graph font)))
    (canvas-graph--draw-common ctx graph font)))

;;;; The selected node's ways, tagged and listed

(defun canvas-graph--about (node)
  "The drawn, labelled arrows out of NODE, then those into it."
  (let ((out (canvas-graph-node-out node)))
    (cl-remove-if-not (lambda (e) (and (canvas-graph--drawn-p e) (canvas-graph-edge-labels e)))
                      (append out (cl-remove-if (lambda (e) (memq e out)) (canvas-graph-node-in node))))))

(defun canvas-graph--groups-about (graph node)
  "The any-node groups of GRAPH into NODE or from it, each with its place
among its target's: (I . GROUP)."
  (cl-remove-if-not (lambda (ig)
                      (let ((group (cdr ig)))
                        (or (eq (car group) node)
                            (cl-some (lambda (e) (eq (canvas-graph-edge-from e) node)) (cddr group)))))
                    (canvas-graph--marker-groups graph)))

(defun canvas-graph--group-tag-point (ig node)
  "Where the tag of NODE's way through the any-node group IG, (I . GROUP),
sits: left of the marker's dot for a way into NODE, else halfway along
the longest leg of NODE's own way to that dot."
  (pcase-let ((`(,i . ,group) ig))
    (if (eq (car group) node)
        (pcase-let ((`(,dx ,dy ,_ ,_) (canvas-graph--marker-geometry (car group) i)))
          (cons (- dx 14) dy))
      (canvas-graph--route-tag-point
       (canvas-graph-edge-route (cl-find node (cddr group) :key #'canvas-graph-edge-from))))))

(defun canvas-graph--tags (graph node)
  "The numbered tags for NODE's ways in GRAPH: (N . POINT) each, the
arrows out then in, then the ways through any-node markers, POINT where
the tag sits in drawing coordinates."
  (let ((n 0))
    (append (mapcar (lambda (edge) (cons (cl-incf n) (canvas-graph--label-point edge graph)))
                    (canvas-graph--about node))
            (mapcar (lambda (ig) (cons (cl-incf n) (canvas-graph--group-tag-point ig node)))
                    (canvas-graph--groups-about graph node)))))

(defun canvas-graph--panel-entries (graph node)
  "What the panel lists for NODE: (N ARROW OTHER LABELS) per tag, OTHER
the node at the far end, or GRAPH's word for any node for a folded way
into NODE, LABELS their text or nil."
  (let ((n 0))
    (append (mapcar (lambda (edge)
                      (let ((out (eq (canvas-graph-edge-from edge) node)))
                        (list (cl-incf n) (if out "→" "←")
                              (canvas-diagram-node-label (if out (canvas-graph-edge-to edge) (canvas-graph-edge-from edge)))
                              (canvas-graph--labels-text edge))))
                    (canvas-graph--about node))
            (mapcar (lambda (ig)
                      (let* ((group (cdr ig))
                             (in (eq (car group) node)))
                        (list (cl-incf n) (if in "←" "→")
                              (if in (canvas-graph--styled graph :any) (canvas-diagram-node-label (car group)))
                              (and (cadr group) (mapconcat #'identity (cadr group) " | ")))))
                    (canvas-graph--groups-about graph node)))))

(defun canvas-graph--panel-lines (graph node)
  "What the panel says of NODE, plainly: one line per tag, its number,
the way and the other end, and the labels."
  (mapcar (lambda (entry)
            (pcase-let ((`(,n ,arrow ,other ,labels) entry))
              (format "%d %s %s%s" n arrow other (if labels (concat "  when " labels) ""))))
          (canvas-graph--panel-entries graph node)))

(defun canvas-graph-span (text hex &optional bold)
  "TEXT in pango markup, coloured HEX and BOLD when asked."
  (format "<span %sforeground=\"%s\">%s</span>"
          (if bold "weight=\"bold\" " "") hex (canvas-diagram-markup-escape text)))

(defun canvas-graph--panel-markup (graph node)
  "What the panel says of NODE, highlighted: one markup line per tag, the
labels marked up in GRAPH's style."
  (mapcar (lambda (entry)
            (pcase-let ((`(,n ,arrow ,other ,labels) entry))
              (concat (canvas-graph-span (number-to-string n)
                                         (canvas-diagram-hex (canvas-diagram-color :selection)) t)
                      " " (canvas-graph-span arrow (canvas-diagram-hex (canvas-diagram-color :edge)))
                      " " (canvas-graph-span other (canvas-diagram-face-hex 'font-lock-type-face) t)
                      (if labels
                          (concat "  " (canvas-graph-span "when" (canvas-diagram-face-hex 'font-lock-keyword-face))
                                  " " (canvas-graph--markup graph labels))
                        ""))))
          (canvas-graph--panel-entries graph node)))

(defun canvas-graph--draw-tag (ctx n point font)
  "Draw tag N, a disc with its number, at POINT on CTX in FONT."
  (let ((text (number-to-string n)) (r canvas-graph--tag-radius))
    (canvas-cairo-new-path ctx)
    (canvas-cairo-arc ctx (car point) (cdr point) r 0 (* 2 float-pi))
    (canvas-diagram-set-rgb ctx (canvas-diagram-color :background))
    (canvas-cairo-fill ctx t)
    (canvas-diagram-set-rgb ctx (canvas-diagram-color :selection))
    (canvas-cairo-set-line-width ctx 1.5)
    (canvas-cairo-stroke ctx)
    (canvas-diagram-set-rgb ctx (canvas-diagram-color :text))
    (pcase-let ((`(,w . ,h) (canvas-cairo-text-size ctx text font)))
      (canvas-cairo-text ctx (- (car point) (/ w 2.0)) (- (cdr point) (/ h 2.0)) text font))))

(defun canvas-graph--draw-tags (diagram ctx node font)
  "Number NODE's ways on CTX where their labels would be."
  (pcase-dolist (`(,n . ,point) (canvas-graph--tags (canvas-diagram-model diagram) node))
    (canvas-graph--draw-tag ctx n point font)))

;;;; The panel

(defvar-local canvas-graph--panel-inner-drawn 400
  "Pixels of text the panel was last drawn to hold on a line.")

(defun canvas-graph--panel-inner (diagram size offset zoom)
  "Pixels of text the panel holds on a line, on a view of SIZE showing
DIAGRAM scrolled by OFFSET at ZOOM: the width set, or on auto the room
right of the drawing less a margin, between 200 and three fifths of the
view."
  (if (numberp canvas-graph-panel-width)
      canvas-graph-panel-width
    (let* ((bounds (canvas-diagram--bounds (canvas-diagram-nodes diagram)))
           (right (- (+ canvas-diagram-margin (* zoom (+ (car bounds) (car (canvas-diagram-slack diagram)))))
                     (car offset))))
      (max 200 (min (floor (* 0.6 (car size))) (- (car size) (round right) 30))))))

(defun canvas-graph--panel-rect (ctx font size lines &optional inner)
  "Where the panel for LINES, markup, goes on a view of SIZE: (X Y W H),
in the top-right corner, INNER pixels of text wide, or as wide as the
view allows, and as tall as its lines set in FONT."
  (let* ((pad canvas-diagram-padding)
         (inner (min (or inner canvas-graph--panel-inner-drawn) (- (car size) (* 4 pad) 20)))
         (h (+ (* 2 pad) (* 4 (1- (length lines)))
               (apply #'+ (mapcar (lambda (l) (cdr (canvas-cairo-markup-size ctx l font inner))) lines))))
         (w (+ inner (* 2 pad))))
    (list (- (car size) w 10) 10 w h)))

(defun canvas-graph--set-panel-width (width)
  "Make the panel WIDTH pixels of text wide, 200 at the least, and redraw."
  (setq canvas-graph-panel-width (max 200 width))
  (when canvas-diagram--diagram
    (canvas-diagram-redraw)))

(defun canvas-graph--panel-width-now ()
  "The width the panel has: set, or as last drawn on auto."
  (if (numberp canvas-graph-panel-width) canvas-graph-panel-width canvas-graph--panel-inner-drawn))

(defun canvas-graph-panel-wider ()
  "Widen the panel of labels by forty pixels, from the width it has."
  (interactive)
  (canvas-graph--set-panel-width (+ (canvas-graph--panel-width-now) 40)))

(defun canvas-graph-panel-narrower ()
  "Narrow the panel of labels by forty pixels, from the width it has."
  (interactive)
  (canvas-graph--set-panel-width (- (canvas-graph--panel-width-now) 40)))

(defun canvas-graph-panel-auto ()
  "Let the panel take the room beside the drawing again."
  (interactive)
  (setq canvas-graph-panel-width 'auto)
  (when canvas-diagram--diagram
    (canvas-diagram-redraw)))

(defvar-local canvas-graph--panel-scroll 0
  "Pixels the panel's text is scrolled up by.")

(defvar-local canvas-graph--panel-overflow 0
  "Pixels of the panel's text that lie past its bottom, as last drawn.")

(defvar-local canvas-graph--panel-node nil
  "The node the panel was last drawn for; a new one starts at the top.")

(defun canvas-graph--panel-box (ctx font size lines &optional inner)
  "Where the panel for LINES is drawn on a view of SIZE, INNER pixels of
text wide: (X Y W H), its full box cut to the view's height less a margin."
  (pcase-let ((`(,x ,y ,w ,h) (canvas-graph--panel-rect ctx font size lines inner)))
    (list x y w (min h (- (cdr size) 20)))))

(defun canvas-graph--panel-scrollbar (ctx x y h overflow)
  "Draw a scrollbar along the right edge X of a panel Y down and H tall,
its thumb where the scroll stands out of OVERFLOW."
  (let* ((track (- h 8))
         (thumb (max 12 (* track (/ h (float (+ h overflow))))))
         (top (+ y 4 (* (- track thumb) (/ canvas-graph--panel-scroll (float overflow))))))
    (canvas-diagram-set-rgb ctx (canvas-diagram-color :edge) 0.35)
    (canvas-cairo-rectangle ctx (- x 5) (+ y 4) 3 track)
    (canvas-cairo-fill ctx)
    (canvas-diagram-set-rgb ctx (canvas-diagram-color :edge))
    (canvas-cairo-rectangle ctx (- x 5) top 3 thumb)
    (canvas-cairo-fill ctx)))

(defun canvas-graph--draw-panel (diagram ctx size offset zoom node font)
  "Draw the panel listing NODE's ways, highlighted, in the corner of the
view of SIZE, showing the drawing scrolled by OFFSET at ZOOM: as wide
as the room beside the drawing unless a width is set, cut to the view
and scrolled when it is taller, with a scrollbar then; the scroll
starts at the top for a new node."
  (unless (eq node canvas-graph--panel-node)
    (setq canvas-graph--panel-node node
          canvas-graph--panel-scroll 0))
  (setq canvas-graph--panel-inner-drawn (canvas-graph--panel-inner diagram size offset zoom))
  (when-let* ((lines (canvas-graph--panel-markup (canvas-diagram-model diagram) node)))
    (pcase-let* ((inner canvas-graph--panel-inner-drawn)
                 (`(,x ,y ,w ,full) (canvas-graph--panel-rect ctx font size lines inner))
                 (`(,_ ,_ ,_ ,h) (canvas-graph--panel-box ctx font size lines inner))
                 (pad canvas-diagram-padding)
                 (overflow (- full h)))
      (setq canvas-graph--panel-overflow overflow
            canvas-graph--panel-scroll (max 0 (min canvas-graph--panel-scroll overflow)))
      (canvas-diagram--card ctx x y w h)
      (canvas-cairo-save ctx)
      (canvas-cairo-rectangle ctx x (+ y 2) w (- h 4))
      (canvas-cairo-clip ctx)
      (canvas-diagram-set-rgb ctx (canvas-diagram-color :text))
      (let ((ty (- (+ y pad) canvas-graph--panel-scroll)))
        (dolist (line lines)
          (cl-incf ty (+ 4 (cdr (canvas-cairo-markup ctx (+ x pad) ty line font (- w (* 2 pad))))))))
      (canvas-cairo-restore ctx)
      (when (> overflow 0)
        (canvas-graph--panel-scrollbar ctx (+ x w) y h overflow)))))

(defun canvas-graph--scroll-panel (by)
  "Scroll the panel BY pixels, within its text, and redraw."
  (setq canvas-graph--panel-scroll
        (max 0 (min (+ canvas-graph--panel-scroll by) canvas-graph--panel-overflow)))
  (canvas-diagram-redraw))

(defun canvas-graph-panel-down ()
  "Scroll the panel of labels down."
  (interactive)
  (canvas-graph--scroll-panel 60))

(defun canvas-graph-panel-up ()
  "Scroll the panel of labels up."
  (interactive)
  (canvas-graph--scroll-panel -60))

(defun canvas-graph--overlay (diagram ctx size offset zoom selected)
  "Draw the panel of SELECTED's ways over the view of SIZE, showing the
drawing scrolled by OFFSET at ZOOM, when the labels shown are the
selected node's."
  (when (and selected (eq (canvas-graph--labels (canvas-diagram-model diagram)) 'selected))
    (canvas-graph--draw-panel diagram ctx size offset zoom selected (canvas-diagram-font))))

;;;; The trail, the colours, the header and the card

(defun canvas-graph--folded-out (node)
  "The ways out of NODE folded into any-node markers."
  (cl-remove-if #'canvas-graph--drawn-p (canvas-graph-node-out node)))

(defun canvas-graph--draw-trail (diagram ctx node width)
  "Draw the arrows out of NODE again in the selection colour, WIDTH wide,
and its ways folded into any-node markers round the boxes to their
dots, then the labels once more on top, so the trail runs under the
words."
  (let ((graph (canvas-diagram-model diagram))
        (edges (cl-remove-if-not #'canvas-graph--drawn-p (canvas-graph-node-out node))))
    (canvas-diagram-set-rgb ctx (canvas-diagram-color :selection))
    (canvas-cairo-set-line-width ctx width)
    (dolist (edge edges)
      (canvas-graph--stroke-edge ctx edge graph))
    (dolist (edge (canvas-graph--folded-out node))
      (canvas-graph--stroke-route ctx (canvas-graph-edge-route edge)))
    (pcase (canvas-graph--labels graph)
      ('all (dolist (edge edges)
              (canvas-graph--draw-label ctx edge graph (canvas-diagram-font)))
            (canvas-graph--draw-common ctx graph (canvas-diagram-font)))
      ('selected (canvas-graph--draw-tags diagram ctx node (canvas-diagram-font))))))

(defun canvas-graph--node-rgb (node)
  "(R G B) filling NODE's box: the palette colour of its depth; the node
colour when nothing reaches it."
  (if-let* ((depth (canvas-graph-node-depth node)))
      (canvas-diagram-palette-rgb depth)
    (canvas-diagram-color :node)))

(defun canvas-graph--depth-label (graph depth)
  "How the legend names DEPTH in GRAPH: its start, 1 step, N steps."
  (pcase depth
    (0 (canvas-graph--styled graph :start))
    (1 "1 step")
    (n (format "%d steps" n))))

(defun canvas-graph--legend-entries (graph)
  "The legend's rows for GRAPH: a filled swatch per depth present."
  (let ((depths (sort (cl-remove-duplicates
                       (delq nil (mapcar #'canvas-graph-node-depth (canvas-graph-nodes graph))))
                      #'<)))
    (mapcar (lambda (d) (list (canvas-graph--depth-label graph d) (canvas-diagram-palette-rgb d) 'fill))
            depths)))

(defun canvas-graph-header (graph node &optional place)
  "The header line of GRAPH on NODE: the graph, PLACE after its name when
given, the node, what it is, its edges."
  (format "%s%s › %s%s · %d out, %d in"
          (canvas-graph-name graph)
          (or place "")
          (canvas-diagram-node-label node)
          (if (canvas-diagram-node-kind node) (concat " · " (canvas-diagram-node-kind node)) "")
          (length (canvas-graph-node-out node))
          (length (canvas-graph-node-in node))))

(defun canvas-graph--edge-line (edge arrow other)
  "One line of a card: ARROW, the OTHER node, and EDGE's labels."
  (concat arrow " " (canvas-diagram-node-label other)
          (if-let* ((text (canvas-graph--labels-text edge))) (concat "  when " text) "")))

(defun canvas-graph--in-lines (graph node)
  "The card lines for the edges into NODE: one per arrow drawn, then one
per any-node marker, saying how many nodes share it."
  (append (mapcar (lambda (e) (canvas-graph--edge-line e "←" (canvas-graph-edge-from e)))
                  (canvas-graph--drawn-in node))
          (cl-loop for group in (canvas-graph--common-groups graph)
                   when (eq (car group) node)
                   collect (let ((shared (cl-count-if-not #'canvas-graph--from-any-p (cddr group))))
                             (format "← %s%s%s"
                                     (canvas-graph--styled graph :any)
                                     (if (> shared 0) (format " (%d)" shared) "")
                                     (if (cadr group)
                                         (concat "  when " (mapconcat #'identity (cadr group) " | "))
                                       ""))))))

(defun canvas-graph--path (graph)
  "What GRAPH's cards say under a node's name: the graph, and its type
when it has one."
  (if-let* ((type (canvas-graph-type graph)))
      (format "%s : %s" (canvas-graph-name graph) type)
    (canvas-graph-name graph)))

(defun canvas-graph-card (graph node)
  "NODE's card: its name, its graph, its edges out and then in."
  (let ((lines (append (mapcar (lambda (e) (canvas-graph--edge-line e "→" (canvas-graph-edge-to e)))
                               (canvas-graph-node-out node))
                       (canvas-graph--in-lines graph node))))
    (list (canvas-diagram-node-label node)
          (canvas-graph--path graph)
          (if lines (mapconcat #'identity lines "\n") "No edges."))))

(defun canvas-graph--content (diagram node)
  "What copying NODE copies, (HEADER BODY): the title and the body of its
card in DIAGRAM, the card of the package on the graph when it has one."
  (pcase-let ((`(,title ,_ ,body) (canvas-diagram-card-text diagram node)))
    (list title body)))

;;;; Walking the graph

(defun canvas-graph--targets (node)
  "The nodes NODE leads to, itself left out, those along drawn arrows first."
  (let ((out (canvas-graph-node-out node)))
    (cl-remove node (mapcar #'canvas-graph-edge-to
                            (append (cl-remove-if-not #'canvas-graph--drawn-p out)
                                    (cl-remove-if #'canvas-graph--drawn-p out))))))

(defun canvas-graph--sources (node)
  "The nodes that lead to NODE, itself and edges from any node left out."
  (cl-remove node (delq nil (mapcar #'canvas-graph-edge-from (canvas-graph-node-in node)))))

(defun canvas-graph--at-depth (nodes depth)
  "The nodes of NODES, in reading order, at DEPTH."
  (cl-remove-if-not (lambda (s) (equal (canvas-graph-node-depth s) depth)) nodes))

(defun canvas-graph--siblings (nodes node)
  "The nodes of NODES, in reading order, first reached from the same node
as NODE; NODE alone when nothing reaches it."
  (if-let* ((parent (canvas-graph-node-parent node)))
      (cl-remove-if-not (lambda (s) (eq (canvas-graph-node-parent s) parent)) nodes)
    (list node)))

(defun canvas-graph--next-layer (nodes node)
  "The first node of NODES one step deeper than NODE, or nil."
  (when-let* ((depth (canvas-graph-node-depth node)))
    (car (canvas-graph--at-depth nodes (1+ depth)))))

(defun canvas-graph--move (diagram node direction)
  "The node DIRECTION leads to from NODE in DIAGRAM's graph, or nil.
In follows the first edge out, out the first in; the depth moves stay
among the nodes as far from the start; the siblings are the nodes first
reached from the same node; the branches are the start and the next
layer."
  (let* ((graph (canvas-diagram-model diagram))
         (nodes (canvas-diagram-nodes diagram)))
    (pcase direction
      ('in (car (canvas-graph--targets node)))
      ('out (car (canvas-graph--sources node)))
      ('next (canvas-diagram-neighbour nodes node 1))
      ('previous (canvas-diagram-neighbour nodes node -1))
      ('next-at-depth (canvas-diagram-neighbour
                       (canvas-graph--at-depth nodes (canvas-graph-node-depth node)) node 1))
      ('previous-at-depth (canvas-diagram-neighbour
                           (canvas-graph--at-depth nodes (canvas-graph-node-depth node)) node -1))
      ('next-sibling (canvas-diagram-neighbour (canvas-graph--siblings nodes node) node 1))
      ('previous-sibling (canvas-diagram-neighbour (canvas-graph--siblings nodes node) node -1))
      ('branch (canvas-graph-start graph))
      ('next-branch (canvas-graph--next-layer nodes node))
      ('first (canvas-graph-start graph))
      ('last (car (last nodes))))))

;;;; The diagram: how the graph plugs into canvas-diagram

(defconst canvas-graph-callbacks
  (list :build (lambda (_diagram spec) (canvas-graph-build spec))
        :layout #'canvas-graph--lay-out
        :draw-edges #'canvas-graph--draw-edges
        :draw-trail #'canvas-graph--draw-trail
        :node-rgb (lambda (_diagram node) (canvas-graph--node-rgb node))
        :header (lambda (diagram node) (canvas-graph-header (canvas-diagram-model diagram) node))
        :card (lambda (diagram node) (canvas-graph-card (canvas-diagram-model diagram) node))
        :content #'canvas-graph--content
        :legend (lambda (diagram) (canvas-graph--legend-entries (canvas-diagram-model diagram)))
        :move #'canvas-graph--move
        :node-key (lambda (_diagram node) (canvas-graph-node-id node))
        :restore (lambda (diagram name) (canvas-graph-node-named (canvas-diagram-model diagram) name))
        :export #'canvas-graph--export
        :double-click (lambda (_diagram node)
                        (canvas-diagram-set-selected node)
                        (canvas-diagram-visit-source))
        :overlay #'canvas-graph--overlay
        :menu 'canvas-graph-menu)
  "How the graph plugs into canvas-diagram.  A package on the graph puts
callbacks of its own in front of these with `canvas-graph-diagram'.")

(defun canvas-graph-diagram (&optional callbacks)
  "A fresh diagram of a graph, CALLBACKS, a plist, before the graph's own."
  (canvas-diagram-create :callbacks (append callbacks canvas-graph-callbacks)))

;;;; The mode and its keys

(canvas-diagram-define-setting canvas-graph-cycle-layout canvas-graph-layout
  '(layered ring) "Put the nodes in rows from the start node, or on a ring.")
(defconst canvas-graph--label-cycle '(selected all nil)
  "The ways of labelling the arrows `canvas-graph-toggle-labels' goes round.")

(defun canvas-graph--next-labels (shown own)
  "The labels setting after SHOWN, the way the arrows are labelled now,
round `canvas-graph--label-cycle'; auto when that way is OWN, the
reader's, so that auto stands for it."
  (let* ((at (or (cl-position shown canvas-graph--label-cycle) -1))
         (next (nth (mod (1+ at) (length canvas-graph--label-cycle)) canvas-graph--label-cycle)))
    (if (eq next own) 'auto next)))

(defun canvas-graph-toggle-labels ()
  "Label the arrows the next way after the one shown: the selected node's,
all of them, or none, the reader's own way as auto; and lay out again.
Every toggle shows something new."
  (interactive)
  (let ((graph (and canvas-diagram--diagram (canvas-diagram-model canvas-diagram--diagram))))
    (setq canvas-graph-show-labels
          (canvas-graph--next-labels (if graph (canvas-graph--labels graph) canvas-graph-show-labels)
                                     (and graph (canvas-graph--styled graph :labels))))
    (when canvas-diagram--diagram
      (canvas-diagram-relayout))))
(canvas-diagram-define-setting canvas-graph-toggle-common canvas-graph-fold-common
  '(t nil) "Fold the edges most nodes share into one marker, or draw each.")
(canvas-diagram-define-setting canvas-graph-toggle-turns canvas-graph-route-turns
  '(rounded sharp) "Round the turns of a folded way's route, or keep them sharp.")

(defconst canvas-graph-setting-keys
  '(("L" . canvas-graph-cycle-layout) ("t" . canvas-graph-toggle-labels)
    ("c" . canvas-graph-toggle-common) ("r" . canvas-graph-toggle-turns)
    ("}" . canvas-graph-panel-wider) ("{" . canvas-graph-panel-narrower)
    ("|" . canvas-graph-panel-auto)
    (">" . canvas-graph-panel-down) ("<" . canvas-graph-panel-up))
  "The graph's own menu keys, bound in the diagram buffer too.")

(defun canvas-graph-bind-keys (map keys)
  "Bind KEYS, ((KEY . COMMAND)...), in MAP, and return MAP."
  (pcase-dolist (`(,key . ,command) keys)
    (define-key map (kbd key) command))
  map)

(defun canvas-graph--make-mode-map ()
  "The keys a graph has over any diagram: its own looks.  The diagram's
keys are its parent from the start, not from the first buffer in the
mode."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map canvas-diagram-mode-map)
    (canvas-graph-bind-keys map canvas-graph-setting-keys)))

(defvar canvas-graph-mode-map (canvas-graph--make-mode-map)
  "Keys of a graph diagram over those of every diagram.")

(define-derived-mode canvas-graph-mode canvas-diagram-mode "Graph"
  "Major mode of a buffer showing a graph on a canvas.

The keyboard is on one node, ringed, its edges out drawn in the same
colour.  The keys that move point move it: forward along the first edge
out, back along the first in, next and previous through the nodes in
reading order; the list commands walk the nodes reached from the same
node and the buffer ends go to the start and the last node;
\\[canvas-diagram-jump] jumps to a node by name.
\\[canvas-diagram-toggle-card] opens the node's card, listing its
edges; \\[canvas-diagram-visit-source] goes to its place in the source,
as does a double click.  \\[canvas-diagram-menu] opens the menu of
layouts, shapes and colours, whose keys work here directly as well.

\\{canvas-graph-mode-map}")

(eval-and-compile
  (defconst canvas-graph--menu-group
    '["Graph"
      ("L" canvas-graph-cycle-layout :transient t
       :description (lambda () (canvas-diagram-setting "layout" 'canvas-graph-layout)))
      ("t" canvas-graph-toggle-labels :transient t
       :description (lambda () (canvas-diagram-setting "labels" 'canvas-graph-show-labels "none")))
      ("c" canvas-graph-toggle-common :transient t
       :description (lambda () (canvas-diagram-setting "common" 'canvas-graph-fold-common "each drawn")))
      ("r" canvas-graph-toggle-turns :transient t
       :description (lambda () (canvas-diagram-setting "turns" 'canvas-graph-route-turns)))
      ("}" canvas-graph-panel-wider :transient t
       :description (lambda () (format "%-10s %s, wider" "panel"
                                       (if (numberp canvas-graph-panel-width)
                                           (format "%d px" canvas-graph-panel-width)
                                         (format "auto, %d px" canvas-graph--panel-inner-drawn)))))
      ("{" canvas-graph-panel-narrower :transient t :description "panel narrower")
      ("|" canvas-graph-panel-auto :transient t :description "panel as wide as the room")
      (">" canvas-graph-panel-down :transient t :description "panel down")
      ("<" canvas-graph-panel-up :transient t :description "panel up")]
    "The graph's group of a diagram's menu."))

(defmacro canvas-graph-define-menu (name doc &rest groups)
  "Define NAME, a transient of a graph's looks, with DOC.
GROUPS, a package's own, come first; the graph's group and then the
shared groups for zoom, boxes, colours and icons follow."
  `(canvas-diagram-define-menu ,name ,doc ,@groups ,canvas-graph--menu-group))

(canvas-graph-define-menu canvas-graph-menu
  "Change how the graph is laid out and drawn.
Each key cycles its setting and redraws; the menu stays up, and so do
the diagram's own keys and the mouse, so the effect can be tried at once.")

;;;; Showing and exporting

;;;###autoload
(defun canvas-graph-show (spec &optional name)
  "Show SPEC, a graph as `canvas-graph-build' reads it, in the buffer
NAME, or *canvas-graph*.  Return the buffer."
  (canvas-diagram-show (or name "*canvas-graph*") #'canvas-graph-mode (canvas-graph-diagram) spec))

(defun canvas-graph-follow (buffer read &optional name callbacks)
  "Show the graph READ finds in BUFFER, in the buffer NAME or
*canvas-graph*, and follow BUFFER: once typing there pauses, READ reads
it again and a changed spec is drawn.  READ is a function of a buffer
giving a spec, or nil when it finds none, the last drawing then
staying; CALLBACKS go before the graph's own.  Return the buffer."
  (canvas-diagram-show (or name "*canvas-graph*") #'canvas-graph-mode
                       (canvas-graph-diagram (append callbacks (list :read-source read)))
                       (or (funcall read buffer)
                           (user-error "canvas-graph: no graph to draw in %s" (buffer-name buffer)))
                       buffer))

(defun canvas-graph--exported-labels ()
  "How an export labels the arrows: all of them when any would show, since
no node is selected in a picture; none when none would."
  (and canvas-graph-show-labels 'all))

(defun canvas-graph--export (diagram spec file)
  "Draw DIAGRAM from SPEC into FILE as a picture does: every label on its
arrow when any would show, since no node is selected there."
  (let ((canvas-graph-show-labels (canvas-graph--exported-labels)))
    (canvas-diagram-export diagram spec file)))

;;;###autoload
(defun canvas-graph-export (spec file &optional diagram)
  "Draw SPEC as a graph into FILE, a PNG, an SVG or a PDF by its name, and
return FILE.  DIAGRAM, a package's own, draws it in place of a plain
graph's.  No buffer or frame is needed, so this works in batch; in a
diagram buffer \\[canvas-diagram-write] writes the graph shown."
  (canvas-graph--export (or diagram (canvas-graph-diagram)) spec file))

;;;; Demo

(defconst canvas-graph--demo
  '(:name "issue" :start "open"
    :nodes (("open") ("triaged") ("in progress") ("in review") ("done") ("won't fix") ("archived"))
    :edges (("open" "triaged" "labelled")
            ("triaged" "in progress" "assigned")
            ("triaged" "won't fix" "declined")
            ("in progress" "in progress" "pushed")
            ("in progress" "in review" "pull request")
            ("in review" "in progress" "changes asked")
            ("in review" "done" "approved")
            (nil "open" "reopened")))
  "An issue's way through a tracker, to try the diagram on: a fan, a way
back, a loop, an edge from any node, and a node nothing reaches.")

;;;###autoload
(defun canvas-graph-demo ()
  "Show a small graph: an issue's way through a tracker."
  (interactive)
  (canvas-graph-show canvas-graph--demo))

(canvas-diagram-define-layout 'graph :follow #'canvas-graph-follow :export #'canvas-graph-export)

(provide 'canvas-graph)
;;; canvas-graph.el ends here
