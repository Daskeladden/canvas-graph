# canvas-graph

canvas-graph draws a directed graph on an Emacs 32 canvas. The nodes are
boxes, and the edges are arrows with labels. The layout puts the nodes in
rows by their distance from a start node, as graphviz does. canvas-graph
is built on [canvas-diagram](https://github.com/Daskeladden/canvas-diagram),
which supplies the canvas, the boxes, the keys and the mouse.

A reader (a function that turns a source into a graph spec) supplies the
graph, and canvas-graph draws it. Three readers exist: canvas-vhdl-fsm
reads the state machines of a VHDL file, canvas-mermaid reads mermaid
flowcharts and state diagrams, and canvas-plantuml reads PlantUML state
and activity diagrams.

This is a prototype.

## Requirements

- Emacs 32.0.50, built from source. Emacs 32 is not released, so you
  must build it from the master branch, with canvas images and modules.
- [canvas-diagram](https://github.com/Daskeladden/canvas-diagram) and its
  `canvas-cairo` module. You compile the module from C against cairo and
  pango. The canvas-diagram README tells you how.
- transient, for the menus. transient comes with Emacs.
- Graphviz, only for the layout `dot`. On Ubuntu, run
  `sudo apt install graphviz`. Without it, the layout `dot` draws as
  `layered`.

## Install

No package archive has canvas-graph, because the drawing needs a module
that you compile on your machine.

1. Clone the two repositories side by side:

   ```sh
   git clone https://github.com/Daskeladden/canvas-diagram.git
   git clone https://github.com/Daskeladden/canvas-graph.git
   ```

2. Build the module:

   ```sh
   make -C canvas-diagram
   ```

3. Put both directories on the load path:

   ```elisp
   (use-package canvas-graph
     :load-path ("/path/to/canvas-diagram" "/path/to/canvas-graph")
     :commands (canvas-graph-show canvas-graph-export canvas-graph-demo))
   ```

4. Run `M-x canvas-graph-demo`. If a graph appears, the module and the
   canvas work.

## Use

`canvas-graph-show` draws a spec in a diagram buffer. A spec is a
property list (a list of keys and their values):

```elisp
(canvas-graph-show
 '(:name "door" :start "closed"
   :nodes (("closed") ("open") ("locked"))
   :edges (("closed" "open" "push")
           ("open" "closed" "pull")
           ("closed" "locked" "turn the key")
           ("locked" "closed" "turn the key back"))))
```

A spec has these keys:

- `:name`, the name of the graph. The header line and the cards show it.
- `:type`, optional. A card shows it after the name, as `NAME : TYPE`.
- `:start`, optional. The depth of a node is its number of steps from
  this node. Without `:start`, the first node is the start.
- `:nodes`, a list of `(NAME [:label LABEL] [:pos POS])`. The edges and
  `:start` refer to a node by its name. The box shows the label, or the
  name if the node has no label, so two nodes can show the same label.
  `POS` is a position in the source buffer, for the keys that go to the
  source.
- `:edges`, a list of `(FROM TO [LABEL [POS]])`, by node names. If `FROM`
  is nil, the edge comes from any node. Two edges between the same two
  nodes become one arrow with both labels.
- `:groups`, optional, a list of `(ID [:title TITLE] [:stroke COLOR]
  [:fill COLOR] :nodes (NAME...))`. Each group gets a frame round its
  nodes. The frame shows `TITLE`. A group with no title shows its `ID`.
  The outline of the frame has the color `:stroke`. With `:fill`, the
  frame gets a faint tint of that color as its background: the hue of
  the color, with a lightness close to the background of the theme. So a
  frame reads in a light and in a dark theme alike. A color is a name or
  `#RRGGBB`.

An unknown key, an unknown node option, a spec without nodes, or an edge
to an unknown node gives an error. A node in two groups, an unknown group
option or a name that is no color also gives an error. canvas-graph does
not draw a wrong graph.

`canvas-graph-export` writes a spec to a PNG, SVG or PDF file. The name
of the file selects the format. The export needs no frame, so it works in
batch:

```sh
emacs -Q --batch -L /path/to/canvas-diagram -L /path/to/canvas-graph \
  -l canvas-graph \
  --eval '(canvas-graph-export (quote (:name "g" :nodes (("A") ("B")) :edges (("A" "B" "go")))) "g.png")'
```

In a diagram buffer, `C-x C-w`, or `W` in the menu, writes the graph that
the buffer shows.

## What it draws

The color of a node comes from its depth, and the legend lists one color
per depth. The start node has the kind `start`. A node that the start
does not reach has the kind `unreachable`. A kind outlines its box.

`canvas-graph-layout` has four values:

- `layered`, the default. The nodes go in rows by depth, top down. Each
  row is in order under the nodes that lead to it, so fewer arrows
  cross. Each label sits beside its arrow, and the rows stand far enough
  apart that 22 pixels of the arrow show above and below a label.
- `compact`. The same rows, closer together. Each label sits on its
  arrow, and the arrow bends through the middle of the label.
- `dot`. The program `dot` of graphviz places the boxes, the labels,
  the curves of the arrows and the frames of the groups, and
  canvas-graph draws them in its own look. A long arrow gets a lane of
  its own, each label gets room beside its arrow, and a group stays
  together in one frame that no other box enters. canvas-graph runs
  `dot` as a separate program and does not link the graphviz library,
  which is under the Eclipse Public License 1.0. If `dot` is missing,
  fails or takes longer than 3 seconds, the graph draws as `layered`
  and the echo area says why.
- `ring`. The nodes go on a circle in the order of the spec.

A group of the spec gets a frame in its color, with its title at the
top. The legend lists each group with its color. In a row, the boxes of
a group stand side by side. Other rows can come between the nodes of a
group. Then the group is in parts, and each part gets a frame of its
own. A frame never holds a box of another group. If one frame reaches
such a box, the part gets two frames. The ring has no rows, so each box
of a group gets a frame of its own there. The boxes in a frame keep the
color of their depth.

An arrow leaves and enters its boxes at the border. If another arrow
comes back the other way, the arrow bows to its right. If a straight
arrow runs through a box, the arrow bows just enough to go round it. An
edge from a node to itself loops out of the right side of its box.

In the `layered` layout, two arrows between the same boxes in rows next
to each other run straight, 20 pixels apart, each with its label on the
outer side. The label of an arrow between two boxes of one row sits
above the arrow. An arrow that spans more rows bows round the labels of
the shorter arrows where it can. In a crowded graph a long arrow can
still pass under a label, when no bow takes it round all of them. The
sizes in pixels are those of the normal spacing, and they grow and
shrink with the spacing of the menu.

`canvas-graph-show-labels` tells where the labels go:

- `auto`, the default. The reader of the graph decides. A reader with
  long labels, such as canvas-vhdl-fsm, uses `selected`. Mermaid,
  PlantUML and a plain spec use `all`.
- `selected`. The arrows in and out of the selected node get numbered
  tags. A panel in the corner of the view lists each number with its
  labels. The tags and the panel follow the keyboard.
- `all`. Every arrow carries its labels. The layout makes room for them
  between the rows and between the boxes.
- `nil`. No arrow carries a label.

`t` changes to the next way after the one that you see, so each press
changes the drawing. `auto` stands for the way of the reader. An export
always shows all labels.

Sometimes most nodes share an edge to one target under one label. The
graph then folds these edges into one any-node marker (a dot and a short
arrow at the target). An edge from any node always goes into a marker. When you
select a node, its trail shows its way to the marker. That way goes round
the boxes and avoids the arrows where it can. `canvas-graph-fold-common`
turns the folding off.

## Keys

The keyboard is on one node, which has a ring round it. Your own
movement keys move it:

- `forward-char` follows the first edge out, and `backward-char`
  follows the first edge in.
- `next-line` and `previous-line` go through the nodes in reading order.
- `M-n` and `M-p` go through the nodes at the same depth.
- The list commands go through the nodes that one node reaches first.
- The defun commands go to the start node and to the next row.
- The buffer ends go to the start node and to the last node.
- `goto-line` goes to a node by name.

`RET` opens the card of the node, which lists its edges out and in.
`C-RET` goes to the place of the node in the source, and a double click
does the same. `M-w` copies what the card shows: the node, a blank line
and its body. `C-u M-w` copies the whole graph as a picture. `SPC` opens
the menu, and `?` lists every key.

The keys of the menu also work directly in the buffer:

- `L` cycles the layout through layered, compact, dot and ring.
- `t` cycles the labels through selected, all and none.
- `c` turns the folding of common edges on or off.
- `r` makes the turns of a folded way round or sharp.
- `}` and `{` make the panel wider or narrower. `|` gives it the room
  beside the drawing again.
- `>` and `<` scroll the panel.

The shared groups of canvas-diagram follow: zoom, boxes, font, palette,
kinds, legend, icons and paper.

## Configuration

Use `M-x customize-group RET canvas-graph`, or set the variables:

| Variable | Default | Effect |
|---|---|---|
| `canvas-graph-layout` | `layered` | rows with labels beside the arrows, `compact` rows with labels on them, `dot` by graphviz, or a `ring` |
| `canvas-graph-dot-program` | `"dot"` | the program of graphviz that the layout `dot` runs |
| `canvas-graph-show-labels` | `auto` | `auto`, `selected`, `all` or `nil` |
| `canvas-graph-gap-x` | 28 | pixels between the nodes in a row |
| `canvas-graph-layered-gap-y` | 64 | pixels at least between the rows of the layered layout |
| `canvas-graph-gap-y` | 48 | pixels between the rows of the compact layout |
| `canvas-graph-bow` | 0.18 | how far a two-way arrow bows, as a fraction of its length |
| `canvas-graph-label-width` | 260 | pixels before a label wraps |
| `canvas-graph-label-max` | 90 | characters before an ellipsis cuts a label |
| `canvas-graph-label-lines` | 3 | labels an arrow lists before it tells how many more |
| `canvas-graph-fold-common` | `t` | fold the edges that most nodes share |
| `canvas-graph-common-min` | 3 | nodes that must share an edge before it folds |
| `canvas-graph-group-colors` | 7 colors | frame colors of the groups that give no `:stroke`, in turn |
| `canvas-graph-panel-width` | `auto` | pixels of text in the panel, or `auto` |
| `canvas-graph-route-turns` | `rounded` | `rounded` or `sharp` turns on a folded way |

The font, the colors, the shape of the boxes, the kinds and the icons are
the configuration of canvas-diagram.

## Building on it

A package on canvas-graph supplies a reader and a style. It can also
supply its own callbacks, keys and menu.

A style tells canvas-graph how the domain of the reader speaks. It is a
property list with five keys:

- `:start`, the kind of the start node. The default is `"start"`.
- `:any`, the name for the origin of an edge from any node. The default
  is `"any node"`.
- `:markup`, a function that turns a label into pango markup. The
  default escapes the text.
- `:shorten`, a function that shortens a label on an arrow. The default
  keeps the label as it is. A card always shows the whole label.
- `:labels`, where the labels go when `canvas-graph-show-labels` is
  `auto`: `selected`, `all` or `nil`. The default is `all`.

`canvas-graph-build` takes the style as its second argument. An unknown
key in the style gives an error.

```elisp
(defconst my-style
  (list :start "reset" :any "any state"
        :markup #'my-highlight :shorten #'my-shorten))

(defun my-build (_diagram spec)
  (canvas-graph-build spec my-style))
```

`canvas-graph-diagram` makes a diagram. The callbacks that you give it
come before the callbacks of the graph, so yours replace them:

```elisp
(defun my-diagram ()
  (canvas-graph-diagram (list :build #'my-build
                              :read-source #'my-read-source
                              :menu 'my-menu)))
```

A reader that follows a source buffer gives `canvas-graph-follow` a
function of the buffer that returns a spec. canvas-graph shows the spec,
and it calls the function again when you stop typing in the buffer. If
the function returns nil, the last drawing stays.

A reader can also write its graph into a draft (a spec that it builds one
node and one edge at a time). These functions write a draft and turn it
into a spec:

- `canvas-graph-draft-create` makes an empty draft.
- `canvas-graph-draft-node` notes a node, with an optional label and
  place. The last label that the reader gives wins.
- `canvas-graph-draft-edge` notes an edge and the nodes at its ends.
- `canvas-graph-draft-start-at` makes a node the start, unless the draft
  already has a start.
- `canvas-graph-draft-transition` notes a transition of a state machine.
  Nil stands for the `[*]` pseudo-state of mermaid and PlantUML.
- `canvas-graph-draft-spec` returns the spec.

canvas-mermaid and canvas-plantuml read their diagrams this way.

`canvas-graph-define-menu` defines a menu. Your groups come first, then
the group of the graph, then the shared groups. `canvas-graph-bind-keys`
binds a list of keys in a keymap. Derive your mode from
`canvas-graph-mode`, and make `canvas-graph-mode-map` the parent of your
keymap.

canvas-vhdl-fsm does all of this. Its style calls the start node `reset`,
highlights the conditions as VHDL, and cuts dotted names to their last
part. Its callbacks put the place of the machine in the header line, and
they read the VHDL buffer again after a change.

## Tests

```sh
make test
```

This command builds the module in the canvas-diagram checkout beside this
one, and then runs the ERT suite in batch. If canvas-diagram is in a
different directory, run `make test DIAGRAM=/path/to/canvas-diagram`.

The suite tests how canvas-graph refuses a bad spec, the styles and both
layouts. It also tests the labels, the panel, the router, the pixels of
the drawing, the keys, the export and the package headers.

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).
