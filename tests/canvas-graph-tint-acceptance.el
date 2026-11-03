;;; canvas-graph-tint-acceptance.el --- the background of a frame in a theme -*- lexical-binding: t -*-

;; These tests load a real theme, show a graph with `canvas-graph-show' in
;; a real window and write its picture with the real command
;; `canvas-diagram-write', then read the colours back from the PNG.  `make
;; tint-acceptance' runs them on a private Xvfb display.

(require 'ert)
(require 'cl-lib)
(require 'color)
(require 'canvas-graph)

;;;; Helpers

(defconst canvas-graph-tint-acceptance--name "*graph-tint-acceptance*"
  "The name of the buffer that shows the graph of a test.")

(defun canvas-graph-tint-acceptance--kitchen (fill)
  "A coffee order in a kitchen whose frame, round the counter, asks for
the background FILL."
  `(:name "kitchen" :start "order"
    :nodes (("order") ("grind") ("brew"))
    :edges (("order" "grind" "paid") ("grind" "brew" "ground"))
    :groups (("counter" :title "Counter" :stroke "#36c" :fill ,fill :nodes ("order" "grind")))))

(defmacro canvas-graph-tint-acceptance--with-theme (theme spec &rest body)
  "Load THEME, show SPEC in a window and run BODY in its buffer."
  (declare (indent 2))
  `(progn
     (mapc #'disable-theme custom-enabled-themes)
     (load-theme ,theme t)
     (delete-other-windows)
     (let ((buffer (canvas-graph-show ,spec canvas-graph-tint-acceptance--name)))
       (unwind-protect
           (progn
             (select-window (or (get-buffer-window buffer) (error "The graph has no window")))
             (redisplay t)
             ,@body)
         (kill-buffer buffer)
         (mapc #'disable-theme custom-enabled-themes)))))

(defun canvas-graph-tint-acceptance--png-size (file)
  "The width and height of the PNG FILE, (W . H), from its header."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 24)
    (cl-flet ((number (from) (cl-loop for i from from below (+ from 4)
                                      for n = (char-after (1+ i)) then (+ (* n 256) (char-after (1+ i)))
                                      finally return n)))
      (cons (number 16) (number 20)))))

(defun canvas-graph-tint-acceptance--picture ()
  "Write the picture of the graph shown with `canvas-diagram-write' and
return a context that holds it."
  (let* ((file (make-temp-file "graph-tint" nil ".png"))
         (_ (canvas-diagram-write file))
         (size (canvas-graph-tint-acceptance--png-size file))
         (ctx (canvas-cairo-context (list 'image :type 'canvas :id (make-symbol "tint")
                                          :data-width (car size) :data-height (cdr size)))))
    (canvas-cairo-image ctx file 0 0 (car size) (cdr size))
    (delete-file file)
    ctx))

(defun canvas-graph-tint-acceptance--inside-the-frame (ctx)
  "The colour, (R G B) from 0 to 1, of the picture on CTX inside the
frame, between its left side and the box of the order."
  (let* ((graph (canvas-diagram-model canvas-diagram--diagram))
         (order (canvas-graph-node-named graph "order"))
         (argb (canvas-cairo-pixel ctx
                                   (+ canvas-diagram-margin (floor (- (canvas-diagram-node-x order) 3)))
                                   (+ canvas-diagram-margin (round (canvas-diagram-middle-y order))))))
    (mapcar (lambda (shift) (/ (logand (ash argb (- shift)) #xFF) 255.0)) '(16 8 0))))

(defun canvas-graph-tint-acceptance--lightness (rgb)
  "The lightness of RGB, from 0 to 1."
  (nth 2 (apply #'color-rgb-to-hsl rgb)))

(defun canvas-graph-tint-acceptance--hue (rgb)
  "The hue of RGB, in degrees."
  (* 360 (nth 0 (apply #'color-rgb-to-hsl rgb))))

(defun canvas-graph-tint-acceptance--luminance (rgb)
  "The relative luminance of RGB, as WCAG 2 defines it."
  (cl-destructuring-bind (r g b)
      (mapcar (lambda (c) (if (<= c 0.03928) (/ c 12.92) (expt (/ (+ c 0.055) 1.055) 2.4))) rgb)
    (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))

(defun canvas-graph-tint-acceptance--contrast (a b)
  "The contrast ratio of the colours A and B, as WCAG 2 defines it."
  (let ((la (canvas-graph-tint-acceptance--luminance a))
        (lb (canvas-graph-tint-acceptance--luminance b)))
    (/ (+ (max la lb) 0.05) (+ (min la lb) 0.05))))

(defun canvas-graph-tint-acceptance--theme-colour (key)
  "The colour KEY, :background or :text, of the diagram, (R G B) from 0 to 1."
  (canvas-diagram-color key))

(defun canvas-graph-tint-acceptance--expect-a-faint-tint (fill-hue)
  "The checks of a frame that shows as a faint tint of FILL-HUE, in
degrees, with a title that reads well on it."
  (let* ((inside (canvas-graph-tint-acceptance--inside-the-frame (canvas-graph-tint-acceptance--picture)))
         (background (canvas-graph-tint-acceptance--theme-colour :background))
         (text (canvas-graph-tint-acceptance--theme-colour :text)))
    (should (< (abs (- (canvas-graph-tint-acceptance--lightness inside)
                       (canvas-graph-tint-acceptance--lightness background)))
               0.12))
    (should (< (abs (- (canvas-graph-tint-acceptance--hue inside) fill-hue)) 20))
    (should (>= (canvas-graph-tint-acceptance--contrast inside text) 4.5))))

;;;; The scenarios

(ert-deftest canvas-graph-tint-acceptance-pale-fill-in-a-dark-theme ()
  ;; GIVEN the dark theme modus-vivendi and a frame round the counter that
  ;;       asks for the pale blue background #e8f0fc
  ;; WHEN the user writes the picture of the graph
  ;; THEN the inside of the frame is dark, close to the background of the
  ;;      theme in lightness, and blue
  ;; AND the title of the frame reads on it with a contrast of at least 4.5
  (canvas-graph-tint-acceptance--with-theme 'modus-vivendi (canvas-graph-tint-acceptance--kitchen "#e8f0fc")
    (canvas-graph-tint-acceptance--expect-a-faint-tint 218)))

(ert-deftest canvas-graph-tint-acceptance-dark-fill-in-a-light-theme ()
  ;; GIVEN the light theme modus-operandi and a frame round the counter that
  ;;       asks for the dark blue background #003366
  ;; WHEN the user writes the picture of the graph
  ;; THEN the inside of the frame is light, close to the background of the
  ;;      theme in lightness, and blue
  ;; AND the title of the frame reads on it with a contrast of at least 4.5
  (canvas-graph-tint-acceptance--with-theme 'modus-operandi (canvas-graph-tint-acceptance--kitchen "#003366")
    (canvas-graph-tint-acceptance--expect-a-faint-tint 210)))

;;;; Running them

(defun canvas-graph-tint-acceptance-run ()
  "Run these tests, show the result on the standard error stream and
leave Emacs."
  (let ((stats (ert-run-tests-batch "^canvas-graph-tint-acceptance-")))
    (princ (with-current-buffer "*Messages*" (buffer-string)) #'external-debugging-output)
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(provide 'canvas-graph-tint-acceptance)
;;; canvas-graph-tint-acceptance.el ends here
