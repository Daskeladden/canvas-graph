;;; canvas-graph-acceptance.el --- the states of a graph in imenu -*- lexical-binding: t -*-

;; This test uses the real command `imenu' in a real window.  `make
;; acceptance' runs it on a private Xvfb display.

(require 'canvas-graph)
(require 'canvas-diagram-acceptance-helpers)

(ert-deftest canvas-graph-acceptance-plain-imenu-goes-to-a-state ()
  ;; GIVEN a graph of a door with the states open, closed and locked
  ;; WHEN the user runs imenu
  ;; THEN the list has each state once
  (delete-other-windows)
  (let ((buffer (canvas-graph-show '(:name "door" :start "open"
                                     :nodes (("open") ("closed") ("locked"))
                                     :edges (("open" "closed" "push")
                                             ("closed" "locked" "turn the key")
                                             ("locked" "closed" "turn back")
                                             ("closed" "open" "pull"))))))
    (unwind-protect
        (progn
          (canvas-diagram-acceptance-select buffer)
          (should (equal (sort (canvas-diagram-acceptance-offered #'imenu) #'string<)
                         '("closed" "locked" "open")))
          ;; WHEN the user chooses locked
          ;; THEN the ring is on locked
          (canvas-diagram-acceptance-choose #'imenu "locked")
          (should (equal (canvas-diagram-acceptance-ring) "locked")))
      (kill-buffer buffer))))

(provide 'canvas-graph-acceptance)
;;; canvas-graph-acceptance.el ends here
