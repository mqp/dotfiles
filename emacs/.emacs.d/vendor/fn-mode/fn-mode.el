; convert only instances of "function" that occur after at least one
; space not at the start of a line. this is convenient for
; javascript, where we really only want to fontify inline anonymous
; functions, not named function declarations.

(defvar fn-search-regex "[^\n\s][\s]*function")
(defvar fn-overlay-category 'func)
(defvar fn-replacement (string 402))

(defun fn-enable-overlays (start end)
  "Updates overlays for any function fontification targets between start and end."
  (save-excursion
    ; clear out existing ones in the buffer
    (fn-disable-overlays start end)
    (goto-char start)
    (while (re-search-forward fn-search-regex end t)
      (let* ((fstart (- (match-end 0) 8))
             (fend (match-end 0))
             (o (car (overlays-at fstart))))
        (unless (and o (eq fn-overlay-category (overlay-get o 'category)))
          (let ((overlay (make-overlay fstart fend)))
            (overlay-put overlay 'category fn-overlay-category)
            (overlay-put overlay 'display fn-replacement)))))))

(defun fn-disable-overlays (start end)
  "Removes function fontification overlays between start and end."
  (remove-overlays start end 'category fn-overlay-category))

(define-minor-mode fn-mode
  "Prettifies the word 'function' to be all fancy-like when it occurs in the middle of a line."
  nil " Fn" nil
  (cond (fn-mode
         (fn-enable-overlays (point-min) (point-max))
         (jit-lock-register 'fn-enable-overlays))
        (t
         (jit-lock-unregister 'fn-enable-overlays)
         (fn-disable-overlays (point-min) (point-max)))))

(provide 'fn-mode)
