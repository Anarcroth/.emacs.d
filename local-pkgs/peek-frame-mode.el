;;; Package --- Summary
;; This mode creates small frames that are the definitions of things.
;; If a mode uses xref, then this minor mode will create the small
;; definition frame.

;;; Commentary:
;; This package is courtesy of https://github.com/clemera

;;; Code:
(advice-add 'xref--pop-to-location
            :filter-args (lambda (args)
                           (list (car args) 'frame)))

(setq pop-up-frame-function
      (defun peek-frame-pop-up-frame+ ()
        (let* ((frame (make-frame `((parent-frame . ,(selected-frame))
                                    (minibuffer . nil)
                                    (name . "*Emacs Peek*")
                                    (width . 80)
                                    (height . ,(if (frame-parent)
						   (frame-height)
                                                 (round (* (frame-height) 0.75))))
                                    (visibility . nil)
                                    (internal-border-width . 1)
                                    (left-fringe . 10)
                                    (right-fringe . 10)
                                    (skip-taskbar . t)
                                    (unsplittable . t)
                                    (no-other-frame . t)
                                    (no-special-glyphs . t)
                                    (undecorated . t)
                                    (unsplittable . t)
                                    (vertical-scroll-bars . nil)
                                    (horizontal-scroll-bars . nil)
                                    (desktop-dont-save . t))))
	       (window (frame-root-window frame)))
          (prog1 frame
            (set-window-parameter window 'mode-line-format
                                  'none)
            (set-window-parameter window 'header-line-format
                                  'none)
            (unless (frame-parent)
              ;; center on current frame
              (set-frame-position
               frame
               (- (/ (frame-pixel-width) 2)
                  (/ (frame-pixel-width frame) 2))
               (round (* 0.1 (frame-pixel-height)))))
            (peek-frame-mode 1)))))

(defun peek-frame-quit (&optional frame)
  (interactive (list (selected-frame)))
  (peek-frame-mode -1)
  (while (frame-parameter nil 'parent-frame)
    (let ((frame (selected-frame)))
      (delete-frame frame)))
  (select-frame-set-input-focus (selected-frame)))

(defvar peek-frame-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map
      (kbd "C-g") 'peek-frame-quit)
    (define-key map
      (kbd "q") 'peek-frame-quit)
    (define-key map
      (kbd "M-,") (lambda ()
                    (interactive)
                    (let ((p (frame-parent)))
                      (delete-frame)
                      (select-frame-set-input-focus p))))
    map))

(define-minor-mode peek-frame-mode
  "Minor mode for peek frame buffers."
  :lighter ""
  (cond (peek-frame-mode
         (setq-local frame-auto-hide-function 'peek-frame-quit)
         ;; FIXME: the message blocks immediate display of internal border...
         (let ((view-inhibit-help-message t))
           (read-only-mode 1)))
        (t
         (kill-local-variable #'frame-auto-hide-function)
         (read-only-mode -1))))

(provide 'peek-frame-mode)
;;; peek-frame-mode.el ends here
