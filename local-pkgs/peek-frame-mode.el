;;; Package --- Summary
;; This mode creates small frames that are the definitions of things.
;; If a mode uses xref, then this minor mode will create the small
;; definition frame.

;;; Commentary:
;; This package is largely written by https://github.com/clemera

;; TODO:
;; 1. Peek frame for documentation
;; 2. Peek frame for assignments/references
;; 3. Peek frame option to either stack the frames or to replace them.
;; 4. C-mouse1 to go to definition option
;; 5. Option to create peek frame or to directly go to definition/reference/assignment

;;; Code:

(defgroup peek-frame-mode nil
  "Peek definitions in separate Emacs frame."
  :group 'tools
  :group 'convenience)

(defcustom pf/frame-width 80
  "Width of the peeked framed.
Defaults to 80."
  :group 'peek-frame-mode
  :type 'integer)

(defcustom pf/frame-heigth 20
  "Height of the peeked frame.
Defaults to 20."
  :group 'peek-frame-mode
  :type 'integer)

(defcustom pf/frame-name "*Emacs Peek*"
  "Name of the peeked frame.
Defaults to *Emacs Peek*."
  :group 'peek-frame-mode
  :type 'string)

(defcustom pf/vertical-scroll nil
  "Enable vertical scrolling in the peeked frame.
Defaults to nil."
  :group 'peek-frame-mode
  :type 'boolean)

(defcustom pf/horizontal-scroll nil
  "Enable horizontal scrolling in the peeked frame.
Defaults to nil."
  :group 'peek-frame-mode
  :type 'boolean)

(advice-add 'xref--pop-to-location
            :filter-args (lambda (args)
                           (list (car args) 'frame)))

(setq pop-up-frame-function
      (defun peek-frame-pop-up-frame+ ()
        (let* ((frame (make-frame `((parent-frame . ,(selected-frame))
                                    (minibuffer . nil)
                                    (name . pf/frame-name)
                                    (width . pf/frame-width)
                                    (height . pf/frame-height)
                                    (visibility . nil)
                                    (internal-border-width . 5)
                                    (left-fringe . 10)
                                    (right-fringe . 10)
                                    (skip-taskbar . t)
                                    (unsplittable . t)
                                    (no-other-frame . t)
                                    (no-special-glyphs . t)
                                    (undecorated . t)
                                    (unsplittable . t)
                                    (vertical-scroll-bars . pf/vertical-scroll)
                                    (horizontal-scroll-bars . pf/horizontal-scroll)
                                    (desktop-dont-save . t))))
	       (window (frame-root-window frame)))
          (prog1 frame
            (set-window-parameter window 'header-line-format 'none)
            (unless (frame-parent)
	      (let (x y
		      (abs-pixel-pos (save-excursion
				       (beginning-of-thing 'symbol)
				       (window-absolute-pixel-position))))
		(setq x (car abs-pixel-pos))
		(setq y (cdr abs-pixel-pos))
		(set-frame-position
		 frame
		 x
		 y)))
            (peek-frame-mode 1)))))

(defun peek-frame-quit (&optional frame)
  "Remove the peeked FRAME."
  (interactive (list (selected-frame)))
  (peek-frame-mode -1)
  (while (frame-parameter nil 'parent-frame)
    (let ((frame (selected-frame)))
      (delete-frame frame)))
  (select-frame-set-input-focus (selected-frame)))

(defvar peek-frame-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map
      (kbd "q") 'peek-frame-quit)
    (define-key map
      (kbd "C-g") 'peek-frame-quit)
    (define-key map
      (kbd "M-,") (lambda ()
                    (interactive)
                    (let ((p (frame-parent)))
                      (delete-frame)
                      (select-frame-set-input-focus p))))
    map))

;;;###autoload
(define-minor-mode peek-frame-mode
  "Minor mode for peek frame buffers."
  :group 'peek-frame-mode
  :global t
  :lighter ""
  (cond
   (peek-frame-mode
    (setq-local frame-auto-hide-function 'peek-frame-quit)
    ;; FIXME: the message blocks immediate display of internal border...
    (let ((view-inhibit-help-message t))
      (read-only-mode 1)))
   (t
    (kill-local-variable #'frame-auto-hide-function)
    (read-only-mode -1))))

(provide 'peek-frame-mode)
;;; peek-frame-mode.el ends here
