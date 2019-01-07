;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------------+ ;;
;; |    Initialization     | ;;
;; +-----------------------+ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-error t)

;; Adjust garbage collection
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(add-to-list 'load-path (expand-file-name "pills" user-emacs-directory))
(require 'start-packages)

;; Define custom variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Start Emacs as a server
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------------+ ;;
;; |  Window manipulation  | ;;
;; +-----------------------+ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set window numbering
(setq winum-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-`") 'winum-select-window-by-number)
        (define-key map (kbd "C-²") 'winum-select-window-by-number)
        (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
        (define-key map (kbd "M-1") 'winum-select-window-1)
        (define-key map (kbd "M-2") 'winum-select-window-2)
        (define-key map (kbd "M-3") 'winum-select-window-3)
        (define-key map (kbd "M-4") 'winum-select-window-4)
        (define-key map (kbd "M-5") 'winum-select-window-5)
        (define-key map (kbd "M-6") 'winum-select-window-6)
        (define-key map (kbd "M-7") 'winum-select-window-7)
        (define-key map (kbd "M-8") 'winum-select-window-8)
	(define-key map (kbd "M-9") 'winum-select-window-9)
        map))
(require 'winum)
(winum-mode)

;; Change window size
(global-set-key (kbd "C-s-m") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-c") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-.") 'shrink-window)
(global-set-key (kbd "C-s-q") 'enlarge-window)
(global-set-key (kbd "C-s-g") 'balance-windows-area)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------------+ ;;
;; | Setup how emacs looks | ;;
;; +-----------------------+ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Load atom one dark theme
(load-theme 'atom-one-dark t)

;; Highlight occurrences under the cursor
(require 'highlight-thing)
(add-hook 'prog-mode-hook 'highlight-thing-mode)
(setq highlight-thing-delay-seconds 1.0)
(setq highlight-thing-limit-to-region-in-large-buffers-p nil
      highlight-thing-narrow-region-lines 15
      highlight-thing-large-buffer-limit 5000)

;; Set default font
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono Nerd Font:antialias=1"))
(set-face-attribute 'default nil
                    :height 121
                    :weight 'normal
                    :width 'normal)

;; Set cursor type
(setq sentence-end-double-space nil)
(setq-default cursor-type '(bar . 2))

;; Set line highlighting
(global-hl-line-mode 1)

;; Set transperancy in emacs (currently not needed)
;;(defun toggle-transparency ()
;;  (interactive)
;;  (let ((alpha (frame-parameter nil 'alpha)))
;;    (set-frame-parameter
;;     nil 'alpha
;;     (if (eql (cond ((numberp alpha) alpha)
;;                    ((numberp (cdr alpha)) (cdr alpha))
;;                    ;; Also handle undocumented (<active> <inactive>) form.
;;                    ((numberp (cadr alpha)) (cadr alpha)))
;;              100)
;;         '(90 . 50) '(100 . 100)))))
;;(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Set line numbers
(global-linum-mode t)

;; Set neotree window width
(require 'neotree)
(setq neo-window-width 33)

(global-visual-line-mode t)

;; Unique names of buffers for files with identical names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "  ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Start nyan-cat mode
(nyan-mode t)
(nyan-toggle-wavy-trail)
(setq nyan-bar-length 18)

;; Set telephone-line
(require 'telephone-line)
(setq telephone-line-primary-left-separator 'telephone-line-abs-left
      telephone-line-primary-right-separator 'telephone-line-abs-right)
(defface atom-red '((t (:foreground "#E06C75" :weight bold :background "#3E4451"))) "")
(defface atom-orange '((t (:foreground "#D19A66" :weight bold :background "#3E4451"))) "")
(defface atom-green '((t (:foreground "#98C379" :weight bold :background "#282C34"))) "")
(defface atom-cyan '((t (:foreground "#56B6C2" :weight bold :background "#282C34"))) "")
(defface atom-blue '((t (:foreground "#61AFEF" :weight bold :background "#3E4451"))) "")
(defface atom-purple '((t (:foreground "#C678DD" :weight bold :background "#3E4451"))) "")
(setq telephone-line-faces
      '((red    . (atom-red . atom-red))
        (orange . (atom-orange . atom-orange))
        (green  . (atom-green . atom-green))
        (cyan   . (atom-cyan . atom-cyan))
        (blue   . (atom-blue . atom-blue))
        (purple . (atom-purple . atom-purple))
        (accent . (telephone-line-accent-inactive . telephone-line-accent-inactive))
        (nil    . (mode-line . mode-line-inactive))))
(setq telephone-line-lhs
      '((red    . (telephone-line-window-number-segment))
        (green  . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
        (blue   . (telephone-line-buffer-segment))
        (nil    . (telephone-line-nyan-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (orange . (telephone-line-atom-encoding-segment))
        (cyan   . (telephone-line-major-mode-segment))
        (purple . (telephone-line-airline-position-segment))))
(telephone-line-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------------+ ;;
;; |   Dev environment     | ;;
;; +-----------------------+ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup magit
(require 'magit)
(magit-mode)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
;; Shows side diff changes
(require 'diff-hl)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Start Python dev environment
(elpy-enable)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(setq-default grep-highlight-matches t
	      grep-scroll-output t)

;; Bracket completion
(electric-pair-mode 1)
(setq electric-pair-pairs
      '((?\` . ?\`)))

;; Vimlike code folding
(vimish-fold-global-mode 1)

;; Set company globally
(global-company-mode t)

;; Set spellcheck
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Set python interpreter environment
(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")

;; LaTeX and AUCtex setup
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Associate other types of files with js-mode
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; Add js2 mode
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------------+ ;;
;; |   General utilities   | ;;
;; +-----------------------+ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ivy setup
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-virtual-abbreviate 'fullpath)
(setq enable-recursive-minibuffers t)
(setq counsel-mode-override-describe-bindings t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f2> f") 'counsel-describe-function)
(global-set-key (kbd "<f2> v") 'counsel-describe-variable)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
(dolist (k '("C-j" "C-RET"))
  (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))
(define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
(define-key ivy-occur-mode-map (kbd "C-c C-q") #'ivy-wgrep-change-to-wgrep-mode)
(add-hook 'after-init-hook 'ivy-historian-mode)

;; Open recently opened files
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-exclude '("/tmp/"))
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Disable backup files
(setq-default make-backup-files nil) ; stop creating backup~ files
(setq-default auto-save-default nil) ; stop creating #autosave# files
(setq-default create-lockfiles nil)

(which-key-mode 1)
(setq which-key-separator " ")
(setq which-key-prefix-prefix "+")

;; Delete trailing white spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set eshell key
(global-set-key [f1] 'eshell)

;; Save/Restore opened files and windows
(desktop-save-mode 1)

;; Setup custom word wrappings
(wrap-region-global-mode t)
(wrap-region-add-wrapper "`" "`" nil 'markdown-mode)
(wrap-region-add-wrapper "~" "~" nil 'markdown-mode)
(wrap-region-add-wrapper "*" "*" nil 'markdown-mode)

;; Custom welcoming screen
(setq initial-scratch-message "
;;███████╗███╗   ███╗ █████╗  ██████╗███████╗    ██████╗ ██╗     ███████╗
;;██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝    ██╔══██╗██║     ╚══███╔╝
;;█████╗  ██╔████╔██║███████║██║     ███████╗    ██████╔╝██║       ███╔╝
;;██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║    ██╔══██╗██║      ███╔╝
;;███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║    ██║  ██║███████╗███████╗
;;╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝    ╚═╝  ╚═╝╚══════╝╚══════╝
")

;; Setup org-reveal root
(require 'ox-reveal)
(setq org-reveal-root "file:///home/anarcroth/reveal.js")

;; Move lines up and down
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "C-s-<up>") 'move-line-up)
(global-set-key (kbd "C-s-<down>") 'move-line-down)

;; Dvorak keys mapping
(global-set-key (kbd "C-z") ctl-x-map)
(global-set-key (kbd "C-x C-h") help-map)
(global-set-key (kbd "C-h") 'backward-kill-word)
(global-set-key (kbd "C-t") 'previous-line)
(global-set-key [?\C-.] 'execute-extended-command)
(global-set-key [?\C-,] (lookup-key global-map [?\C-x]))
(global-set-key [?\C-'] 'hippie-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------------+ ;;
;; |      Org setup        | ;;
;; +-----------------------+ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call todo list from register
(set-register ?t '(file . "~/org/todo.org"))

(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-files (list "~/org"))

(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

(setq org-priority-faces '((?A . (:foreground "#D39276" :weight bold))
                           (?B . (:foreground "#1164AF" :weight bold))
                           (?C . (:foreground "#525E6D" :weight bold))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "TESTING(e)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")
        (sequence "IDEA(i)" "RE-THINK(t)" "LATER(l)" "APPOINTMENT(a)" "|")))

;;Open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;;Capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/org/todo.org" "What kind of a day do I want to have?")
         "* TODO [#A] %?")
        ("a" "appointment" entry (file+headline "~/org/todo.org" "Appointments")
         "* APPOINTMENT [#B] %?")
        ("i" "idea" entry (file+headline "~/org/ideas.org" "Ideas")
         "* IDEA [#C] %?")
        ("u" "uni" entry (file+headline "~/org/todo.org" "Uni")
         "* TODO [#A] %?")))

;; Expand org files globally
(setq org-startup-folded nil)
