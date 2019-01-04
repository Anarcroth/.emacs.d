;; -*- lexical-binding: t -*-
(setq debug-on-error t)

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;;(require 'init-benchmarking) ;; Measure startup time

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(elpy-enable)
;;(require-package 'wgrep)
;;(require-package 'diminish)
;;(require-package 'scratch)
;;(require-package 'command-log-mode)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(require 'magit)
(require 'company)

(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;;-----------------------+
;; Setup how emacs looks |
;;-----------------------+

;; Load atom one dark theme
(load-theme 'atom-one-dark t)

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

;; Set transperancy in emacs
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(90 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Set line numbers
(global-linum-mode t)

;; Set neotree window width
(require 'neotree)
(setq neo-window-width 33)

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
        map))
(require 'winum)
(winum-mode)

(global-visual-line-mode t)

;; Start nyan-cat mode...
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

;; Change window size
(global-set-key (kbd "C-s-m") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-c") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-.") 'shrink-window)
(global-set-key (kbd "C-s-q") 'enlarge-window)
(global-set-key (kbd "C-s-g") 'balance-windows-area)

;;-----------------------+
;; Setup dev environment |
;;-----------------------+

;; Bracket complete mode - electric pairs
(electric-pair-mode 1)
(setq electric-pair-pairs
      '((?\` . ?\`)))

;; Vimlike code folding
(vimish-fold-global-mode 1)

;; Set company globally
(global-company-mode t)

;; Set coding styles and indents
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   (quote
    ((other . "stroustrup")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(js-indent-level 4)
 '(package-selected-packages
   (quote
    (which-key wrap-region winum vimish-fold telephone-line ox-reveal nyan-mode neotree magit js2-mode flyspell-correct elpy auctex-latexmk atom-one-dark-theme)))
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(smie-indent-basic 2))

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

;;-------------------------+
;; Setup general utilities |
;;-------------------------+

;; Delete trailing white spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Re-map backward-kill-word
;;(global-set-key (kbd "C-q") 'backward-kill-word)

;; Set eshell key
(global-set-key [f1] 'eshell)

;; Call todo list from register
(set-register ?t '(file . "~/org/todo.org"))

;; Associate other types of files with js-mode
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; Add js2 mode
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)

;; save/restore opened files and windows config
(desktop-save-mode 1) ; 0 for off

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

;;------------------+
;; Org agenda setup |
;;------------------+

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
