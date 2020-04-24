;;; package --- Summary
;; init.el
;; -*- lexical-binding: t -*-

;;; Commentary:
;;
;;  ____
;; /\  _`\
;; \ \ \L\_\    ___ ___      __      ___    ____
;;  \ \  _\L  /' __` __`\  /'__`\   /'___\ /',__\
;;   \ \ \L\ \/\ \/\ \/\ \/\ \L\.\_/\ \__//\__, `\
;;    \ \____/\ \_\ \_\ \_\ \__/.\_\ \____\/\____/
;;     \/___/  \/_/\/_/\/_/\/__/\/_/\/____/\/___/
;;        ___
;;       /\_ \
;;   _ __\//\ \    ____
;;  /\`'__\\ \ \  /\_ ,`\
;;  \ \ \/  \_\ \_\/_/  /_
;;   \ \_\  /\____\ /\____\
;;    \/_/  \/____/ \/____/
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------------+ ;;
;; |    Initialization     | ;;
;; +-----------------------+ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-error t)

;; This needs fixing
;; Adjust garbage collection
;; (let ((normal-gc-cons-threshold (* 20 1024 1024))
;;       (init-gc-cons-threshold (* 128 1024 1024)))
;;   (setq gc-cons-threshold init-gc-cons-threshold)
;;   (add-hook 'emacs-startup-hook
;; 	    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(add-to-list 'load-path (expand-file-name "local-pkgs" user-emacs-directory))
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

;; end-initialization-section ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------------+ ;;
;; |   General utilities   | ;;
;; +-----------------------+ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ivy setup
(paradox-require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "%d/%d ")
(setq ivy-virtual-abbreviate 'fullpath)
(setq enable-recursive-minibuffers t)
(setq counsel-mode-override-describe-bindings t)
(global-set-key "\C-s" 'swiper-isearch)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f2> f") 'counsel-describe-function)
(global-set-key (kbd "<f2> v") 'counsel-describe-variable)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
(dolist (k '("C-j" "C-RET"))
  (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))
(define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
(define-key ivy-occur-mode-map (kbd "C-c C-q") #'ivy-wgrep-change-to-wgrep-mode)

;; Disable backup files
(setq backup-directory-alist '(("." . "~/.backups")))
(setq delete-old-versions t
  kept-new-versions 3
  kept-old-versions 2
  version-control t)
(setq-default create-lockfiles nil)

(which-key-mode 1)
(setq which-key-separator " ")
(setq which-key-prefix-prefix "+")

;; Delete selected area by yank or overwrite
(delete-selection-mode 1)

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
(wrap-region-add-wrapper "+" "+" nil 'org-mode)

;; Setup org-reveal root
(paradox-require 'ox-reveal)
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

(global-set-key (kbd "C-s-t") 'move-line-up)
(global-set-key (kbd "C-s-n") 'move-line-down)

;; Dvorak keys mapping
(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)
(global-set-key (kbd "C-h") 'backward-kill-word)
(global-set-key [?\C-.] 'execute-extended-command)
(global-set-key [?\C-,] (lookup-key global-map [?\C-x]))
(global-set-key [?\C-'] 'hippie-expand)

(setq browse-url-generic-program
      (executable-find "firefox"))
(setq browse-url-browser-function 'browse-url-firefox)

;; Copy line
(defun copy-line()
  "Copy the whole line on which the cursor is currently on."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank))
(global-set-key (kbd "C-s-d") 'copy-line)

;; Save files needing root privileges
(paradox-require 'sudo-save)

;; Configure org-pomodoro
(setq org-pomodoro-length 30)
(setq org-pomodoro-short-break-length 5)
(setq org-pomodoro-long-break-length 20)
(setq org-pomodoro-clock-break 4)

;; Setup undo tree mode
(paradox-require 'undo-tree)
(global-undo-tree-mode 1)
(global-set-key (kbd "C-z") 'undo)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)

;; Setup very-large-file mode
(paradox-require 'vlf-setup)

;; Bindings for jumping to beginning and end of any buffer
;; helpful when working on very large files
(global-set-key (kbd "C-s-b") 'beginning-of-buffer)
(global-set-key (kbd "C-s-k") 'end-of-buffer)

;; Refresh buffer after file change on disk
(global-auto-revert-mode t)

;; Set company globally
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-p") 'company-select-next)
(global-set-key (kbd "M-n") 'company-select-previous)
(setq company-idle-delay 0)
(setq company-tooltip-limit 20)
(setq company-selection-wrap-around t)

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis.
Else go to the opening parenthesis one level up."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
	(t
	 (backward-char 1)
	 (cond ((looking-at "\\s\)")
		(forward-char 1) (backward-list 1))
	       (t
		(while (not (looking-at "\\s("))
		  (backward-char 1)
		  (cond ((looking-at "\\s\)")
			 (message "->> )")
			 (forward-char 1)
			 (backward-list 1)
			 (backward-char 1)))))))))
(global-set-key (kbd "C-M-g") 'goto-match-paren)

;; Optimize working on a large line with Emacs
;; (bidi-inhibit-bpa t)
(setq bidi-display-reordering 'nil)
(setq bidi-paragraph-direction 'left-to-right)

;; Update packages automatically when it's monday
(defun filter-installed-packages ()
  (dolist (package package-activated-list
		   (when (and (package-installed-p package)
			      (cadr (assq package package-archive-contents)))
		     (let* ((newest-desc (cadr (assq package package-archive-contents)))
			    (installed-desc (cadr (or (assq package package-alist)
						      (assq package package--builtins))))
			    (newest-version  (package-desc-version newest-desc))
			    (installed-version (package-desc-version installed-desc)))
		       (version-list-<= newest-version installed-version))))))

(defun is-monday ()
  (if (string-match "Mon" (current-time-string))
      1))

(defun update-package ()
  (dolist (package (filter-installed-packages))
      (condition-case ex
	  (progn
	    (package-install-from-archive (cadr (assoc package package-archive-contents)))))))

(defun update-packages ()
  (when is-monday
    (update-package)))

;; end-general-utilities-section ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------------+ ;;
;; |   Dev environment     | ;;
;; +-----------------------+ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup magit
(paradox-require 'magit)
(magit-mode)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
;; Shows side diff changes
(paradox-require 'diff-hl)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Enable flycheck syntax checker
(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;; Start Python dev environment
(elpy-enable)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;; Set python interpreter environment
(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")
(setq python-check-command (expand-file-name "flake8"))
(with-eval-after-load 'flycheck
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Set peek frame mode
(paradox-require 'peek-frame-mode)

;; Set Lisp dev environment
(paradox-require 'slime)
;; package.el compiles the contrib subdir, but the compilation order
;; causes problems, so we remove the .elc files there.
(mapc #'delete-file
      (file-expand-wildcards (concat user-emacs-directory "elpa/slime-2*/contrib/*.elc")))
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")
(setq slime-contribs '(slime-fancy))
(setq slime-protocol-version 'ignore)
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-complete-symbol*-fancy t)
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
(add-to-list 'slime-contribs 'slime-repl)
(slime-setup (append '(slime-repl slime-fuzzy)))
(define-key slime-repl-mode-map (kbd "TAB") 'indent-for-tab-command)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(global-set-key (kbd "C-c r e") 'eval-region)

;; Multiple cursors
(paradox-require 'multiple-cursors)
(global-set-key (kbd "C-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(setq-default grep-highlight-matches t
	      grep-scroll-output t)

;; Bracket completion
(electric-pair-mode 1)
(setq electric-pair-pairs
      '((?\` . ?\`)))

;; Bracket highlight
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; Vimlike code folding
(vimish-fold-global-mode 1)

;; C/C++ environment setup
(paradox-require 'c-cpp-dev-env)

;; Compile and Recompile global keys
(global-set-key (kbd "C-x C-m") 'compile)
(global-set-key (kbd "C-x C-v") 'recompile)

;; Set spellcheck
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

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
(defun latex-count-words ()
  "Call external Perl script to count the words in current latex file."
  (interactive)
  (shell-command (concat "/usr/local/bin/texcount.pl"
                         (buffer-file-name))))

;; Add js2 mode
(paradox-require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)

(paradox-require 'diminish)
(eval-after-load "ivy" '(diminish 'ivy-mode))
(eval-after-load "elpy" '(diminish 'elpy-mode))
(eval-after-load "flymake" '(diminish 'flymake-mode))
(eval-after-load "hi-lock" '(diminish 'hi-lock-mode))
(eval-after-load "flyspell" '(diminish 'flyspell-mode))
(eval-after-load "flycheck" '(diminish 'flycheck-mode))
(eval-after-load "simple" '(diminish 'visual-line-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "anaconda-mode" '(diminish 'anaconda-mode))
(eval-after-load "autorevert" '(diminish 'auto-revert-mode))
(eval-after-load "wrap-region" '(diminish 'wrap-region-mode))
(eval-after-load "highlight-thing" '(diminish 'highlight-thing-mode))
(eval-after-load "highlight-indentation" '(diminish 'highlight-indentation-mode))

;; Setup clojure mode
(paradox-require 'cider-mode)
(paradox-require 'clojure-mode)
(paradox-require 'clj-refactor)
(paradox-require 'flycheck-clojure)
;; Clojure refactor setup
(add-hook 'clojure-mode-hook
	  (lambda ()
            (clj-refactor-mode 1)
	    (yas-minor-mode 1) ; for adding require/use/import statements
	    ;; insert keybinding setup here
	    (cljr-add-keybindings-with-prefix "C-c C-m")))
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
;; I don't know what to do with this. It's too slow currently to be practical.
;; (add-hook 'cider-mode-hook
;;           (lambda ()
;;              (add-hook 'after-save-hook 'cider-format-buffer nil 'make-it-local)))
(eval-after-load 'flycheck '(flycheck-clojure-setup))
(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
(add-hook 'cider-mode-hook
  (lambda () (setq next-error-function #'flycheck-next-error-function)))

;; Setup Rust mode
(paradox-require 'racer)
(paradox-require 'cargo)
(paradox-require 'rust-mode)
(paradox-require 'toml-mode)
(paradox-require 'flycheck-rust)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; Format rust code on each file save
(setq rust-format-on-save t)
(setq company-tooltip-align-annotations t)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Create new empty buffers on the fly
(defun new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc."
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))
(setq initial-major-mode 'text-mode)
(global-set-key (kbd "C-c b u") 'new-empty-buffer)

;; Setup XML folding
(paradox-require 'hideshow)
(paradox-require 'sgml-mode)
(paradox-require 'nxml-mode)
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))
(add-hook 'nxml-mode-hook 'hs-minor-mode)
;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

;; end-dev-environment-section ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------------+ ;;
;; |  Window manipulation  | ;;
;; +-----------------------+ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Allows to scroll to end or beginning of buffer without producing an error
(setq scroll-error-top-bottom t)

;; Set window numbering
(setq winum-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-`") 'winum-select-window-by-number)
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
(paradox-require 'winum)
(winum-mode)

;; Change window size
(global-set-key (kbd "C-s-m") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-c") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-.") 'shrink-window)
(global-set-key (kbd "C-s-q") 'enlarge-window)
(global-set-key (kbd "C-s-g") 'balance-windows-area)

(defun slide-buffer (dir)
  "Move current buffer into window at direction DIR.
DIR is handled as by `windmove-other-window-loc'."
  (paradox-require 'windmove)
  (let ((buffer (current-buffer))
        (target (windmove-find-other-window dir)))
    (if (null target)
        (user-error "There is no window %s from here" dir)
      (switch-to-buffer (window-buffer target) nil t)
      (select-window target)
      (switch-to-buffer buffer nil t))))

(defun slide-buffer-up () "Slide buffer up." (interactive) (slide-buffer 'up))
(defun slide-buffer-down () "Slide buffer down." (interactive) (slide-buffer 'down))
(defun slide-buffer-left () "Slide buffer left." (interactive) (slide-buffer 'left))
(defun slide-buffer-right () "Slide buffer right." (interactive) (slide-buffer 'right))

(define-key global-map (kbd "C-S-<up>")    #'slide-buffer-up)
(define-key global-map (kbd "C-S-<down>")  #'slide-buffer-down)
(define-key global-map (kbd "C-S-<left>")  #'slide-buffer-left)
(define-key global-map (kbd "C-S-<right>") #'slide-buffer-right)

(global-set-key (kbd "S-C-h") 'windmove-left)
(global-set-key (kbd "S-C-n") 'windmove-right)
(global-set-key (kbd "S-C-c") 'windmove-up)
(global-set-key (kbd "S-C-t") 'windmove-down)

;; Setup somewhat smooth scrolling + scroll margins
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq scroll-margin 5)

;; end-window-manipulation-section ;;

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
(paradox-require 'highlight-thing)
(add-hook 'prog-mode-hook 'highlight-thing-mode)
(setq highlight-thing-delay-seconds 1.0)
(setq highlight-thing-limit-to-region-in-large-buffers-p nil
      highlight-thing-narrow-region-lines 15
      highlight-thing-large-buffer-limit 5000)

;; Set default font
(add-to-list 'default-frame-alist
             '(font . "Iosevka-12:spacing=m:width=condensed:antialias=1"))

;; Set cursor type
(setq sentence-end-double-space nil)
(setq-default cursor-type '(bar . 2))

;; Fancy battery mode
(add-hook 'after-init-hook #'fancy-battery-mode)

;; Have color brackets on programming modes
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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
(paradox-require 'neotree)
(setq neo-window-width 33)

(global-visual-line-mode t)

;; Setup whitespaces to be depicted with dots
(global-whitespace-mode)
(setq whitespace-style '(spaces face tabs newline tab-mark space-mark newline-mark trailing))
;; God help you if these ascii characters don't work for you and you have to find new chars
(setq whitespace-display-mappings
      '(
	;; this is a mapping for the '|▷' characters
	(tab-mark 9 [124 9655 9] [92 9])
	;; this is a mapping for the '↵' character
	(newline-mark 10 [8629 10])
	;; this is a mapping for the '·' character
	(space-mark 32 [183] [46])))

;; Unique names of buffers for files with identical names
(paradox-require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "  ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Setup doom modeline
(paradox-require 'doom-modeline)
(column-number-mode 1) ;; an addition to have column numbers as well
(doom-modeline-mode 1)

(defun xah-syntax-color-hex ()
  "Xah's take on highlighting hex values with interactive call."
  (interactive)
  "Syntax color text of the form [#ff1100] and [#abc] in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_olors.html'"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[[:xdigit:]]\\{3\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list
		 :background
                 (let* (
                        (ms (match-string-no-properties 0))
                        (r (substring ms 1 2))
                        (g (substring ms 2 3))
                        (b (substring ms 3 4)))
                   (concat "#" r r g g b b))))))
     ("#[[:xdigit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list
		 :background (match-string-no-properties 0)))))))
  (font-lock-flush))

(defun pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols."
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          )))

(add-hook 'lisp-mode-hook 'pretty-lambda)
(add-hook 'scheme-mode-hook 'pretty-lambda)
(add-hook 'emacs-lisp-mode-hook 'pretty-lambda)
(global-prettify-symbols-mode 1)

;; end-emacs-looks-section ;;

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
(setq org-default-priority ?B)

(setq org-priority-faces '((?A . (:foreground "#E06C75" :weight bold))
                           (?B . (:foreground "#61AFEF" :weight bold))
                           (?C . (:foreground "#98C379" :weight bold))))

(setq org-todo-keywords
      '((sequence "TODO(w)" "TODO(o)" "IN-PROGRESS(p)" "TESTING(e)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)" "DIDN'T(n)")
        (sequence "IDEA(i)" "RE-THINK(r)" "LATER(l)" "APPOINTMENT(a)" "|")))

;;Open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;;Capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("w" "todo" entry (file+headline "~/org/todo.org" "Work")
         "** TODO [#A] %?")
	("o" "todo" entry (file+headline "~/org/todo.org" "Personal")
         "** TODO [#A] %?")
        ("a" "appointment" entry (file+headline "~/org/todo.org" "Appointments")
         "* APPOINTMENT [#B] %?")
        ("i" "idea" entry (file+headline "~/org/ideas.org" "Ideas")
         "* IDEA [#C] %?")
        ("s" "stud" entry (file+headline "~/org/todo.org" "Study")
         "** TODO [#A] %?")))

;; Expand org files globally
(setq org-startup-folded nil)

(defun aj/appt-notify (until time msg)
  "Pop a reminder every TIME(3) minutes, 30 minutes UNTIL each appointment.
This is also refreshed when the todo.org file is saved.
Use `alert' to for appointment notifications."
  (if (listp msg)
      (dolist (i (number-sequence 0 (1- (length until))))
        (alert (nth i msg) :title "Reminder" :category 'calendar))
    (alert msg :title "Reminder" :category 'calendar)))

(defun aj/org-agenda-to-appt ()
  "Load agenda entries into `appt'."
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; Advice the agenda refresh to update appts.
(defadvice org-agenda-redo (after update-appts activate)
  "Update `appt' lists from the agenda."
  (message "Updating appointments...")
  (aj/org-agenda-to-appt))

(appt-activate t)
(setq appt-message-warning-time 30
      appt-display-interval 3
      appt-display-mode-line nil
      appt-disp-window-function #'aj/appt-notify
      appt-delete-window-function #'ignore)
(setq alert-default-style (cond ((eq system-type 'gnu/linux) 'libnotify)
				(t 'message)))

(defun anarcroth/after-org-save ()
  "Used in `after-save-hook'."
  (if (eq (current-buffer) 'todo.org)
      (org-agenda-redo)))
(add-hook 'after-save-hook 'anarcroth/after-org-save)

;; Define org-strike-through color
(paradox-require 'cl)   ; for delete*
(setq org-emphasis-alist
      (cons '("+" '(:strike-through t :foreground "#61AFEF"))
            (delete* "+" org-emphasis-alist :key 'car :test 'equal)))

;; Set icons for the priorities in org mode (eye candy)
(paradox-require 'org-fancy-priorities)
(add-hook 'org-mode-hook 'org-fancy-priorities-mode)
(setq org-fancy-priorities-list '((?A . "❗")
                                  (?B . "❗")
                                  (?C . "❗")
                                  (?D . "☕")
                                  (?1 . "⚡")
                                  (?2 . "⚡")
                                  (?3 . "⚡")
                                  (?4 . "☕")
                                  (?I . "Important")))

;; Set org bullet points to look nice (eye candy)
(paradox-require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; Change the font color of the completed sub-tasks in an Org TODO list
(defface org-checkbox-todo-text
  '((t (:inherit org-todo)))
  "Face for the text part of an unchecked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?: \\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-todo-text prepend))
 'append)

(defface org-checkbox-done-text
    '((t (:inherit org-done)))
    "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
 'append)

;;; init.el ends here
