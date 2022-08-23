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

;; Rebind beginning/end -of-buffer functions since 'M-<' or 'M->' is not convenient
;; due to keyboard language mapping and peeking/finding method definitions
(global-set-key (kbd "C-x M-z") 'end-of-buffer)
(global-set-key (kbd "C-x M-;") 'beginning-of-buffer)

;; Setup recentf-mode with minibuffer dialog box
;; This allows to find recently opened files more easily
(paradox-require 'recentf-minibuffer)

;; Ivy setup
(paradox-require 'ivy)
(paradox-require 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "%d/%d ")
(setq ivy-virtual-abbreviate 'fullpath)
(setq enable-recursive-minibuffers t)
(setq counsel-mode-override-describe-bindings t)
(global-set-key "\C-s" 'swiper-isearch)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-g") 'counsel-find-file)
(global-set-key (kbd "<f2> f") 'counsel-describe-function)
(global-set-key (kbd "<f2> v") 'counsel-describe-variable)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
;; IMPORTANT: this is for when you catch a match and want to end it at that point
;; Useful for when renaming longer files to shorter ones
(dolist (k '("C-j" "C-RET"))
  (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))
(define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
(define-key ivy-occur-mode-map (kbd "C-c C-q") #'ivy-wgrep-change-to-wgrep-mode)
(setq ivy-re-builders-alist
      '((swiper . regexp-quote)
        (t      . ivy--regex-fuzzy)))
;; Do a normal word search through isearch, when in need to
;; exactly match a word in a buffer. This relates to the above
;; swiper search functionality.
(global-set-key (kbd "C-c C-s") 'isearch-forward)
;; Info - Ivy/Swiper allow for two really cool features
;; 1. Rotate the built-in regexp builders in ivy through the mapping <C-o M>
;; This would require the package 'ivy-hydra' to be installed. Hence...
(paradox-require 'ivy-hydra)
;; 2. Turn-off the regexp completely and use a normal search through <M-r>


;; Disable backup files
(paradox-require 'idle-auto-save)
(idle-auto-save-mode t)
(setq ias/save-all-buffers t)
;; Inhibit default Emacs auto-save, lock-file, backup shenanigans
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/.backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2)

(which-key-mode 1)
(setq which-key-separator " ")
(setq which-key-prefix-prefix "+")

;; Delete selected area by yank or overwrite
(delete-selection-mode 1)

;; Delete trailing white spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set eshell key
(global-set-key [f1] 'eshell)
(require 'eshell-git-prompt)
(eshell-git-prompt-use-theme 'powerline)
;; Add eshell built-in autojump functionality
(eval-after-load 'eshell
  '(require 'eshell-autojump nil t))
(setq eshell-last-dir-ring-size 500)

;; Save/Restore opened files and windows
(desktop-save-mode 1)

;; Setup custom word wrappings
(wrap-region-global-mode t)
(wrap-region-add-wrapper "`" "`" nil '(markdown-mode text-mode org-mode prog-mode))
(wrap-region-add-wrapper "*" "*" nil '(markdown-mode text-mode org-mode prog-mode))
(wrap-region-add-wrapper "_" "_" nil '(markdown-mode text-mode org-mode prog-mode))
(wrap-region-add-wrapper "~" "~" nil '(markdown-mode text-mode org-mode))
(wrap-region-add-wrapper "<kbd>" "</kbd>" "<" 'markdown-mode)
(wrap-region-add-wrapper "+" "+" nil 'org-mode)
(wrap-region-add-wrapper "#+BEGIN_SRC\n" "#+END_SRC" "#" 'org-mode)

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
(defun dvorak-translation ()
  "Remap keys for Dvorak layout in the current FRAME."
  (keyboard-translate ?\C-t ?\C-x)
  (keyboard-translate ?\C-x ?\C-t))
(defun setup-frame-keyboard (frame)
  "Call the dvorak mapping on the FRAME."
  (with-selected-frame frame
    (dvorak-translation)))
(dvorak-translation)
(add-hook 'after-make-frame-functions #'setup-frame-keyboard)
;; Disable transposing characters (C-t),
;; since sometimes it's called by accident when working with the C-x (t) binding
(global-unset-key (kbd "C-t"))

(global-set-key (kbd "C-h") 'backward-kill-word)
(global-set-key [?\C-.] 'execute-extended-command)
(global-set-key [?\C-,] (lookup-key global-map [?\C-x]))
(global-set-key [?\C-'] 'hippie-expand)

(defun set-browser-open-func (browser-open-func)
  "Set the generic BROWSER-OPEN-FUNC used for opening URLs from within Emacs."
  (setq browser-url-generic-program
	(executable-find browser-open-func)))

(cond
 ((string-equal system-type "windows-nt")
  (progn
    (message "Not supported opening URLs")))
 ((string-equal system-type "darwin")
  (progn
    (set-browser-open-func "open -a Firefox")))
 ((string-equal system-type "gnu/linux")
  (progn
    (set-browser-open-func "firefox"))))

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
;; Create the auto-save-undo-tree directory if it doesn't exist so that it can be populated with undo-tree files
(let ((dir (file-name-directory "~/.emacs.d/auto-save-undo-tree")))
  (unless (file-exists-p dir)
    (make-directory dir t)))
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/auto-save-undo-tree")))

;; Setup very-large-file mode
(paradox-require 'vlf)

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
;; (setq-default bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'nil)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Bind (u)rl (a)t (p)oint globally
(global-set-key (kbd "C-c u a p") 'browse-url-at-point)

;; Create missing directory if missing while opening a file
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

;; Format JSON objects on the fly
(global-set-key (kbd "C-c u j") 'json-pretty-print)
(global-set-key (kbd "C-c u b") 'json-pretty-print-buffer)

;; end-general-utilities-section ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------------+ ;;
;; |   Dev environment     | ;;
;; +-----------------------+ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make sure ag is installed. Currently it's needed only in one place,
;; but more packages might require it in the future, so install it more at the top...
;; This also requires the actual OS package to be installed, so make sure that 'ag' the package is also present on your system
(paradox-require 'ag)

;; If you want to change prefix for lsp-mode keybindings.
(setq lsp-keymap-prefix "C-c l")
;; Setup lsp-mode
(paradox-require 'lsp-mode)
;; Setup lsp-ui
(paradox-require 'lsp-ui)
(setq lsp-enable-indentation nil)
(setq lsp-enable-completion-at-point nil)
(setq lsp-ui-peek-enable t)
(setq lsp-ui-peek-show-directory t)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-diagnostics nil)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-doc-enable nil)
;; Setup lsp for different languages
;; Clojure
(add-hook 'clojure-mode-hook #'lsp)
(add-hook 'clojurec-mode-hook #'lsp)
(add-hook 'clojurescript-mode-hook #'lsp)
;; Python
(add-hook 'python-mode-hook #'lsp)
(add-hook 'elpy-mode-hook #'lsp)
;; Map lsp-ui key bindings
(global-set-key [mouse-1] 'lsp-ui-doc-hide)
(global-set-key (kbd "C-q") 'lsp-ui-doc-glance)
(global-set-key (kbd "C-M-.") 'lsp-ui-peek-find-references)
(global-set-key (kbd "C-M-,") 'lsp-ui-peek-find-definitions)
(global-set-key (kbd "C-M-'") 'lsp-ui-peek-find-implementation)

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

;; Setup Python dev environment
;; This is a quick explanation on how to setup elpy
;; 1. Make sure that elpy and lsp-pylsp are installed and enabled
;; We need lsp-pylsp for the language server integration with jedi
;; We need elpy for the IDE features and also jedi integration
(paradox-require 'elpy)
;; 0. Enable elpy (for the most part, this would be enough)
(elpy-enable)
;; 1. If elpy doesn't create a local environment by default, you can make one yourself and use it instead
;; Here, the necessary packages for the elpy integration shall be installed
;; Example: `python -m venv ENV`
;; 3. Install necessary python packages in order for elpy to be magical
;; This includes flake8, black, jedi, rope, autopep8, yapf
;; Example: `pip install black jedi rope autopep8 yapf`
;; NOTE: as of elpy=1.35.0, elpy seems like it has a broken integration with jedi=0.18, so you MUST use jedi=0.17.2
;; 4. Active the appropriate (in this case, default) pyvenv directory
(pyvenv-activate "~/.emacs.d/ENV")
;; 5. elpy-rpc will use the above environment for all of its integrations
(setq elpy-rpc-virtualenv-path 'current)
;; 6. Set python-shell interpreter
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
;; 7. Setup the check command to be flake8
(setq python-check-command (expand-file-name "flake8"))
;; 8. Setup flycheck for working with elpy
(with-eval-after-load 'flycheck
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; 9. To active lsp for python as well, you need to install lsp pylsp in the same virtual environment
;; Example: `pip install 'python-lsp-server[all]'`
;; And now we load it instead of using pyls
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-disabled-clients 'pyls))

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

;; Beamer setup
(paradox-require 'ox-beamer)
(paradox-require 'ox-latex)
(setq org-export-allow-bind-keywords t)
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
;; (org-babel-do-load-languages 'org-babel-load-languages '((sh . t) (python . t) (C . t) (js . t)))
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Un-comment the following block of code in order to highlight code snippets in org mode in the editor
;; (font-lock-add-keywords 'org-mode
;; 			'(("\\(src_\\)\\([^[{]+\\)\\(\\[:.*\\]\\){\\([^}]*\\)}"
;; 			   (1 '(:foreground "black" :weight 'normal :height 10)) ; src_ part
;; 			   (2 '(:foreground "cyan" :weight 'bold :height 75 :underline "red")) ; "lang" part.
;; 			   (3 '(:foreground "#555555" :height 70)) ; [:header arguments] part.
;; 			   (4 'org-code) ; "code..." part.
;; 			   )))

;; Add js2 mode
(paradox-require 'js2-mode)
(paradox-require 'js2-refactor)
(paradox-require 'xref-js2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

;; (global-set-key (kbd "C-M-.") 'lsp-ui-peek-find-references)
;; (global-set-key (kbd "C-M-,") 'lsp-ui-peek-find-definitions)

(setq js2-include-node-externs t)
(setq js2-global-externs '("customElements"))
(setq js2-highlight-level 3)
(setq js2r-prefer-let-over-var t)
(setq js2r-prefered-quote-type 2)
(setq js-indent-align-list-continuation t)
(setq global-auto-highlight-symbol-mode t)
(setq js-indent-level 2)
(advice-add #'js2-identifier-start-p
            :after-until
            (lambda (c) (eq c ?#)))

;; Setup diminish - shortens (or hides) minor modes on the mode line
(paradox-require 'diminish)
(eval-after-load "ivy" '(diminish 'ivy-mode))
(eval-after-load "elpy" '(diminish 'elpy-mode))
(eval-after-load "flymake" '(diminish 'flymake-mode))
(eval-after-load "hi-lock" '(diminish 'hi-lock-mode))
(eval-after-load "flyspell" '(diminish 'flyspell-mode))
(eval-after-load "flycheck" '(diminish 'flycheck-mode))
(eval-after-load "simple" '(diminish 'visual-line-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "autorevert" '(diminish 'auto-revert-mode))
(eval-after-load "wrap-region" '(diminish 'wrap-region-mode))
(eval-after-load "highlight-indentation" '(diminish 'highlight-indentation-mode))

;; Setup clojure mode
(paradox-require 'cider)
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
(add-hook 'cider-mode-hook
          (lambda () (add-hook 'after-save-hook 'cider-format-buffer nil 'make-it-local)))
(eval-after-load 'flycheck '(flycheck-clojure-setup))
(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
(add-hook 'cider-mode-hook
	  (lambda () (setq next-error-function #'flycheck-next-error-function)))
(setenv "PATH" (concat
		"/usr/local/bin" path-separator
		(getenv "PATH")))
(dolist (m '(clojure-mode
	     clojurec-mode
	     clojurescript-mode))
  (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
;; Add lispy mode for clojure work
(add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
;; Lispy supports compatibility with cider (shouldn't cause conflicts in key bindings)
(setq lispy-compat '(cider))

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

;; This is some Custom HTML mode that I have no idea if I need
(paradox-require 'sgml-mode)

;; Setup XML mode with code folding below
(paradox-require 'hideshow)
(paradox-require 'nxml-mode)

;; Specifies which files should be opened with nxml-mode
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt
                                  '("xml" "xsd" "sch" "xslt" "svg" "rss") t)
                           "\\'") 'nxml-mode))
(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)
(require 'rng-loc nil t)

(add-hook 'nxml-mode-hook (lambda () (hs-minor-mode 1)))

;; Defines what are the starting and ending regex expression for code folding
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>" ;; regexp for start block
               "-->\\|</[^/>]*[^/]>" ;; regexp for end block
               "<!--"
               nxml-forward-element
               nil))

(defun nxml-show-path ()
  "Display the hierarchy of XML elements the point is on as a path.
Taken from http://www.emacswiki.org/emacs/NxmlMode"
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while
            (and (< (point-min) (point)) ;; Doesn't error if point is at
                 ;; Beginning of buffer
                 (condition-case nil
                     (progn
                       (nxml-backward-up-element) ;; Always returns nil
                       t)
                   (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

;; Import local fold-diwm package that provides a unified
;; interface for code folding
(require 'fold-dwim)
(eval-after-load 'prog-mode
  '(progn
     (define-key prog-mode-map [mouse-3] 'fold-dwim-toggle)
     (define-key prog-mode-map (kbd "C-s-8") 'fold-dwim-toggle)
     (define-key prog-mode-map (kbd "C-s-9") 'fold-dwim-hide-all)
     (define-key prog-mode-map (kbd "C-s-0") 'fold-dwim-show-all)))
(eval-after-load 'nxml-mode
  '(progn
     (define-key nxml-mode-map [mouse-3] 'fold-dwim-toggle)
     (define-key nxml-mode-map (kbd "C-s-8") 'fold-dwim-toggle)
     (define-key nxml-mode-map (kbd "C-s-9") 'fold-dwim-hide-all)
     (define-key nxml-mode-map (kbd "C-s-0") 'fold-dwim-show-all)))

;; Load hideshowvis for adding symbols to the fringe for code folding
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)
;; Define modes which should also be accompanied with hideshowvis
(dolist (hook (list 'nxml-mode-hook
		    'js-mode-hook
		    'python-mode-hook
		    'clojure-mode-hook
		    'html-mode-hook))
  (add-hook hook 'hideshowvis-enable))

;; Setup projectile
(paradox-require 'projectile)
(projectile-mode t)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-sort-order 'recently-active)

(paradox-require 'hl-todo)
(global-hl-todo-mode t)
(setq hl-todo-keyword-faces
      '(("TODO"  . "#61AFEF")
        ("FIXME" . "#E5C07B")
        ("DEBUG" . "#E06C75")
        ("BUG" . "#E06C75")
        ("STUB"  . "#98C379")))
(define-key hl-todo-mode-map (kbd "C-c C-p") 'hl-todo-previous)
(define-key hl-todo-mode-map (kbd "C-c C-n") 'hl-todo-next)
(define-key hl-todo-mode-map (kbd "C-c C-o") 'hl-todo-occur)

;; Open .rasi files with css-mode
(add-to-list 'auto-mode-alist '("\\.rasi\\'" . css-mode))

;; Setup pomidor package (pomodoro technique package)
(global-set-key (kbd "<f12>") #'pomidor)
(setq pomidor-sound-tick nil
      pomidor-sound-tack nil)

;; Setup yasnippet
;; Use popup for popup yasnippet choices
(paradox-require 'popup)
(paradox-require 'yasnippet)

;; Enable yasnippet globally
(yas-global-mode 1)

;; Add some shortcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

(defun yas/popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions '(yas/popup-isearch-prompt yas-no-prompt))

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

;; end-window-manipulation-section ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------------+ ;;
;; | Setup how Emacs looks | ;;
;; +-----------------------+ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Load atom one dark theme
(load-theme 'atom-one-dark t)

;; Set default font
(add-to-list 'default-frame-alist
             '(font . "Iosevka Nerd Font Mono-12:spacing=m:width=condensed:antialias=1"))

;; Set cursor type
(setq sentence-end-double-space nil)
(setq-default cursor-type '(bar . 2))

;; Have color brackets on programming modes
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Set line highlighting
(global-hl-line-mode 1)

;;Toggle transparency in Emacs
(defun toggle-transparency ()
  "Toggle the background of Emacs to be transparent or not."
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

;; Set line numbers
(global-linum-mode t)

;; Set neotree window width
(paradox-require 'neotree)
(setq neo-window-width 33)

(global-visual-line-mode t)

;; Setup whitespaces to be depicted with dots
;; Then start `whitespace-mode` in order to see the spaces/tabs
(setq whitespace-style '(face spaces space-mark face tab tab-mark trailing))
(setq whitespace-display-mappings
      '(
	;; this is a mapping for the '—' character
	(tab-mark ?\t [?\u2014 ?\t] [92 ?\t])
	;; this is a mapping for the '∙' character
	(space-mark ?\ [?\u2219] [46])))

;; Unique names of buffers for files with identical names
(paradox-require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " @ ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Setup doom modeline
(paradox-require 'doom-modeline)
(column-number-mode 1) ;; an addition to have column numbers as well
(doom-modeline-mode 1)

;; Interactive command to show color values in text
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
      '(("w" "todo" entry (file+headline "~/org/todo.org" "Work Today")
         "** TODO [#A] %?")
	("o" "todo" entry (file+headline "~/org/todo.org" "Personal Today")
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

;; Define custom org strike-through color since none is present yet
(defface org-custom-strike-through '((t (:strike-through t :foreground "#56B6C2")))
  "Face for struck-through text."
  :group 'org-faces)

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
