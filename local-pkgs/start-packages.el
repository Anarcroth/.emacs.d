;; Setup repositories for pulling packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Needed packages
(setq package-list
      '(anaconda-mode
	atom-one-dark-theme
	auctex-latexmk
	counsel
	diff-hl
	elpy
	flycheck
	flycheck-pos-tip
	flyspell-correct
	gitconfig-mode
	gitignore-mode
	highlight-escape-sequences
	highlight-thing
	js2-mode
	magit
	markdown-mode
	multiple-cursors
	neotree
	nyan-mode
	ox-reveal
	pdf-tools
	pip-requirements
	scratch
	telephone-line
	vimish-fold
	which-key
	whitespace-cleanup-mode
	winum
	wrap-region
	ivy-historian))

;; Load all the packages
(package-initialize)

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'start-packages)
