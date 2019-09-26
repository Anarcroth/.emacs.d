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
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(js2-basic-offset 2)
 '(package-selected-packages
   (quote
    (fancy-battery paradox spaceline-all-the-icons all-the-icons ac-js2 org-pomodoro diminish smooth-scrolling xref-js2 js2-refactor js2-mode yaml-mode flycheck-irony company-irony irony historian latex-math-preview flycheck-rtags rtags company-rtags htmlize rainbow-delimiters ac-slime slime highlight-thing diff-hl gitconfig-mode gitignore-mode pip-requirements anaconda-mode whitespace-cleanup-mode highlight-escape-sequences flycheck-pos-tip flycheck multiple-cursors pdf-tools markdown-mode wrap-region winum vimish-fold ox-reveal nyan-mode neotree magit flyspell-correct elpy auctex-latexmk atom-one-dark-theme)))
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(smie-indent-basic 2)
 '(spaceline-all-the-icons-flycheck-alternate nil)
 '(spaceline-all-the-icons-hide-long-buffer-path t)
 '(spaceline-all-the-icons-highlight-file-name t)
 '(spaceline-all-the-icons-icon-set-eyebrowse-slot (quote square))
 '(spaceline-all-the-icons-icon-set-git-ahead (quote commit))
 '(spaceline-all-the-icons-icon-set-window-numbering (quote square))
 '(spaceline-all-the-icons-primary-separator "")
 '(spaceline-all-the-icons-separator-type (quote arrow))
 '(spaceline-all-the-icons-slim-render nil)
 '(spaceline-all-the-icons-window-number-always-visible t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hi-yellow ((t (:background "#3E4451"))))
 '(spaceline-highlight-face ((t (:foreground "#282C34" :background "#528BFF")))))
