;;; package --- Summary
;; Many different custom variables.

;;; Commentary:
;; Doom setup, icons, dev styles, packages, elpy modules, many many faces.
;; Set coding styles and indents

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-default-adjust 0.0)
 '(all-the-icons-scale-factor 1)
 '(c-default-style
   '((other . "stroustrup")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu")))
 '(doom-modeline-bar-width 10)
 '(doom-modeline-buffer-encoding t)
 '(doom-modeline-buffer-file-name-style 'buffer-name)
 '(doom-modeline-env-enable-python nil)
 '(doom-modeline-env-version t)
 '(doom-modeline-height 25)
 '(doom-modeline-persp-name t)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults))
 '(js2-basic-offset 4)
 '(org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(p)" "TESTING(e)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)" "DIDN'T(n)")
     (sequence "IDEA(i)" "RE-THINK(r)" "LATER(l)" "APPOINTMENT(a)" "|")))
 '(package-selected-packages
   '(epresent lsp-jedi lsp-ui lsp-mode hl-todo projectile auto-package-update which-key with-editor tablist swiper scratch pyvenv lispy pdf-tools iedit highlight-indentation find-file-in-project counsel ace-window transient elpy magit ivy flycheck-clojure flycheck-inline-mode flycheck-mode racer toml-mode cargo flycheck-rust rust-mode vlf clj-refactor-mode clj-refactor cider clojure-mode doom-modeline undo-tree org-bullets org-fancy-priorities fancy-battery paradox spaceline-all-the-icons all-the-icons ac-js2 org-pomodoro diminish xref-js2 js2-refactor js2-mode yaml-mode flycheck-irony company-irony latex-math-preview flycheck-rtags company-rtags htmlize rainbow-delimiters ac-slime highlight-thing diff-hl gitconfig-mode gitignore-mode pip-requirements whitespace-cleanup-mode highlight-escape-sequences flycheck-pos-tip multiple-cursors markdown-mode wrap-region winum vimish-fold ox-reveal neotree flyspell-correct auctex-latexmk atom-one-dark-theme))
 '(sh-basic-offset 4)
 '(sh-indentation 2)
 '(smie-indent-basic 2)
 '(spaceline-all-the-icons-flycheck-alternate nil)
 '(spaceline-all-the-icons-hide-long-buffer-path t)
 '(spaceline-all-the-icons-highlight-file-name t)
 '(spaceline-all-the-icons-icon-set-eyebrowse-slot 'square)
 '(spaceline-all-the-icons-icon-set-git-ahead 'commit)
 '(spaceline-all-the-icons-icon-set-window-numbering 'square)
 '(spaceline-all-the-icons-primary-separator "")
 '(spaceline-all-the-icons-separator-type 'arrow)
 '(spaceline-all-the-icons-slim-render nil)
 '(spaceline-all-the-icons-window-number-always-visible t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-debug ((t (:foreground "#61AFEF"))))
 '(doom-modeline-info ((t (:foreground "#61AFEF"))))
 '(ediff-current-diff-A ((t (:background "#121417"))))
 '(ediff-current-diff-B ((t (:background "#121417"))))
 '(ediff-current-diff-C ((t (:background "#121417"))))
 '(ediff-even-diff-A ((t (:background "#121417"))))
 '(ediff-even-diff-B ((t (:background "#121417"))))
 '(ediff-even-diff-C ((t (:background "#121417"))))
 '(ediff-fine-diff-A ((t (:background "#121417"))))
 '(ediff-fine-diff-B ((t (:background "#121417"))))
 '(ediff-fine-diff-C ((t (:background "#121417"))))
 '(ediff-odd-diff-A ((t (:background "#121417"))))
 '(ediff-odd-diff-B ((t (:background "#121417"))))
 '(ediff-odd-diff-C ((t (:background "#121417"))))
 '(hi-yellow ((t (:background "#3E4451"))))
 '(org-agenda-date ((t (:foreground "#61AFEF"))))
 '(org-agenda-date-today ((t (:background "#121417" :foreground "#528BFF" :slant italic :weight bold))))
 '(org-agenda-done ((t (:foreground "#98C379" :strike-through t))))
 '(org-agenda-structure ((t (:foreground "#61AFEF"))))
 '(org-date ((t (:foreground "#C678DD"))))
 '(org-done ((t (:foreground "#98C379" :weight bold :strike-through t))))
 '(org-headline-done ((t (:foreground "#98C379" :strike-through t))))
 '(org-level-1 ((t (:background "#121417" :foreground "#528BFF" :weight bold))))
 '(org-level-2 ((t (:background "#121417" :foreground "#E06C75" :weight bold))))
 '(org-level-3 ((t (:foreground "#ABB2BF" :weight bold))))
 '(org-level-4 ((t (:foreground "#E5C07B"))))
 '(org-scheduled ((t (:foreground "#C678DD"))))
 '(org-scheduled-today ((t (:foreground "#C678DD"))))
 '(org-special-keyword ((t (:foreground "#E06C75"))))
 '(org-todo ((t (:foreground "#61AFEF" :weight bold))))
 '(org-upcoming-deadline ((t (:foreground "#E06C75"))))
 '(org-warning ((t (:foreground "#E06C75"))))
 '(spaceline-all-the-icons-sunrise-face ((t (:foreground "#f6c175"))))
 '(spaceline-all-the-icons-sunset-face ((t (:foreground "#fe7714"))))
 '(spaceline-highlight-face ((t (:foreground "#282C34" :background "#528BFF"))))
 '(spaceline-python-venv ((t (:foreground "#528BFF"))))
 '(whitespace-space ((t (:foreground "#4B5363"))))
 '(whitespace-tab ((t (:foreground "#4B5363")))))
;;; custom.el ends here
