;;; package --- Summary
;; c-cpp-dev-env is a custom developer environment, providing a nice editing
;; experience for any C/C++ code.

;;; Commentary:
;; Currently, it doesn't seem that this should be made into its own mode. Right
;; now it has the functionality to be turned on/off with a command. While a mode
;; can do the same thing, these functions call a lot of other modes so it would
;; just be encapsulating one thing into another, making the package even more
;; confusing.

;;; Code:

(defun setup-flycheck-rtags ()
  "Make sure that Emacs use only rtags for its checking."
  (interactive)
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

(defun start-c-cpp-dev-env ()
  "Only run this if rtags is installed."
  (interactive)
  (when (require 'rtags nil :noerror)
    ;; Define company and rtags keys
    (require 'company)
    (define-key c-mode-base-map (kbd "M-.")
      (function rtags-find-symbol-at-point))
    (define-key c-mode-base-map (kbd "M-,")
      (function rtags-find-references-at-point))
    ;; Install standard rtags keybindings. Do M-. on the symbol below to
    ;; jump to definition and see the keybindings.
    (rtags-enable-standard-keybindings)
    ;; Company completion setup
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setq rtags-completions-enabled t)
    (push 'company-rtags company-backends)
    (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
    ;; Use rtags flycheck mode -- clang warnings shown inline
    (require 'flycheck-rtags)
    (add-hook 'c++-mode-hook #'setup-flycheck-rtags))

  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (require 'flycheck)
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony)))

(provide 'c-cpp-dev-env)
;;; c-cpp-dev-env ends here
