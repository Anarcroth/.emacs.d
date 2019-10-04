;;; package -- Summary:
;; Setup the remote Emacs repos.
;; Initialize Emacs packages
;; Define a new function to replace `require'.

;;; Commentary:

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Load all the packages
(package-initialize)

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Thank you Artur Malabarbra for this.
;; source: https://endlessparentheses.com/require-feature-or-install-package.html
(defun paradox-require (feature &optional filename noerror package refresh)
  "A replacement for `require' which also will install the feature if absent.
- If FEATURE is present, `require' it and return t.

- If FEATURE is not present, install PACKAGE with `package-install'.
If PACKAGE is nil, assume FEATURE is the package name.
After installation, `require' FEATURE.

FILENAME is passed to `require'.

If NOERROR is non-nil, don't complain if the feature couldn't be
installed, just return nil.

By default, the current package database (stored in
`package-archive-contents') is only updated if it is empty.
Passing a non-nil REFRESH argument forces this update."
  (or (require feature filename t)
      (let ((package (or package
                         (if (stringp feature)
                             (intern feature)
                           feature))))
        (require 'package)
        (unless (and package-archive-contents (null refresh))
          (package-refresh-contents))
        (and (condition-case e
                 (package-install package)
               (error (if noerror nil (error (cadr e)))))
             (require feature filename noerror)))))

(provide 'start-packages)

;;; start-packages.el ends here
