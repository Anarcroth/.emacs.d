;;; idle-auto-save.el --- Auto-save buffers, based on a timer. -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;; Code:
(defgroup idle-auto-save nil
  "(Kind-of) smart-saving of buffers."
  :group 'tools
  :group 'convenience)

(defcustom ias/duration 30
  "Seconds to idle, before auto-saving all buffers.
Defaults to 30."
  :group 'idle-auto-save
  :type 'integer)

(defcustom ias/save-all-buffers nil
  "Save all buffers.
Defaults to nil."
  :group 'idle-auto-save
  :type 'boolean)

(defcustom ias/inhibit-default-emacs-auto-save nil
  "Stop default auto-save behavior of Emacs.
Defaults to nil"
  :group 'idle-auto-save
  :type 'boolean)

(defvar idle-auto-save-timer)

(defun save-buffer-command ()
  "Save the current buffer if needed."
  (when (and buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (if ias/save-all-buffers
	(save-some-buffers t)
      (save-buffer))))

(defun idle-auto-save-initialize-timer ()
  "Initialize idle-auto-save timer."
  (setq idle-auto-save-timer
	(run-with-idle-timer ias/duration t #'save-buffer-command)))

(defun idle-auto-save-stop-timer ()
  "Stop idle-auto-save timer if `idle-auto-save-timer' is set."
  (when idle-auto-save-timer
    (cancel-timer idle-auto-save-timer)))

(defun inhibit-emacs-auto-save ()
  "Stop Emacs shenanigans with it's auto-save mode."
  (setq-default create-lockfiles nil)
  (setq-default auto-save-default nil)
  (setq backup-directory-alist '(("." . "~/.emacs.d/.backups")))
  (setq delete-old-versions t
	kept-new-versions 3
	kept-old-versions 2
	version-control t))

(defun idle-auto-save-start ()
  "Start idle-auto-save mode."
  (if ias/inhibit-default-emacs-auto-save
      (inhibit-emacs-auto-save))
  (idle-auto-save-initialize-timer))

(defun idle-auto-save-stop ()
  "Stop idle timer."
  (idle-auto-save-stop-timer))

;;;###autoload
(define-minor-mode idle-auto-save-mode
  "A minor mode that saves your Emacs buffers every couple seconds."
  :group 'idle-auto-save
  :global t
  (cond
   (idle-auto-save-mode (idle-auto-save-start))
   (t (idle-auto-save-stop))))

(provide 'idle-auto-save)
;;; idle-auto-save.el ends here
