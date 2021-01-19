;;; package --- Summary
;; recentf-minibuffer is an extension to recentf-mode. It creates a minibuffer
;; instead of having a whole buffer for the most recent files.

;;; Commentary:
;; Taken from https://www.emacswiki.org/emacs/RecentFiles. Created by SteveBerman.

;;; Code:

(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)

(defun recentf-filtered-list (arg)
  "Return a filtered list of ARG recentf items."
  (recentf-apply-menu-filter
   recentf-menu-filter
   (mapcar 'recentf-make-default-menu-element
	   (butlast recentf-list (- (length recentf-list) arg)))))

(defun recentf-list-submenus (arg)
  "Return a list of ARG recentf submenu names."
  (if (listp (cdar (recentf-filtered-list arg))) ; submenues exist
      (delq nil (mapcar 'car (recentf-filtered-list arg)))))

(defmacro recentf-list-entries (fn arg)
  "Return a list of ARG recentf menu entries as determined by FN.
When FN is `'car' return the menu entry names, when FN is `'cdr'
return the absolute file names."
  `(mapcar (lambda (x) (mapcar ,fn x))
	   (if (recentf-list-submenus ,arg)
	       (mapcar 'cdr (recentf-filtered-list ,arg))
	     (list (recentf-filtered-list ,arg)))))

;; This function is not specific to recentf mode but is needed by
;; `recentf-minibuffer-dialog'.  I've also made enough use of it in
;; other contexts that I'm surprised it's not part of Emacs, and the
;; fact that it isn't makes me wonder if there's a preferred way of
;; doing what I use this function for.
(defun recentf-memindex (mem l)
  "Return the index of MEM in list L."
  (let ((mempos -1) ret)
    (while (eq ret nil)
      (setq mempos (1+ mempos))
      (when (equal (car l) mem) (setq ret mempos))
      (setq l (cdr l)))
    ret))

(defun recentf-minibuffer-dialog (arg)
  "Open the recentf menu via the minubuffer, with completion.
With positive prefix ARG, show the ARG most recent items.
Otherwise, show the default maximum number of recent items."
  (interactive "P")
  (let* ((num (prog1 (if (and (not (null arg))
			      (> arg 0))
			 (min arg (length recentf-list))
		       recentf-max-menu-items)
		(and (not (null arg))
		     (> arg (length recentf-list))
		     (message "There are only %d recent items."
			      (length recentf-list))
		     (sit-for 2))))
	 (menu (if (recentf-list-submenus num)
		   (completing-read "Open recent: "
				    (recentf-list-submenus num))))
	 (i (recentf-memindex menu (recentf-list-submenus num)))
	 (items (nth i (recentf-list-entries 'car num)))
	 (files (nth i (recentf-list-entries 'cdr num)))
	 (item (completing-read "Open recent: " items))
	 (j (recentf-memindex item items))
	 (file (nth j files)))
    (funcall recentf-menu-action file))) ; find-file by default

(global-set-key (kbd "C-x C-f") 'recentf-minibuffer-dialog)

(provide 'recentf-minibuffer)
;;; recentf-minibuffer.el ends here
