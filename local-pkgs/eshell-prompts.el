;;; Package --- Summary
;; Utility functions for re-defining how eshell prompt looks like.

;;; Commentary:
;;

;;; Code:

;; Necessary built-in lib for using `cl-reduce`
(require 'cl-lib)
(require 'eshell)

(defun shortened-path (path max-len)
  "Return a modified version of `PATH' up to `MAX-LEN' long.
Replacing some components with single characters starting from the left to try
 and get the path down to `MAX-LEN'"
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (cl-reduce #'+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
              (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/"
                              (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(defun shortened-eshell-prompt-function ()
  "Create a shortened eshell prompt."
  (concat (shortened-path (eshell/pwd) 10)
          (if (= (user-uid) 0) " # " " $ ")))

(defun even-shorter-path (path max-len)
  "Return a trimmed-down version of the directory `PATH' up to `MAX-LEN'.
Replacing parent directories with their initial characters to try to get the
 character length of `PATH' (sans directory slashes) down to `MAX-LEN'."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (cl-reduce #'+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
              (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(defun even-shorter-eshell-prompt-function ()
  "Create an even shorter eshell prompt."
  (concat (even-shorter-path (eshell/pwd) 10)
          (if (= (user-uid) 0) " # " " $ ")))

(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
	 "[" (user-login-name) "@" (system-name) " "
	 (if (string= (eshell/pwd) (getenv "HOME"))
	     "~" (eshell/basename (eshell/pwd)))
	 "]"
	 (if (= (user-uid) 0) "# " "$ "))))

(provide 'eshell-prompts)
;;; eshell-prompts ends here
