;;; nt-script.el --- Settings for NTEmacs

;; This settings is from mew-win32.el

;;; Code:

(require 'mw32script)

(unless mw32script-original-file-executable-p
  (setq mw32script-original-file-executable-p
	(symbol-function 'file-executable-p))
  (fset 'file-executable-p 'mw32script-file-executable-p))
(unless mw32script-original-executable-find
  (setq mw32script-original-executable-find
	(symbol-function 'executable-find))
  (fset 'executable-find 'mw32script-executable-find))

(mw32script-make-pathext-regexp)

(defun mw32script-argument-editing-function (argument)
  "Resolv the script/filename association,
and do the argument editiong."
  (let ((argv0 (car argument)) sargs func ret)
    (and mw32script-resolve-extention
	 (setq sargs (mw32script-resolve-extention argv0)))
    (and mw32script-resolve-script
	 (or (not sargs) (eq sargs 'notfound))
	 (setq sargs (mw32script-resolve-script argv0)))
    (if (and sargs (not (eq sargs 'notfound)))
	(progn
	  (setq argv0 (car sargs))
	  (catch 'tag
	    (mapcar
	     (lambda (pat)
	       (and (string-match (car pat) argv0)
		    (setq argv0 (mw32script-openp (cdr pat)))
		    (throw 'tag t)))
	     mw32script-argument-editing-alist))
	  (append (list argv0) (cdr sargs) argument))
      argument)))

(defun nt-script-argument-editing-function (program args)
  (let ((default-process-argument-editing-function 'identity)
	(process-argument-editing-alist nil)
	prog)
    (setq prog (mw32script-openp program))
    (if prog
	(mw32script-argument-editing-function (cons prog args))
      (cons program args))))

(defadvice call-process
  (before nt-script-call-process
	  (program &optional infile buffer display &rest args)
	  activate)
  (let ((sargs (nt-script-argument-editing-function program args)))
    (setq program (car sargs)
	  args (cdr sargs))))

(defadvice call-process-region
  (before nt-script-call-process-region
	  (start end program &optional infile buffer display &rest args)
	  activate)
  (let ((sargs (nt-script-argument-editing-function program args)))
    (setq program (car sargs)
	  args (cdr sargs))))

(defadvice start-process
  (before nt-script-start-process
	  (name buffer program &rest program-args)
	  activate)
  (let ((sargs (nt-script-argument-editing-function program program-args)))
    (setq program (car sargs)
	  program-args (cdr sargs))))

(provide 'nt-script)
