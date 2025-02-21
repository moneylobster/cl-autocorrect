;;;; Autocorrect
;; A misspelled function auto-corrector for the REPL.

(defpackage :autocorrect
  (:use :cl)
  (:export
   #:autocorrecting-debugger
   #:*correction-mode*
   #:sly-install-autocorrect
   #:slime-install-autocorrect))

(in-package :autocorrect)

;;; Part 1: Connecting the autocorrect to the REPL

(defvar *original-debugger-hook* *debugger-hook*)

(defun autocorrecting-debugger (c debugger)
  "A debugger-hook that adds in an autocorrected replacement option for
undefined-function errors and then calls the original *debugger-hook*."
  (declare (ignorable debugger))
  (if (typep c 'undefined-function)
	  (let ((%suggested-fn (autocorrect-function (write-to-string (cell-error-name c)))))
		;; call the original debugger with the new restart
		;; if the original debugger hook is nil (eg. cli) we raise the error normally.
		(restart-case (if (null *original-debugger-hook*)
						  (error c)
						  (funcall *original-debugger-hook* c *original-debugger-hook*))
		  (autocorrect ()
			:report (lambda (stream)
					  (format stream "Replace the function with ~A." %suggested-fn))
			;; if use-value is available, present this restart.
			:test (lambda (c)
					(declare (ignorable c))
					(find-restart 'use-value))
			(invoke-restart 'use-value (read-from-string %suggested-fn)))))
	  (invoke-debugger c)))

;; Some relevant links for overriding *debugger-hook* in slime/sly:
;; SO: https://stackoverflow.com/questions/16118283/turn-off-debugger-in-emacs-slime
;; slime-devel: https://mailman3.common-lisp.net/hyperkitty/list/slime-devel@common-lisp.net/thread/DTUXGJFBWHRN35JQUQYRCJ6EUP4ZUDI6/#UKVXADOPZGCI57ZXTM6AE24ZQYRABX7N
;; slime-devel2: https://mailman3.common-lisp.net/hyperkitty/list/slime-devel@common-lisp.net/thread/YLU7BJADUH3ERUB3NESSFA6YVS4T7TTM/#RVLKAROEN77JK3PLNR6G6NKNT5M52Z2R

;; We use find-symbol here so that we don't get unknown package errors
;; during loading. The find-symbol strings need to be uppercase.
(defun slime-install-autocorrect ()
  "Add this to your ~/.swank.lisp file to use it with SLIME."
  (setf *original-debugger-hook* (symbol-function (find-symbol "SWANK-DEBUGGER-HOOK" :swank)))
  (setf (symbol-function (find-symbol "SWANK-DEBUGGER-HOOK" :swank)) #'autocorrecting-debugger)
  (format t "Autocorrect hook installed!"))

(defun sly-install-autocorrect ()
  "Add this to your ~/.slynk.lisp file to use it with Sly."
  (setf *original-debugger-hook* (symbol-function (find-symbol "SLYNK-DEBUGGER-HOOK" :slynk)))
  (setf (symbol-function (find-symbol "SLYNK-DEBUGGER-HOOK" :slynk)) #'autocorrecting-debugger)
  (format t "Autocorrect hook installed!"))

;;; Part 2: Auto-correction stuff
(defvar *correction-mode* :local
  "Determines which functions to look at as possible corrections.

Options: (sorted in terms of compute load)

:LOCAL Current package only. (which includes :cl usually)
:EXPORTED Exported functions in all packages + local symbols (LIKELY DOES NOT WORK)")

(defun autocorrect-function (misspelled-fn)
  "Return one of the most likely corrections for the function name (as a string)
MISSPELLED-FN."
  (first (fuzzy-match:fuzzy-match misspelled-fn (get-all-functions))))

(defun get-all-functions (&optional (mode *correction-mode*))
  "Return a list of all functions' symbols as strings.
Which functions are included is dependent on `*correction-mode*' or
can be specified using MODE."
  (let ((lst ()))
	(case mode
	  (:local (do-symbols (s)
				(when (fboundp s)
				  (push (write-to-string s) lst))))
	  (:exported (do-symbols (s)
				   (when (fboundp s)
					 (push (write-to-string s) lst)))
	   (dolist (p (list-all-packages))
		 (do-external-symbols (s p)
		   (when (fboundp s)
			 (push (write-to-string s) lst))))))
	lst))
