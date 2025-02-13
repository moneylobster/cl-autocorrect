;;;; Autocorrect
(defpackage :autocorrect
  (:use :cl)
  (:export
   #:autocorrecting-debugger))

(in-package :autocorrect)
;; Attempts at implementing a misspelled function auto-corrector at the REPL.

;;; Part 1: Connecting the autocorrect to the REPL

(defvar *original-debugger-hook* *debugger-hook*)

(defun autocorrecting-debugger (c debugger)
  "A debugger-hook that adds in an autocorrected replacement option for
undefined-function errors."
  (declare (ignorable debugger))
  ;; change the debugger back to the original
  (let ((*debugger-hook* *original-debugger-hook*))
	(if (typep c
			   #+sbcl 'undefined-function
			   #+ccl 'ccl::undefined-function-call) ; add your implementation here!
		(let ((%suggested-fn (autocorrect-function (write-to-string (cell-error-name c)))))
		  (restart-case (error c)
			(autocorrect ()
			  :report (lambda (stream)
						(format stream "Replace the function with ~A." %suggested-fn))
			  (invoke-restart 'use-value (read-from-string %suggested-fn)))))
		(invoke-debugger c))))

#+test
(let ((*debugger-hook* #'autocorrecting-debugger))
  (caf '(2 2)))

;;; Part 2: Auto-correction stuff
;; Adapted from https://norvig.com/spell-correct.html
;; and its CL translation at https://github.com/mikaelj/snippets/blob/master/lisp/spellcheck/spellcheck.lisp

;; Issues:
;; 1) Generating edits takes a lot of time, maybe check edits1 first
;;    before generating edits2?
;; 2) May not work when it's moved to its' own package, will need to
;;    modify `get-all-functions' then. Maybe use package info from
;;    sly/slime?

(defvar *correction-mode* :local
  "Determines which functions to look at as possible corrections.
Any choice besides :local takes a long time, not recommended.

Options: (sorted in terms of compute load)

:LOCAL Current package only. (which includes :cl usually)
:EXPORTED Exported functions in all packages. (and all functions in current package)
:ALL All functions in all packages.")

(defvar *alphabet* "ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*[]-=_+{}:<>/."
  "The characters we allow to be misspelt. CL allows unicode etc. in
function and symbol names as well, but we ignore that in the name of
speed.")


(defun autocorrect-function (misspelled-fn)
  "Return one of the most likely corrections for the function name (as a string)
MISSPELLED-FN."
  (candidate misspelled-fn (get-all-functions)))

(defun known (word dictionary)
  "Return whether WORD is in DICTIONARY."
  (member word dictionary :test #'equalp))

(defun candidate (word dictionary)
  "Find the first possible spelling correction for the string WORD."
  (first (member-if (lambda (x) (known x dictionary))
					(nconc (edits1 word) (edits2 word)))))

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
			 (push (write-to-string s) lst)))))
	  (:all (do-symbols (s)
				   (when (fboundp s)
					 (push (write-to-string s) lst)))
	   (dolist (p (list-all-packages))
		 (do-all-symbols (s p)
		   (when (fboundp s)
			 (push (write-to-string s) lst))))))
	lst))

(defun edits1 (word)
  "All edits that are one edit away from the string WORD."
  (declare (type string word))
  (let* ((splits (loop for i from 0 upto (length word)
					   collecting (cons (subseq word 0 i) (subseq word i))))
		 (deletes (loop for (a . b) of-type string in splits
						when (not (zerop (length b)))
						  collect (concatenate 'string a (subseq b 1))))
		 (transposes (loop for (a . b) of-type string in splits
						   when (> (length b) 1)
							 collect (concatenate 'string
												  a
												  (subseq b 1 2)
												  (subseq b 0 1)
												  (subseq b 2))))
		 (replaces (loop for (a . b) of-type string in splits
						 nconcing (loop for c of-type standard-char across *alphabet*
										when (not (zerop (length b)))
										  collect (concatenate 'string
															   a
															   (string c)
															   (subseq b 1)))))
		 (inserts (loop for (a . b) of-type string in splits
						nconcing (loop for c of-type standard-char across *alphabet*
									   collect (concatenate 'string
															a
															(string c)
															b)))))
	(nconc deletes transposes replaces inserts)))

(defun edits2 (word)
  "All edits that are two edits away from the string WORD."
  (loop for e1 in (edits1 word) nconcing (edits1 e1)))
