;;;; Autocorrect
;; Attempts at implementing a misspelled function auto-corrector at the REPL.

;;; Part 1: Connecting the autocorrect to the REPL

;; Issues:
;; 1) SBCL and CCL have different errors for undefined functions:
;;    undefined-function and ccl::undefined-function-call
;; 2) When fast-eval'ing (which seems to be what both implementations
;;    mostly do when using REPL) the condition is raised from generated
;;    code (or somehow the source isn't recorded) so I can't inject my
;;    restart into those functions
;;   for SBCL this is somewhere in `%simple-eval'
;;   for CCL this is somewhere in `cheap-eval'

;; Option 1: Override `%coerce-name-to-fun'

(defmacro inject-autocorrect (fun)
  "Replace a function definition with a wrapper that calls the function
with the autocorrect restart. Makes some assumptions about FUN's
inputs, so don't use for other purposes."
  `(progn (defvar ,(read-from-string (concatenate 'string "*original-" (symbol-name fun) "*")) #',fun)
		  (with-unlocked-packages (,(symbol-package fun))
			(defun ,fun (symbol)
			  (let ((%suggested-fn nil))
				(restart-case (funcall ,(read-from-string (concatenate 'string "*original-" (symbol-name fun) "*")) symbol)
				  (autocorrect ()
					:report (lambda (stream) (format stream "Replace the function with ~A." %suggested-fn))
					:test (lambda (c)
							(if (typep c 'undefined-function)
								;; compute and store the autocorrection
								(setf %suggested-fn
									  (autocorrect-function (write-to-string (cell-error-name c))))
								nil))
					(,fun (read-from-string %suggested-fn)))))))))

;; This works on SBCL the *second* time the undefined function is attempted to be called
#+sbcl
(inject-autocorrect sb-kernel:%coerce-name-to-fun)
;; This one is for coercing to 'function
#+sbcl
(inject-autocorrect sb-kernel:coerce-symbol-to-fun)

;; Option 2: Override `symbol-function'
;; SBCL only calls this if the REPL call is made using `eval'
;; so doesn't really work well. CCL doesn't call this at all.

;; (inject-autocorrect symbol-function)

;; Option 3: Wrap all REPL calls in your own function
;; Couldn't get this working well since I can't get the restart to
;; return from a lower stack in the backtrace. Fixing the function
;; call at this level of the stack also seems hard since we don't know
;; where the undefined function is.

;; (defun eval-with-autocorrect (form)
;;   (let ((%suggested-fn nil))
;; 	(restart-case (eval form)
;; 	  (autocorrect ()
;; 		:report (lambda (stream) (format stream "Replace the function with ~A." %suggested-fn))
;; 		:test (lambda (c)
;; 				(if (typep c 'undefined-function)
;; 					;; compute and store the autocorrection
;; 					(setf %suggested-fn
;; 						  (autocorrect-function (write-to-string (cell-error-name c))))
;; 					nil))
;; 		;; Problems start here
;; 		(let ()
;; 		  (slynk-backend:return-from-frame 0 %suggested-fn))))))


;; Option 4: Reimplement `%coerce-name-to-fun'
;; Doing rewrites of the implementation source may work but feels wrong.

;; (with-unlocked-packages (sb-kernel)
;;   (defun sb-kernel:%coerce-name-to-fun (name)
;; 	(typecase name
;; 	  ((and symbol (not null))
;; 	   (let ((fun (sb-kernel:%symbol-function name)))
;; 		 (when (and fun (not (sb-impl::macro/special-guard-fun-p fun)))
;; 		   (return-from sb-kernel:%coerce-name-to-fun fun))))
;; 	  (cons
;; 	   (sb-int:binding* ((sb-kernel:fdefn (sb-impl::find-fdefn name) :exit-if-null)
;; 				  (fun (sb-impl::fdefn-fun fdefn) :exit-if-null))
;; 		 (return-from sb-kernel:%coerce-name-to-fun fun))))
;; 	;; We explicitly allow any function name when retrying,
;; 	;; even if the erring caller was SYMBOL-FUNCTION. It is consistent
;; 	;; that both #'(SETF MYNEWFUN) and '(SETF MYNEWFUN) are permitted
;; 	;; as the object to use in the USE-VALUE restart.
;; 	(setq name (restart-case (if (sb-int:legal-fun-name-p name)
;; 								 (error 'undefined-function :name name)
;; 								 (sb-int:legal-fun-name-or-type-error name))
;; 				 (continue ()
;; 				   :report (lambda (stream)
;; 							 (format stream "Retry using ~s." name))
;; 				   name)
;; 				 (use-value (value)
;; 				   :report (lambda (stream)
;; 							 (format stream "Use specified function"))
;; 				   :interactive sb-int:read-evaluated-form
;; 				   (if (functionp value)
;; 					   (return-from sb-kernel:%coerce-name-to-fun value)
;; 					   value))
;; 				 (autocorrect ()
;; 				   :report (lambda (stream) (format stream "Replace the function with ~A." (autocorrect-function (write-to-string name))))
;; 				   (return-from sb-kernel:%coerce-name-to-fun
;; 					 (symbol-function (read-from-string
;; 									   (autocorrect-function (write-to-string name))))))))
;; 	(%coerce-name-to-fun name)))

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

:LOCAL Current package only.
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

(defun known (word dictionary)
  "Return whether WORD is in DICTIONARY."
  (member word dictionary :test #'equalp))

(defun candidate (word dictionary)
  "Find the first possible spelling correction for the string WORD."
  (first (member-if (lambda (x) (known x dictionary))
					(nconc (edits1 word) (edits2 word)))))

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
