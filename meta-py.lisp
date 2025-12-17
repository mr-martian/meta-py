(defpackage :meta-py-user
  (:use :common-lisp)
  (:shadow
   :+ :- :* :/
   :not :and :or
   :< :> :<= :>=
   := :/=
   :return))

(defmacro as-user (&body body)
  `(let ((*package* (find-package :meta-py-user)))
     ,@body))

(defmacro with-caps (&body body)
  (let ((rtc (gensym)))
    `(let ((,rtc (readtable-case *readtable*)))
       (setf (readtable-case *readtable*) :preserve)
       (prog1 (progn ,@body)
	 (setf (readtable-case *readtable*) ,rtc)))))

(defun is-bound (symbol)
  (as-user
   (let ((alt-sym (intern (string-upcase (string symbol)))))
     (or (macro-function symbol)
	 (macro-function alt-sym)))))

(defun read-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun parse-file (filename)
  (let ((text (concatenate 'string
			   "( " (read-contents filename) " )")))
    (as-user (with-caps (read-from-string text)))))

(defun fix-args (sym)
  (if (char= (elt (string sym) 0) #\&)
      (intern (string-upcase (string sym)))
      sym))

(defun eval-macros (tree)
  (if (listp tree)
      (if (and (symbolp (car tree))
	       (string= (string-downcase (string (car tree))) "defmacro"))
	  (progn
	    (as-user (eval (cons 'defmacro
				 (cons (cadr tree)
				       (cons
					(mapcar #'fix-args (caddr tree))
					(cdddr tree))))))
	    nil)
	  (mapcar #'eval-macros tree))
      tree))

(defun apply-macros (tree)
  (if (listp tree)
      (if (symbolp (car tree))
	  (let ((fn (is-bound (car tree))))
	    (if fn
		(apply-macros (macroexpand tree))
		(mapcar #'apply-macros tree)))
	  (mapcar #'apply-macros tree))
      tree))

(defun prettyprint (tree)
  (let ((indent -1)
	(block-has-statement nil))
    (labels
	((escape-string (s)
	   (concatenate 'string
			(loop for c across s
			      appending (case c
					  (#\Newline '(#\\ #\n))
					  (otherwise (list c))))))
	 (ppe-sep (sequence separator &key (parens nil))
	   (when parens (format t "("))
	   (when sequence
	     (ppe (car sequence))
	     (loop for item in (cdr sequence)
		   do (progn
			(format t "~a" separator)
			(ppe item))))
	   (when parens (format t ")")))
	 (ppe (tree)
	   (cond
	     ((symbolp tree) (format t "~a" tree))
	     ((and (listp tree) (symbolp (car tree)))
	      (case (intern (string-upcase (string (car tree))))
		((** * @ / // % << >> & ^ < <= > >= != ==)
		 (ppe-sep (cdr tree) (car tree) :parens t))
		((+ -)
		 (if (= (length tree) 2)
		     (progn
		       (format t "(~a" (car tree))
		       (ppe (cadr tree))
		       (format t ")"))
		     (ppe-sep (cdr tree) (car tree) :parens t)))
		(attr
		 (if (symbolp (caddr tree))
		     (progn
		       (ppe (cadr tree))
		       (format t ".~a" (caddr tree)))
		     (ppe (cons '|getattr| (cdr tree)))))
		(otherwise
		 (progn
		   (format t "~a" (car tree))
		   (ppe-sep (cdr tree) ", " :parens t)))))
	     ((stringp tree)
	      (format t "~a"
		      (concatenate 'string '(#\")
				   (escape-string tree)
				   '(#\"))))
	     ((numberp tree) (format t "~a" tree))
	     ))
	 (ppb (statements)
	   (incf indent)
	   (setf block-has-statement nil)
	   (mapcar #'pps statements)
	   (unless block-has-statement
	     (format t "~v{~a~:*~}pass~%" indent '("    ")))
	   (setf block-has-statement t)
	   (decf indent))
	 (pps (tree)
	   (let ((istr (format nil "~v{~a~:*~}" indent '("    "))))
	     (if (listp tree)
		 (when tree
		   (case (intern (string-upcase (string (car tree))))
		     (def
		      (progn
			(format t "~adef ~a(~{~a~^, ~}):~%"
				istr (cadr tree) (caddr tree))
			(ppb (cdddr tree))))
		     (class
		      (progn
			(format t "~aclass ~a(~{~a~^, ~}):~%"
				istr (cadr tree) (caddr tree))
			(ppb (cdddr tree))))
		     (progn (mapcar #'pps (cdr tree)))
		     (= ; TODO: should this do setattr() automatically?
		      (progn
			(format t "~a" istr)
			(ppe (cadr tree))
			(format t " = ")
			(ppe (caddr tree))
			(format t "~%")
			(setf block-has-statement t)))
		     (otherwise
		      (progn
			(format t "~a" istr)
			(ppe tree)
			(format t "~%")
			(setf block-has-statement t)))))
		 (format t "~a#~a~%" indent tree)))))
      (ppb tree))))

(let* ((orig (parse-file "test.lisp"))
       (post-eval (eval-macros orig))
       (post-apply (apply-macros post-eval)))
  ;(print orig)
  ;(print post-eval)
  ;(print post-apply)
  ;(format t "~%###########~%")
  (prettyprint post-apply))
