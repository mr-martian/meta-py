(defpackage :meta-py-user
  (:use :common-lisp)
  (:shadow
    :+ :- :* :/
    :not :and :or
    :< :> :<= :>=
    := :/=
    :cond
    :return))

(in-package :common-lisp)

(defmacro as-user (&body body)
  `(let ((*package* (find-package :meta-py-user)))
     ,@body))

(defmacro with-caps (&body body)
  (let ((rtc (gensym)))
    `(let ((,rtc (readtable-case *readtable*)))
       (setf (readtable-case *readtable*) :preserve)
       (prog1 (progn ,@body)
         (setf (readtable-case *readtable*) ,rtc)))))

(defun switch-language (stream char arg)
  (let ((rtc (readtable-case *readtable*))
         (pack *package*))
    (if (char= char #\!)
      (setf
        (readtable-case *readtable*) :preserve
        *package* (find-package :meta-py-user))
      (setf
        (readtable-case *readtable*) :upcase
        *package* (find-package :common-lisp)))
    (prog1
      (read stream t nil t)
      (setf
        (readtable-case *readtable*) rtc
        *package* pack))))
(set-dispatch-macro-character #\# #\! #'switch-language)
(set-dispatch-macro-character #\# #\$ #'switch-language)

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

(defun eval-macros (tree)
  (labels
    ((is-arg-op (sym)
       (char= (elt (string sym) 0) #\&))
      (reintern (sym)
        (intern (string-upcase (string sym))))
      (fix-arg-op (sym)
        (if (is-arg-op sym) (reintern sym) sym))
      (make-bind (sym)
        (list (reintern sym) sym))
      (eval-def (subtree)
        (if (listp subtree)
          (if (and (symbolp (car subtree))
                (eq (reintern (car subtree)) 'defmacro))
            (let* ((args (caddr subtree))
                    (fixed (mapcar #'fix-arg-op args))
                    (names (remove-if #'is-arg-op args))
                    (bindings (mapcar #'make-bind names)))
              (print `(defmacro ,(cadr subtree) ,fixed
                                (let ,bindings ,@(cdddr subtree))))
              (as-user (eval `(defmacro ,(cadr subtree) ,fixed
                                (let ,bindings ,@(cdddr subtree)))))
              nil)
            (mapcar #'eval-def subtree))
          subtree)))
    (eval-def tree)))

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
        (name (sym)
          (string-downcase (string sym)))
        (ppe-sep (sequence separator &key (parens nil))
          (when parens (format t "("))
          (when sequence
            (ppe (car sequence))
            (loop for item in (cdr sequence)
              do (progn
                   (format t "~a" separator)
                   (ppe item))))
          (when parens (format t ")")))
        (ppe-bin (op a b &key (parens t))
          (when parens (format t "("))
          (ppe a)
          (format t " ~a " op)
          (ppe b)
          (when parens (format t ")")))
        (ppe-unary (op x &key (parens t))
          (when parens (format t "("))
          (format t "~a" op)
          (ppe x)
          (when parens (format t ")")))
        (ppe (tree &key (parens t))
          (cond
            ((symbolp tree) (format t "~a" tree))
            ((and (listp tree) (symbolp (car tree)))
              (case (intern (string-upcase (string (car tree))))
                ((** * @ / // % << >> & ^ < <= > >= != ==)
                  (ppe-sep (cdr tree) (car tree) :parens parens))
                ((+ -)
                  (if (= (length tree) 2)
                    (ppe-unary (car tree) (cadr tree) :parens parens)
                    (ppe-sep (cdr tree) (car tree) :parens parens)))
                (~ (ppe-unary "~" (cadr tree) :parens parens))
                ((and or)
                  (ppe-sep (cdr tree) (name (car tree)) :parens t))
                (not (ppe-unary "not " (cadr tree) :parens parens))
                ((is in)
                  (ppe-bin (name (car tree)) (cadr tree) (caddr tree)
                    :parens parens))
                (is-not (ppe-bin "is not" (cadr tree) (caddr tree)
                          :parens parens))
                (not-in (ppe-bin "not in" (cadr tree) (caddr tree)
                          :parens parens))
                (attr
                  (if (symbolp (caddr tree))
                    (progn
                      (ppe (cadr tree))
                      (format t ".~a" (caddr tree)))
                    (ppe (cons '|getattr| (cdr tree)))))
                ; TODO: lambda, if, dict, set, list, comprehensions
                ; TODO: await, yield, yield-from
                ; TODO: strings, getitem, slice
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
            ((and (listp tree) (listp (car tree)))
              (progn
                (ppe (car tree) :parens nil)
                (ppe-sep (cdr tree) ", " :parens t)))
            ; TODO: error for unknown?
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
            (if (and (listp tree) (symbolp (car tree)))
              (when tree
                (case (intern (string-upcase (string (car tree))))
                  (progn (mapcar #'pps (cdr tree)))
                  (def ; TODO: decorators
                    (progn
                      (format t "~adef ~a(~{~a~^, ~}):~%"
                        istr (cadr tree) (caddr tree))
                      (ppb (cdddr tree))))
                  (class ; TODO: decorators, keywords
                    (progn
                      (format t "~aclass ~a(~{~a~^, ~}):~%"
                        istr (cadr tree) (caddr tree))
                      (ppb (cdddr tree))))
                  ((return del raise)
                    (progn
                      (format t "~a~a" istr
                        (string-downcase (string (car tree))))
                      (when (cdr tree)
                        (format t " ")
                        (ppe (cadr tree)))
                      (format t "~%")
                      (setf block-has-statement t)))
                  ((= += -= *= @= /= %= **= <<= >>= ^= //=)
                    ; TODO: &= |=
                    (progn
                      (format t "~a" istr)
                      (ppe (cadr tree))
                      (format t " ~a " (car tree))
                      (ppe (caddr tree) :parens nil)
                      (format t "~%")
                      (setf block-has-statement t)))
                  (for
                    (progn
                      (format t "~afor " istr)
                      (ppe (cadr tree))
                      (format t " in ")
                      (ppe (caddr tree))
                      (format t ":~%")
                      (ppb (cdddr tree))
                      (setf block-has-statement t)))
                  (for-else
                    (progn
                      (format t "~afor " istr)
                      (ppe (second tree))
                      (format t " in ")
                      (ppe (third tree))
                      (format t ":~%")
                      (ppb (fourth tree))
                      (format t "~aelse:~%" istr)
                      (ppb (fifth tree))
                      (setf block-has-statement t)))
                  (while
                    (progn
                      (format t "~awhile " istr)
                      (ppe (cadr tree))
                      (format t ":~%")
                      (ppb (cddr tree))
                      (setf block-has-statement t)))
                  (while-else
                    (progn
                      (format t "~awhile " istr)
                      (ppe (second tree))
                      (format t ":~%")
                      (ppb (third tree))
                      (format t "~aelse:~%" istr)
                      (ppb (fourth tree))
                      (setf block-has-statement t)))
                  (cond
                    (let ((ln (length (cdr tree))))
                      (loop for subtree on (cdr tree) do
                        (let ((curln (length subtree)))
                          (if (and (> ln 1) (= curln 1))
                            (progn
                              (format t "~aelse:~%" istr)
                              (ppb subtree))
                            (progn
                              (format t "~a~a " istr
                                (if (= ln curln) "if" "elif"))
                              (ppe (caar subtree))
                              (format t ":~%")
                              (ppb (cdar subtree))))))
                      (setf block-has-statement t)))
                  (with
                    (progn
                      (format t "~awith " istr)
                      (let* ((items (cadr tree))
                              (first (car items))
                              (ln (length items)))
                        (when (> ln 1) (format t "("))
                        (ppe (cadr first) :parens nil)
                        (format t " as ")
                        (ppe (car first) :parens nil)
                        (loop for it in (cdr items) do
                          (progn
                            (format t ", ")
                            (ppe (cadr it) :parens nil)
                            (format t " as ")
                            (ppe (car it) :parens nil)))
                        (when (> ln 1) (format t ")"))
                        (format t ":~%"))
                      (ppb (cddr tree))))
                  ; TODO: try, assert
                  ((import global nonlocal)
                    (progn
                      (format t "~a~a " istr
                        (string-downcase (string (car tree))))
                      (ppe-sep (cdr tree) ", ")
                      (format t "~%")
                      (setf block-has-statement t)))
                  (import-from
                    (progn
                      (format t "~afrom " istr)
                      (ppe (cadr tree) :parens nil)
                      (format t " import ")
                      (ppe-sep (cddr tree) ", ")
                      (format t "~%")
                      (setf block-has-statement t)))
                  ((pass break continue)
                    (progn
                      (format t "~a~a~%" istr
                        (string-downcase (string (car tree))))
                      (setf block-has-statement t)))
                  (otherwise
                    (progn
                      (format t "~a" istr)
                      (ppe tree)
                      (format t "~%")
                      (setf block-has-statement t)))))
              (progn
                (format t "~a" istr)
                (ppe tree)
                (format t "~%")
                (setf block-has-statement t))))))
      (ppb tree))))

(let ((orig (parse-file "test.lisp")))
  (print orig)
  (format t "~%###########~%")
  (let ((post-eval (eval-macros orig)))
    (print post-eval)
    (format t "~%###########~%")
    (let ((post-apply (apply-macros post-eval)))
      (print post-apply)
      (format t "~%###########~%")
      (prettyprint post-apply))))
