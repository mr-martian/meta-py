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
         (block-has-statement nil)
         (out-files nil)
         (stream-stack '(t)))
    (labels
      ((escape-string (s)
         (concatenate 'string
           (loop for c across s
             appending (case c
                         (#\Newline '(#\\ #\n))
                         (otherwise (list c))))))
        (name (sym)
          (string-downcase (string sym)))
        (output (&rest args)
          (apply #'format (cons (car stream-stack) args)))
        (ppe-sep (sequence separator &key (parens nil))
          (when parens (output "("))
          (when sequence
            (ppe (car sequence))
            (loop for item in (cdr sequence)
              do (progn
                   (output "~a" separator)
                   (ppe item))))
          (when parens (output ")")))
        (ppe-bin (op a b &key (parens t))
          (when parens (output "("))
          (ppe a)
          (output " ~a " op)
          (ppe b)
          (when parens (output ")")))
        (ppe-unary (op x &key (parens t))
          (when parens (output "("))
          (output "~a" op)
          (ppe x)
          (when parens (output ")")))
        (ppe (tree &key (parens t))
          (cond
            ((symbolp tree) (output "~a" tree))
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
                      (output ".~a" (caddr tree)))
                    (ppe (cons '|getattr| (cdr tree)))))
                ; TODO: lambda, if, dict, set, list, comprehensions
                ; TODO: await, yield, yield-from
                ; TODO: strings, getitem, slice
                (otherwise
                  (progn
                    (output "~a" (car tree))
                    (ppe-sep (cdr tree) ", " :parens t)))))
            ((stringp tree)
              (output "~a"
                (concatenate 'string '(#\")
                  (escape-string tree)
                  '(#\"))))
            ((numberp tree) (output "~a" tree))
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
            (output "~v{~a~:*~}pass~%" indent '("    ")))
          (setf block-has-statement t)
          (decf indent))
        (pps (tree)
          (let ((istr (format nil "~v{~a~:*~}" indent '("    "))))
            (if (and (listp tree) (symbolp (car tree)))
              (when tree
                (case (intern (string-upcase (string (car tree))))
                  (with-secondary-file
                    (let ((cur-indent indent))
                      (setf indent -1)
                      (push (cdr (assoc (cadr tree) out-files))
                        stream-stack)
                      (ppb (cddr tree))
                      (pop stream-stack)
                      (setf indent cur-indent)))
                  (progn (mapcar #'pps (cdr tree)))
                  (def ; TODO: decorators
                    (progn
                      (output "~adef ~a(~{~a~^, ~}):~%"
                        istr (cadr tree) (caddr tree))
                      (ppb (cdddr tree))))
                  (class ; TODO: decorators, keywords
                    (progn
                      (output "~aclass ~a(~{~a~^, ~}):~%"
                        istr (cadr tree) (caddr tree))
                      (ppb (cdddr tree))))
                  ((return del raise)
                    (progn
                      (output "~a~a" istr
                        (string-downcase (string (car tree))))
                      (when (cdr tree)
                        (output " ")
                        (ppe (cadr tree)))
                      (output "~%")
                      (setf block-has-statement t)))
                  ((= += -= *= @= /= %= **= <<= >>= ^= //=)
                    ; TODO: &= |=
                    (progn
                      (output "~a" istr)
                      (ppe (cadr tree))
                      (output " ~a " (car tree))
                      (ppe (caddr tree) :parens nil)
                      (output "~%")
                      (setf block-has-statement t)))
                  (for
                    (progn
                      (output "~afor " istr)
                      (ppe (cadr tree))
                      (output " in ")
                      (ppe (caddr tree))
                      (output ":~%")
                      (ppb (cdddr tree))
                      (setf block-has-statement t)))
                  (for-else
                    (progn
                      (output "~afor " istr)
                      (ppe (second tree))
                      (output " in ")
                      (ppe (third tree))
                      (output ":~%")
                      (ppb (fourth tree))
                      (output "~aelse:~%" istr)
                      (ppb (fifth tree))
                      (setf block-has-statement t)))
                  (while
                    (progn
                      (output "~awhile " istr)
                      (ppe (cadr tree))
                      (output ":~%")
                      (ppb (cddr tree))
                      (setf block-has-statement t)))
                  (while-else
                    (progn
                      (output "~awhile " istr)
                      (ppe (second tree))
                      (output ":~%")
                      (ppb (third tree))
                      (output "~aelse:~%" istr)
                      (ppb (fourth tree))
                      (setf block-has-statement t)))
                  (cond
                    (let ((ln (length (cdr tree))))
                      (loop for subtree on (cdr tree) do
                        (let ((curln (length subtree)))
                          (if (and (> ln 1) (= curln 1))
                            (progn
                              (output "~aelse:~%" istr)
                              (ppb subtree))
                            (progn
                              (output "~a~a " istr
                                (if (= ln curln) "if" "elif"))
                              (ppe (caar subtree))
                              (output ":~%")
                              (ppb (cdar subtree))))))
                      (setf block-has-statement t)))
                  (with
                    (progn
                      (output "~awith " istr)
                      (let* ((items (cadr tree))
                              (first (car items))
                              (ln (length items)))
                        (when (> ln 1) (output "("))
                        (ppe (cadr first) :parens nil)
                        (output " as ")
                        (ppe (car first) :parens nil)
                        (loop for it in (cdr items) do
                          (progn
                            (output ", ")
                            (ppe (cadr it) :parens nil)
                            (output " as ")
                            (ppe (car it) :parens nil)))
                        (when (> ln 1) (output ")"))
                        (output ":~%"))
                      (ppb (cddr tree))))
                  ; TODO: try, assert
                  ((import global nonlocal)
                    (progn
                      (output "~a~a " istr
                        (string-downcase (string (car tree))))
                      (ppe-sep (cdr tree) ", ")
                      (output "~%")
                      (setf block-has-statement t)))
                  (import-from
                    (progn
                      (output "~afrom " istr)
                      (ppe (cadr tree) :parens nil)
                      (output " import ")
                      (ppe-sep (cddr tree) ", ")
                      (output "~%")
                      (setf block-has-statement t)))
                  ((pass break continue)
                    (progn
                      (output "~a~a~%" istr
                        (string-downcase (string (car tree))))
                      (setf block-has-statement t)))
                  (otherwise
                    (progn
                      (output "~a" istr)
                      (ppe tree)
                      (output "~%")
                      (setf block-has-statement t)))))
              (progn
                (output "~a" istr)
                (ppe tree)
                (output "~%")
                (setf block-has-statement t))))))
      (if (and
            (listp tree)
            (listp (car tree))
            (symbolp (caar tree))
            (string= (name (caar tree)) "set-files"))
        (let* ((files (car tree))
                (main (cadr files))
                (rest (cddr files)))
          (push (cons nil (open main :direction :output
                            :if-exists :overwrite
                            :if-does-not-exist :create))
            out-files)
          (push (cdar out-files) stream-stack)
          (loop for pair in rest do
            (push (cons (car pair)
                    (open (cadr pair) :direction :output
                      :if-exists :overwrite
                      :if-does-not-exist :create))
              out-files))
          (ppb (cdr tree))
          (loop for pair in out-files do (close (cdr pair))))
        (ppb tree)))))

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
