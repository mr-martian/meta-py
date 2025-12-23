(def x (a b)
  (print "%s
%s" a b))

(def y (c))

(defmacro z (a &rest b) `(blah ,a ,@b))

(z 1 2 3)

(Abc 1 2 3)

(= const "yyy")

(class Point (object)
       (def __init__ (self x y)
	 (= (attr self x) x)
	 (= (attr self y) y)
	 (global const)
	 (+= (attr self y) const)
	 ((attr self potato) 3 4)))

(defmacro blah2 (a b)
  #$(if (= #!a 3)
	#!b
	#!`(+ ,b 7)))

(= quagle (blah2 3 9))
(= quagle (blah2 4 quagle))

(for x (range 10)
     (print x)
     (cond
       ((== (% x 2) 1) (print "ODD!"))
       (print "EVEN!"))
     (print (** x 3)))

(import-from collections Counter defaultdict)

(with ((fin (open "potato.txt")))
      (print ((attr fin read))))
(with ((fin (open "potato.txt" "r"))
       (fout (open "potato.txt" "w")))
      ((attr fout write) ((attr fin read))))

(defmacro file-lines (var file &body body)
  #$(let* ((fobj (gensym))
	   (fargs (if (listp file) file (list file))))
      #!`(with ((#$,fobj (open ,@#$fargs)))
	       (for ,var #$,fobj ,@body))))

(file-lines ln "potato.txt"
	    (print ((attr ln strip))))
(file-lines ln ("potato.txt" "rw+")
	    (print ((attr ln strip))))
