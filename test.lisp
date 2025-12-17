(def x (a b)
  (print "%s
%s" a b))

(def y (c))

(defmacro z (a &rest b) `(blah ,a ,@b))

(z 1 2 3)

(Abc 1 2 3)

(class Point (object)
       (def __init__ (self x y)
	 (= (attr self x) x)
	 ; TODO: this line gives invalid Python
	 (= (attr self (+ y z)) y)))
