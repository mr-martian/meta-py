(defmacro octothorpe (name)
  `(def ,name (x)
     (return (* 4 x))))
