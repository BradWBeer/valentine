(in-package :valentine)


(defmacro make-btVector3 (x y z)
    (cffi:with-foreign-objects ((xx :float)
				(yy :float)
				(zz :float))
      (setf (cffi:mem-aref xx :float) (coerce x  'single-float))
      (setf (cffi:mem-aref yy :float) (coerce y  'single-float))
      (setf (cffi:mem-aref zz :float) (coerce z  'single-float))
      
      (valentine::new_btvector3 xx yy zz)))

