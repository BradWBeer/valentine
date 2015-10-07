(in-package #:valentine)

;; (let ((path (concatenate
;; 		       'string 
;; 		       (directory-namestring
;; 			(asdf:system-relative-pathname :valentine "valentine.asd"))
;; 		       "libbullet_wrap.so")))
;;   (print path))

;; (cffi:define-foreign-library :libBullet
;;   (cffi-features:unix "/home/warweasle/quicklisp/local-projects/valentine/libbullet_wrap.so"))
				   
(cffi:load-foreign-library "/home/warweasle/quicklisp/local-projects/valentine/libbullet_wrap.so")

