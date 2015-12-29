(ql:quickload :cffi)
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defvar *function-hash* (make-hash-table :test 'equal))


(defmacro add-to-hash (hash key value)
  (let ((ghash (gensym "hash"))
	(gkey  (gensym "key"))
	(gvalue (gensym "value")))
    
  `(let ((,ghash  ,hash)
	 (,gKey   ,key)
	 (,gvalue ,value))
     (setf (gethash ,gkey ,ghash)
		  (cons ,gvalue
			(gethash ,gkey ,ghash))))))

(defvar *known-types*
  '(("char *" :string)
    ("bool" :boolean)
    ("btScalar" :float)
    ("double" :double)
    ("float" :float)
    ("int" :int)
    ("long" :long)
    ("short" :short)
    ("unsigned int" :unsigned-int)
    ("unsigned long" :unsigned-long)
    ("unsigned short" :unsigned-short)
    ("void" :void)
    ("void *" :pointer)))

(defun remove-known-types (types)
  (remove-if (lambda (x)
	       (member x *known-types* :test (lambda (x y)
					       (print (car y))
					       (string-equal x (car y)))))
	     types))
	
(defun chomp (str)
  (ppcre:regex-replace-all
   (ppcre:create-scanner "^\\s+|\\s+$" :extended-mode t)
   str ""))

(defun parse-file (path)

  (let ((lines (ppcre:split "\\n" (alexandria:read-file-into-string path)))
	(re2 (ppcre:create-scanner "\\s*EXPORT\\s+(.*)_wrap_(\\w+)\\s+\\((.*)\\)" :case-insensitive-mode t :extended-mode t)))

    (loop for l in lines
       for parse = (map 'list
			#'chomp
			(coerce
			 (cadr
			  (multiple-value-list (ppcre:scan-to-strings re2 l)))
			 'list))
       if parse collect (append (list (first parse) (second parse)) (process-args (third parse))))))
		   
				

(defun process-args (args)
  (loop with re = (cl-ppcre:create-scanner "(.+)\\s+([&*])*(.+)")
     for i in (cl-ppcre:split "," args)
     collect (map 'list
		  #'chomp
		  (coerce
		   (second (multiple-value-list (cl-ppcre:scan-to-strings re i)))
		   'list))))

(defun find-argument-types (args)
	   (map 'list (lambda (x)
			(if (second x)
			    (concatenate 'string (car x) " *")
			    (car x)))
		args))


(defun find-data-types (funcs)
  (sort
   (remove-duplicates
    (append
     (sort
      (remove-duplicates
       (map 'list #'first funcs)
       :test #'string-equal)
      #'string-lessp)
  
     (remove-duplicates
      (sort (loop for i in (cddr funcs) 
	       append (find-argument-types (cddr i)))
	    #'string-lessp)
      :test #'string-equal))
    :test #'string-equal)
   #'string-lessp))


(defun find-callbacks (types)

  (let ((callbacks (loop for i in types
		      if (ppcre:scan (ppcre:create-scanner "callback" :case-insensitive-mode t) i) 
		      collect i)))
    (values callbacks
	    (remove-if (lambda (x) (member x callbacks))
		       types))))


(defun find-pointers (types)
  (let ((pointer-re (ppcre:create-scanner " \\*+$" :case-insensitive-mode t))
	(callback-re (ppcre:create-scanner "callback(\\s*\\*)?$" :case-insensitive-mode t)))
	
    (labels ((pointerp (x)
	       (ppcre:scan pointer-re x))
	     (callbackp (x)
	       (ppcre:scan callback-re x))
	     (remove-* (lst)
	       (map 'list 
		    (lambda (x)
		      (ppcre:regex-replace-all "\\s+|::" (ppcre:regex-replace-all pointer-re x "") "_"))
		    lst)))
      
      (let ((pointers (remove-* (remove-if #'callbackp (remove-if-not #'pointerp types))))
	    (non-pointers (remove-if (lambda (x)
				       (member x enums :test (lambda (x y)
							       (string-equal x (second y)))))

				     (remove-* (remove-if #'callbackp (remove-if #'pointerp types)))))
	    (callbacks (remove-* (remove-if-not #'callbackp (remove-if-not #'pointerp types)))))
	(values pointers non-pointers callbacks)))))
	
;; (IN-PACKAGE DEFMACRO EVAL-WHEN CFFI:DEFCVAR DEFCONSTANT CFFI:DEFCENUM
;;            CFFI:DEFCFUN CFFI:DEFCSTRUCT)

(defun read-bindings (path)
  (alexandria:with-input-from-file (in path)
    (loop for s = (read in nil nil)
       while s 
       collect s)))

(defun find-bindings-of-type (bindings type)
  (loop for i in bindings
       if (equal (car i) type)
       collect i))

(defun find-enums (bindings)
  (find-bindings-of-type bindings 'CFFI:DEFCENUM))

(defun find-constants (bindings)
  (find-bindings-of-type bindings 'defconstant))

(defun find-structs (bindings)
  (find-bindings-of-type bindings 'CFFI:defcstruct))

(defun find-empty-structs (structs)
  (remove-if (lambda (x)
	       (> (length x) 2))
	     structs))

(defun find-non-empty-structs (structs)
  (remove-if-not (lambda (x)
	       (> (length x) 2))
	     structs))



(defun find-vars (bindings)
  (find-bindings-of-type bindings 'CFFI:defcvar))


(defvar C++Functions (parse-file "~/quicklisp/local-projects/valentine/bullet_wrap.cxx"))
(defvar bindings 
  (read-bindings "~/quicklisp/local-projects/valentine/swigbullet.lisp"))

(defvar enums (find-enums bindings))
(defvar data-types (remove-known-types (find-data-types C++FUNCTIONS)))
(defvar structs (find-structs bindings))

(defvar pointers)
(defvar non-pointers)
(defvar callbacks)
(multiple-value-setq  (pointers non-pointers callbacks) (find-pointers data-types))


