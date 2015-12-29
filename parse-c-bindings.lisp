(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defun chomp (str)
  (ppcre:regex-replace-all
   (ppcre:create-scanner "^\\s+|\\s+$" :extended-mode t)
   str ""))

(DEFUN PROCESS-ARGS (ARGS)
  (LOOP WITH RE = (CL-PPCRE:CREATE-SCANNER "(.+)\\s+([&*])*(.+)")
     FOR I IN (CL-PPCRE:SPLIT "," ARGS)
     COLLECT (map 'list
		  #'chomp
		  (COERCE
		   (SECOND (MULTIPLE-VALUE-LIST (CL-PPCRE:SCAN-TO-STRINGS RE I)))
		   'LIST))))

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
		   
				
