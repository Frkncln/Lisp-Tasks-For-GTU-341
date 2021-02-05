
  


(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream) string))
 ))


;**********************************************
(defun read-file-to-list (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((list (make-list (file-length instream))))
        (read-sequence list instream)
        
        list))
      
    )
 )




(defun writeToFile (filename content)
  (with-open-file (stream  filename :external-format charset:iso-8859-1
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create )
  (format stream content)

  ))

(defun read-and-flatten(filename)
(with-open-file (stream filename)
  (setq str (write-to-string(flatten-list (read (make-concatenated-stream (make-string-input-stream "(")
                                  stream
                                  (make-string-input-stream ")")))))

      )
    (return-from read-and-flatten str)
    )
)


(defun flatten-list (x)

(if (null x)
  '()
    (if (atom (car x))
        (cons (car x) (flatten-list (cdr x)))
        (append (flatten-list (car x)) (flatten-list (cdr x)))))

)

 (defun tokenize (string)
  (loop
     for start = 0 then (+ space 1)
     for space = (position #\space string :start start)
     for token = (subseq string start space)
     unless (string= token " ") collect token
     until (not space)))



(with-input-from-string (open "(")
        (with-input-from-string (close ")")
          (with-open-file (file "/home/frkn/Desktop/Assignment/nested_list.txt")
            (read (make-concatenated-stream open file close))
            
            )
  ))


;********************

  (setq str2 (read-and-flatten "/home/frkn/Desktop/Assignment/nested_list.txt"))
  

(format t str2)
(writeToFile "/home/frkn/Desktop/Assignment/flattened_list.txt"  str2);writing to file
