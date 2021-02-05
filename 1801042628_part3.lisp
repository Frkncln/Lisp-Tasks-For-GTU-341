

;writefile
(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string  (file-length instream))))
        (read-sequence  string instream)
        
        string)) 
    )
 )

;writefile
(defun writeToFile (filename content)
  (with-open-file (stream  filename 
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create )
  (format stream content)

  )


)

(defun collatz(x);collatz calculating

	(block outer
		;(format t "~D " x)
    (writeToFile  "/home/frkn/Desktop/Assignment/collatz_outputs.txt" (concatenate 'string  (write-to-string x) " " ))
		(if (= x 1)
       (return-from outer))
		(cond
	       ((= (mod x 2) 0) (+ (collatz (/ x 2)) 1))
    ((> x 1) (+ (collatz (+ (* 3 x) 1)) 1 ))

	  )

	)
	x
)
 ;reading file for inputs
 (setq str  (read-file "/home/frkn/Desktop/Assignment/integer_inputs.txt"))

(format t "inputs ~s" str)



(defun stringer (s);making array with numbers
  (let ((*read-eval* nil))             
    (with-input-from-string (in s)
      (loop for e = (read in nil in)
          for n upfrom 0
        until (eq e in)
        collect e into es
          finally (return (make-array n :initial-contents es))))
  ))





(setf arr (make-array'(20)))

(setf arr2 (stringer str))

(setq size (- (length arr2) 1))

(loop for i from 0 to size

  do(writeToFile  "/home/frkn/Desktop/Assignment/collatz_outputs.txt" (concatenate 'string  (write-to-string (aref arr2 i)) ": " ))
  do(collatz (aref arr2 i))
  do(writeToFile  "/home/frkn/Desktop/Assignment/collatz_outputs.txt" "~%")

)


 