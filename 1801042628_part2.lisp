
;reading file with given loc
(defun read-file (infile)
  (with-open-file (instream infile :direction :input 
    :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string  (file-length instream))))
        (read-sequence  string instream)
        
        string)) 
    )
 )



;array for getting boundaries
(setf arr (make-array'(2)))

(setq str  (read-file "/home/frkn/Desktop/Assignment/boundries.txt"))


(setq x (read-from-string str))

(setq y (read-from-string str t nil :start 2))

(format t "Boundaries ~% ~D--~D " x y)


(setq r 0)


 
(defun primep (n &optional (a 2))
  (cond ((> a (isqrt n)) t)
        ((zerop (rem n a)) nil)
        (t (primep n (+ a 1)))))

(defun is_prime(n);check for prime or not
  (setq flag 0)
  (loop for i from 2 to (- n 1) do
     (if (= (mod n i) 0)
         (setq flag 1)
    ))

  (if (< n 2) (setq flag 1))

 flag
)


(defun semiprimep (n &optional (a 2))
  (cond ((> a (isqrt n)) nil)
        ((zerop (rem n a)) (and (primep a) (primep (/ n a))))
        (t (semiprimep n (+ a 1))))
  )

;writing to file with given loc
(defun writeToFile (filename content)
  (with-open-file (stream  filename 
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create )
  (format stream content)
  (terpri stream))

)


(loop for i from x to y
     
     do(if(= (is_prime i) 0);writing primes and semiprimes with loop
            (writeToFile  "/home/frkn/Desktop/Assignment/primedistribution.txt" (concatenate 'string  (write-to-string i) " is prime" ))

        )
     do(if(semiprimep i)
          (writeToFile  "/home/frkn/Desktop/Assignment/primedistribution.txt" (concatenate 'string  (write-to-string i) " is semiprime" ))

    ))
(format t "~% numbers writed to file")


