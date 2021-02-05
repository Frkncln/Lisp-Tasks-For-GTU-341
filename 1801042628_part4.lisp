
;structure for tree
(defstruct Huffnode

  item

  freq

   left 

   right

)


;getting letter frequencies from file
(defun letter-freq (file)
  (with-open-file (stream file)
    (let ((str (make-string (file-length stream)))
	     (arr (make-array 12000 :element-type 'integer :initial-element 0)))
      (read-sequence str stream)
      (loop for c across str 
        do (incf (aref arr (char-code c))));we increase char code for next char
      
      (loop for c from 32 to 126 for i from 0 ;we loop 32-126 because of ASCII
        do(if (/= 0 (aref arr c))
          (format t "~c: ~d i=~d~%" (code-char c) (aref arr c) i)           
        ))
      (return-from letter-freq arr)
    )
  )

)

(format t "frequencies of letters :~%")
(setq arr2 (letter-freq "/home/frkn/Desktop/Assignment/paragraph.txt"))
(setq size (- (length arr2) 1))



(setq arr3 (make-array (length arr2) :fill-pointer 0))



;pushing elements to vector

(loop for i from 32 to  126

  do(if (/= 0 (aref arr2 i)) 
          
            (setq huff(make-Huffnode 
                :item (code-char i)
                :freq (aref arr2 i)
                :left nil
                :right nil
              )
            )
    )
    do(if (/= 0 (aref arr2 i))
           (vector-push huff arr3)   

      )
)



;printing tree with traversal


(defun printhuff  (root srt)

(block outer
  (if (and (= (Huffnode-left root) nil) (= (Huffnode-right root) nil))

    (format t "~c : ~s~%" (Huffnode-item root) srt)


    (return-from outer)

    )


    ;recursive for left we add 0,for right add 1
    (printhuff (Huffnode-left root) (concatenate 'string  srt "0" ))
    (printhuff (Huffnode-right root) (concatenate 'string  srt "1" ))
    
    
    ))



;sorting arr with freqs
(sort arr3 #'> :key #'Huffnode-freq)


;(write arr3)
(setq i 1)
(setq huffroot (make-Huffnode))




(loop while (> (length arr3) 1)


  do(setq huffx  (aref arr3 (- (length arr3) i)))


 do(vector-pop arr3)
 
  do(setq huffy (aref arr3 (- (length arr3) i)))


   do(vector-pop arr3)
  
     do(setq huffl (make-Huffnode 
                :item  '-'
                :freq (+ (Huffnode-freq huffx) (Huffnode-freq huffy))
                :left huffx
                :right huffy
              ))

    do(vector-push huffl arr3)

    do(setf  huffroot(make-Huffnode 
                :item  (Huffnode-item huffl)
                :freq (Huffnode-freq huffl )
                :left (Huffnode-left huffl )
                :right (Huffnode-right huffl )
              ))


  (setq i 2)
  do(if(= (length arr3) 2)
  (setq i 1))

)


(printhuff 'huffroot "")








