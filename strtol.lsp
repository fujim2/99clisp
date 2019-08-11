;;
;; strtol.lsp
;; (strtol "123+44" 10)
;;  -> ("123" "+44")
;;
(defun seq(n)
   (labels (
              (seq-iter (n cnt seqlist)
                 (if (= cnt n)  
                     seqlist 
                     (seq-iter n (+ cnt 1) (cons 
                                              (write-to-string cnt)
                                              seqlist))
                 )
              )
           )
           (seq-iter n 0 '())
   )
)

(defun check-radix (c radix)
    (find c (seq radix) :test #'string=)
)

;;
;; (strtol "123+44" 10)
;;  -> ("123" "+44")
(defun strtol(str radix)
  (let ((charlist (concatenate 'list str)))
    (labels ((strtol-iter (cutlist restlist)
                (cond ((null restlist)
                           (list (concatenate 'string cutlist) 
                                 (concatenate 'string restlist)))
                      ((check-radix (car restlist) 10) 
                           (strtol-iter (append cutlist (list (car restlist))) (cdr restlist)))
                      (t   
                           (list (concatenate 'string cutlist) 
                                 (concatenate 'string restlist)))
                 )
            ))
            (strtol-iter '() charlist )
     )
  )
)

;;(format t "case 1 : ~A~%" (seq 5))
;;(format t "case 2 : ~A~%" (find "0" '("0" "1" "2" "3" "4") :test #'string=))
;;(format t "case 3 : ~A~%" (find "0" (seq 5) :test #'string=))
;;(format t "case 4 : ~A~%" (find "4" (seq 5) :test #'string=))
;;(format t "case 5 : ~A~%" (find "5" (seq 5) :test #'string=))
;;(format t "case 6 : ~A~%" (find "a" (seq 5) :test #'string=))
;;(format t "case 7 : ~A~%" (check-radix  "3" 8))
;;(format t "case 8 : ~A~%" (check-radix  "8" 8))
;;(format t "case 8 : ~A~%" (check-radix  "8" 8))

;;(format t "strtol(123+4) result=~A ~A ~%" 
;;                       (car (strtol "123+4" 10)) 
;;                       (cadr (strtol "123+4" 10)))
