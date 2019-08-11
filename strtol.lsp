;;
;; strtol.lsp
;; 

;;
;; (seq 4)
;; -> ("3" "2" "1" "0")
;;
(defun seq(n)
  (labels (
      (seq-iter (n cnt seqlist)
        (if (= cnt n)  
          seqlist 
          (seq-iter n (+ cnt 1) (cons (write-to-string cnt)
                                              seqlist)))))
    (seq-iter n 0 '())))

;; (check-radix "6" 5)
;;  -> NIL
;; (check-radix "3" 5)
;;  -> "3"
(defun check-radix (c radix)
  (find c (seq radix) :test #'string=))

;;
;; (strtol "123+44" 10)
;;  -> ("123" "+44")
(defun strtol(str radix)
  (let ((charlist (concatenate 'list str)))
    (labels (
      (strtol-iter (cutlist restlist)
        (cond ((null restlist)
               (list (concatenate 'string cutlist) 
                     (concatenate 'string restlist)))
              ((check-radix (car restlist) radix) 
                (strtol-iter (append cutlist (list (car restlist))) (cdr restlist)))
              (t   
                (list (concatenate 'string cutlist) 
                      (concatenate 'string restlist))))))
      (strtol-iter '() charlist ))))
