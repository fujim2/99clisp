(load "strtol.lsp")
;;
;; token structure
;; (kind val str)
;;
;; token(initial) = ("3" "+" "4" "+" "5")
;; token(current)  =         ("4" "+" "5")
;;
(defun new_token(kind str cur)
   (append cur (list (list kind str))))

(defun get-current()
     (car token))

(defun get-kind(token)
     (car token))

(defun get-str(token)
     (cadr token))

(defun get-num(token)
     (let ((str (parse-integer (get-str token) :junk-allowed T)))
         (if (null str)
               (format t "token ~A not number! ~%" (get-str token))
               str)))

(defun get-next()
     (setf token (cdr token)))

(defvar token  '())

(defun tokenize(str)
  (labels
    (
      (tokenize-iter (str current)
        (if (string= str "") 
          (let ()
            (setf current (new_token 'TK_EOF "eof" current))
            (setf token current)
          )
          (let ((head (subseq str 0 1)))
            (cond 
              ((string= head    " ")
                (tokenize-iter (subseq str 1) current))
              ((string= head    "+") (tokenize-iter (subseq str 1) 
                (new_token 'TK_RESERVED "+" current)))
              ((string= head    "-") (tokenize-iter (subseq str 1) 
                (new_token 'TK_RESERVED "-" current)))
              ((find head (seq 10) :test #'string=)  
                (tokenize-iter (cadr (strtol str 10)) 
                (new_token 'TK_NUM (car (strtol str 10)) current)))
              (t (format t "couldn't tokenize~A~%" head)))))))
     (tokenize-iter str '())))

(defun expect(op)
   (if (or (not (equal (get-kind (get-current)) 'TK_RESERVED))
           (not (equal (get-str  (get-current))  op)))
       (format t "not ~A~%" op)
       (let((op (get-str (get-current)))) 
           (get-next)
           op)))

(defun expect-number()
   (if (not (equal (get-kind (get-current)) 'TK_NUM))
       (format t "~A is not number~%" (get-str (get-current)))
       (let ((val (get-num (get-current))))
            (get-next)
            val)))

(defun at-eof()
   (equal (get-kind (get-current)) 'TK_EOF))

(defun consume(op)
  (if (or (not (equal (get-kind (get-current)) 'TK_RESERVED))
          (not (equal (get-str  (get-current)) op)))
       nil
       (let ()
          (get-next)
           t)))

(defun str-iter(str n) 
    (if (= n 0) "" (concatenate 'string str (sp (- n 1)))))

(defun 99c(code)

  (tokenize code)

  (format t ".intel_syntax noprefix~%")
  (format t ".global main~%")
  (format t "main:~%")

  (format t "  mov rax, ~D~%" (expect-number))

  (labels (
     (compile-iter()
        (if (not(at-eof))
           (let ()
              (if (consume "+")
                  (format t "  add rax, ~D~%" (expect-number)))
              (if (consume "-")
                  (format t "  sub rax, ~D~%" (expect-number)))))))
     (compile-iter)
     (format t "  ret~%")))

(defun main()
  (if (not (= (length *args*) 1)) 
      (print (format t "bad number of arguments ~%" *args*))
      (99c (car *args*))))

(main)
