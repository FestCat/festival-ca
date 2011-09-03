
;;; Tranformador de dígits a dates (xx/xx/xxxx)
;;; (require 'upc_catalan_numbers)

(define (catala_date name)
"(catala_date name)
Converteix una cadena de dígits en una llista de paraules per dir la data."
 (set! day (string-before name "/"))
 (set! month (number_month (string-before (string-after name "/") "/")))
   (cond 
    ((or (string-equal month "d'abril") (string-equal month "d'agost"))
     (append (list (car (catala_number day "0")) month "del") (catala_number (string-after (string-after name "/") "/") "0"))) 
     (t (append (list (car (catala_number day "0")) "de" month "del") (catala_number (string-after (string-after name "/") "/") "0"))))) 


(define (number_month name)
;;(string-append "Gener")

(if (equal? (length name) 1) (set! name (string-append "0" name))) 
(cond 
        ((string-equal name "01") (string-append "gener"))
        ((string-equal name "02") (string-append "febrer"))
	((string-equal name "03") (string-append "març"))
        ((string-equal name "04") (string-append "d'abril"))
        ((string-equal name "05") (string-append "maig"))
        ((string-equal name "06") (string-append "juny"))
	((string-equal name "07") (string-append "juliol"))
        ((string-equal name "08") (string-append "d'agost"))
	((string-equal name "09") (string-append "setembre"))  
        ((string-equal name "10") (string-append "octubre"))	
        ((string-equal name "11") (string-append "novembre"))
        ((string-equal name "12") (string-append "desembre"))
         (t (string-append name))))


(provide 'upc_catalan_date)
