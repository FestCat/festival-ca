;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                     Carnegie Mellon University                      ;;;
;;;                  and Alan W Black and Kevin Lenzo                   ;;;
;;;                      Copyright (c) 1998-2000                        ;;;
;;;                        All Rights Reserved.                         ;;;
;;;                                                                     ;;;
;;; Permission is hereby granted, free of charge, to use and distribute ;;;
;;; this software and its documentation without restriction, including  ;;;
;;; without limitation the rights to use, copy, modify, merge, publish, ;;;
;;; distribute, sublicense, and/or sell copies of this work, and to     ;;;
;;; permit persons to whom this work is furnished to do so, subject to  ;;;
;;; the following conditions:                                           ;;;
;;;  1. The code must retain the above copyright notice, this list of   ;;;
;;;     conditions and the following disclaimer.                        ;;;
;;;  2. Any modifications must be clearly marked as such.               ;;;
;;;  3. Original authors' names are not deleted.                        ;;;
;;;  4. The authors' names are not used to endorse or promote products  ;;;
;;;     derived from this software without specific prior written       ;;;
;;;     permission.                                                     ;;;
;;;                                                                     ;;;
;;; CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK        ;;;
;;; DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING     ;;;
;;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT  ;;;
;;; SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE     ;;;
;;; FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   ;;;
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  ;;;
;;; AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,         ;;;
;;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      ;;;
;;; THIS SOFTWARE.                                                      ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tranformador de digits a nombres

(define (catala_number_ordinals name kind)
"(catala_number_ordinals name kind)
Retorna la forma cardinal del nombre 
NAME = número
KIND = gènere&nombre = 0     masculí singular
		       1     femení singular
                       2     masculí plural
                       3     femení plural
                      altres  error"

(let ((num (parse-number (intern name))))
  (if (>= num 1000000) (list "número" "massa" "gran")
  (cond
;; Masculí singular
((string-equal kind "0")
      (cond
    	((string-matches name "1") (list "primer"))
    	((string-matches name "2") (list "segon"))
    	((string-matches name "3") (list "tercer"))
    	((string-matches name "4") (list "quart"))
    	((string-matches name "5") (list "cinquè"))
    	((string-matches name "9") (list "novè"))
    	((string-matches name "10") (list "desè"))
    	(t 
            (cond
          	((or    (string-matches name "[0-9]+30") (string-matches name "[0-9]+40") (string-matches name "[0-9]+50")
			(string-matches name "[0-9]+60") (string-matches name "[0-9]+70") (string-matches name "[0-9]+80")
			(string-matches name "[0-9]+90") (string-matches name "[0-9]+4")  (string-matches name "[0-9]+11")
			(string-matches name "[0-9]+12") (string-matches name "[0-9]+13") (string-matches name "[0-9]+14")
			(string-matches name "[0-9]+15") (string-matches name "[0-9]+16") 
			(string-matches name "30") 	 (string-matches name "40") 	  (string-matches name "12")
			(string-matches name "50") 	 (string-matches name "60") 	  (string-matches name "70") 
			(string-matches name "80") 	 (string-matches name "90") 	  (string-matches name "11") 
		 	(string-matches name "13") 	 (string-matches name "14")       (string-matches name "15") 
			(string-matches name "16"))
            		(reverse (append (list (catala_sufix (car (reverse (catala_number name "0"))) "è" "1")) (cdr (reverse (catala_number name "0"))))))
		((string-matches name "[0-9]+5")
              		(reverse (append (list (catala_sufix (car (reverse (catala_number name "0"))) "què" "1")) (cdr (reverse (catala_number name "0"))))))
           	((string-matches name "[0-9]+9")
              		(reverse (append (list (catala_sufix (car (reverse (catala_number name "0"))) "vè" "1")) (cdr (reverse (catala_number name "0"))))))
		((string-matches name "[0-9]+10")
              		(reverse (append (list (catala_sufix (car (reverse (catala_number name "0"))) "sè" "1")) (cdr (reverse (catala_number name "0"))))))
                (t (reverse (append (list (catala_sufix (car (reverse (catala_number name "0"))) "è" "0")) (cdr (reverse (catala_number name "0"))))))))))
 
;; Femení singular
   ((string-equal kind "1")
      (cond
    	((string-matches name "1") (list "primera"))
    	((string-matches name "2") (list "segona"))
    	((string-matches name "3") (list "tercera"))
    	((string-matches name "4") (list "quarta"))
    	((string-matches name "5") (list "cinquena"))
    	((string-matches name "9") (list "novena"))
    	((string-matches name "10") (list "desena"))
    	(t 
            (cond
          	((or    (string-matches name "[0-9]+30") (string-matches name "[0-9]+40") (string-matches name "[0-9]+50")
			(string-matches name "[0-9]+60") (string-matches name "[0-9]+70") (string-matches name "[0-9]+80")
			(string-matches name "[0-9]+90") (string-matches name "[0-9]+4")  (string-matches name "[0-9]+11")
			(string-matches name "[0-9]+12") (string-matches name "[0-9]+13") (string-matches name "[0-9]+14")
			(string-matches name "[0-9]+15") (string-matches name "[0-9]+16") (string-matches name "[0-9]+1")
			(string-matches name "30") 	 (string-matches name "40") 	  (string-matches name "12")
			(string-matches name "50") 	 (string-matches name "60") 	  (string-matches name "70") 
			(string-matches name "80") 	 (string-matches name "90") 	  (string-matches name "11") 
		 	(string-matches name "13") 	 (string-matches name "14")       (string-matches name "15") 
			(string-matches name "16"))
            		(reverse (append (list (catala_sufix (car (reverse (catala_number name "1"))) "ena" "1")) (cdr (reverse (catala_number name "1"))))))
		((string-matches name "[0-9]+5")
              		(reverse (append (list (catala_sufix (car (reverse (catala_number name "1"))) "quena" "1")) (cdr (reverse (catala_number name "1"))))))
           	((string-matches name "[0-9]+9")
              		(reverse (append (list (catala_sufix (car (reverse (catala_number name "1"))) "vena" "1")) (cdr (reverse (catala_number name "1"))))))
		((string-matches name "[0-9]+10")
              		(reverse (append (list (catala_sufix (car (reverse (catala_number name "1"))) "sena" "1")) (cdr (reverse (catala_number name "1"))))))
                (t (reverse (append (list (catala_sufix (car (reverse (catala_number name "0"))) "ena" "0")) (cdr (reverse (catala_number name "1"))))))))))

;; Masculí plural
((string-equal kind "2")
      (cond
    	((string-matches name "1") (list "primers"))
    	((string-matches name "2") (list "segons"))
    	((string-matches name "3") (list "tercers"))
    	((string-matches name "4") (list "quarts"))
    	((string-matches name "5") (list "cinquens"))
    	((string-matches name "9") (list "novens"))
    	((string-matches name "10") (list "desens"))
    	(t 
            (cond
          	((or    (string-matches name "[0-9]+30") (string-matches name "[0-9]+40") (string-matches name "[0-9]+50")
			(string-matches name "[0-9]+60") (string-matches name "[0-9]+70") (string-matches name "[0-9]+80")
			(string-matches name "[0-9]+90") (string-matches name "[0-9]+4")  (string-matches name "[0-9]+11")
			(string-matches name "[0-9]+12") (string-matches name "[0-9]+13") (string-matches name "[0-9]+14")
			(string-matches name "[0-9]+15") (string-matches name "[0-9]+16") 
			(string-matches name "30") 	 (string-matches name "40") 	  (string-matches name "12")
			(string-matches name "50") 	 (string-matches name "60") 	  (string-matches name "70") 
			(string-matches name "80") 	 (string-matches name "90") 	  (string-matches name "11") 
		 	(string-matches name "13") 	 (string-matches name "14")       (string-matches name "15") 
			(string-matches name "16"))
            		(reverse (append (list (catala_sufix (car (reverse (catala_number name "0"))) "ens" "1")) (cdr (reverse (catala_number name "0"))))))
		((string-matches name "[0-9]+5")
              		(reverse (append (list (catala_sufix (car (reverse (catala_number name "0"))) "quens" "1")) (cdr (reverse (catala_number name "0"))))))
           	((string-matches name "[0-9]+9")
              		(reverse (append (list (catala_sufix (car (reverse (catala_number name "0"))) "vens" "1")) (cdr (reverse (catala_number name "0"))))))
		((string-matches name "[0-9]+10")
              		(reverse (append (list (catala_sufix (car (reverse (catala_number name "0"))) "sens" "1")) (cdr (reverse (catala_number name "0"))))))
                (t (reverse (append (list (catala_sufix (car (reverse (catala_number name "0"))) "ens" "0")) (cdr (reverse (catala_number name "0"))))))))))

;; Femení plural
   ((string-equal kind "3")
      (cond
    	((string-matches name "1") (list "primeres"))
    	((string-matches name "2") (list "segones"))
    	((string-matches name "3") (list "terceres"))
    	((string-matches name "4") (list "quartes"))
    	((string-matches name "5") (list "cinquenes"))
    	((string-matches name "9") (list "novenes"))
    	((string-matches name "10") (list "desenes"))
    	(t 
            (cond
          	((or    (string-matches name "[0-9]+30") (string-matches name "[0-9]+40") (string-matches name "[0-9]+50")
			(string-matches name "[0-9]+60") (string-matches name "[0-9]+70") (string-matches name "[0-9]+80")
			(string-matches name "[0-9]+90") (string-matches name "[0-9]+4")  (string-matches name "[0-9]+11")
			(string-matches name "[0-9]+12") (string-matches name "[0-9]+13") (string-matches name "[0-9]+14")
			(string-matches name "[0-9]+15") (string-matches name "[0-9]+16") (string-matches name "[0-9]+1")
			(string-matches name "30") 	 (string-matches name "40") 	  (string-matches name "12")
			(string-matches name "50") 	 (string-matches name "60") 	  (string-matches name "70") 
			(string-matches name "80") 	 (string-matches name "90") 	  (string-matches name "11") 
		 	(string-matches name "13") 	 (string-matches name "14")       (string-matches name "15") 
			(string-matches name "16"))
            		(reverse (append (list (catala_sufix (car (reverse (catala_number name "1"))) "enes" "1")) (cdr (reverse (catala_number name "1"))))))
		((string-matches name "[0-9]+5")
              		(reverse (append (list (catala_sufix (car (reverse (catala_number name "1"))) "quenes" "1")) (cdr (reverse (catala_number name "1"))))))
           	((string-matches name "[0-9]+9")
              		(reverse (append (list (catala_sufix (car (reverse (catala_number name "1"))) "venes" "1")) (cdr (reverse (catala_number name "1"))))))
		((string-matches name "[0-9]+10")
              		(reverse (append (list (catala_sufix (car (reverse (catala_number name "1"))) "senes" "1")) (cdr (reverse (catala_number name "1"))))))
                (t (reverse (append (list (catala_sufix (car (reverse (catala_number name "0"))) "enes" "0")) (cdr (reverse (catala_number name "1"))))))))))


      (t (list "Error cardinal"))))))
   
   
             
       
         
          
    




;;       (set! b (catala_number name kind))
;;       (last b)
;;    )))   
       

(define (catala_number_decimals name kind)
"(catala_number_decimals name) 
Tractament de nombres decimals
NAME = dígit
KIND = gènere (1 = femení / altres = masculí)"
(if (string-matches name ".*[,].*")
    (cond
       ((string-matches (string-after name ",") "0.*") 
	(append (catala_number_point (string-before name ",") kind ) '("coma") (catala_speller (string-after name ","))))
        (t (append (catala_number_point (string-before name ",") kind ) '("coma") (catala_number_point (string-after name ",") kind))))
(catala_number_point name kind)))

(define (catala_number_point name control)
;;"(catala_number_point name)
;;Elimina els punts dels números
;;NAME = dígits
;;CONTROL = 2    només retorna el número sense punts en format string
;;          1    número femení 
;;       altres  número masculí"
(cond          
((string-matches name ".*[\.].*") (catala_number_point (string-append (string-before name "\.") (string-after name "\.")) control))
((string-matches control "2") (string-append name))
(t (catala_number name control)))
)

(define (catala_number name kind)
"(catala_number name kind)
Transforma els dígits en paraules
NAME = dígit
KIND = gènere (1 = femení / altres = masculí)"

(if (not (string-matches kind "1"))
   (set! kind "0"))

(if (string-matches name "0")
      (list "zero")
      (catala_number_from_digits (symbolexplode name) kind)))

(define (just_zeros digits)
"(just_zeros digits)
Tractament del cas que tot el nombre siguin 0s "
 (cond
  ((not digits) t)
  ((string-equal "0" (car digits))
   (just_zeros (cdr digits)))
  (t nil)))

(define (catala_number_from_digits digits kind)
  "(catala_number_from_digits digits kind)
Agafa una llista de dígits i la converteix en una llista de paraules dient el número."
  (let ((l (length digits)))
    (cond
     ((equal? l 0)
      nil)
     ((string-equal (car digits) "0") ;; digits = 0x, elimina el 0 i torna a cridar la funció
      (catala_number_from_digits (cdr digits) kind))
     ((equal? l 1);; single digit
      (cond 
       ((string-equal (car digits) "0") (list "zero"))
       ((string-equal (car digits) "1") 
	  (if (string-equal kind "1")
            (list "una")
	    (list "un")))
       ((string-equal (car digits) "2") 
	  (if (string-equal kind "1")
	    (list "dues") 
	    (list "dos")))
       ((string-equal (car digits) "3") (list "tres"))
       ((string-equal (car digits) "4") (list "quatre"))
       ((string-equal (car digits) "5") (list "cinc"))
       ((string-equal (car digits) "6") (list "sis"))
       ((string-equal (car digits) "7") (list "set"))
       ((string-equal (car digits) "8") (list "vuit"))
       ((string-equal (car digits) "9") (list "nou"))
       ;; fill in the rest
       (t (list "Error"))));; $$$ what should say?
     ((equal? l 2);; less than 100
      (cond
       ((string-equal (car digits) "0");; 0x 
	(catala_number_from_digits (cdr digits) kind))
     
       ((string-equal (car digits) "1");; 1x
	(cond
	 ((string-equal (car (cdr digits)) "0") (list "deu"))
	 ((string-equal (car (cdr digits)) "1") (list "onze"))
	 ((string-equal (car (cdr digits)) "2") (list "dotze"))
	 ((string-equal (car (cdr digits)) "3") (list "tretze"))
	 ((string-equal (car (cdr digits)) "4") (list "catorze"))
	 ((string-equal (car (cdr digits)) "5") (list "quinze"))
	 ((string-equal (car (cdr digits)) "6") (list "setze"))
         ((string-equal (car (cdr digits)) "7") (list "disset"))
	 (t 
	  (list  (string-append "di" (car (catala_number_from_digits (cdr digits) kind)))))))
     
       ((string-equal (car digits) "2");; 2x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "vint")
	    (list (string-append  "vint-i-" (car (catala_number_from_digits (cdr digits) kind))))))

       ((string-equal (car digits) "3");; 3x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "trenta")
	    (list (string-append  "trenta-" (car (catala_number_from_digits (cdr digits) kind))))))  


       ((string-equal (car digits) "4");; 4x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "quaranta")
	    (list (string-append  "quaranta-" (car (catala_number_from_digits (cdr digits) kind))))))  	    

       ((string-equal (car digits) "5");; 5x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "cinquanta")
 	    (list (string-append  "cinquanta-" (car (catala_number_from_digits (cdr digits) kind))))))   	      	    	    
       ((string-equal (car digits) "6");; 6x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "seixanta")
            (list (string-append  "seixanta-" (car (catala_number_from_digits (cdr digits) kind))))))  
       
	((string-equal (car digits) "7");; 7x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "setanta")
	    (cons "setanta-" (catala_number_from_digits (cdr digits) kind))))

       ((string-equal (car digits) "8");; 8x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "vuitanta")
	    (list (string-append  "vuitanta-" (car (catala_number_from_digits (cdr digits) kind))))))  

       ((string-equal (car digits) "9");; 9x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "noranta")
	    (list (string-append  "noranta-" (car (catala_number_from_digits (cdr digits) kind))))))  

       ))

     ((equal? l 3);; in the hundreds
      (cond 
     
       ((string-equal (car digits) "1");; 1xx
	(cons "cent" (catala_number_from_digits (cdr digits) kind)))

       (t;; ?xx
	(if (string-equal kind "1") 
	(append (list (string-append (car (catala_number_from_digits (list (car digits)) kind)) "-centes")) (catala_number_from_digits (cdr digits) kind))
	(append (list (string-append (car (catala_number_from_digits (list (car digits)) kind)) "-cents")) (catala_number_from_digits (cdr digits) kind))))))
 

     ((< l 7)
      (let ((sub_thousands 
	     (list 
	      (car (cdr (cdr (reverse digits))))
	      (car (cdr (reverse digits)))
	      (car (reverse digits))))
	    (thousands (reverse (cdr (cdr (cdr (reverse digits)))))))
	(set! x (catala_number_from_digits thousands kind))
	(append
	 (if (or (string-equal (car x) "un") (string-equal (car x) "una")) nil x)
	 (list "mil")
	 (catala_number_from_digits sub_thousands kind))))

     ((< l 13)
      (let ((sub_million 
	     (list 
	      (car (cdr (cdr (cdr (cdr (cdr(reverse digits)))))))
	      (car (cdr (cdr (cdr (cdr (reverse digits))))))
	      (car (cdr (cdr (cdr (reverse digits)))))
	      (car (cdr (cdr (reverse digits))))
	      (car (cdr (reverse digits)))
	      (car (reverse digits))
	      ))
	    (millions (reverse (cdr (cdr (cdr (cdr (cdr (cdr (reverse digits))))))))))
	(set! x (catala_number_from_digits millions kind))
	(append
	 (if (or (string-equal (car x) "un") (string-equal (car x) "una")) 
	     (list "un" "milió")
             (append x (list "milions")))
	 (catala_number_from_digits sub_million kind))))

     (t
      (print "Error: Número massa gran")
      (list "Error" "Número" "massa" "gran ")))))
 
(define (catala_score name)
"(catala_score name) 
Tractament del cas x-x -> Resultat esportiu"
(append (catala_number (string-before name "\-") "0") '("a") (catala_number (string-after name "\-") "0"))

) 

(define (catala_money name)
"(catala_money name)
Tracatament del caso xxx$ o xxx¤"
;;(print "moneda in")
;;(print name)
(cond
  ;; Dòlars
  ((string-matches name ".*[\$]") 
    (set! quantity (string-before name "\$")) 
    (set! money "dòlars") 
    (set! money_sing "dòlar")
    (set! sub_money "centaus")
    (set! sub_money_sing "centau")
    (set! gen_money "0")           ;; gènere de la moneda (0: masculí 1: femení)
    (set! gen_submoney "0") 	   ;; gènere de la moneda (0: masculí 1: femení)
    (set! mode "1"))		   ;; la moneda té una submoneda Ex: Dòlar-centau / Euro-cèntim (mode 1: Si 0: No) 
  
;; Lliura
  ((string-matches name ".*[\£]") 
    (set! quantity (string-before name "\£")) 
    (set! money "lliures") 
    (set! money_sing "lliura")
    (set! sub_money "penics")
    (set! sub_money_sing "penic")
    (set! gen_money "1")           ;; gènere de la moneda (0: masculí 1: femení)
    (set! gen_submoney "0") 	   ;; gènere de la moneda (0: masculí 1: femení)
    (set! mode "1"))	  

  ;; Euros
  ((string-matches name ".*[¤]") 
    (set! quantity (string-before name "¤")) 
    (set! money "euros")
    (set! money_sing "euro")
    (set! sub_money "cèntims")
    (set! sub_money_sing "cèntim")
    (set! gen_money "0")
    (set! gen_submoney "0")
    (set! mode "1")))	

(cond 
   ((string-equal mode "1") ;; Moneda amb submoneda
     (set! quantity (catala_number_point quantity "2"))
     (cond 
      ((string-matches quantity ".*[0-9]+,[0-9]+.*") ;; Quantitat amb decimals 
        (set! decimals (string-after quantity ","))
       (set! quantity (string-before quantity ","))
       (set! l (length decimals))
       (if (string-matches quantity "1")
         (set! a money_sing)
         (set! a money))
       (if (string-matches decimals "01")
         (set! b sub_money_sing)
         (set! b sub_money))
       (cond 
         ((equal? l 1) (append (catala_number quantity gen_money) (list a) '( "amb" ) (catala_number (string-append decimals "0") gen_submoney) (list b)))
         ((equal? l 2) (append (catala_number quantity gen_money) (list a) '( "amb" ) (catala_number decimals gen_submoney) (list b)))
         (t (append (catala_number quantity "0") '( "coma" ) (catala_number decimals gen_money) (list a)))))
     (t 
       (if (string-matches quantity "1")
         (set! a money_sing)
         (set! a money))
        (append (catala_number_point quantity gen_money) (list a)))))

   ;;Moneda sense sub-moneda
   (t 
     (if (string-matches money "1")
       (set! a money_sing)
       (set! a money))
     (append (catala_number_decimal quantity gen_money) (list a)))))      
     

(define (catala_sufix word sufix num)
"(catala_sufix word sufix) 
Elimina de WORD les últimes NUM lletres i li afeigeix el SUFIX al final"
(if (not (string-matches num "[0-9]+"))
    (string-append word)
    (if (not (string-matches sufix "[a-zàáèéíìòóúç]+"))
      (string-append word)
   	(let ((l (length word)))
   	    (set! num (parse-number (intern num)))
            (if (not (> l num)) 
              (string-append word)
              (if (equal? l 1)
                (string-append word)
        	(let ((wordexplode (symbolexplode word)) (i (+ 0 num)) (word2 ""))
           		(while (> l i)
           		  (set! word2 (string-append word2 (car wordexplode)))
             		  (set! wordexplode (cdr wordexplode))
             		  (set! i (+ i 1)))
                        (string-append word2 sufix)))))))) 



(provide 'upc_catalan_numbers)    
