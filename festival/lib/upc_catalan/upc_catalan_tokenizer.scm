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
;;; Tokenizer for catalan
;;;
;;;  To share this among voices you need to promote this file to
;;;  to say festival/lib/upc_catalan/ so others can use it.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Load any other required files
(require 'upc_catalan_numbers)
(require 'upc_catalan_hour)
(require 'upc_catalan_date)
(require 'upc_catalan_tokenpos)
;; Punctuation for the particular language


(set! upc_catalan::token.punctuation "\"'`.,:;!?(){}[]")
(set! upc_catalan::token.prepunctuation "¿¡\"'`({[")
(set! upc_catalan::token.whitespace " \t\n\r")
(set! upc_catalan::token.singlecharsymbols "")
;(set! token.unknown_word_name "")

(define (upc_catalan::eliminacaracter token name caracter function)
"(upc_catalan::eliminacaracter token name caracter function)
 Aquesta funció elimina el caracter 'caracter' de la cadena 'name'
 i crida function amb els parametres token i els corresponents texts"
 (print (string-append "eliminacaracter " name))
 (cond
   ((string-matches name (string-append ".+" caracter "$"))
      (print "Hola1")
                (append (function token (string-before name caracter))
                )
   )
   ((string-matches name (string-append "^" caracter ".+"))
      (print "Hola2")
                (append (function token ( string-after name caracter))
                )
   )
   ((string-matches name (string-append ".+" caracter ".+"))
      (print "Hola3")
                (append (function token (string-before name caracter)) 
                        (function token ( string-after name caracter))
                )
   )
 )
)

;;; "Voice/ca token_to_word rules 
(define (upc_catalan::token_to_words token name)
  "(upc_catalan::token_to_words token name)
Specific token to word rules for the voice upc_catalan.  Returns a list
of words that expand given token with name."
;  (format t "upc_catalan::token_to_words  %s\n" name)

  (cond
   ;; Percentatges
   ((or (string-matches name "[0-9]+\,[0-9]+[%]") (string-matches name "[0-9]+[%]"))
    (append (catala_number_decimals (string-before name "%") "0") '("%"))) 
   
   ((or (string-matches name "[0-9]+\.[0-9]%") (string-matches name "[0-9]+\.[0-9][0-9]%"))
  	(let ((number (string-append (string-before name "\.") "," (string-after name "\."))))
	(append (catala_number_decimals (string-before number "%") "0") '("%"))          ))

   ;; Ordinals
   ((string-matches name "[0-9]+[rntèa]")
       (cond
           ((string-matches name ".*[è]") (catala_number_ordinals (string-before name "è") "0"))
	   ((string-matches name ".*[r]") (catala_number_ordinals (string-before name "r") "0"))
	   ((string-matches name ".*[n]") (catala_number_ordinals (string-before name "n") "0"))
	   ((string-matches name ".*[t]") (catala_number_ordinals (string-before name "t") "0"))
	   ((string-matches name ".*[a]") (catala_number_ordinals (string-before name "a") "1"))))

   ((string-matches name "4rt") (catala_number_ordinals (string-before name "rt") "0"))
   
	
   ((string-matches name "[13]+rs") (catala_number_ordinals (string-before name "rs") "2"))
   ((string-matches name "[0-9]+ns") (catala_number_ordinals (string-before name "ns") "2"))
   ((string-matches name "4ts") (catala_number_ordinals (string-before name "ts") "2"))
   ((string-matches name "[0-9]+es") (catala_number_ordinals (string-before name "es") "3"))
     		


   ;; Monedes
   ((or (string-matches name "[0-9]+[\$¤\£]") (string-matches name "[0-9]+,[0-9]+[\$¤\£]") 
 	(string-matches name "[0-9]+[\.][0-9][0-9][0-9][\.][0-9][0-9][0-9][\$¤\£]")
        (string-matches name "[0-9]+[\.][0-9][0-9][0-9][\.][0-9][0-9][0-9][\.][0-9][0-9][0-9][\$¤\£]")
        (string-matches name "[0-9]+[\.][0-9][0-9][0-9][\.][0-9][0-9][0-9][\.][0-9][0-9][0-9][\$¤\£]")  	  	  	
        (string-matches name "[0-9]+[\.][0-9]+[\$¤\£]"))
	(cond 
          ((or (string-matches name "[0-9]+[\.][0-9][\$¤\£]") (string-matches name "[0-9]+[\.][0-9][0-9][\$¤\£]"))
             (catala_money (string-append (string-before name "\.") "," (string-after name "\."))))
          (t (catala_money name))))
   
   ((or (string-matches name "[\$][0-9]+")  		(string-matches name "[\$][0-9]+[\,][0-9]+") 
	(string-matches name "[\$][0-9]+[\.][0-9]+")    (string-matches name "[\$][0-9]+[\.][0-9]+[\.][0-9]+") 
	(string-matches name "[\$][0-9]+[\.][0-9]+[\.][0-9]+[\.][0-9]+") 
	(and (string-matches name "[\$][0-9]+.*[\,][0-9]+") (not (string-matches name ".*[A-Za-z].*"))))  
		(catala_money (string-append (string-after name "\$") "\$")))
  
   ;; Hores
   ((or (string-matches name "[0-2][0-9]:[0-5][0-9]") (string-matches name "[0-9]:[0-5][0-9]"))
    (cond 
        ((or (string-equal "pm" (item.feat token "n.name")) (string-equal "p.m" (item.feat token "n.name")))
             (let ((hour (parse-number (intern (string-before name ":")))))
		(set! hour (+ hour 12))
                (set! name (string-append hour ":" (string-after name ":"))))
              (catala_hour name))
        (t (catala_hour name))))
   
   ((or (string-matches name "[aApP]\\.?[mM]") ) (list ".")) ;Dir un punt és callar-se. FIXME: Si no digués res diria "nil".
   
   ;; Hora exacta (cronòmetre)
   ((string-matches name "[0-9]+:[0-5][0-9]:[0-5][0-9]")
     (append (catala_number (string-before name ":") "1") '("hores") 
             (catala_number (string-before (string-after name ":") ":") "0") '("minuts")
             (list "i")
             (catala_number (string-after (string-after name ":") ":") "0") '("segons"))) 

   ;; Dates
   (( and (or (string-matches name "[0-3][0-9]/[0-1][0-9]/[0-9][0-9]+") (string-matches name "[1-9]/[0-1][0-9]/[0-9][0-9]+") 
        (string-matches name "[0-3][0-9]/[1-9]/[0-9][0-9]+")      (string-matches name "[1-9]/[1-9]/[0-9][0-9]+"))
        (not (or (string-matches name "[0-9]+/1[3-9]/[0-9]+") (string-matches name "3[2-9]/[0-9]+/[0-9]+"))))
  	 (catala_date name))
   
    (( and (or (string-matches name "[1-3][0-9]-1[0-9]-[0-9][0-9]+") 
	(string-matches name "[1-9]-1[0-2]-[0-9][0-9]+") 
	(string-matches name "[1-3][0-9]-[1-9]-[0-9][0-9]+") 
	(string-matches name "[1-9]-[1-9]-[0-9][0-9]+"))
        (not (or (string-matches name "[0-9]+-1[3-9]-[0-9]+") (string-matches name "3[2-9]-[0-9]+-[0-9]+"))))
  	 (catala_date (catala_subs name "-" "/")))

  
   ;; Números de teléfon
   ((and (string-equal "1" (catala_telph token)) (or 
	;; Format 925725623
	(string-matches name "[0-9][0-9][0-9][0-9][0-9]+")
	;; Format 93-4385232
	(string-matches name "[0-9][0-9]-[0-9][0-9][0-9][0-9][0-9][0-9][0-9]")
	;; Format 934-385-232
	(string-matches name "[0-9][0-9][0-9]-[0-9][0-9][0-9]-[0-9][0-9][0-9]")))
;;     (print "telefon mode")
     (catala_speller name))
   
   ;; Llista de números (són lletrejades les xifres) => Falla si el penúltim número té un sola xifra (no lletreja el darrer nombre)
   ((and (string-matches name "[0-9]+") 
	(or (string-matches (item.feat token "n.name") "[1-9][0-9]+") (string-matches (item.feat token "p.name") "[1-9][0-9]+")))
      (catala_speller name))
    
   ;; Resultats esportius
   ((string-matches name "[0-9]+-[0-9]+") (catala_score name))
    
   ;; WWW
    ; Només www:
   ((string-equal name "www") (append (list "tres" "#ws") ))
   ((string-equal name "WWW") (append (list "tres" "#ws") ))
     ; www seguit del que sigui:
   ((string-matches name "www.*") 
            (append (list "tres" "#ws") 
                    (upc_catalan::token_to_words token (string-after name "www"))
            )
   )
   ((string-matches name ".*www.*")
           (append (upc_catalan::token_to_words token (string-before name "www")) 
                   (list "tres" "#ws") 
                   (upc_catalan::token_to_words token (string-after name "www"))
           )
   )
  ((string-matches name "WWW.*") 
           (append (list "tres" "#ws") 
                   (upc_catalan::token_to_words token (string-after name "WWW"))
           )
  )
  ((string-matches name ".*WWW.*")
           (append (upc_catalan::token_to_words token (string-before name "WWW"))
                   (list "tres" "#ws")
                   (upc_catalan::token_to_words token (string-after name "WWW"))
           )
  )
  ((string-matches name ".*/.*") ; si trobem un text separat per barres festcat.cat/cosa/bossa/prova ...
     (let ( (tmplist) (tmplist2) (elem) )
       (set! tmplist ; Apliquem token_to_words a cada part de la paraula (fescat.cat, cosa, bossa...) i guardem el resultat a tmplist.
        (mapcar 
             (lambda (d) (upc_catalan::token_to_words token d) ) 
             (split name "/")
          )
       )
       ; Per cada resultat...
       (while (car tmplist)
          (set! elem (car tmplist))
         (if (eq? (length elem 0)) ; Afegim la paraula o les paraules del token_to_words a tmplist2, separades per la paraula barra.
                                   ; TODO: Falta afegir una petita pausa amb la barra a upc_ca_generic_phrasing
          (set! tmplist2 (append tmplist2 (list elem)  (list "barra") ))
          (set! tmplist2 (append tmplist2 elem (list "barra")))
         )
          (set! tmplist (cdr tmplist))
       )
      ; Movem a tmplist el resultat, traient el darrer "barra" que sobra.
      (set! tmplist (reverse (cdr (reverse tmplist2))) )
      ; Eliminem totes les paraules buides "":
      ; i tornem la resta de paraules:
       (remove_empty tmplist)
     )
   )
   
   ((string-matches name "[\.][a-z\.]+") 
        (append (list "punt") 
                (upc_catalan::token_to_words token (string-after name "\."))
        )
   )
   ((string-matches name "[a-z]+[\.][a-z\.]+") 
        (append (upc_catalan::token_to_words token (string-before name "\.")) 
                (list "punt")
                (upc_catalan::token_to_words token (string-after name "\."))
        )
   )

   ;; e-m@il
   ((string-matches name "<.*@.*>")  ;; quoted e-mail
    (let ((nick (string-after (string-before name "@") "<")) (server (string-before (string-after name "@") ">")))
       (append (upc_catalan::token_to_words token nick) (list "@") (upc_catalan::token_to_words token server))))
   ((string-matches name ".*@.*")  ;; e-mail
    (let ((nick (string-before name "@")) (server (string-after name "@")))
       (append (upc_catalan::token_to_words token nick) (list "@") (upc_catalan::token_to_words token server))))


   ;; Números
   ((string-matches name "0[0-9]+") 
    (catala_speller name)) 

   ((string-matches name "[0-9]+") (catala_number name "0"))
   
   ;; Números en format xxx.xxx xxx.xxx.xxx xxx.xxx.xxx.xxx
   ((or (string-matches name "[0-9]+[\.][0-9][0-9][0-9]") 
	(string-matches name "[0-9]+[\.][0-9][0-9][0-9][0-9][\.][0-9][0-9][0-9]") 
	(string-matches name "[0-9]+[\.][0-9][0-9][0-9][\.][0-9][0-9][0-9][\.][0-9][0-9][0-9]")) 
		(catala_number_point name "0"))
   
   ;; Números amb decimals
   ((and (string-matches name "[0-9]+,[0-9]+") (not (string-matches name "[A-Za-z].*")))
		(catala_number_decimals name "0"))
   ((or (string-matches name "[0-9]+[\.][0-9][0-9][0-9],[0-9]+") (string-matches name "[0-9]+[\.][0-9][0-9][0-9][\.][0-9][0-9][0-9],[0-9]+")
	(string-matches name "[0-9]+[\.][0-9][0-9][0-9][\.][0-9][0-9][0-9][\.][0-9][0-9][0-9],[0-9]+")) 			
        (catala_number_decimals name "0"))
   ((or (string-matches name "[0-9]+[\.][0-9]") (string-matches name "[0-9]+[\.][0-9][0-9]")) 
		(catala_number_decimals (string-append (string-before name "\.") "," (string-after name "\.")) "0")) 
   ;; Comparacions amb "més gran que", "més petit que": Primera versio, per provar, nomes funciona amb nombres.
   ((string-matches name "[0-9]+<[0-9]+")
       (append 
           (upc_catalan::token_to_words token (string-before name "<" ) )
           (list "menor" "que")
           (upc_catalan::token_to_words token (string-after name "<" ) )
       )
   )
   ((string-matches name "[0-9]+>[0-9]+")
       (append 
           (upc_catalan::token_to_words token (string-before name ">" ) )
           (list "més" "gran" "que")
           (upc_catalan::token_to_words token (string-after name ">" ) )
       )
   )
   ((string-matches name "[0-9a-zA-Z]+<<+[0-9a-zA-Z]+.*")
       (append 
           (upc_catalan::token_to_words token (string-before name "<<" ) )
           (list "molt" "menor" "que")
           (upc_catalan::token_to_words token (string-after name "<<" ) )
       )
   )
   ((string-matches name "[0-9a-zA-Z]+>>+[0-9a-zA-Z]+.*")
       (append 
           (upc_catalan::token_to_words token (string-before name ">>" ) )
           (list "molt" "més" "gran" "que")
           (upc_catalan::token_to_words token (string-after name ">>" ) )
       )
   )

;; "<" ">" com cometes:
   ((string-matches name (string-append "<+" catalan-regex-all-letters "+>+"))
       (upc_catalan::token_to_words token (string-after+ (string-before name ">") "<" ) )
   )

;; BEGIN Apòstrofs

((string-matches name "^d'.+")
 (append (upc_catalan::token_to_words token "d'") (upc_catalan::token_to_words token (string-after name "d'")))
 )
((string-matches name "^D'.+")
 (append (upc_catalan::token_to_words token "D'") (upc_catalan::token_to_words token (string-after name "D'")))
 )
((string-matches name ".+-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-hi")) (upc_catalan::token_to_words token "-hi"))
 )
((string-matches name ".+-ho$")
 (append (upc_catalan::token_to_words token (string-before name "-ho")) (upc_catalan::token_to_words token "-ho"))
 )
((string-matches name ".+'l$")
 (append (upc_catalan::token_to_words token (string-before name "'l")) (upc_catalan::token_to_words token "'l"))
 )
((string-matches name "^l'.+")
 (append (upc_catalan::token_to_words token "l'") (upc_catalan::token_to_words token (string-after name "l'")))
 )
((string-matches name "^L'.+")
 (append (upc_catalan::token_to_words token "L'") (upc_catalan::token_to_words token (string-after name "L'")))
 )
((string-matches name ".+-la$")
 (append (upc_catalan::token_to_words token (string-before name "-la")) (upc_catalan::token_to_words token "-la"))
 )
((string-matches name ".+-la-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-la-hi")) (upc_catalan::token_to_words token "-la") (upc_catalan::token_to_words token "-hi"))
 )
((string-matches name ".+-la'n$")
 (append (upc_catalan::token_to_words token (string-before name "-la'n")) (upc_catalan::token_to_words token "-la") (upc_catalan::token_to_words token "'n"))
 )
((string-matches name ".+-l'en$")
 (append (upc_catalan::token_to_words token (string-before name "-l'en")) (upc_catalan::token_to_words token "-l") (upc_catalan::token_to_words token "'en"))
 )
((string-matches name ".+-les$")
 (append (upc_catalan::token_to_words token (string-before name "-les")) (upc_catalan::token_to_words token "-les"))
 )
((string-matches name ".+-les-en$")
 (append (upc_catalan::token_to_words token (string-before name "-les-en")) (upc_catalan::token_to_words token "-les") (upc_catalan::token_to_words token "-en"))
 )
((string-matches name ".+-les-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-les-hi")) (upc_catalan::token_to_words token "-les") (upc_catalan::token_to_words token "-hi"))
 )
((string-matches name ".+-l'hi$")
 (append (upc_catalan::token_to_words token (string-before name "-l'hi")) (upc_catalan::token_to_words token "-l") (upc_catalan::token_to_words token "'hi"))
 )
((string-matches name ".+-li$")
 (append (upc_catalan::token_to_words token (string-before name "-li")) (upc_catalan::token_to_words token "-li"))
 )
((string-matches name ".+-li-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-li-hi")) (upc_catalan::token_to_words token "-li") (upc_catalan::token_to_words token "-hi"))
 )
((string-matches name ".+-li-ho$")
 (append (upc_catalan::token_to_words token (string-before name "-li-ho")) (upc_catalan::token_to_words token "-li") (upc_catalan::token_to_words token "-ho"))
 )
((string-matches name ".+-li'l$")
 (append (upc_catalan::token_to_words token (string-before name "-li'l")) (upc_catalan::token_to_words token "-li") (upc_catalan::token_to_words token "'l"))
 )
((string-matches name ".+-li-la$")
 (append (upc_catalan::token_to_words token (string-before name "-li-la")) (upc_catalan::token_to_words token "-li") (upc_catalan::token_to_words token "-la"))
 )
((string-matches name ".+-li-les$")
 (append (upc_catalan::token_to_words token (string-before name "-li-les")) (upc_catalan::token_to_words token "-li") (upc_catalan::token_to_words token "-les"))
 )
((string-matches name ".+-li'ls$")
 (append (upc_catalan::token_to_words token (string-before name "-li'ls")) (upc_catalan::token_to_words token "-li") (upc_catalan::token_to_words token "'ls"))
 )
((string-matches name ".+-li'n$")
 (append (upc_catalan::token_to_words token (string-before name "-li'n")) (upc_catalan::token_to_words token "-li") (upc_catalan::token_to_words token "'n"))
 )
((string-matches name ".+-lo$")
 (append (upc_catalan::token_to_words token (string-before name "-lo")) (upc_catalan::token_to_words token "-lo"))
 )
((string-matches name ".+-los$")
 (append (upc_catalan::token_to_words token (string-before name "-los")) (upc_catalan::token_to_words token "-los"))
 )
((string-matches name ".+-los-el$")
 (append (upc_catalan::token_to_words token (string-before name "-los-el")) (upc_catalan::token_to_words token "-los") (upc_catalan::token_to_words token "-el"))
 )
((string-matches name ".+-los-els$")
 (append (upc_catalan::token_to_words token (string-before name "-los-els")) (upc_catalan::token_to_words token "-los") (upc_catalan::token_to_words token "-els"))
 )
((string-matches name ".+-los-en$")
 (append (upc_catalan::token_to_words token (string-before name "-los-en")) (upc_catalan::token_to_words token "-los") (upc_catalan::token_to_words token "-en"))
 )
((string-matches name ".+-los-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-los-hi")) (upc_catalan::token_to_words token "-los") (upc_catalan::token_to_words token "-hi"))
 )
((string-matches name ".+-los-ho$")
 (append (upc_catalan::token_to_words token (string-before name "-los-ho")) (upc_catalan::token_to_words token "-los") (upc_catalan::token_to_words token "-ho"))
 )
((string-matches name ".+-los-la$")
 (append (upc_catalan::token_to_words token (string-before name "-los-la")) (upc_catalan::token_to_words token "-los") (upc_catalan::token_to_words token "-la"))
 )
((string-matches name ".+-los-les$")
 (append (upc_catalan::token_to_words token (string-before name "-los-les")) (upc_catalan::token_to_words token "-los") (upc_catalan::token_to_words token "-les"))
 )
((string-matches name ".+'ls$")
 (append (upc_catalan::token_to_words token (string-before name "'ls")) (upc_catalan::token_to_words token "'ls"))
 )
((string-matches name ".+'ls-els$")
 (append (upc_catalan::token_to_words token (string-before name "'ls-els")) (upc_catalan::token_to_words token "'ls") (upc_catalan::token_to_words token "-els"))
 )
((string-matches name ".+'ls-en$")
 (append (upc_catalan::token_to_words token (string-before name "'ls-en")) (upc_catalan::token_to_words token "'ls") (upc_catalan::token_to_words token "-en"))
 )
((string-matches name ".+'ls-hi$")
 (append (upc_catalan::token_to_words token (string-before name "'ls-hi")) (upc_catalan::token_to_words token "'ls") (upc_catalan::token_to_words token "-hi"))
 )
((string-matches name ".+'ls-la$")
 (append (upc_catalan::token_to_words token (string-before name "'ls-la")) (upc_catalan::token_to_words token "'ls") (upc_catalan::token_to_words token "-la"))
 )
((string-matches name ".+'ls-les$")
 (append (upc_catalan::token_to_words token (string-before name "'ls-les")) (upc_catalan::token_to_words token "'ls") (upc_catalan::token_to_words token "-les"))
 )
((string-matches name ".+'m$")
 (append (upc_catalan::token_to_words token (string-before name "'m")) (upc_catalan::token_to_words token "'m"))
 )
((string-matches name "^m'.+")
 (append (upc_catalan::token_to_words token "m'") (upc_catalan::token_to_words token (string-after name "m'")))
 )

((string-matches name "^M'.+")
 (append (upc_catalan::token_to_words token "M'") (upc_catalan::token_to_words token (string-after name "M'")))
 )
((string-matches name ".+-me$")
 (append (upc_catalan::token_to_words token (string-before name "-me")) (upc_catalan::token_to_words token "-me"))
 )
((string-matches name ".+-me'l$")
 (append (upc_catalan::token_to_words token (string-before name "-me'l")) (upc_catalan::token_to_words token "-me") (upc_catalan::token_to_words token "'l"))
 )
((string-matches name ".+-me-la$")
 (append (upc_catalan::token_to_words token (string-before name "-me-la")) (upc_catalan::token_to_words token "-me") (upc_catalan::token_to_words token "-la"))
 )
((string-matches name ".+-me-les$")
 (append (upc_catalan::token_to_words token (string-before name "-me-les")) (upc_catalan::token_to_words token "-me") (upc_catalan::token_to_words token "-les"))
 )
((string-matches name ".+-me-li$")
 (append (upc_catalan::token_to_words token (string-before name "-me-li")) (upc_catalan::token_to_words token "-me") (upc_catalan::token_to_words token "-li"))
 )
((string-matches name ".+-me'ls$")
 (append (upc_catalan::token_to_words token (string-before name "-me'ls")) (upc_catalan::token_to_words token "-me") (upc_catalan::token_to_words token "'ls"))
 )
((string-matches name ".+-me'n$")
 (append (upc_catalan::token_to_words token (string-before name "-me'n")) (upc_catalan::token_to_words token "-me") (upc_catalan::token_to_words token "'n"))
 )
((string-matches name ".+-m'hi$")
 (append (upc_catalan::token_to_words token (string-before name "-m'hi")) (upc_catalan::token_to_words token "-m") (upc_catalan::token_to_words token "'hi"))
 )
((string-matches name ".+-m'ho$")
 (append (upc_catalan::token_to_words token (string-before name "-m'ho")) (upc_catalan::token_to_words token "-m") (upc_catalan::token_to_words token "'ho"))
 )
((string-matches name ".+'n$")
 (append (upc_catalan::token_to_words token (string-before name "'n")) (upc_catalan::token_to_words token "'n"))
 )
((string-matches name "^n'.+")
 (append (upc_catalan::token_to_words token "n'") (upc_catalan::token_to_words token (string-after name "n'")))
 )
((string-matches name "^N'.+")
 (append (upc_catalan::token_to_words token "N'") (upc_catalan::token_to_words token (string-after name "N'")))
 )
((string-matches name ".+-ne$")
 (append (upc_catalan::token_to_words token (string-before name "-ne")) (upc_catalan::token_to_words token "-ne"))
 )
((string-matches name ".+-n'hi$")
 (append (upc_catalan::token_to_words token (string-before name "-n'hi")) (upc_catalan::token_to_words token "-n") (upc_catalan::token_to_words token "'hi"))
 )
((string-matches name ".+-nos$")
 (append (upc_catalan::token_to_words token (string-before name "-nos")) (upc_catalan::token_to_words token "-nos"))
 )
((string-matches name ".+-nos-el$")
 (append (upc_catalan::token_to_words token (string-before name "-nos-el")) (upc_catalan::token_to_words token "-nos") (upc_catalan::token_to_words token "-el"))
 )
((string-matches name ".+-nos-els$")
 (append (upc_catalan::token_to_words token (string-before name "-nos-els")) (upc_catalan::token_to_words token "-nos") (upc_catalan::token_to_words token "-els"))
 )
((string-matches name ".+-nos-en$")
 (append (upc_catalan::token_to_words token (string-before name "-nos-en")) (upc_catalan::token_to_words token "-nos") (upc_catalan::token_to_words token "-en"))
 )
((string-matches name ".+-nos-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-nos-hi")) (upc_catalan::token_to_words token "-nos") (upc_catalan::token_to_words token "-hi"))
 )
((string-matches name ".+-nos-ho$")
 (append (upc_catalan::token_to_words token (string-before name "-nos-ho")) (upc_catalan::token_to_words token "-nos") (upc_catalan::token_to_words token "-ho"))
 )
((string-matches name ".+-nos-les$")
 (append (upc_catalan::token_to_words token (string-before name "-nos-les")) (upc_catalan::token_to_words token "-nos") (upc_catalan::token_to_words token "-les"))
 )
((string-matches name ".+-nos-li$")
 (append (upc_catalan::token_to_words token (string-before name "-nos-li")) (upc_catalan::token_to_words token "-nos") (upc_catalan::token_to_words token "-li"))
 )
((string-matches name ".+'ns$")
 (append (upc_catalan::token_to_words token (string-before name "'ns")) (upc_catalan::token_to_words token "'ns"))
 )
((string-matches name ".+'ns-el$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-el")) (upc_catalan::token_to_words token "'ns") (upc_catalan::token_to_words token "-el"))
 )
((string-matches name ".+'ns-els$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-els")) (upc_catalan::token_to_words token "'ns") (upc_catalan::token_to_words token "-els"))
 )
((string-matches name ".+'ns-en$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-en")) (upc_catalan::token_to_words token "'ns") (upc_catalan::token_to_words token "-en"))
 )
((string-matches name ".+'ns-hi$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-hi")) (upc_catalan::token_to_words token "'ns") (upc_catalan::token_to_words token "-hi"))
 )
((string-matches name ".+'ns-ho$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-ho")) (upc_catalan::token_to_words token "'ns") (upc_catalan::token_to_words token "-ho"))
 )
((string-matches name ".+'ns-la$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-la")) (upc_catalan::token_to_words token "'ns") (upc_catalan::token_to_words token "-la"))
 )
((string-matches name ".+'ns-les$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-les")) (upc_catalan::token_to_words token "'ns") (upc_catalan::token_to_words token "-les"))
 )
((string-matches name ".+'ns-li$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-li")) (upc_catalan::token_to_words token "'ns") (upc_catalan::token_to_words token "-li"))
 )
((string-matches name ".+'s$")
 (append (upc_catalan::token_to_words token (string-before name "'s")) (upc_catalan::token_to_words token "'s"))
 )
((string-matches name "^s'.+")
 (append (upc_catalan::token_to_words token "s'") (upc_catalan::token_to_words token (string-after name "s'")))
 )
((string-matches name "^S'.+")
 (append (upc_catalan::token_to_words token "S'") (upc_catalan::token_to_words token (string-after name "S'")))
 )
((string-matches name ".+-se$")
 (append (upc_catalan::token_to_words token (string-before name "-se")) (upc_catalan::token_to_words token "-se"))
 )
((string-matches name ".+-se'l$")
 (append (upc_catalan::token_to_words token (string-before name "-se'l")) (upc_catalan::token_to_words token "-se") (upc_catalan::token_to_words token "'l"))
 )
((string-matches name ".+-se-la$")
 (append (upc_catalan::token_to_words token (string-before name "-se-la")) (upc_catalan::token_to_words token "-se") (upc_catalan::token_to_words token "-la"))
 )
((string-matches name ".+-se-les$")
 (append (upc_catalan::token_to_words token (string-before name "-se-les")) (upc_catalan::token_to_words token "-se") (upc_catalan::token_to_words token "-les"))
 )
((string-matches name ".+-se-li$")
 (append (upc_catalan::token_to_words token (string-before name "-se-li")) (upc_catalan::token_to_words token "-se") (upc_catalan::token_to_words token "-li"))
 )
((string-matches name ".+-se'ls$")
 (append (upc_catalan::token_to_words token (string-before name "-se'ls")) (upc_catalan::token_to_words token "-se") (upc_catalan::token_to_words token "'ls"))
 )
((string-matches name ".+-se'm$")
 (append (upc_catalan::token_to_words token (string-before name "-se'm")) (upc_catalan::token_to_words token "-se") (upc_catalan::token_to_words token "'m"))
 )
((string-matches name ".+-se'n$")
 (append (upc_catalan::token_to_words token (string-before name "-se'n")) (upc_catalan::token_to_words token "-se") (upc_catalan::token_to_words token "'n"))
 )
((string-matches name ".+-se'ns$")
 (append (upc_catalan::token_to_words token (string-before name "-se'ns")) (upc_catalan::token_to_words token "-se") (upc_catalan::token_to_words token "'ns"))
 )
((string-matches name ".+-se't$")
 (append (upc_catalan::token_to_words token (string-before name "-se't")) (upc_catalan::token_to_words token "-se") (upc_catalan::token_to_words token "'t"))
 )
((string-matches name ".+-se-us$")
 (append (upc_catalan::token_to_words token (string-before name "-se-us")) (upc_catalan::token_to_words token "-se") (upc_catalan::token_to_words token "-us"))
 )
((string-matches name ".+-s'hi$")
 (append (upc_catalan::token_to_words token (string-before name "-s'hi")) (upc_catalan::token_to_words token "-s") (upc_catalan::token_to_words token "'hi"))
 )
((string-matches name ".+-s'ho$")
 (append (upc_catalan::token_to_words token (string-before name "-s'ho")) (upc_catalan::token_to_words token "-s") (upc_catalan::token_to_words token "'ho"))
 )
((string-matches name ".+'t$")
 (append (upc_catalan::token_to_words token (string-before name "'t")) (upc_catalan::token_to_words token "'t"))
 )
((string-matches name "^t'.+")
 (append (upc_catalan::token_to_words token "t'") (upc_catalan::token_to_words token (string-after name "t'")))
 )
((string-matches name "^T'.+")
 (append (upc_catalan::token_to_words token "t'") (upc_catalan::token_to_words token (string-after name "T'")))
 )
((string-matches name ".+-te$")
 (append (upc_catalan::token_to_words token (string-before name "-te")) (upc_catalan::token_to_words token "-te"))
 )
((string-matches name ".+-te'l$")
 (append (upc_catalan::token_to_words token (string-before name "-te'l")) (upc_catalan::token_to_words token "-te") (upc_catalan::token_to_words token "'l"))
 )
((string-matches name ".+-te-la$")
 (append (upc_catalan::token_to_words token (string-before name "-te-la")) (upc_catalan::token_to_words token "-te") (upc_catalan::token_to_words token "-la"))
 )
((string-matches name ".+-te-les$")
 (append (upc_catalan::token_to_words token (string-before name "-te-les")) (upc_catalan::token_to_words token "-te") (upc_catalan::token_to_words token "-les"))
 )
((string-matches name ".+-te-li$")
 (append (upc_catalan::token_to_words token (string-before name "-te-li")) (upc_catalan::token_to_words token "-te") (upc_catalan::token_to_words token "-li"))
 )
((string-matches name ".+-te'ls$")
 (append (upc_catalan::token_to_words token (string-before name "-te'ls")) (upc_catalan::token_to_words token "-te") (upc_catalan::token_to_words token "'ls"))
 )
((string-matches name ".+-te'n$")
 (append (upc_catalan::token_to_words token (string-before name "-te'n")) (upc_catalan::token_to_words token "-te") (upc_catalan::token_to_words token "'n"))
 )
((string-matches name ".+-te'ns$")
 (append (upc_catalan::token_to_words token (string-before name "-te'ns")) (upc_catalan::token_to_words token "-te") (upc_catalan::token_to_words token "'ns"))
 )
((string-matches name ".+-t'hi$")
 (append (upc_catalan::token_to_words token (string-before name "-t'hi")) (upc_catalan::token_to_words token "-t") (upc_catalan::token_to_words token "'hi"))
 )
((string-matches name ".+-t'ho$")
 (append (upc_catalan::token_to_words token (string-before name "-t'ho")) (upc_catalan::token_to_words token "-t") (upc_catalan::token_to_words token "'ho"))
 )
((string-matches name ".+-us-el$")
 (append (upc_catalan::token_to_words token (string-before name "-us-el")) (upc_catalan::token_to_words token "-us") (upc_catalan::token_to_words token "-el"))
 )
((string-matches name ".+-us-els$")
 (append (upc_catalan::token_to_words token (string-before name "-us-els")) (upc_catalan::token_to_words token "-us") (upc_catalan::token_to_words token "-els"))
 )
((string-matches name ".+-us-em$")
 (append (upc_catalan::token_to_words token (string-before name "-us-em")) (upc_catalan::token_to_words token "-us") (upc_catalan::token_to_words token "-em"))
 )
((string-matches name ".+-us-en$")
 (append (upc_catalan::token_to_words token (string-before name "-us-en")) (upc_catalan::token_to_words token "-us") (upc_catalan::token_to_words token "-en"))
 )
((string-matches name ".+-us-ens$")
 (append (upc_catalan::token_to_words token (string-before name "-us-ens")) (upc_catalan::token_to_words token "-us") (upc_catalan::token_to_words token "-ens"))
 )
((string-matches name ".+-us-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-us-hi")) (upc_catalan::token_to_words token "-us") (upc_catalan::token_to_words token "-hi"))
 )
((string-matches name ".+-us-ho$")
 (append (upc_catalan::token_to_words token (string-before name "-us-ho")) (upc_catalan::token_to_words token "-us") (upc_catalan::token_to_words token "-ho"))
 )
((string-matches name ".+-us-la$")
 (append (upc_catalan::token_to_words token (string-before name "-us-la")) (upc_catalan::token_to_words token "-us") (upc_catalan::token_to_words token "-la"))
 )
((string-matches name ".+-us-les$")
 (append (upc_catalan::token_to_words token (string-before name "-us-les")) (upc_catalan::token_to_words token "-us") (upc_catalan::token_to_words token "-les"))
 )
((string-matches name ".+-us-li$")
 (append (upc_catalan::token_to_words token (string-before name "-us-li")) (upc_catalan::token_to_words token "-us") (upc_catalan::token_to_words token "-li"))
 )
((string-matches name ".+-vos$")
 (append (upc_catalan::token_to_words token (string-before name "-vos")) (upc_catalan::token_to_words token "-vos"))
 )
((string-matches name ".+-vos-el$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-el")) (upc_catalan::token_to_words token "-vos") (upc_catalan::token_to_words token "-el"))
 )
((string-matches name ".+-vos-els$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-els")) (upc_catalan::token_to_words token "-vos") (upc_catalan::token_to_words token "-els"))
 )
((string-matches name ".+-vos-em$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-em")) (upc_catalan::token_to_words token "-vos") (upc_catalan::token_to_words token "-em"))
 )
((string-matches name ".+-vos-en$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-en")) (upc_catalan::token_to_words token "-vos") (upc_catalan::token_to_words token "-en"))
 )
((string-matches name ".+-vos-ens$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-ens")) (upc_catalan::token_to_words token "-vos") (upc_catalan::token_to_words token "-ens"))
 )
((string-matches name ".+-vos-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-hi")) (upc_catalan::token_to_words token "-vos") (upc_catalan::token_to_words token "-hi"))
 )
((string-matches name ".+-vos-ho$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-ho")) (upc_catalan::token_to_words token "-vos") (upc_catalan::token_to_words token "-ho"))
 )
((string-matches name ".+-vos-la$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-la")) (upc_catalan::token_to_words token "-vos") (upc_catalan::token_to_words token "-la"))
 )
((string-matches name ".+-vos-les$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-les")) (upc_catalan::token_to_words token "-vos") (upc_catalan::token_to_words token "-les"))
 )
((string-matches name ".+-vos-li$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-li")) (upc_catalan::token_to_words token "-vos") (upc_catalan::token_to_words token "-li"))
 )

;; END Apòstrofs

   ;; Números romans
  ((string-matches name "\\(II?I?\\|IV\\|VI?I?I?\\|IX\\|X[VIX]*\\)")
 
     (cond
         ((and (string-equal name "I") 	(string-equal "1" (catala_start_caps (item.feat token "p.name"))) 
					(string-equal "0" (catala_start_caps (item.feat token "n.name"))))
        				        (let ((number (parse-number (tok_roman_to_numstring name))))
							(if (>= number 10)
                					  (catala_number number "0")
           					  	  (catala_number_ordinals (tok_roman_to_numstring name) "0"))))
        
        ((and (string-equal name "I") 	(string-equal "1" (tok_allcaps token)) 
					(string-equal "0" (catala_tok_rex token)) 
					(string-equal "0" (catala_tok_queen token)))
					          (list "i"))
	
	((or (string-matches (item.feat token "p.name") "segle") (string-matches (item.feat token "p.name") "s[\.]")) 
          (catala_number (tok_roman_to_numstring name) "0"))
        
	((string-equal "1" (catala_tok_rex token)) 
          (let ((number (parse-number (tok_roman_to_numstring name))))
		(if (>= number 10)
                  (catala_number number "0")
             	  (catala_number_ordinals (tok_roman_to_numstring name) "0"))))
        
	((string-equal "1" (catala_tok_queen token)) 
    	   (let ((number (parse-number (tok_roman_to_numstring name))))
		(if (>= number 10)
                  (catala_number number "1")
             	  (catala_number_ordinals (tok_roman_to_numstring name) "1"))))
        
	((string-equal "1" (catala_tok_queen_names token)) 
    	     (let ((number (parse-number (tok_roman_to_numstring name))))
		(if (>= number 10)
                  (catala_number number "1")
             	  (catala_number_ordinals (tok_roman_to_numstring name) "1"))))
        
	((string-equal "1" (catala_tok_rex_names token)) 
    	     (let ((number (parse-number (tok_roman_to_numstring name))))
		(if (>= number 10)
                  (catala_number number "0")
             	  (catala_number_ordinals (tok_roman_to_numstring name) "0"))))
 
       ((string-equal "1" (catala_words_ordinals_fs token))  (catala_number_ordinals (tok_roman_to_numstring name) "1"))
       ((string-equal "1" (catala_words_ordinals_ms token))  (catala_number_ordinals (tok_roman_to_numstring name) "0"))
       ((string-equal "1" (catala_words_ordinals_mp token))  (catala_number_ordinals (tok_roman_to_numstring name) "2"))
       ((string-equal "1" (catala_words_ordinals_fp token))  (catala_number_ordinals (tok_roman_to_numstring name) "3"))
       
       ((and (string-equal name "I") (string-equal "1" (tok_allcaps token))) 
          (list "i"))
       (t (catala_number (tok_roman_to_numstring name) "0"))))

  ((string-matches name "s[\.]\\(II?I?\\|IV\\|VI?I?I?\\|IX\\|X[VIX]*\\)")
    ( append (list "segle") (catala_number (tok_roman_to_numstring (string-after name "\.")) "0")))
   
   ;; Abreviatures comunes
   ((and (string-matches name "s") (string-matches (item.feat token "n.name") "\\(II?I?\\|IV\\|VI?I?I?\\|IX\\|X[VIX]*\\)")) (list "segle"))
   
   ((and (string-matches name "dC") (or (string-matches (item.feat token "pp.name") "segle") 
					(string-matches (item.feat token "pp.name") "s.") 
					(string-matches (item.feat token "pp.name") "s") 
					(string-matches (item.feat token "p.name") "[0-9]+"))) 
						(list "després" "de" "Crist"))

   ((and (string-matches name "aC") (or (string-matches (item.feat token "pp.name") "segle") 
					(string-matches (item.feat token "pp.name") "s.") 
					(string-matches (item.feat token "pp.name") "s") 
					(string-matches (item.feat token "p.name") "[0-9]+"))) 
						(list "abans" "de" "Crist"))


   ((or (string-matches name "telf\.") (string-matches name "telf") (string-matches name "tel")) (list "telèfon")) ; FIXME: el "tel" és el que es forma a la llet escalfada!
   ((or (string-matches name "[Ss][tT]\.") (string-matches name "[sS][tT]")) (list "sant"))  

   ;; Abreviatures d'adreces
   ((string-equal name "i/o") (append (upc_catalan::token_to_words token "i") (upc_catalan::token_to_words token "o")) )
   ((string-matches name "[Cc][/\.]+") 	(list "carrer"))
   ((string-matches name "[Aa]v[/\.]+") 	(list "avinguda"))
   ((string-matches name "[Pp]g[/\.]+")	(list "passeig"))
   ((string-matches name "[Pp]l[/\.]+") 	(list "plaça"))
   ((string-matches name "entr") (list "entresol"))
   ((string-matches name "pr")   (list "principal"))
   ((string-matches name "esc") (list "escala"))
   
  
  ;; Separació de Paraules tipus WindowsXP
   ((string-equal "1" (catala_two_caps name))
          (append (upc_catalan::token_to_words token (car (catala_splitter name))) 
                  (upc_catalan::token_to_words token (car (cdr (catala_splitter name))))
          )
   )

  ;; Paraules compostes amb text y número separades per apòstrofs: Ex. Sidney'02
  ((string-matches name (string-append catalan-regex-all-letters "+\'[0-9]+")) 
        (append (upc_catalan::token_to_words token (string-before name "\'")) 
                (upc_catalan::token_to_words token (string-after name "\'"))
        )
  )

  ;; Paraules compostes amb text y número separades per apòstrofs: Ex. Barcelona-92
  ((string-matches name (string-append catalan-regex-all-letters "+\-[0-9]+" ) )
        (append (upc_catalan::token_to_words token (string-before name "\-"))
                (upc_catalan::token_to_words token (string-after name "\-"))
        )
  )
 

  ((string-matches name ".*_____+.*")
     (remove_empty
        (append (upc_catalan::token_to_words token (string-before name "_"))
           (list "ratlla" "de" "subratllats")
           (upc_catalan::token_to_words token (string-after+ name "_"))
        )
     )
  )

  ((string-matches name ".*=====+.*")
     (remove_empty
        (append (upc_catalan::token_to_words token (string-before name "="))
           (list "ratlla" "d'" "iguals")
           (upc_catalan::token_to_words token (string-after+ name "="))
        )
     )
  )

  ((string-matches name ".*-----+.*")
     (remove_empty
        (append (upc_catalan::token_to_words token (string-before name "-"))
           (list "ratlla" "de" "guionets")
           (upc_catalan::token_to_words token (string-after+ name "-"))
        )
     )
  )

  ((string-matches name ".*\\*\\*\\*\\*\\*+.*")
     (remove_empty
        (append (upc_catalan::token_to_words token (string-before name "*"))
           (list "ratlla" "d'" "asteriscs")
           (upc_catalan::token_to_words token (string-after+ name "*"))
        )
     )
  )

  ((string-matches name ".#####+.*")
     (remove_empty
        (append (upc_catalan::token_to_words token (string-before name "#"))
           (list "ratlla" "de" "coixinets")
           (upc_catalan::token_to_words token (string-after+ name "#"))
        )
     )
  )

  ;; Lletres: acceptem #a (per dir a1, i no la neutra ax)
  ((string-matches name "#[A-Za-z·ÀÈÉÍÏÒÓÚÜÇàèéíïòóúüç]") (list name))
 

  ;; Paraules sense vocals
  ((and (string-matches name "[A-ZÇÑa-zçñ]+") (not (string-matches name ".*[AEIOUÁÉÍÓÚÜÏÀÈÒaeiouàèéíòóúïü]+.*")))
   (catala_speller name))

  ;; Signes puntuacio aïllats : bug: no es tracten bé, com puntuació ...
  ((string-matches name "([.,?¿!¡:;])")(list name))


  ;; Codis
   ((not (lts.in.alphabet name 'catala_downcase_letters))
;   (print "codis: " name)
   (let ((subwords))
      (mapcar
       (lambda (letter)
	 ;; might be symbols or digits
	 (set! subwords
	       (append
		subwords
		(cond
		 ((string-matches letter "[0-9]") 
		  (catala_number letter "0"))
		 ((string-matches letter "[a-záéíóúüïñçàèòA-ZÁÉÍÓÚÜÏÑÇÀÈÒ]")
		    (list (string-append "#" (car (catala_downcase letter)))))
                 ((string-matches letter "[\.]") (list "punt"))
	         ((string-matches letter ":") (list "dos" "punts"))
	         ((string-matches letter "-") (list "guió" ))
	         ((string-matches letter "/") (list "barra" ))
	         ((string-matches letter "#") (list "coixinet" ))
	         ((string-matches letter "\\+") (list "més" ))
;		 (t (list letter))
		 ))))
       (symbolexplode name))
      subwords))
   (t ;; when no specific rules apply do the general ones
   ; (format t "General Rule: %s\n" name)
    (list name)))
)



(define (catala_speller name)
"(catala_speller name)
Split a string into letters, numbers or symbol chars."
;(format t "entering catala_speller %s\n" name)
   (let ((subwords))
      (mapcar
       (lambda (letter)
	 ;; might be symbols or digits
	 (set! subwords
	       (append
		subwords
		(cond
		 ((string-matches letter "[0-9]") 
		  (catala_number letter "0"))
		 ((string-matches letter "[A-ZÁÉÍÏÓÚÜÑÇÀÈÒa-záéíïóúüñçàèò]")
		  (list (string-append "#" (car (catala_downcase letter)))))
                 ((string-matches letter "[\.]") (list "punt"))
	         ((string-matches letter ":") (list "dos" "punts"))
		 ((string-matches letter "_") (list "guió" "baix")) 
		 (t
		  (list letter))))))
       (symbolexplode name))
      subwords))

(define (catala_subs name char1 char2)
"(catala_subs name char1 char2) 
Find char1 in name and replace it by char2."
 (let ((subwords ""))
      (mapcar
       (lambda (letter)
	 (set! subwords
	       (string-append
		subwords
		(cond
		 ((string-matches letter char1) 
		  char2)
		(t
		  (string-append letter))))))
       (symbolexplode name))
      subwords)
)

(define (cut_string name position)
"(cut_string NAME POSITION)
Cuts the string or symbol provided at NAME in two parts. POSITION defines the cut point. 
Set POSITION to 0 and the first part will be empty, set POSITION to the length of NAME and the
second part will be empty."
  (let ( (lengthname (string-length  name))
       )
     (list (substring name 0 position) (substring name position lengthname))
  )
)

(define (split name character)
"(split NAME CHARACTER)
Example: (split \"hello/my/friend\" \"/\") returns (\"hello\" \"my\" \"friend\")"
  (let ( (explosion (symbolexplode name))
         (result) 
         (currstring "") 
         (currsymbol)
       )
     
     ;Check position is in lengthname:
     (while (car explosion)
        (set! currsymbol (car explosion))
        (if (string-equal currsymbol character)
            (begin 
                   (set! result (append result (list currstring)))
                   (set! currstring "")
            )
            (set! currstring (string-append currstring currsymbol))
        )
        (set! explosion (cdr explosion))
     )
     (set! result (append result (list currstring)))
  result
  )
)

(define (join mylist character)
"(join LIST CHARACTER)
Example: (join (\"hello\" \"my\" \"friend\") \"/\") returns \"hello/my/friend\""
  (let ( (tmpstring "")
         (first t)
       )
      (while (car mylist)
         (if (not first)
            (set! tmpstring (string-append tmpstring character (car mylist)))
            (begin
              (set! tmpstring (string-append (car mylist)))
              (set! first nil)
            )
         )
         (set! mylist (cdr mylist))
      )
  tmpstring
  )
)


(define (strconcat mylist)                                                  
"convert list into string"
  (if (null mylist)
      ""
      (string-append (car mylist) (strconcat (cdr mylist)))
  )
)


(define (string-reverse STRING)
"Reverses the string"
  (strconcat (reverse (symbolexplode STRING)))
)


(define (string-after-greedy ATOM AFTER)
"Like string after, but returning the least possible number of characters"
 (string-reverse (string-before (string-reverse ATOM) AFTER))
)

(define (string-after+ ATOM AFTER)
"Like string after, but discards consecutive AFTER characters"
  (let ( (str (string-after ATOM AFTER) )
       )
       (set! str (symbolexplode str))
       (while (string-equal (car str) AFTER)
          (set! str (cdr str))
       )
       (strconcat str)
  )
)

(define (remove_empty mylist)
  (let ( (output) )
   (while (car mylist)
         (set! elem (car mylist))
         (if (symbol? elem)
         (begin
            (set! output (append output (list elem)))
            (set! mylist (cdr mylist))
         )
         (begin
            (if (> (string-length elem) 0 )
              (set! output (append output (list elem)))
            )
            (set! mylist (cdr mylist))
         )
         )
   )
  output
  )
)

(define (catala_splitter name)
"(catala_splitter name)
Split name in two words that start with a cap letter."

 (let ( (wordout1 ) (wordout2 "") (letters (symbolexplode name)) (caps))
   (set! wordout1 (string-append "" (car letters)))
   (set! letters (cdr letters))
   (while (and (not (string-matches (car letters) catalan-regex-upcase-letters))
               (not (eq? nil (car letters)))
          )
          (set! wordout1 (string-append wordout1 (car letters)))
          (set! letters (cdr letters))
   )
   (while (not (eq? nil (car letters)))
          (set! wordout2 (string-append wordout2 (car letters)))
          (set! letters (cdr letters))
   )
   (list wordout1 wordout2)
 )
)
   
   
(define (catala_able_to_say name)
"(catala_able_to_say name)
Return 1 if it's possible say name as a word, 0 otherwise"
(if (and (string-matches name "[A-Za-z]") (string-matches name ".*[aeiouàèéíòóúïüAEIOUÀÈÉÍÒÓUÏÜ].*")))
   "1"
   "0")



(provide 'upc_catalan_tokenizer)
