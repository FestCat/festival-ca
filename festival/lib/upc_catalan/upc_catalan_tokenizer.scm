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
(require 'upc_catalan)
;; Punctuation for the particular language


(set! upc_catalan::token.punctuation "\"'`.,:;!?(){}[]")
(set! upc_catalan::token.prepunctuation "¿¡\"'`({[")
(set! upc_catalan::token.whitespace " \t\n\r")
(set! upc_catalan::token.singlecharsymbols "")

;(set! token.unknown_word_name "")

(define (upc_catalan::token_to_words_debug token name rulename)
  "(upc_catalan::token_to_words_debug token name rulename)
A function to print to stderr some information about the rule applied
from the token to words function"
 (if upc_catalan::debug_token_to_words
  (begin
   (format stderr "===========\n")
   (format stderr "  NAME: %s\n" name)
   (format stderr "  RULE: %s\n" rulename)
  )
 )
)

(set! upc_catalan::token_to_words_context nil)
;;; "Voice/ca token_to_word rules 
(define (upc_catalan::token_to_words token name)
  "(upc_catalan::token_to_words token name)
Specific token to word rules for the voice upc_catalan.  Returns a list
of words that expand given token with name."
  ;(format stderr "upc_catalan::token_to_words  %s\n" name)
  
  (cond
   ; Si el Token no té nom, tampoc té paraules:
   ((string-equal name "") 
     (upc_catalan::token_to_words_debug token name "Sense nom")
   nil)
   ; Si el Token conté una sola vocal que no és cap paraula per sí mateixa, la lletregem.
   ((string-matches name "[áàäâeéèëêíìïîóòöôuúùüûÁÀÄÂÉÈËÊÍÌÏÎÓÒÖÔUÚÙÜÛ]")
     (upc_catalan::token_to_words_debug token name "Nomes es una vocal sola")
       (list (string-append "#" (car (catala_simplify_vowels (car (catala_downcase name)))))))

   ;; Com a separador decimal hem d'assumir que es fa servir la "," perquè el "." es pot fer servir
   ;; per separar milers de centenes.
   ;; Percentatges: 12,32% 2,54% 3% 123%
   ((or (string-matches name "[0-9\.]+\\(\,[0-9]+\\)?%") )
       (upc_catalan::token_to_words_debug token name "Percentatges")
       (append (upc_catalan::token_to_words token (string-before name "%"))
               (list "per" "cent")
       )
   ) 
   ;; Percentatges: 12.32% 2.54% 3% 123%   
   ;((or (string-matches name "[0-9]+\.[0-9]%") (string-matches name "[0-9]+\.[0-9][0-9]%"))
   ;	(let ((number (string-append (string-before name "\.") "," (string-after name "\."))))
   ;	(append (catala_number_decimals (string-before number "%") "0" "," "coma") (list "per" "cent")) ))

   ;; Ordinals
   ((string-matches name "[0-9]+[rntèaºª]")
     (upc_catalan::token_to_words_debug token name "Ordinals generic")
       (cond
           ((string-matches name ".*[è]") (catala_number_ordinals (string-before name "è") "0"))
	   ((string-matches name ".*[r]") (catala_number_ordinals (string-before name "r") "0"))
	   ((string-matches name ".*[n]") (catala_number_ordinals (string-before name "n") "0"))
	   ((string-matches name ".*[t]") (catala_number_ordinals (string-before name "t") "0"))
	   ((string-matches name ".*[º]") (catala_number_ordinals (string-before name "º") "0"))
	   ((string-matches name ".*[ª]") (catala_number_ordinals (string-before name "ª") "1"))
	   ((string-matches name ".*[a]") (catala_number_ordinals (string-before name "a") "1"))))

   ((string-matches name "4rt") 
    (upc_catalan::token_to_words_debug token name "Ordinals 4rt")
      (catala_number_ordinals (string-before name "rt") "0"))
   
	
   ((string-matches name "[13]+rs") 
      (upc_catalan::token_to_words_debug token name "Ordinals 1rs i 3rs")
      (catala_number_ordinals (string-before name "rs") "2"))
   ((string-matches name "[0-9]+ns") 
      (upc_catalan::token_to_words_debug token name "Ordinals XXns")
      (catala_number_ordinals (string-before name "ns") "2"))
   ((string-matches name "4ts") 
      (upc_catalan::token_to_words_debug token name "Ordinals 4ts")
      (catala_number_ordinals (string-before name "ts") "2"))
   ((string-matches name "[0-9]+es") 
      (upc_catalan::token_to_words_debug token name "Ordinals XXes")
      (catala_number_ordinals (string-before name "es") "3"))
     		


   ;; Monedes
   ((or (string-matches name "[0-9]+[\$¤\£]") (string-matches name "[0-9]+,[0-9]+[\$¤\£]") 
 	(string-matches name "[0-9]+[\.][0-9][0-9][0-9][\.][0-9][0-9][0-9][\$¤\£]")
        (string-matches name "[0-9]+[\.][0-9][0-9][0-9][\.][0-9][0-9][0-9][\.][0-9][0-9][0-9][\$¤\£]")
        (string-matches name "[0-9]+[\.][0-9][0-9][0-9][\.][0-9][0-9][0-9][\.][0-9][0-9][0-9][\$¤\£]")  	  	  	
        (string-matches name "[0-9]+[\.][0-9]+[\$¤\£]"))
          (upc_catalan::token_to_words_debug token name "Monedes")

	(cond 
          ((or (string-matches name "[0-9]+[\.][0-9][\$¤\£]") (string-matches name "[0-9]+[\.][0-9][0-9][\$¤\£]"))
             (catala_money (string-append (string-before name "\.") "," (string-after name "\."))))
          (t (catala_money name))))
   
   ((or (string-matches name "[\$][0-9]+")  		(string-matches name "[\$][0-9]+[\,][0-9]+") 
	(string-matches name "[\$][0-9]+[\.][0-9]+")    (string-matches name "[\$][0-9]+[\.][0-9]+[\.][0-9]+") 
	(string-matches name "[\$][0-9]+[\.][0-9]+[\.][0-9]+[\.][0-9]+") 
	(and (string-matches name "[\$][0-9]+.*[\,][0-9]+") (not (string-matches name ".*[A-Za-z].*"))))
          (upc_catalan::token_to_words_debug token name "Monedes (dolar a davant)")
		(catala_money (string-append (string-after name "\$") "\$")))
  
   ;; Hores
   ((or (string-matches name "[0-2][0-9]:[0-5][0-9]") (string-matches name "[0-9]:[0-5][0-9]"))
          (upc_catalan::token_to_words_debug token name "Hores")
    (cond 
        ((or (string-equal "pm" (item.feat token "n.name"))  (string-equal "p.m" (item.feat token "n.name"))
             (string-equal "PM" (item.feat token "n.name")) (string-equal "P.M" (item.feat token "n.name")))
             (let ((hour (parse-number (intern (string-before name ":")))))
		(set! hour (+ hour 12))
                (set! name (string-append hour ":" (string-after name ":"))))
              (catala_hour name))
        (t (catala_hour name))))
   
   ((and (or (string-matches (item.feat token "p.name") "[0-2][0-9]:[0-5][0-9]")
             (string-matches (item.feat token "p.name") "[0-9]:[0-5][0-9]")
         )
         (string-matches name "[aApP]\\.?[mM]") )
     (upc_catalan::token_to_words_debug token name "Ometre AM/PM (ja esta dit)")
     nil)
   
   ;; Hora exacta (cronòmetre)
   ((string-matches name "[0-9]+:[0-5][0-9]:[0-5][0-9]")
          (upc_catalan::token_to_words_debug token name "Hora exacta cronometre")
     (append (catala_number (string-before name ":") "1") '("hores") 
             (catala_number (string-before (string-after name ":") ":") "0") '("minuts")
             (list "i")
             (catala_number (string-after (string-after name ":") ":") "0") '("segons"))) 

   ;; Dates
   (( and (or (string-matches name "[0-3][0-9]/[0-1][0-9]/[0-9][0-9]+") (string-matches name "[1-9]/[0-1][0-9]/[0-9][0-9]+") 
        (string-matches name "[0-3][0-9]/[1-9]/[0-9][0-9]+")      (string-matches name "[1-9]/[1-9]/[0-9][0-9]+"))
        (not (or (string-matches name "[0-9]+/1[3-9]/[0-9]+") (string-matches name "3[2-9]/[0-9]+/[0-9]+"))))
          (upc_catalan::token_to_words_debug token name "Dates")
  	 (catala_date name))
   
    (( and (or (string-matches name "[1-3][0-9]-1[0-9]-[0-9][0-9]+") 
	(string-matches name "[1-9]-1[0-2]-[0-9][0-9]+") 
	(string-matches name "[1-3][0-9]-[1-9]-[0-9][0-9]+") 
	(string-matches name "[1-9]-[1-9]-[0-9][0-9]+"))
        (not (or (string-matches name "[0-9]+-1[3-9]-[0-9]+") (string-matches name "3[2-9]-[0-9]+-[0-9]+"))))
          (upc_catalan::token_to_words_debug token name "Dates, amb guio")
  	 (catala_date (catala_subs name "-" "/")))

   ;; Números de teléfon
   ((and (string-equal "1" (catala_telph token)) 
	 ;; Format 92-572-5623 (optional + followed by numbers or dashes)
	 (string-matches name "[+]?[-0-9][-0-9][-0-9][-0-9][-0-9]+")
    )
       (upc_catalan::token_to_words_debug token name "Num. de telefon")
     (catala_speller name))
   
   ;; Llista de números (són lletrejades les xifres)
   ((and (string-matches name "[0-9]+") 
	 (or (string-matches (item.feat token "n.name") "[1-9][0-9]+") 
             (or (string-matches (item.feat token "p.name") "[1-9][0-9]+")
                 (string-matches (item.feat token "p.p.name") "[1-9][0-9]+")
             )
         )
    )
      (upc_catalan::token_to_words_debug token name "LLista de numeros")
      (item.set_feat token "punc" ",")
      (catala_speller name)
   )
    
   ;; Resultats esportius
   ((string-matches name "[0-9]+-[0-9]+") 
     (upc_catalan::token_to_words_debug token name "Resultat esportiu")
     (catala_score name))
    
   ;; WWW
   ;; Pàgines web:
   ((and 
      (not (equal? upc_catalan::token_to_words_context "webpage"))
      (or 
        (and (string-matches name "\\([a-zA-Z]+://\\)?\\([wW][wW][wW]\\.\\)?\\([a-z0-9]+\\.\\)+[a-z0-9]+\\([a-z&=?_/0-9\\.]\\)*") 
        ; http opcional, www opcional i despres web
             (not (string-matches name "[0-9\\.]+")) ; but not 1.234
        )
        (string-matches name "[a-z]://")
        (and (string-matches name "[a-z0-9]+[\.][a-z0-9\.]+/?") ; festcat.talp.cat
             (not (string-matches name "[0-9\\.]+")) ; but not 1.234
        )
        (string-matches name "\\([a-zA-Z]+://\\)\\([wW][wW][wW]\\.\\)?")
        (string-matches name "www")
        (string-matches name "WWW")
      )
    )
    (upc_catalan::token_to_words_debug token name "Pagina web")
      (let ( (laweb name) (output))
        (set! upc_catalan::token_to_words_context "webpage")
        (if (string-matches laweb "[a-zA-Z]+://.*")
              (begin 
                (set! output (append output 
                                      (upc_catalan::token_to_words token (string-before laweb "://")) 
                                      (list "dos" "punts" "barra" "barra"))
                 )
                 (set! laweb (string-after laweb "://"))
              )
        )
        (if (string-matches laweb "[wW][wW][wW].*")
             (begin
                (set! output (append output
                                     (list "tres" "#ws"))
                )
                (set! laweb (cadr (cut_string laweb (length "www"))))
             )
        )
        (if (string-matches laweb "\\..*")
            (begin
               (set! output (append output (list "punt")))
               (set! laweb (cadr (cut_string laweb 1)))
            )
        )
        (set! output (append output (upc_catalan::token_to_words token laweb)))
        (set! upc_catalan::token_to_words_context nil)
      output
      )
   )
   ((string-equal name "i/o") 
     (upc_catalan::token_to_words_debug token name "conjuncio i/o")
     (list "i" "o")
   )
  ; ==============    Abreviatures   ================
   ((or (string-matches name "telf\.") 
        (string-matches name "telf") 
        (and (string-matches name "tel") 
             (string-matches (item.feat token "n.name") "[-0-9]+" ) 
        )
    )
     (upc_catalan::token_to_words_debug token name "Abrev. telefon")
     (list "telèfon")
   )

   ((string-matches name "[Ss][tT]\.?") 
     (upc_catalan::token_to_words_debug token name "Abrev. Sant")
     (list "sant")
   )

   ;; Abreviatures d'adreces
   ((and (or (string-matches name "[Cc]/")
             (and (string-matches name "[Cc]")
                  (string-equal (item.feat token "punc") ".")
             )
         )
         (not (equal? upc_catalan::token_to_words_context "webpage"))
    )
     (upc_catalan::token_to_words_debug token name "Abrev. carrer")
     (if (string-equal (item.feat token "punc") ".")
         (item.set_feat token "punc" "")
     )
     (list "carrer"))

   ((string-matches name "[Aa]v\\.?/?")
     (upc_catalan::token_to_words_debug token name "Abrev. avinguda")
     (if (string-equal (item.feat token "punc") ".")
         (item.set_feat token "punc" "")
     )
     (list "avinguda"))

   ((string-matches name "[Pp]g\\.?/?")
     (upc_catalan::token_to_words_debug token name "Abrev. passeig")
     (if (string-equal (item.feat token "punc") ".")
         (item.set_feat token "punc" "")
     )
     (list "passeig"))

   ((string-matches name "[Pp]l\\.?/?")
     (upc_catalan::token_to_words_debug token name "Abrev. placa")
     (if (string-equal (item.feat token "punc") ".")
         (item.set_feat token "punc" "")
     )
     (list "plaça"))

   ((string-matches name "entr\\.?/?")
     (upc_catalan::token_to_words_debug token name "Abrev. entresol")
     (if (string-equal (item.feat token "punc") ".")
         (item.set_feat token "punc" "")
     )
     (list "entresol"))
   ((string-matches name "pr\\.?/?")
     (upc_catalan::token_to_words_debug token name "Abrev. principal")
     (if (string-equal (item.feat token "punc") ".")
         (item.set_feat token "punc" "")
     )

     (list "principal"))
   ((string-matches name "esc\\.?/?")
     (upc_catalan::token_to_words_debug token name "Abrev. escala")
     (if (string-equal (item.feat token "punc") ".")
         (item.set_feat token "punc" "")
     )
     (list "escala"))

  ;; Abreviatures d'unitats
  ((string-matches name "kg")
     (upc_catalan::token_to_words_debug token name "Abrev. kg")
     (if (string-equal (item.feat token "punc") ".")
         (item.set_feat token "punc" "")
     )
   (list "quilograms")
  )
  ; ============== Fi d'abreviatures ================
  ; Separar les paraules per barres llegint "barra"
  ((string-matches name ".*/.*")
   (upc_catalan::token_to_words_debug token name "Token amb barra")
    (catala_divide_by_separator token name "/" "barra")
  )
   ;; Take care of websites without "www"  .festcat.talp.cat.
   ;  Sentences like "era gran.molt gran" will be read as "era gran punt molt gran"
   ; Sentences like "era gran.Molt gran" will be fixed later with reparapuntuacio.
   ((and (equal? upc_catalan::token_to_words_context "webpage") 
         (string-matches name "[a-z0-9]+[\.][a-z0-9\.]+")
    )
      (upc_catalan::token_to_words_debug token name "Domini web")
        (append (upc_catalan::token_to_words token (string-before name "\.")) 
               (list "punt")
               (upc_catalan::token_to_words token (string-after name "\."))
       )
   )
   ; Si estem tractant una web, els interrogants han de ser llegits
   ((and (equal? upc_catalan::token_to_words_context "webpage")
         (string-matches name ".*\\?.*")
    )
       (upc_catalan::token_to_words_debug token name "Separa interrogant")
       (catala_divide_by_separator token name "?" "interrogant")
   )
   ; Si estem tractant una web, els ampersands han de ser llegits
   ((and (equal? upc_catalan::token_to_words_context "webpage")
         (string-matches name ".*&.*")
    )
       (upc_catalan::token_to_words_debug token name "Separa ampersand")
       (catala_divide_by_separator token name "&" "ampersand")
   )
   ; Si estem tractant una web, els signes = han de ser llegits
   ((and (equal? upc_catalan::token_to_words_context "webpage")
         (string-matches name ".*=.*")
    )
       (upc_catalan::token_to_words_debug token name "Separa iguals")
       (catala_divide_by_separator token name "=" "igual")
   )

   ;; e-m@il
   ((string-matches name "<.*@.*>")  ;; quoted e-mail
      (upc_catalan::token_to_words_debug token name "email amb format <test@example.com>")
    (let ((nick (string-after (string-before name "@") "<")) (server (string-before (string-after name "@") ">")))
       (append (upc_catalan::token_to_words token nick) (list "@") (upc_catalan::token_to_words token server))))
   ((string-matches name ".*@.*")  ;; e-mail
      (upc_catalan::token_to_words_debug token name "arrova")
    (let ((nick (string-before name "@")) (server (string-after name "@")))
       (append (upc_catalan::token_to_words token nick) (list "arrova") (upc_catalan::token_to_words token server))))

   ;; Números
   ((string-matches name "0[0-9]+") 
      (upc_catalan::token_to_words_debug token name "Numero que comenca per zero")
    (catala_speller name))

   ((string-matches name "[0-9]+") 
      (upc_catalan::token_to_words_debug token name "Numeros")
    (catala_number name "0"))

   ;; Números en format xxx.xxx xxx.xxx.xxx xxx.xxx.xxx.xxx
   ((or (string-matches name "[0-9]+[\.][0-9][0-9][0-9]") 
	(string-matches name "[0-9]+[\.][0-9][0-9][0-9][\.][0-9][0-9][0-9]") 
	(string-matches name "[0-9]+[\.][0-9][0-9][0-9][\.][0-9][0-9][0-9][\.][0-9][0-9][0-9]"))
      (upc_catalan::token_to_words_debug token name "Numeros amb punts xxx.xxx xxx.xxx.xxx...")
		(catala_number_point name "0"))

   ;; Números amb decimals
   ((and      (string-matches name "[0-9]+,[0-9]+") 
         (not (string-matches name "[A-Za-z].*")))
                (upc_catalan::token_to_words_debug token name "Numeros amb decimals")
		(catala_number_decimals name "0" "," "coma"))
   ((or (string-matches name "[0-9]+[\.][0-9][0-9][0-9],[0-9]+")
        (string-matches name "[0-9]+[\.][0-9][0-9][0-9][\.][0-9][0-9][0-9],[0-9]+")
	(string-matches name "[0-9]+[\.][0-9][0-9][0-9][\.][0-9][0-9][0-9][\.][0-9][0-9][0-9],[0-9]+"))
            (upc_catalan::token_to_words_debug token name "Numeros amb decimals (2)")
            (catala_number_decimals name "0" "," "coma"))
   ((or (string-matches name "[0-9]+[\.][0-9]")
        (string-matches name "[0-9]+[\.][0-9][0-9]"))
            (upc_catalan::token_to_words_debug token name "Numeros amb decimals amb punt")
            (catala_number_decimals name "0" "." "punt" )) 

   ;; Comparacions amb "més gran que", "més petit que": Primera versió, per provar, només funciona amb nombres.
   ((string-matches name "[0-9]+<[0-9]+")
       (upc_catalan::token_to_words_debug token name "Comparem numeros: menor que")
       (append 
           (upc_catalan::token_to_words token (string-before name "<" ) )
           (list "menor" "que")
           (upc_catalan::token_to_words token (string-after name "<" ) )
       )
   )
   ((string-matches name "[0-9]+>[0-9]+")
       (upc_catalan::token_to_words_debug token name "Comparem numeros: mes gran que")
       (append 
           (upc_catalan::token_to_words token (string-before name ">" ) )
           (list "més" "gran" "que")
           (upc_catalan::token_to_words token (string-after name ">" ) )
       )
   )
   ((string-matches name "[0-9a-zA-Z]+<<+[0-9a-zA-Z]+.*")
       (upc_catalan::token_to_words_debug token name "Comparem numeros: molt menor que")
       (append 
           (upc_catalan::token_to_words token (string-before name "<<" ) )
           (list "molt" "menor" "que")
           (upc_catalan::token_to_words token (string-after name "<<" ) )
       )
   )
   ((string-matches name "[0-9a-zA-Z]+>>+[0-9a-zA-Z]+.*")
       (upc_catalan::token_to_words_debug token name "Comparem numeros: molt mes gran que")
       (append 
           (upc_catalan::token_to_words token (string-before name ">>" ) )
           (list "molt" "més" "gran" "que")
           (upc_catalan::token_to_words token (string-after name ">>" ) )
       )
   )

;; "<" ">" com cometes:
   ((string-matches name (string-append "<+" catalan-regex-all-letters "+>+"))
       (upc_catalan::token_to_words_debug token name "Cometes baixes ascii <<Hola>>")
       (upc_catalan::token_to_words token (string-after+ (string-before name ">") "<" ) )
   )

;; BEGIN Apòstrofs

((string-matches name "^d'.+")
 (append (list "d'") (upc_catalan::token_to_words token (string-after name "d'")))
 )
((string-matches name "^D'.+")
 (append (list "D'") (upc_catalan::token_to_words token (string-after name "D'")))
 )
((string-matches name ".+-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-hi")) (list "-hi"))
 )
((string-matches name ".+-ho$")
 (append (upc_catalan::token_to_words token (string-before name "-ho")) (list "-ho"))
 )
((string-matches name ".+'l$")
 (append (upc_catalan::token_to_words token (string-before name "'l")) (list "'l"))
 )
((string-matches name "^l'.+")
 (append (list "l'") (upc_catalan::token_to_words token (string-after name "l'")))
 )
((string-matches name "^L'.+")
 (append (list "L'") (upc_catalan::token_to_words token (string-after name "L'")))
 )
((string-matches name ".+-la$")
 (append (upc_catalan::token_to_words token (string-before name "-la")) (list "-la"))
 )
((string-matches name ".+-la-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-la-hi")) (list "-la" "-hi"))
 )
((string-matches name ".+-la'n$")
 (append (upc_catalan::token_to_words token (string-before name "-la'n")) (list  "-la" "'n"))
 )
((string-matches name ".+-l'en$")
 (append (upc_catalan::token_to_words token (string-before name "-l'en")) (list "-l" "'en"))
 )
((string-matches name ".+-les$")
 (append (upc_catalan::token_to_words token (string-before name "-les")) (list "-les"))
 )
((string-matches name ".+-les-en$")
 (append (upc_catalan::token_to_words token (string-before name "-les-en")) (list "-les" "-en"))
 )
((string-matches name ".+-les-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-les-hi")) (list "-les" "-hi"))
 )
((string-matches name ".+-l'hi$")
 (append (upc_catalan::token_to_words token (string-before name "-l'hi")) (list  "-l" "'hi"))
 )
((string-matches name ".+-li$")
 (append (upc_catalan::token_to_words token (string-before name "-li")) (list "-li"))
 )
((string-matches name ".+-li-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-li-hi")) (list "-li" "-hi"))
 )
((string-matches name ".+-li-ho$")
 (append (upc_catalan::token_to_words token (string-before name "-li-ho")) (list "-li" "-ho"))
 )
((string-matches name ".+-li'l$")
 (append (upc_catalan::token_to_words token (string-before name "-li'l")) (list "-li"  "'l"))
 )
((string-matches name ".+-li-la$")
 (append (upc_catalan::token_to_words token (string-before name "-li-la")) (list "-li" "-la"))
 )
((string-matches name ".+-li-les$")
 (append (upc_catalan::token_to_words token (string-before name "-li-les")) (list "-li" "-les"))
 )
((string-matches name ".+-li'ls$")
 (append (upc_catalan::token_to_words token (string-before name "-li'ls")) (list "-li" "'ls"))
 )
((string-matches name ".+-li'n$")
 (append (upc_catalan::token_to_words token (string-before name "-li'n")) (list "-li" "'n"))
 )
((string-matches name ".+-lo$")
 (append (upc_catalan::token_to_words token (string-before name "-lo")) (list "-lo"))
 )
((string-matches name ".+-los$")
 (append (upc_catalan::token_to_words token (string-before name "-los")) (list "-los"))
 )
((string-matches name ".+-los-el$")
 (append (upc_catalan::token_to_words token (string-before name "-los-el")) (list  "-los" "-el"))
 )
((string-matches name ".+-los-els$")
 (append (upc_catalan::token_to_words token (string-before name "-los-els")) (list "-los" "-els"))
 )
((string-matches name ".+-los-en$")
 (append (upc_catalan::token_to_words token (string-before name "-los-en")) (list "-los" "-en"))
 )
((string-matches name ".+-los-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-los-hi")) (list "-los" "-hi"))
 )
((string-matches name ".+-los-ho$")
 (append (upc_catalan::token_to_words token (string-before name "-los-ho")) (list "-los" "-ho"))
 )
((string-matches name ".+-los-la$")
 (append (upc_catalan::token_to_words token (string-before name "-los-la")) (list "-los" "-la"))
 )
((string-matches name ".+-los-les$")
 (append (upc_catalan::token_to_words token (string-before name "-los-les")) (list "-los" "-les"))
 )
((string-matches name ".+'ls$")
 (append (upc_catalan::token_to_words token (string-before name "'ls")) (list "'ls"))
 )
((string-matches name ".+'ls-els$")
 (append (upc_catalan::token_to_words token (string-before name "'ls-els")) (list "'ls" "-els"))
 )
((string-matches name ".+'ls-en$")
 (append (upc_catalan::token_to_words token (string-before name "'ls-en")) (list "'ls" "-en"))
 )
((string-matches name ".+'ls-hi$")
 (append (upc_catalan::token_to_words token (string-before name "'ls-hi")) (list "'ls" "-hi"))
 )
((string-matches name ".+'ls-la$")
 (append (upc_catalan::token_to_words token (string-before name "'ls-la")) (list "'ls" "-la"))
 )
((string-matches name ".+'ls-les$")
 (append (upc_catalan::token_to_words token (string-before name "'ls-les")) (list "'ls" "-les"))
 )
((string-matches name ".+'m$")
 (append (upc_catalan::token_to_words token (string-before name "'m")) (list "'m"))
 )
((string-matches name "^m'.+")
 (append (list "m'") (upc_catalan::token_to_words token (string-after name "m'")))
 )

((string-matches name "^M'.+")
 (append (list "M'") (upc_catalan::token_to_words token (string-after name "M'")))
 )
((string-matches name ".+-me$")
 (append (upc_catalan::token_to_words token (string-before name "-me")) (list "-me"))
 )
((string-matches name ".+-me'l$")
 (append (upc_catalan::token_to_words token (string-before name "-me'l")) (list "-me" "'l"))
 )
((string-matches name ".+-me-la$")
 (append (upc_catalan::token_to_words token (string-before name "-me-la")) (list "-me" "-la"))
 )
((string-matches name ".+-me-les$")
 (append (upc_catalan::token_to_words token (string-before name "-me-les")) (list "-me" "-les"))
 )
((string-matches name ".+-me-li$")
 (append (upc_catalan::token_to_words token (string-before name "-me-li")) (list "-me" "-li"))
 )
((string-matches name ".+-me'ls$")
 (append (upc_catalan::token_to_words token (string-before name "-me'ls")) (list "-me" "'ls"))
 )
((string-matches name ".+-me'n$")
 (append (upc_catalan::token_to_words token (string-before name "-me'n")) (list "-me" "'n"))
 )
((string-matches name ".+-m'hi$")
 (append (upc_catalan::token_to_words token (string-before name "-m'hi")) (list "-m" "'hi"))
 )
((string-matches name ".+-m'ho$")
 (append (upc_catalan::token_to_words token (string-before name "-m'ho")) (list "-m" "'ho"))
 )
((string-matches name ".+'n$")
 (append (upc_catalan::token_to_words token (string-before name "'n")) (list "'n"))
 )
((string-matches name "^n'.+")
 (append (list "n'") (upc_catalan::token_to_words token (string-after name "n'")))
 )
((string-matches name "^N'.+")
 (append (list "N'") (upc_catalan::token_to_words token (string-after name "N'")))
 )
((string-matches name ".+-ne$")
 (append (upc_catalan::token_to_words token (string-before name "-ne")) (list "-ne"))
 )
((string-matches name ".+-n'hi$")
 (append (upc_catalan::token_to_words token (string-before name "-n'hi")) (list "-n" "'hi"))
 )
((string-matches name ".+-nos$")
 (append (upc_catalan::token_to_words token (string-before name "-nos")) (list "-nos"))
 )
((string-matches name ".+-nos-el$")
 (append (upc_catalan::token_to_words token (string-before name "-nos-el")) (list "-nos" "-el"))
 )
((string-matches name ".+-nos-els$")
 (append (upc_catalan::token_to_words token (string-before name "-nos-els")) (list "-nos" "-els"))
 )
((string-matches name ".+-nos-en$")
 (append (upc_catalan::token_to_words token (string-before name "-nos-en")) (list "-nos" "-en"))
 )
((string-matches name ".+-nos-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-nos-hi")) (list "-nos" "-hi"))
 )
((string-matches name ".+-nos-ho$")
 (append (upc_catalan::token_to_words token (string-before name "-nos-ho")) (list "-nos"  "-ho"))
 )
((string-matches name ".+-nos-les$")
 (append (upc_catalan::token_to_words token (string-before name "-nos-les")) (list "-nos"  "-les"))
 )
((string-matches name ".+-nos-li$")
 (append (upc_catalan::token_to_words token (string-before name "-nos-li")) (list "-nos"  "-li"))
 )
((string-matches name ".+'ns$")
 (append (upc_catalan::token_to_words token (string-before name "'ns")) (list "'ns"))
 )
((string-matches name ".+'ns-el$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-el")) (list "'ns"  "-el"))
 )
((string-matches name ".+'ns-els$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-els")) (list "'ns"  "-els"))
 )
((string-matches name ".+'ns-en$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-en")) (list "'ns"  "-en"))
 )
((string-matches name ".+'ns-hi$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-hi")) (list "'ns"  "-hi"))
 )
((string-matches name ".+'ns-ho$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-ho")) (list "'ns"  "-ho"))
 )
((string-matches name ".+'ns-la$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-la")) (list "'ns"  "-la"))
 )
((string-matches name ".+'ns-les$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-les")) (list "'ns"  "-les"))
 )
((string-matches name ".+'ns-li$")
 (append (upc_catalan::token_to_words token (string-before name "'ns-li")) (list "'ns"  "-li"))
 )
((string-matches name ".+'s$")
 (append (upc_catalan::token_to_words token (string-before name "'s")) (list "'s"))
 )
((string-matches name "^s'.+")
 (append (list "s'") (upc_catalan::token_to_words token (string-after name "s'")))
 )
((string-matches name "^S'.+")
 (append (list "S'") (upc_catalan::token_to_words token (string-after name "S'")))
 )
((string-matches name ".+-se$")
 (append (upc_catalan::token_to_words token (string-before name "-se")) (list "-se"))
 )
((string-matches name ".+-se'l$")
 (append (upc_catalan::token_to_words token (string-before name "-se'l")) (list "-se"  "'l"))
 )
((string-matches name ".+-se-la$")
 (append (upc_catalan::token_to_words token (string-before name "-se-la")) (list "-se"  "-la"))
 )
((string-matches name ".+-se-les$")
 (append (upc_catalan::token_to_words token (string-before name "-se-les")) (list "-se"  "-les"))
 )
((string-matches name ".+-se-li$")
 (append (upc_catalan::token_to_words token (string-before name "-se-li")) (list "-se"  "-li"))
 )
((string-matches name ".+-se'ls$")
 (append (upc_catalan::token_to_words token (string-before name "-se'ls")) (list "-se"  "'ls"))
 )
((string-matches name ".+-se'm$")
 (append (upc_catalan::token_to_words token (string-before name "-se'm")) (list "-se"  "'m"))
 )
((string-matches name ".+-se'n$")
 (append (upc_catalan::token_to_words token (string-before name "-se'n")) (list "-se"  "'n"))
 )
((string-matches name ".+-se'ns$")
 (append (upc_catalan::token_to_words token (string-before name "-se'ns")) (list "-se"  "'ns"))
 )
((string-matches name ".+-se't$")
 (append (upc_catalan::token_to_words token (string-before name "-se't")) (list "-se"  "'t"))
 )
((string-matches name ".+-se-us$")
 (append (upc_catalan::token_to_words token (string-before name "-se-us")) (list "-se"  "-us"))
 )
((string-matches name ".+-s'hi$")
 (append (upc_catalan::token_to_words token (string-before name "-s'hi")) (list "-s"  "'hi"))
 )
((string-matches name ".+-s'ho$")
 (append (upc_catalan::token_to_words token (string-before name "-s'ho")) (list "-s"  "'ho"))
 )
((string-matches name ".+'t$")
 (append (upc_catalan::token_to_words token (string-before name "'t")) (list "'t"))
 )
((string-matches name "^t'.+")
 (append (list "t'") (upc_catalan::token_to_words token (string-after name "t'")))
 )
((string-matches name "^T'.+")
 (append (list "T'") (upc_catalan::token_to_words token (string-after name "T'")))
 )
((string-matches name ".+-te$")
 (append (upc_catalan::token_to_words token (string-before name "-te")) (list "-te"))
 )
((string-matches name ".+-te'l$")
 (append (upc_catalan::token_to_words token (string-before name "-te'l")) (list "-te"  "'l"))
 )
((string-matches name ".+-te-la$")
 (append (upc_catalan::token_to_words token (string-before name "-te-la")) (list "-te"  "-la"))
 )
((string-matches name ".+-te-les$")
 (append (upc_catalan::token_to_words token (string-before name "-te-les")) (list "-te"  "-les"))
 )
((string-matches name ".+-te-li$")
 (append (upc_catalan::token_to_words token (string-before name "-te-li")) (list "-te"  "-li"))
 )
((string-matches name ".+-te'ls$")
 (append (upc_catalan::token_to_words token (string-before name "-te'ls")) (list "-te"  "'ls"))
 )
((string-matches name ".+-te'n$")
 (append (upc_catalan::token_to_words token (string-before name "-te'n")) (list "-te"  "'n"))
 )
((string-matches name ".+-te'ns$")
 (append (upc_catalan::token_to_words token (string-before name "-te'ns")) (list "-te"  "'ns"))
 )
((string-matches name ".+-t'hi$")
 (append (upc_catalan::token_to_words token (string-before name "-t'hi")) (list "-t"  "'hi"))
 )
((string-matches name ".+-t'ho$")
 (append (upc_catalan::token_to_words token (string-before name "-t'ho")) (list "-t"  "'ho"))
 )
((string-matches name ".+-us-el$")
 (append (upc_catalan::token_to_words token (string-before name "-us-el")) (list "-us"  "-el"))
 )
((string-matches name ".+-us-els$")
 (append (upc_catalan::token_to_words token (string-before name "-us-els")) (list "-us"  "-els"))
 )
((string-matches name ".+-us-em$")
 (append (upc_catalan::token_to_words token (string-before name "-us-em")) (list "-us"  "-em"))
 )
((string-matches name ".+-us-en$")
 (append (upc_catalan::token_to_words token (string-before name "-us-en")) (list "-us"  "-en"))
 )
((string-matches name ".+-us-ens$")
 (append (upc_catalan::token_to_words token (string-before name "-us-ens")) (list "-us"  "-ens"))
 )
((string-matches name ".+-us-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-us-hi")) (list "-us"  "-hi"))
 )
((string-matches name ".+-us-ho$")
 (append (upc_catalan::token_to_words token (string-before name "-us-ho")) (list "-us"  "-ho"))
 )
((string-matches name ".+-us-la$")
 (append (upc_catalan::token_to_words token (string-before name "-us-la")) (list "-us"  "-la"))
 )
((string-matches name ".+-us-les$")
 (append (upc_catalan::token_to_words token (string-before name "-us-les")) (list "-us"  "-les"))
 )
((string-matches name ".+-us-li$")
 (append (upc_catalan::token_to_words token (string-before name "-us-li")) (list "-us"  "-li"))
 )
((string-matches name ".+-vos$")
 (append (upc_catalan::token_to_words token (string-before name "-vos")) (list "-vos"))
 )
((string-matches name ".+-vos-el$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-el")) (list "-vos"  "-el"))
 )
((string-matches name ".+-vos-els$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-els")) (list "-vos"  "-els"))
 )
((string-matches name ".+-vos-em$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-em")) (list "-vos"  "-em"))
 )
((string-matches name ".+-vos-en$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-en")) (list "-vos"  "-en"))
 )
((string-matches name ".+-vos-ens$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-ens")) (list "-vos"  "-ens"))
 )
((string-matches name ".+-vos-hi$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-hi")) (list "-vos"  "-hi"))
 )
((string-matches name ".+-vos-ho$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-ho")) (list "-vos"  "-ho"))
 )
((string-matches name ".+-vos-la$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-la")) (list "-vos"  "-la"))
 )
((string-matches name ".+-vos-les$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-les")) (list "-vos"  "-les"))
 )
((string-matches name ".+-vos-li$")
 (append (upc_catalan::token_to_words token (string-before name "-vos-li")) (list "-vos"  "-li"))
 )

;; END Apòstrofs

   ;; Números romans
  ((string-matches name "\\(II?I?\\|IV\\|VI?I?I?\\|IX\\|X[VIX]*\\)")
     (upc_catalan::token_to_words_debug token name "Numeros romans")
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
    (upc_catalan::token_to_words_debug token name "Segles")
    ( append (list "segle") (catala_number (tok_roman_to_numstring (string-after name "\.")) "0")))
   
   ;; Abreviatures comunes
   ((and (string-matches name "s") (string-matches (item.feat token "n.name") "\\(II?I?\\|IV\\|VI?I?I?\\|IX\\|X[VIX]*\\)"))
     (upc_catalan::token_to_words_debug token name "Abrev. Segle")
     (list "segle"))
   
   ((and (string-matches name "dC") (or (string-matches (item.feat token "pp.name") "segle") 
					(string-matches (item.feat token "pp.name") "s.") 
					(string-matches (item.feat token "pp.name") "s") 
					(string-matches (item.feat token "p.name") "[0-9]+"))) 
     (upc_catalan::token_to_words_debug token name "Abrev. Despres de Crist")
     (list "després" "de" "Crist"))

   ((and (string-matches name "aC") (or (string-matches (item.feat token "pp.name") "segle") 
					(string-matches (item.feat token "pp.name") "s.") 
					(string-matches (item.feat token "pp.name") "s") 
					(string-matches (item.feat token "p.name") "[0-9]+")))
     (upc_catalan::token_to_words_debug token name "Abrev. abans de Crist")
     (list "abans" "de" "Crist"))
  
  ;; Separació de Paraules tipus WindowsXP
   ((string-equal "1" (catala_two_caps name))
        (upc_catalan::token_to_words_debug token name "Paraula dues majuscules")
          (append (upc_catalan::token_to_words token (car (catala_splitter name))) 
                  (upc_catalan::token_to_words token (car (cdr (catala_splitter name))))
          )
   )

  ;; Paraules compostes amb text y número separades per apòstrofs: Ex. Sidney'02
  ((string-matches name (string-append catalan-regex-all-letters "+\'[0-9]+")) 
     (upc_catalan::token_to_words_debug token name "Barcelona'92")
        (append (upc_catalan::token_to_words token (string-before name "\'")) 
                (upc_catalan::token_to_words token (string-after name "\'"))
        )
  )

  ;; Paraules compostes amb text y número separades per apòstrofs: Ex. Barcelona-92
  ((string-matches name (string-append catalan-regex-all-letters "+\-[0-9]+" ) )
     (upc_catalan::token_to_words_debug token name "Barcelona-92")
        (append (upc_catalan::token_to_words token (string-before name "\-"))
                (upc_catalan::token_to_words token (string-after name "\-"))
        )
  )
 
  ; Ratlles que delimiten parts de documents
  ((string-matches name ".*_____+.*")
     (upc_catalan::token_to_words_debug token name "ratlla de guio baix")
     (remove_empty
        (append (upc_catalan::token_to_words token (string-before name "_"))
           (list "ratlla" "de" "guió" "baix")
           (upc_catalan::token_to_words token (string-after+ name "_"))
        )
     )
  )

  ((string-matches name ".*=====+.*")
     (upc_catalan::token_to_words_debug token name "ratlla d'iguals")
     (remove_empty
        (append (upc_catalan::token_to_words token (string-before name "="))
           (list "ratlla" "d'" "iguals")
           (upc_catalan::token_to_words token (string-after+ name "="))
        )
     )
  )

  ((string-matches name ".*-----+.*")
     (upc_catalan::token_to_words_debug token name "ratlla de guions")
     (remove_empty
        (append (upc_catalan::token_to_words token (string-before name "-"))
           (list "ratlla" "de" "guions")
           (upc_catalan::token_to_words token (string-after+ name "-"))
        )
     )
  )

  ((string-matches name ".*\\*\\*\\*\\*\\*+.*")
     (upc_catalan::token_to_words_debug token name "ratlla d'asteriscs")
     (remove_empty
        (append (upc_catalan::token_to_words token (string-before name "*"))
           (list "ratlla" "d'" "asteriscs")
           (upc_catalan::token_to_words token (string-after+ name "*"))
        )
     )
  )

  ((string-matches name ".#####+.*")
     (upc_catalan::token_to_words_debug token name "ratlla de coixinets")
     (remove_empty
        (append (upc_catalan::token_to_words token (string-before name "#"))
           (list "ratlla" "de" "coixinets")
           (upc_catalan::token_to_words token (string-after+ name "#"))
        )
     )
  )
  ; If we detect a list with "*" or "-" at the beginning of the line, we process it properly.
  ( (and (string-matches name "^\s*[*-].*")
         (string-matches (item.feat token "whitespace") "\n+")
    )
     (upc_catalan::token_to_words_debug token name "Element de llista")
         (let ( (paraula nil ) (output nil) )
             (set! paraula (string-after-regex+ name "[*-]"))
;             (format t "Name: %s\n" name)
;             (format t "Paraula: %s\n" paraula)
             (if (> (length paraula) 0 )
                 (set! output (upc_catalan::token_to_words token paraula))
                 (set! output nil)
             )

         (if (item.prev token)
             (item.set_feat (item.prev token) "punc" ",")
         )
         output
         ) 
  )

  ;; Paraules amb numeros al final com "camisa20"
  ((string-matches name "[a-zA-ZÁÀÄÂÉÈËÊÍÌÏÎÓÒÖÔÚÙÜÛàáäâèéëêíìïîòóöôúùüûçÇñÑ'·]+[0-9]+")
     (upc_catalan::token_to_words_debug token name "Paraula amb num enganxat")
    (append (upc_catalan::token_to_words token (string-before-regex name "[0-9]"))
            (upc_catalan::token_to_words token (string-after-regex+ name "[a-zA-ZÁÀÄÂÉÈËÊÍÌÏÎÓÒÖÔÚÙÜÛàáäâèéëêíìïîòóöôúùüûçÇñÑ'·]"))
    )
  )
  ;; Paraules amb numeros al principi com "20camisa"
  ((string-matches name "[0-9]+[a-zA-ZÁÀÄÂÉÈËÊÍÌÏÎÓÒÖÔÚÙÜÛàáäâèéëêíìïîòóöôúùüûçÇñÑ'·]+")
     (upc_catalan::token_to_words_debug token name "Num amb paraula enganxada")
    (append (upc_catalan::token_to_words token (string-before-regex name "[a-zA-ZÁÀÄÂÉÈËÊÍÌÏÎÓÒÖÔÚÙÜÛàáäâèéëêíìïîòóöôúùüûçÇñÑ'·]"))
            (upc_catalan::token_to_words token (string-after-regex+ name "[0-9]"))
    )
  )
  ;; Paraules que tenen numeros al mig com "camisa10samarreta"
  ((string-matches name "[a-zA-ZÁÀÄÂÉÈËÊÍÌÏÎÓÒÖÔÚÙÜÛàáäâèéëêíìïîòóöôúùüûçÇñÑ'·]+[0-9]+[a-zA-ZÁÀÄÂÉÈËÊÍÌÏÎÓÒÖÔÚÙÜÛàáäâèéëêíìïîòóöôúùüûçÇñÑ'·]+")
     (upc_catalan::token_to_words_debug token name "Paraula1234paraula")
    (append (upc_catalan::token_to_words token (string-before-regex name "[0-9]"))
            (upc_catalan::token_to_words token (string-after-regex+ name "[a-zA-ZÁÀÄÂÉÈËÊÍÌÏÎÓÒÖÔÚÙÜÛàáäâèéëêíìïîòóöôúùüûçÇñÑ'·]"))
    )
  )
  ;; Numeros que tenen paraules al mig com "10camisa30"
  ((string-matches name "[0-9]+[a-zA-ZÁÀÄÂÉÈËÊÍÌÏÎÓÒÖÔÚÙÜÛàáäâèéëêíìïîòóöôúùüûçÇñÑ'·]+[0-9]+")
     (upc_catalan::token_to_words_debug token name "1234paraula4321")
    (append (upc_catalan::token_to_words token (string-before-regex name "[a-zA-ZÁÀÄÂÉÈËÊÍÌÏÎÓÒÖÔÚÙÜÛàáäâèéëêíìïîòóöôúùüûçÇñÑ'·]"))
            (upc_catalan::token_to_words token (string-after-regex+ name "[0-9]"))
    )
  )
  ;; Lletres: acceptem #a (per dir a1, i no la neutra ax)
  ((string-matches name "#[a-zçñ]") 
    (upc_catalan::token_to_words_debug token name "Lletreja lletra sola explícitament #e")
    (list name))

  ;; Paraules sense vocals
  ((and      (string-matches name "[A-ZÇÑa-zçñ·-]+") 
        (not (string-matches name ".*[AEIOUÁÉÍÓÚÜÏÀÈÒaeiouàèéíòóúïü]+.*"))
   )
     (upc_catalan::token_to_words_debug token name "Paraules sense vocals")
      (catala_speller name))

  ;; Signes puntuacio aïllats:
  ((string-matches name "[.,?¿!¡:;]")
     (upc_catalan::token_to_words_debug token name "signes de puntuacio aillats")
      (catala_speller name)
  )

;; Paraules diferents, enganxades per algun caràcter com_això_que_també-es-pot#pronunciar
  ((or(string-matches name ".+_.*")
      (string-matches name ".*_.+")
   )
     (upc_catalan::token_to_words_debug token name "aquesta_regla")
    (let ( (nomsimbol "" ) )
     (if (string-equal upc_catalan::token_to_words_context "webpage")
         (set! nomsimbol (list "guió" "baix"))
     )
     ( catala_divide_by_separator token name "_" nomsimbol)
    )
  )

  ((or(string-matches name ".+#.*")
      (string-matches name ".*#.+")
   )
     (upc_catalan::token_to_words_debug token name "aquesta#regla")
    (let ( (nomsimbol "" ) )
     (if (string-equal upc_catalan::token_to_words_context "webpage")
         (set! nomsimbol (list "coixinet"))
     )
     ( catala_divide_by_separator token name "#" nomsimbol)
    )  
   )

  ((and (or (string-matches name ".*,.+")
            (string-matches name ".+,.*")
            (string-matches name ",.+,")
        )
        (not (string-matches name ",+")) ; fix iff there is more than just ","
   )
   (upc_catalan::token_to_words_debug token name "Arregla puntuacio amb coma")
   (catala_reparapuntuacio token name "," "punc")
  )

  ((and (or (string-matches name ".*;.+")
            (string-matches name ".+;.*")
            (string-matches name ";.+;")
        )
        (not (string-matches name ";+")) ; fix iff there is more than just ";"
   )
   (upc_catalan::token_to_words_debug token name "Arregla puntuacio amb punt i coma")
   (catala_reparapuntuacio token name ";" "punc")
  )

  ((and (or (string-matches name ".*\\.\\.\\..+")
            (string-matches name ".+\\.\\.\\..*")
            (string-matches name "\\.\\.\\..+\\.\\.\\.")
        )
       (not (string-matches name "(\\.\\.\\.)+")) ; fix iff there is more than just "..."
   )
   (upc_catalan::token_to_words_debug token name "Arregla puntuacio amb punts suspensius")
   (catala_reparapuntuacio token name "..." "punc")
  )
  ((and (or (string-matches name ".*\\.[A-ZÁÀÄÂÉÈËÊÍÌÏÎÓÒÖÔÚÙÜÛÇÑ].+")
            (string-matches name ".+\\.+")
            (string-matches name "\\..+\\.")
        )
        (not (string-matches name "\\.+")) ; fix iff there is more than just "."
   )
   (upc_catalan::token_to_words_debug token name "Arregla puntuacio amb punt")
   (catala_reparapuntuacio token name "." "punc")
  )

  ;; Codis
   ((not (string-matches name "[A-Za-zÁÀÄÂÉÈËÊÍÌÏÎÓÒÖÔÚÙÜÛàáäâèéëêíìïîòóöôúùüûçÇñÑ'·]+"))
   (upc_catalan::token_to_words_debug token name "Algo raro -> lletregem")
    (catala_speller name))
   (t ;; when no specific rules apply do the general ones
   ; (format stderr "General Rule: %s\n" name)
   (upc_catalan::token_to_words_debug token name "Paraula normal")
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
		 ((string-matches letter "[äâáàëêéèìîïíöôóòùûúüÄÂÁÀÉÈËÊÍÏÌÎÖÔÓÒÚÜÙÛ]")
		    (list (string-append "#" (car (catala_simplify_vowels (car (catala_downcase letter)))))))
		 ((string-matches letter "[A-ZÑÇa-zñç]")
		  (list (string-append "#" (car (catala_downcase letter)))))
		 ((string-equal letter "$") (list "dòlar"))
		 ((string-equal letter "%") (list "tant" "per" "cent"))
                 ((string-equal letter ".") (list "punt"))
	         ((string-equal letter ":") (list "dos" "punts"))
		 ((string-equal letter "_") (list "guió" "baix"))
		 ((string-equal letter "·") (list "punt" "volat"))
                 ((string-equal letter ".") (list "punt"))
                 ((string-equal letter ",") (list "coma"))
	         ((string-equal letter "-") (list "guió" ))
	         ((string-equal letter "=") (list "igual" ))
	         ((string-equal letter "/") (list "barra" ))
	         ((string-equal letter "º") (list "indicador" "ordinal" "masculí" ))
	         ((string-equal letter "ª") (list "indicador" "ordinal" "femení" ))
	         ((string-equal letter "\\") (list "contrabarra" ))
	         ((string-equal letter "'") (list "apòstrof" ))
	         ((string-equal letter "@") (list "arrova" ))
	         ((string-equal letter "*") (list "asterisc" ))
	         ((string-equal letter "#") (list "coixinet" ))
                 ((string-equal letter "!") (list "exclamació"))
                 ((string-equal letter "&") (list "ampersand"))
                 ((string-equal letter "×") (list "signe" "de" "multiplicació"))
                 ((string-equal letter "÷") (list "signe" "de" "divisió"))
                 ((string-equal letter "|") (list "barra" "vertical"))
                 ((string-equal letter "\"") (list "cometes" "dobles"))
	         ((string-equal letter "+") (list "més" ))
                 ((string-equal letter "}") (list "clau" "corba" "tancada"))
                 ((string-equal letter "{") (list "clau" "corba" "oberta"))
                 ((string-equal letter "[") (list "claudàtor" "obert"))
                 ((string-equal letter "]") (list "claudàtor" "tancat" ))
                 ((string-equal letter ";") (list "punt" "i" "coma" ))
                 ((string-equal letter "©") (list "copyright"))
		 ;(t(list letter))
               ))))
       (symbolexplode name))
      subwords))

(define (catala_reparapuntuacio token name punctchar feature)
"Separa \"paraules,que\" en dos tokens."
  (let ( (llista) (elem) (currtok token) (output) (posa_punc_al_final nil) )
      (set! llista (split name punctchar))
      ; Mirem si l'últim token ha de tenir puntuació:
      (if (string-equal (car (reverse llista)) "")
          (set! posa_punc_al_final punctchar)
      )
      (if (not (equal? 0 (item.feat token feature)) )
          (set! posa_punc_al_final (item.feat token feature))
      )
 
 
      ; Mirem si el token anterior ha de tenir puntuació, i ho fem:
      (if (and (string-equal (car llista) "")
               (item.prev token)
          )
          (item.set_feat (item.prev token) feature punctchar)
      )
      
      ; Eliminem elements buits ( "paraules,,que" -> "paraules,que" )
      (set! llista (remove_empty llista))
      
      (if (car llista) 
          (begin ; modifiquem el token actual, el processem i afegim tants tokens com calgui a continuació.
             (set! elem (car llista))
             (item.set_name token elem)
             (item.set_feat token feature punctchar)
             (set! output (upc_catalan::token_to_words token elem))
             (set! llista (cdr llista))
             ;; Afegim tokens:
             (while (car llista)
                (set! elem (car llista))
                (item.insert currtok elem)
                (set! currtok (item.next currtok))
                (item.set_name currtok elem)
                (item.set_feat currtok feature punctchar)
                (set! llista (cdr llista))
             )
           (if posa_punc_al_final
              (item.set_feat currtok feature posa_punc_al_final)
              (item.set_feat currtok feature "")
           )
          )
          (set! output nil) ; ens hem quedat sense res, token buit.
      )
  output
  )
)

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

(define (comparelongsplit wordexpl charexpl)
  (let ( (explosion wordexpl)
         (singlechar )
         (i 0)
         (goon t)
         (result)
       )
       (if (< (length explosion) (length charexpl))
         (set! result (list nil wordexpl))
         (begin 
           (while (and (< i (length charexpl)) goon)
               (if (string-equal (nth i charexpl) (car explosion))
                   (begin 
                     (set! goon t)
                     (set! explosion (cdr explosion))
                     (set! i (+ i 1))
                   )
                   (set! goon nil)
               )
           )
           (if (not goon)
              (set! result (list nil wordexpl))
              (set! result (list t explosion))
           )
         )
       )
      result
  )
)

(define (split name character)
"(split NAME CHARACTER)
Example: (split \"hello/my/friend\" \"/\") returns (\"hello\" \"my\" \"friend\")"
  (let ( (explosion (symbolexplode name))
         (result) 
         (currstring "") 
         (currsymbol)
         (characterexpl (symbolexplode character))
         (longsplitcomparison)
       )
     
     ;Check position is in lengthname:
     (while (car explosion)
        (set! currsymbol (car explosion))
        (set! longsplitcomparison (comparelongsplit explosion characterexpl))

        (if (car longsplitcomparison)
            (begin 
                   (set! result (append result (list currstring)))
                   (set! currstring "")
                   (set! explosion (car (cdr longsplitcomparison)))
            )
            (begin 
                   (set! currstring (string-append currstring currsymbol))
                   (set! explosion (cdr explosion))
            )
        )
       
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


(define (string-before-regex ATOM REGEX)
"Like string before, but with a regular expression. 
Limited regular expressions that can be applied
to a single character at a time"
  (let ( (text (symbolexplode ATOM))
         (output "")
         (continue t)
         (character)
       )
    (while (and (> (length text) 0) continue)
       (set! character (car text))
       (if (string-matches character REGEX)
           (begin 
              (set! continue nil)
           )
           (begin
              (set! output (string-append output character))
           )
       )
       (set! text (cdr text))
    )
    (if continue
       (set! output "")
    )
    output
  )
)

(define (string-after-regex+ ATOM REGEX)
"Like string after, but with a regular expression. 
Limited regular expressions that can be applied
to a single character at a time"
  (let ( (text (symbolexplode ATOM))
         (output "")
         (continue t)
         (character)
       )
    ;discard loop
    (while (and (> (length text) 0) continue)
       (set! character (car text))
       (if (string-matches character REGEX)
              (begin (set! continue nil))
              (begin (set! text (cdr text)))
       )
    )
    (if continue
       (set! output "")
       (begin
         ; match loop
         (set! continue t)
         (while (and (> (length text) 0) continue)
            (set! character (car text))
            (if (string-matches character REGEX)
	       (begin (set! text (cdr text)))
               (begin (set! continue nil))
            )
         )
         (if continue
             (set! output "")
             (set! output (strconcat  text))
         )
       )
    )
    output
  )
)

(define (string-reverse STRING)
"Reverses the string"
  (strconcat (reverse (symbolexplode STRING)))
)


(define (string-after-greedy ATOM AFTER)
"Like string after, but returning the least possible number of characters"
 (string-reverse (string-before (string-reverse ATOM) (string-reverse AFTER)))
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
               (not (equal? nil (car letters)))
          )
          (set! wordout1 (string-append wordout1 (car letters)))
          (set! letters (cdr letters))
   )
   (while (not (equal? nil (car letters)))
          (set! wordout2 (string-append wordout2 (car letters)))
          (set! letters (cdr letters))
   )
   (list wordout1 wordout2)
 )
)
   
(define (catala_divide_by_separator token name separator separator_name)
"(catala_divide_by_separator token name separator separator_name) example:
Calling from token_to_words using (catala_divide_by_separator token name \"/\" \"barra\")
with a name as \"festcat.cat/cosa/bossa/prova\" will separate the string and convert to words 
every part. It will also add \"barra\" between each part leading to:
\"festcat punt cat barra cosa barra bossa barra prova\" as expected.
"
 (let ( (tmplist) (tmplist2) (elem) )
       (set! tmplist ; Apliquem token_to_words a cada part de la paraula (fescat.cat, cosa, bossa...) i guardem el resultat a tmplist.
        (mapcar 
             (lambda (d) (upc_catalan::token_to_words token d) ) 
             (split name separator)
          )
       )
       ; Per cada resultat...
       (if (string-equal (typeof separator_name) "string")
           (set! separator_name (list separator_name))
       )
       (while (> (length tmplist) 0)
          (set! elem (car tmplist))
         (if (equal? (length elem 0)) ; Afegim la paraula o les paraules del token_to_words a tmplist2, separades per la paraula barra.
                                   ; TODO: Falta afegir una petita pausa?
          (set! tmplist2 (append tmplist2 (list elem)  separator_name  ))
          (set! tmplist2 (append tmplist2       elem   separator_name))
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
   
(define (catala_able_to_say name)
"(catala_able_to_say name)
Return 1 if it's possible say name as a word, 0 otherwise"
(if (and (string-matches name "[A-Za-z]") (string-matches name ".*[aeiouàèéíòóúïüAEIOUÀÈÉÍÒÓUÏÜ].*")))
   "1"
   "0")

(provide 'upc_catalan_tokenizer)

