;;; Copyright (C) 2009-2011  Antonio Bonafonte et al.
;;;            Universitat Politècnica de Catalunya, Barcelona, Spain
;;;
;;;  This script is free software; you can redistribute it and/or
;;;  modify it under the terms of the GNU Lesser General Public
;;;  License as published by the Free Software Foundation,
;;;  version 2.1 of the License.
;;;
;;;  This library is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;  Lesser General Public License for more details.
;;;
;;;  You should have received a copy of the GNU Lesser General Public
;;;  License along with this library; if not, write to the Free Software
;;;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We want to acknowledge the original English version lib/tokenpos.scm
;;; by the "Centre for Speech Technology Research;University of Edinburgh, UK"
;;; available in Festival which served as a reference to this script.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Functions used in identifying token types.
;;;

(defvar catalan-regex-upcase-letters "[A-ZÀÁÈÉÌÍÒÓÙÚÏÜÑÇ]")
(defvar catalan-regex-downcase-letters "[a-zàéèíòóúïüñç]")
(defvar catalan-regex-all-letters "[a-zàéèíòóúïüñçA-ZÀÁÈÉÌÍÒÓÙÚÏÜÑÇ·]")
(defvar catalan-regex-all-letters-and-numbers "[a-zàéèíòóúïüñçA-ZÀÁÈÉÌÍÒÓÙÚÏÜÑÇ·0-9]")
(define (catala_tok_rex sc)
  "(cat_tok_rex sc)
Returns 1 if King like title is within 3 tokens before or 2 after."
  (let ((kings '(rei papa pare duc tsar emperador cèsar baró comte)))
    (if (or (member_string 
	     (catala_downcase_string (item.feat sc "R:Token.pp.name"))
	     kings)
	    (member_string 
	     (catala_downcase_string (item.feat sc "R:Token.pp.p.name"))
	     kings)
	    (member_string 
	     (catala_downcase_string (item.feat sc "R:Token.n.name"))
	     kings))
	"1"
	"0")))

(define (catala_tok_queen sc)
  "(cat_tok_queen sc)
Returns 1 if Queen like title is within 3 tokens before or 2 after."
  (let ((queens '(reina duquessa tsarina emperadriu baronessa comtessa)))
    (if (or (member_string 
	     (catala_downcase_string (item.feat sc "R:Token.pp.name"))
	     queens)
	    (member_string 
	     (catala_downcase_string (item.feat sc "R:Token.pp.p.name"))
	     queens)
	    (member_string 
	     (catala_downcase_string (item.feat sc "R:Token.n.name"))
	     queens))
	"1"
	"0")))

(define (catala_tok_rex_names sc)
  "(cat_tok_rex sc)
Returns 1 if King like title is within 1 token before."
    (let ((kings '(  ;; Noblessa Catalana
        ramon berenguer arnau guifré 
	;; Reis espanyols
	carles felip ferran josep amadeu alfons joan carles 
        ;; Papes catòlics
        alexandre sixte pius víctor calixte urbà fèlix marcel silvestre juli anastasi benet
        inocenci bonifaci benedicte gregori honori martí pau climent
        ;; Altres reis
        enric lluís jordi eduard guillem ricard napoleó pere albert jaume)))
    (if (member_string (catala_downcase_string (item.feat sc "R:Token.p.name"))
	     kings)
	  
	"1"
	"0")))

(define (catala_tok_queen_names sc)
  "(cat_tok_rex sc)
Returns 1 if King like title is within 1 token before."
  (let ((queens '(caterina alexandra isabel maria joana cristina)))
    (if (member_string (catala_downcase_string (item.feat sc "R:Token.p.name"))
	     queens)
	  
	"1"
	"0")))

(define (catala_start_caps name)
 "(catala_start_caps name)
Returns 1 if name start with a caps letter and the 2nd letter isn't a cap letter"
(if (and (string-matches (string-append (car (symbolexplode name))) catalan-regex-upcase-letters) (string-matches (string-append (car (cdr (symbolexplode name)))) catalan-regex-downcase-letters)
    )
   "1"
     "0"
 )
)

(define (tok_allcaps sc)
  "(tok_allcaps sc)
Returns 1 if sc's name is all capitals, 0 otherwise"
  (if (string-matches (item.name sc) (string-append catalan-regex-upcase-letters "+"))
      "1"
      "0"))


(define (catala_two_caps name)
  "(catala_two_caps name)
Returns 1 if name has two capitals letters (one at start) with a no capital letter between them at least, 0 otherwise"
  (let ((explode (symbolexplode name)) (letter) (output "0"))
     (set! letter (car explode))
     (if (string-matches letter catalan-regex-upcase-letters)
        (begin
           (set! explode (cdr explode))
           (set! letter (car explode))
           (if (string-matches letter catalan-regex-downcase-letters)
              (begin
                 (set! explode (cdr explode))
                 (set! letter (car explode))
                 (while (not (eq? nil letter))
                    (if (string-matches letter catalan-regex-upcase-letters)
                        (begin
                            (set! letter nil)
                            (set! output "1")
                        )
                        (begin
                            (set! explode (cdr explode))
                            (set! letter (car explode))
                        )
                    )
                )
              )
           )
      )
     )
   output
   )
)

(define (catala_words_ordinals_ms sc)
  "(catala_words_ordinals_ms sc )
Returns 1 if a list's word is within 1 token before or 1 after. Sing. & male words"

  (let ((words '(article vers acte llibre concurs aniversari capítol volúm)))
    (if (member_string (catala_downcase_string (item.feat sc "R:Token.p.name"))
	     words)
	  
	"1"
	    (if (member_string (catala_downcase_string (item.feat sc "R:Token.n.name"))
	     words)
    	    "1"
	    "0"))))

(define (catala_words_ordinals_fs sc)
  "(catala_words_ordinals_ms sc )
Returns 1 if a list's word is within 1 token before or 1 after. Sing. & Female words "

  (let ((words '(secció setmana part frase escena llibreta posició secció guerra assamblea jornada edició olimpiada)))
    (if (member_string (catala_downcase_string (item.feat sc "R:Token.p.name"))
	    words)
	  
	"1"
	    (if (member_string (catala_downcase_string (item.feat sc "R:Token.n.name"))
	     words)
    	    "1"
	    "0"))))

(define (catala_words_ordinals_mp sc)
  "(catala_words_ordinals_ms sc )
Returns 1 if a list's word is within 1 token before or 1 after. Pl. & Male words "

  (let ((words '())) ;; I don't know any word now :P
    (if (member_string (catala_downcase_string (item.feat sc "R:Token.p.name"))
	     words)
	  
	"1"
	    (if (member_string (catala_downcase_string (item.feat sc "R:Token.n.name"))
	     words)
    	    "1"
	    "0"))))

(define (catala_words_ordinals_fp sc)
  "(catala_words_ordinals_ms sc )
Returns 1 if a list's word is within 1 token before or 1 after. Pl. & Female words "

  (let ((words '(jornades edicions olimpiades)))
    (if (member_string (catala_downcase_string (item.feat sc "R:Token.p.name"))
	     words)
	  
	"1"
	    (if (member_string (catala_downcase_string (item.feat sc "R:Token.n.name"))
	     words)
    	    "1"
	    "0"))))

(define (catala_telph sc)
  "(catala_telph sc)
Returns 1 if telephone or some call verb form is within 4, 3, 2 o 1 tokens before."
  (let ((telph '(telèfon telf. telf tel. tel
		trucant trucam trucava truca trucada trucades trucant trucar trucaran 
		trucarem trucaren trucares trucareu trucaria trucarien trucaries
		trucarà trucaràs trucaré trucaríem trucaríeu trucat trucats trucava
		trucaven trucaves truco trucà trucàrem trucàreu trucàvem trucàveu truquem
		truquen truques truquessin truquessis truqueu truqui truquin truquis truqués
		truquéssim truquéssiu truquí)))
    (if (or (member_string 
	     (catala_downcase_string (item.feat sc "pp.name"))
	     telph)
	    (member_string 
	     (catala_downcase_string (item.feat sc "pp.p.name"))
	     telph)
	    (member_string 
	     (catala_downcase_string (item.feat sc "pp.pp.name"))
	     telph)
	    (member_string 
	     (catala_downcase_string (item.feat sc "p.name"))
	     telph))
	"1"
	"0"
   )
  )
)




(provide 'upc_catalan_tokenpos)
