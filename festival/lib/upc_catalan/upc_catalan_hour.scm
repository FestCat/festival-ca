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

;;; Tranformador de dígits a hores (xx:xx)
(require 'upc_catalan_numbers)

(define (catala_hour name)
"(catala_number name)
Converteix una cadena de dígits en una llista de paraules per dir l'hora."
 (set! hours (string-before name ":"))
 (if (equal? (length hours) 1) (set! hours (string-append "0" hours)))
 (set! range (range_hour hours))
 (set! hours (pm_hours hours))
 (set! nexthour (next_hour hours))
 (set! minuts (string-after name ":"))
 (cond 
       	((string-equal minuts "00") 
            (cond                       
             ((string-equal hours "01") (append (list  "una" "en" "punt") range))
             (t (append (list (car (catala_number hours "1")) "en" "punt") range))))                   
      	((string-equal minuts "07") 
            (cond
             ((string-equal nexthour "01") (append (list "mig" "quart" "d'una") range))
             (t (append (list "mig" "quart" "de" (car (catala_number nexthour "1"))) range) )))
        ((string-equal minuts "15")
            (cond
             ((string-equal nexthour "01") (append (list "un" "quart" "d'una") range))
             (t (append (list "un" "quart" "de" (car (catala_number nexthour "1"))) range))))
	((string-equal minuts "22")
            (cond
             ((string-equal nexthour "01") (append (list "un" "quart" "i" "mig" "d'una") range))
             (t (append (list "un" "quart" "i" "mig" "de" (car (catala_number nexthour "1"))) range))))
       	((string-equal minuts "30")
            (cond
             ((string-equal nexthour "01") (append (list "dos" "quarts" "d'una") range))
             (t (append (list "dos" "quart" "de" (car (catala_number nexthour "1"))) range ))))
       	((string-equal minuts "37")
            (cond
             ((string-equal nexthour "01") (append (list "dos" "quarts" "i" "mig" "d'una") range))
             (t (append (list "dos" "quarts" "i" "mig" "de" (car (catala_number nexthour "1"))) range))))
      	((string-equal minuts "45")
            (cond
             ((string-equal nexthour "01") (append (list "tres" "quarts" "d'una") range))
             (t (append (list "tres" "quarts" "de" (car (catala_number nexthour "1"))) range))))
       	((string-equal minuts "52") 
            (cond
             ((string-equal nexthour "01") (append (list "tres" "quarts" "i" "mig" "d'una") range))
             (t (append (list "tres" "quarts" "i" "mig" "de" (car (catala_number nexthour "1"))) range))))
        (t (cond
            ((string-equal hours "01") (append (list "una" "i" (car (catala_number minuts "0"))) range))
	    (t (append (list (car (catala_number hours "1" )) "i" (car (catala_number minuts "0"))) range ))))))  


(define (pm_hours hour)
"(pm_hours hour)
Redueix les hores als casos 01..12."
(cond 
       	((string-equal hour "13") (string-append "01"))
       	((string-equal hour "14") (string-append "02"))
	((string-equal hour "15") (string-append "03")) 
	((string-equal hour "16") (string-append "04"))
 	((string-equal hour "17") (string-append "05"))
       	((string-equal hour "18") (string-append "06"))
	((string-equal hour "19") (string-append "07")) 
	((string-equal hour "20") (string-append "08"))
	((string-equal hour "21") (string-append "09"))
       	((string-equal hour "22") (string-append "10"))
	((string-equal hour "23") (string-append "11")) 
	((string-equal hour "24") (string-append "12"))
	((string-equal hour "00") (string-append "12"))
        (t (string-append hour))))


(define (next_hour hour)
"(define (next_hour hour)
Retorna la següent hora a l'actual" 
 (cond 
        ((string-equal hour "12") (string-append "01"))
       	((string-equal hour "01") (string-append "02"))
	((string-equal hour "02") (string-append "03")) 
	((string-equal hour "03") (string-append "04"))
 	((string-equal hour "04") (string-append "05"))
       	((string-equal hour "05") (string-append "06"))
	((string-equal hour "06") (string-append "07")) 
	((string-equal hour "07") (string-append "08"))
	((string-equal hour "08") (string-append "09"))
       	((string-equal hour "09") (string-append "10"))
	((string-equal hour "10") (string-append "11")) 
	((string-equal hour "11") (string-append "12"))
        ((string-equal hour "00") (string-append "01"))
	(t (string-append hour))))   

(define (range_hour hours)
(cond
        ((or (string-equal hours "01") (string-equal hours "02") (string-equal hours "03") 
             (string-equal hours "04") (string-equal hours "05")) (list "de" "la" "matinada"))
        ((or (string-equal hours "06") (string-equal hours "07") (string-equal hours "08") 
             (string-equal hours "09") (string-equal hours "10") (string-equal hours "11")) (list "del" "matí"))
        ((or (string-equal hours "12") (string-equal hours "13") (string-equal hours "14")) (list "del" "migdia"))
	((or (string-equal hours "15") (string-equal hours "16") (string-equal hours "17") 
             (string-equal hours "18")) (list "de" "la" "tarda"))
	((or (string-equal hours "19") (string-equal hours "20") (string-equal hours "21")) (list "del" "vespre"))
	((or (string-equal hours "22") (string-equal hours "23") (string-equal hours "00")
             (string-equal hours "24")) (list "de" "la" "nit"))))

  
        
