;;; Copyright (C) 2009-2011  Antonio Bonafonte et al.
;;;            Universitat Polit�cnica de Catalunya, Barcelona, Spain
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

;;; Tranformador de d�gits a dates (xx/xx/xxxx)
;;; (require 'upc_catalan_numbers)

(define (catala_date name)
"(catala_date name)
Converteix una cadena de d�gits en una llista de paraules per dir la data."
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
	((string-equal name "03") (string-append "mar�"))
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
