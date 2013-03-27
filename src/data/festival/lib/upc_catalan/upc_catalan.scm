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


(define (intro-catalan)
"(intro-catalan)
 Synthesize an introduction to the Festival Speech Synthesis System
 in Catalan.  Catalan voice must already be selected for this."
 (if (boundp 'datadir)
   (tts (path-append datadir "upc_catalan/cat_intro.text") nil)
   (tts (path-append libdir "upc_catalan/cat_intro.text") nil))

)

(defvar upclexdir (path-append lexdir "upc")
"upclexdir is the directory where Catalan dictionaries are stored")

(defvar upc_catalan::debug_token_to_words nil
"upc_catalan::debug_token_to_words: If not nil, prints information
about token to words")

(define (load-catalan-support)
"(load-catalan-support)
Loads dictionaries, phoneset and all the voice independent language tools for Catalan."
  (load-catalan-support-dialect "central")
)  

(define (load-catalan-support-dialect dialect)
"(load-catalan-support)
Loads dictionaries, phoneset and all the voice independent language tools for Catalan."
 
    (if (not (member_string upclexdir load-path))
                      (set! load-path (cons upclexdir load-path)))
    (require 'upclex_catalan)
    (upc_catalan_lex_select_dialect dialect)
    (require 'upc_ca_generic_phoneset)
    (require 'upc_ca_generic_tokenizer)
    (require 'upc_ca_generic_tagger)
    (require 'upc_ca_generic_lexicon)
    (require 'upc_ca_generic_phrasing)
    
    ; Not used in hts voices:
    (require 'upc_ca_generic_duration)
    (require 'upc_ca_generic_f0model)
    (require 'upc_ca_generic_intonation)

    ;; Select appropriate phone set
    (upc_ca_generic::select_phoneset_dialect dialect)

    ;; Select appropriate tokenization
    (upc_ca_generic::select_tokenizer)

    ;; For part of speech tagging
    (upc_ca_generic::select_tagger)

    (upc_ca_generic::select_lexicon_dialect dialect)
    (upc_ca_generic::select_phrasing)
    
    (upc_ca_generic::select_duration)
    (upc_ca_generic::select_intonation)
    ; Catalan language can't synthesize without voices
    (Param.set 'Synth_Method nil)
t
)
(provide 'upc_catalan)
