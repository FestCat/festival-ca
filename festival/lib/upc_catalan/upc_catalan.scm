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
(provide 'upc_catalan)
