;;; Copyright (C) 2009-2011  Antonio Bonafonte
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

(require 'lts_build)
(set! allowables 
      '
	((a _epsilon_ a a1 ax)
        (b _epsilon_ p b b-b )
        (c _epsilon_ k s g k-s  t-S)
	(ç _epsilon_ s)
        (d _epsilon_ d  t t-s)
        (e _epsilon_ ax e1 E1 e E)
        (f _epsilon_ f )
        (g _epsilon_ g Z t-S k d-Z d g-g k-s)
        (h _epsilon_)
        (i _epsilon_ i i1 j )
        (j _epsilon_ Z)
        (k _epsilon_ k)
        (l _epsilon_ l L)
        (m _epsilon_ m)
        (n _epsilon_ n m J)
        (ñ _epsilon_ J)
        (o _epsilon_ u o1 O1 o O)
        (p _epsilon_ p b b-b)
        (q _epsilon_ k)
        (r _epsilon_ r rr)
        (s _epsilon_ s z)
        (t _epsilon_ t d l m d-Z d-z)
        (u _epsilon_ u u1 w)
        (v _epsilon_ b)
        (w _epsilon_ b u w)
        (x _epsilon_ S k k-s g g-z t-S )
        (y _epsilon_ J)
        (z _epsilon_ z s)
	(á _epsilon_ a1)
        (à _epsilon_ a1 a)
	(è _epsilon_ E1) 
	(é _epsilon_ e1 e)
        (í _epsilon_ i1 i)
	(ò _epsilon_ O1 O)
	(ó _epsilon_ o1 o)
	(ú _epsilon_ u1 u)
	(ü _epsilon_ w u )
        (ï _epsilon_ i i1)
    	(· _epsilon_)
 	(- _epsilon_)
        (# #)))
