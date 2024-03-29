#!/bin/bash
# Copyright (C) 2009-2011  Antonio Bonafonte
#            Universitat Polit�cnica de Catalunya, Barcelona, Spain
#
#  This script is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation,
#  version 2.1 of the License.
#
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with this library; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

myfile="$1"

if [ ! -e "${myfile}" ]; then
  echo "${myfile} does not exist. Aborting"
  exit 1
fi

mv "${myfile}" "${myfile}.tmp" || exit 1

echo "MNCL" > "${myfile}"
cat >> "${myfile}" << EOF
("#a" nn (((a1) 1)))
("#b" nn (((b E1) 1)))
("#c" nn (((s E1) 1)))
("#d" nn (((d E1) 1)))
("#e" nn (((E1) 1)))
("#f" nn (((E1) 1) ((f ax) 0)))
("#g" nn (((Z E1) 1)))
("#h" nn (((a1 k) 1)))
("#i" nn (((i1) 1)))
("#j" nn (((Z O1) 1) ((t ax) 0)))
("#k" nn (((k a1) 1)))
("#l" nn (((E1) 1) ((l ax) 0)))
("#m" nn (((E1) 1) ((m ax) 0)))
("#n" nn (((E1) 1) ((n ax) 0)))
("#o" nn (((O1) 1)))
("#p" nn (((p e1) 1)))
("#q" nn (((k u1) 1)))
("#r" nn (((E1) 1) ((rr ax) 0)))
("#s" nn (((E1) 1) ((s ax) 0)))
("#t" nn (((t e1) 1)))
("#u" nn (((u1) 1)))
("#v" nn (((b e1) 1) ((b a1) 1) ((S ax) 0)))
("#w" nn (((b e1) 1) ((d o1 b) 1) ((b l ax) 0)))
("#ws" nn (((b e1) 1) ((d o1 b) 1) ((b l ax s) 0)))
("#x" nn (((i1 k s) 1)))
("#y" nn (((i1) 1) ((g r e1) 1) ((g ax) 0)))
("#z" nn (((z e1) 1) ((t ax) 0)))
("#�" nn (((s E1) 1) ((t r ax n) 0) ((k a1) 1) ((d ax) 0)))
("#�" nn (((E1) 1) ((J ax) 0)))
EOF

tail -n +2 "${myfile}.tmp" >> "${myfile}" || exit 1
