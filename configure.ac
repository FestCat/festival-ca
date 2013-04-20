# Copyright (C) 2012  Sergio Oller <sergioller@gmail.com>
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


# AC_INIT (package, version, [bug-report], [tarname], [url])
AC_INIT([FestCat: Paquet bàsic], [3.0.3], [https://github.com/festcat/festival-ca/issues],[upc_ca_base], [http://festcat.talp.cat])
AC_PREREQ([2.59])
AC_PROG_SED
AC_PROG_GREP
AC_PROG_AWK
AC_PROG_INSTALL
AC_PROG_MKDIR_P

# If we only want to install...
onlyinstall="dontknow"
AC_ARG_ENABLE(onlyinstall,AS_HELP_STRING([--enable-onlyinstall],[--enable-onlyinstall (do not build, just install.)]),[onlyinstall="yes"])

if test "${onlyinstall}" = "dontknow"; then
  if test -d "src"; then
     onlyinstall="no"
  else
     onlyinstall="yes"
  fi
fi

ONLYINSTALL=${onlyinstall}

if test "${ONLYINSTALL}" = "no"; then

  # Checks for perl.
  AC_PATH_PROG(PERL,perl)

  # Checks for perl modules:
  ## File::Basename
  AC_MSG_CHECKING(for Perl Module File::Basename)
  $PERL -MFile::Basename -e exit >/dev/null 2>&1
  if test $? -ne 0; then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR(Perl Module File::Basename not available)
  else
      AC_MSG_RESULT(yes)
  fi

  ## XML::LibXML
  AC_MSG_CHECKING(for Perl Module XML::LibXML)
  $PERL -MXML::LibXML -e exit >/dev/null 2>&1
  if test $? -ne 0; then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR(Perl Module XML::LibXML not available)
  else
      AC_MSG_RESULT(yes)
  fi
  #

  # Checks for using Wagon:
  AC_ARG_ENABLE(wagonpath,AS_HELP_STRING([--enable-wagonpath],[--enable-wagonpath=/path/to/wagon (specify a custom wagon path)]),[wagonpath=$enableval])

  if test -d "${wagonpath}"; then 
      echo "using custom wagon path: $wagonpath"
  else
      wagonpath="$PATH"
  fi

  # WAGON
  AC_PATH_PROG([WAGON], [wagon], [:], [${wagonpath}])

  if test ! -f "${WAGON}"; then
          AC_MSG_ERROR(Can't find wagon)
  fi

  # Checks for using ngrambuild:
  AC_ARG_ENABLE(ngrambuildpath,AS_HELP_STRING([--enable-ngrambuildpath],[--enable-ngrambuildpath=/path/to/ngram_build (specify a custom ngram_build path)]),[ngrambuildpath=$enableval])

  if test -d "${ngrambuildpath}"; then 
      echo "using custom ngram_build path: $ngrambuildpath"
  else
      ngrambuildpath="$PATH"
  fi
  # NGRAM_BUILD
  AC_PATH_PROG([NGRAMBUILD], [ngram_build], [:], [${ngrambuildpath}])

  if test ! -f "${NGRAMBUILD}"; then
       AC_MSG_ERROR(Can't find ngram_build)
  fi
else
  PERL=
  WAGON=
  NGRAMBUILD=
fi


# Checks for Festival: Required to build and to install.
AC_ARG_ENABLE(festivalpath,AS_HELP_STRING([--enable-festivalpath],[--enable-festivalpath=/path/to/festival (specify a custom festival path)]),[festivalpath=$enableval])

if test -d "${festivalpath}"; then 
    echo "using custom festival path: $festivalpath"
else
    festivalpath="$PATH"
fi

# FESTIVALBIN
AC_PATH_PROG([FESTIVALBIN], [festival], [:], [${festivalpath}])

if test ! -f "${FESTIVALBIN}"; then
        AC_MSG_ERROR(Can't find festival)
fi


# Checks for libdir and datadir
${FESTIVALBIN} --pipe > /dev/null 2>&1 << EOF
(set! fp (fopen "datadir.txt" "w"))
(if (boundp 'datadir)
(format fp datadir)
(format fp libdir)
)
(fclose fp)
EOF

FESTDATADIR=`cat "datadir.txt"`
rm "datadir.txt"

if test ! -d "${FESTDATADIR}"; then
        AC_MSG_ERROR(Can't find neither datadir or libdir)
fi
echo "Festival data directory: ${FESTDATADIR} found."


# There are two ways of adding a language to festival:
# The 2.1 official way: Editing languages.scm
# The languages/ directory way (since 1:2.1~release-2, see http://bugs.debian.org/606173 )
#
# From my perspective the second one is safer.
# Here we detect the version of languages.scm to determine how we should do the installation:
LANGUAGESSCMFILE="${FESTDATADIR}/languages.scm"

if test ! -f "${LANGUAGESSCMFILE}"; then
      AC_MSG_ERROR(Wrong libdir or datadir)
fi

${GREP} "language.select" "${LANGUAGESSCMFILE}" > /dev/null && LANGUAGESSCMVERSION="directory" || LANGUAGESSCMVERSION="2.1"

#if test "$LANGUAGESSCMVERSION" = "2.1" ; then
#    ${GREP} "catalan" "${LANGUAGESSCMFILE}" > /dev/null && LANGUAGESSCMVERSION="alreadydone" || LANGUAGESSCMVERSION="2.1"
#fi


AC_SUBST(ONLYINSTALL)
AC_SUBST(FESTIVALBIN)
AC_SUBST(FESTDATADIR)
AC_SUBST(LANGUAGESSCMFILE)
AC_SUBST(LANGUAGESSCMVERSION)
AC_SUBST(PERL)
AC_SUBST(WAGON)
AC_SUBST(NGRAMBUILD)

if test "${onlyinstall}" = "no"; then
  AC_CONFIG_FILES([src/02-LTSMake/build_lts], [chmod +x src/02-LTSMake/build_lts])
  AC_CONFIG_FILES([src/00-Dictionary/CompileDictionary.sh], [chmod +x src/00-Dictionary/CompileDictionary.sh])
fi

AC_CONFIG_FILES([Makefile])

AC_OUTPUT

