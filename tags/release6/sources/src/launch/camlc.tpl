#!/bin/sh

stdlib=LIBDIR
linkalso=true
includes=""
compopt=""
linkopt=""
custom=""
linkfiles=""
cc=CC
ccfiles=""
cclib=""
ccopt=""
linkout=a.out

while : ; do
  case $1 in
    "")
      break;;
    *.ml)
      camlrun $stdlib/camlcomp -stdlib $stdlib $includes $compopt $1 || exit $?
      linkfiles="$linkfiles $1";;
    *.mli)
      camlrun $stdlib/camlcomp -stdlib $stdlib $includes $compopt $1 || exit $?
      ;;
    *.zo)
      linkfiles="$linkfiles $1";;
    -c)
      linkalso=false;;      
    -I|-include)
      includes="$includes -I $2"
      shift;;
    -O|-open)
      compopt="$compopt -O $2"
      shift;;
    -i)
      compopt="$compopt $1";;
    -g|-debug)
      compopt="$compopt $1"
      linkopt="$linkopt $1";;
    -o|-exec)
      linkout=$2
      shift;;
    -stdlib)
      stdlib=$2
      shift;;
    -v|-version)
      echo "The Caml Light system, version 0.6"
      echo "  (standard library from $stdlib)"
      camlrun -V
      camlrun $stdlib/camlcomp -version
      camlrun $stdlib/camllink -version;;
    -files)
      linkfiles="$linkfiles $1 $2"
      shift;;
    -custom)
      custom="-custom /tmp/camlprim.$$.c";;
    *.c)
      $cc -c -I$stdlib $ccopt $1 || exit $?
      ccfiles="$ccfiles `basename $1 .c`.o";;
    *.[oa])
      ccfiles="$ccfiles $1";;
    -l*)
      cclib="$cclib $1";;
    -cc)
      cc=$2; shift;;
    -ccopt)
      ccopt="$ccopt $2"; shift;;
    -*)
      echo "Unknown option \"$1\", ignored" >&2;;
    *)
      echo "I don't know what to do with file \"$1\", ignored" >&2;;
  esac
  shift
done

if $linkalso && test -n "$linkfiles"; then
  camlrun $stdlib/camllink -stdlib $stdlib $includes $custom $linkopt \
    -exec $linkout $stdlib/stdlib.zo $linkfiles || exit $?
  if test -n "$custom"; then
    if mv $linkout /tmp/camlcode.$$ \
       && $cc -I$stdlib -o $linkout $ccopt /tmp/camlprim.$$.c $ccfiles  \
              -L$stdlib $cclib -lcaml -lm \
       && strip $linkout \
       && cat /tmp/camlcode.$$ >> $linkout
    then
      rm -f /tmp/camlprim.$$.c camlprim.$$.o /tmp/camlcode.$$; exit 0
    else
      rm -f $linkout /tmp/camlprim.$$.c camlprim.$$.o /tmp/camlcode.$$; exit 2
    fi
  fi
fi

exit 0
