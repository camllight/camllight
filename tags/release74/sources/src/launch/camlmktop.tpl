#!/bin/sh

stdlib=LIBDIR
linkfiles=""
custom=""
includes=""
cc="CC"
ccfiles=""
cclib=""
ccopt=""
output=camltop.out
p=""
debug=""

perv="baltree bool char eq exc fchar filename float format fstring fvect \
    gc genlex hashtbl int io iparsing lexing list map obj pair parsing \
    printexc printf queue random ref set sort stack stream string \
    toplevel vect"

while :; do
  case $1 in
    "")
      break;;
    *.zo)
      linkfiles="$linkfiles $1"
      perv="$perv `echo $1 | sed -e 's/\.zo$//'`";;
    *.zi)
      perv="$perv `echo $1 | sed -e 's/\.zi$//'`";;
    -I|-include)
      includes="$includes -I $2"
      shift;;
    -o)
      output=$2; shift;;
    -p*)
      perv="$perv profiler"; p=p;; 
    -stdlib)
      stdlib=$2; shift;;
    -custom)
      custom="-custom /tmp/camlprim.$$.c";;
    -g|-debug)
      debug="-g";;
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

camlrun $stdlib/provide -stdlib $stdlib $includes $perv > /tmp/camlreq.$$ \
  || exit $?
camlrun $stdlib/camllink -stdlib $stdlib $custom -require /tmp/camlreq.$$ \
  -exec /tmp/camlout.$$ -g $includes stdlib$p.zo toplib.zo $linkfiles topmain.zo || exit $?
camlrun $stdlib/expunge $debug /tmp/camlout.$$ $output sys $perv || exit $?
rm -f /tmp/camlreq.$$ /tmp/camlout.$$
if test -n "$custom"; then
  if mv $output /tmp/camlcode.$$ \
     && $cc -I$stdlib -o $output $ccopt /tmp/camlprim.$$.c $ccfiles  \
            -L$stdlib $cclib -lcaml LIBS -lm \
     && cat /tmp/camlcode.$$ >> $output
  then
    rm -f /tmp/camlprim.$$.c camlprim.$$.o /tmp/camlcode.$$
    exit 0
  else
    rm -f $output /tmp/camlprim.$$.c camlprim.$$.o /tmp/camlcode.$$
    exit 2
  fi
fi

exit 0
