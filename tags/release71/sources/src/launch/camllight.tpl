#!/bin/sh

stdlib=LIBDIR
includes=""
options=""

case "$LANG" in
  "") ;;
   *) options="$options -lang $LANG";;
esac

while : ; do
  case $1 in
    "")
      exec camlrun $stdlib/camltop -stdlib $stdlib $includes $options;;
    -I|-include)
      includes="$includes -I $2"
      shift;;
    -O|-open)
      options="$options -O $2"
      shift;;
    -g|-debug)
      options="$options $1";;
    -stdlib)
      stdlib=$2
      shift;;
    -lang)
      options="$options -lang $2"
      shift;;
    -*)
      echo "Unknown option \"$1\", ignored" >&2;;
    *)
      PATH=${stdlib}:${PATH}
      export PATH
      exec $1 -stdlib $stdlib $includes $options;;
  esac
  shift
done


