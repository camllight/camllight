#!/bin/sh

stdlib=LIBDIR
includes=""
options=""

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
    -*)
      echo "Unknown option \"$1\", ignored" >&2;;
    *)
      exec $1 -stdlib $stdlib $includes $options;;
  esac
  shift
done


