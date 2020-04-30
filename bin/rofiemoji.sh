#!/bin/sh

DIR="$HOME/.config/rofiemoji-rofiunicode/lists"
# URL="https://github.com/Kabouik/rofiemoji/raw/master/unicode.txt"
# FILE="$DIR/unicode.txt"
VER='12.0'
URL="https://www.unicode.org/Public/emoji/${VER}/emoji-test.txt"
FILE="$DIR/emojis.txt"

if [ ! -r $FILE ]
then
  if [ ! -d $DIR ]; then mkdir -p $DIR; fi
  curl $URL | grep -v '^#' | grep ' ; fully-qualified ' | cut -d'#' -f2 > $FILE
fi

if [ "$@" ]
then
  smiley=$(echo $@ | cut -d' ' -f1)
  echo -n "$smiley" | xsel -bi
  exit 0
fi

cat $FILE
