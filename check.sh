#!/usr/bin/env bash

FILE=$1

BN=$(basename "$FILE")

OUT=${BN%%.*}.png
REF=ref/${BN%%.*}.png

cabal run postscript -- "$FILE" "$OUT"
if [[ $? -ne 0 ]]; then
  echo "Running the program failed."
  exit 1
fi
if [ -f "$OUT" ]; then
  echo "Image file $OUT successfully generated."
else 
  echo "$OUT does not exist."
  exit 1
fi

LIMIT=0.002

SCORE=$(compare -metric MAE $OUT $REF null: 2>&1 | sed 's/.*(\(.*\))/\1/')

if (( $(echo "$SCORE $LIMIT" | awk '{print ($1 > $2)}' ) )); then
  echo "Error: Difference between images too large: $SCORE > $LIMIT."
  exit 1
fi
echo "Image passed checks (score $SCORE)!"
