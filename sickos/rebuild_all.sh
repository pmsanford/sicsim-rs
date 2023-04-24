#!/bin/bash
FILES="src/asm/*.sic"
for f in $FILES
do
  BASENAME=$(basename $f)
  EXTLESS="${BASENAME%.*}"
  echo "Rebuilding $f"
  cargo run -p sicasm $f "src/bin/$EXTLESS"
done
