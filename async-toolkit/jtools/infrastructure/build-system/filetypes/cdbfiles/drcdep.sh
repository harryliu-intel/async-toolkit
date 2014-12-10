#!/bin/bash

cdbdep="$1"
drcdep="$2"

files=$(cat "$cdbdep" | xargs -n 1 echo | grep '^/')

cat <<EOF > "$drcdep"
DRCDEP_TARGET_FILES := $(echo "$drcdep" | sed -e 's/\#/\\\#/g' )
\$(DRCDEP_TARGET_FILES): \\
EOF


for file in $files; do
    signoff="$(dirname "$file")/drc_signoff"
    [ -e "$signoff" ] && echo "$signoff \\" | sed -e 's/\#/\\\#/g' >> "$drcdep"
done
echo "" >> "$drcdep"



