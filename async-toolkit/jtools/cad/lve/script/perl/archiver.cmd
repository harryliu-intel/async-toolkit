#!/bin/sh
exec unzip -qq -c "$1" "$2" >"$3" 2>/dev/null  
