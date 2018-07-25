#!/bin/sh -x
OLD="CVS src bindir.sh"
rm -rf .hidden
mkdir .hidden
mv ${OLD} .hidden
rm -rf *
mv .hidden/* .

cat > Main.m3 << EOF
MODULE Main; BEGIN END Main.
EOF

if [ "x$CM3" = "x" ]; then
	M3BUILD=m3build
else
	M3BUILD=cm3
fi

$M3BUILD >/dev/null 2>&1
ls | grep -v CVS | grep -v src | grep -v bindir.sh | grep -v Main.m3
