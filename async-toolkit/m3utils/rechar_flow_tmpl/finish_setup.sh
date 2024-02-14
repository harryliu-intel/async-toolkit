#!/bin/sh 

rmdir $RECHAR_SIS_WORKDIR || true
if [ ! -d $RECHAR_SIS_WORKDIR ]; then
	echo "Creating work dir $RECHAR_SIS_WORKDIR"
	mkdir $RECHAR_SIS_WORKDIR
else
	echo ""
	echo "Work dir $RECHAR_SIS_WORKDIR NOT EMPTY..."
	echo ""
	echo "REMEMBER TO RUN, if appropriate:"
	echo ""
	echo "rm -rf $RECHAR_SIS_WORKDIR; mkdir  $RECHAR_SIS_WORKDIR"
	echo ""
fi

shortname=`echo $RECHAR_SIS_WORKDIR | sed 's/siliconsmart\.//'`

echo "shortname is $shortname"

cat > go << EOF
#!/bin/sh -x

./runme |& tee runme.${shortname}

EOF

chmod +x go

echo "run ./go to run the full flow"


