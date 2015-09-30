#!/bin/sh -x

chmod +x *.script

for s in *.script; do
		echo launching $s
		./$s 2> ../output/${s}.e > ../output/${s}.o &
done

echo waiting...
wait
