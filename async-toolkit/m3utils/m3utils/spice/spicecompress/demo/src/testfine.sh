#!/bin/sh -x

../../spicestream/AMD64_LINUX/spicestream -mode ReadBinary -t xa_100fs -n 0 -o 00.dat
../../spicestream/AMD64_LINUX/spicestream -mode ReadBinary -t xa_100fs -n 1 -o 01.dat
../../spicestream/AMD64_LINUX/spicestream -mode ReadBinary -t xa_100fs -n 2 -o 02.dat
../../spicestream/AMD64_LINUX/spicestream -mode ReadBinary -t xa_100fs -n 3 -o 03.dat
../../spicestream/AMD64_LINUX/spicestream -mode Compress -dump -i 03.dat -o 03.z
