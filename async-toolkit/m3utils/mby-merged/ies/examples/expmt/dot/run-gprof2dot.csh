#! /bin/csh -f

set gprof2dot = /home/tjnagler/tools/compiles/gprof2dot/gprof2dot.py

set datfile = `ls -tr callgrind.out.* | head -1`
set outfile = model_server

$gprof2dot \
    --format=callgrind \
    --node-thres=1.0 \
    --edge-thres=0.2 \
    --colormap=color \
    --strip \
    --root=main \
    --output=${outfile}.dot \
    ${datfile}

if ( -e ${outfile}.dot ) then
    dot -Tpng ${outfile}.dot -o ${outfile}.png
    dot -Tsvg ${outfile}.dot -o ${outfile}.svg
endif
