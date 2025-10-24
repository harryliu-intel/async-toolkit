MEM=32G
export MEM

hostname
uname -a
ulimit -a

time  fulcrum \
 --latest jflat --tool=new-dsim --no-append-colon\
 --define=chip.alta.port.attribute.PortAttributes.allowPhantom:false\
 --max-heap-size=64G --cell="`cat $1`"\
   --cast-path=/mnt/fulcrum/alta/mnystrom/p4/hw-main/cast:/mnt/fulcrum/alta/mnystrom/p4/hw-alta/layout/tsmc65/spec \
> /mnt/fulcrum/scratch1/mnystrom/"`cat $1`".dsim

