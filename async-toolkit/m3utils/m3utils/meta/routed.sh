MEM=32G
export MEM

hostname
uname -a
ulimit -a

time  fulcrum \
 cast_query --max-heap-size=64G --task=subcells --cell="`cat $1`"\
   --filter=routed\
   --define=chip.alta.port.attribute.PortAttributes.allowPhantom:false\
   --define=lib.sram.10T.sram_leaf_attributes.exclude_sram_bits:true \
   --define=lib.6T.6T.attr.exclude_sram_bits:true \
   --define=lib.cam.core.attr.exclude_tcam_bits:true \
   --define=chip.alta.datapath.dp_attr.hstor_prs:true \
   --define=chip.alta.datapath.dp_attr.modctrl_prs:true \
   --define=chip.alta.datapath.dp_attr.modstats_prs:true \
   --define=chip.alta.datapath.array.attr.useRealArraySubseg:true \
   --define=chip.alta.fsched.base.FschedTop.fsched_prs:true \
   --define=chip.alta.fsched.base.FschedTop.esched_prs:true \
   --cast-path=/mnt/fulcrum/alta/mnystrom/p4/hw-main/cast:/mnt/fulcrum/alta/mnystrom/p4/hw-alta/layout/tsmc65/spec \
| tee "/mnt/fulcrum/scratch1/mnystrom/`cat $1`.routed"

