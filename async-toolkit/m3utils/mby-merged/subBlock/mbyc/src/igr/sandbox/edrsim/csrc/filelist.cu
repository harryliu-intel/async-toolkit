PIC_LD=$(VCS_CC)

ARCHIVE_OBJS=
ARCHIVE_OBJS += _93898_archive_1.so
_93898_archive_1.so : archive.0/_93898_archive_1.a
	@$(AR) -s $<
	@$(PIC_LD) -shared  -o .//../simv.daidir//_93898_archive_1.so -Wl,--whole-archive $< -Wl,--no-whole-archive
	@rm -f $@
	@ln -sf .//../simv.daidir//_93898_archive_1.so $@






%.o: %.c
	$(CC_CG) $(CFLAGS_CG) -c -o $@ $<
CU_UDP_OBJS = \


CU_LVL_OBJS = \
SIM_l.o 

MAIN_OBJS = \
objs/amcQw_d.o 

CU_OBJS = $(MAIN_OBJS) $(ARCHIVE_OBJS) $(CU_UDP_OBJS) $(CU_LVL_OBJS)

