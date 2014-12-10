#rule to make c files into s.jumps files using vcc.
%s.jumps %.d : %.c
	$(VCC) $(VCCFLAGS) $< > $@

#Tempory fix while we are still using old vistran
%.s : %.s.jumps
	@ echo "FIXME: I need new vistran!!"
	$(GNUSED) -e '/jump &end/d' < $< > $@

%.vis : %.s
	$(SVASM) $< -o $@

%.vis : %.vasm
	$(SVASM) $< -o $@

%.vo : %.vis
	$(VISTRAN) $< -dup $(VISTRAN_DUP) -o $@

%.vis.c : %.vis
	$(VASIM) 1 $< > $@


#Make the command line switches for the vasim include directories
VASIMINCLUDEPATHSWITCHES := $(foreach dir, $(VASIMINC), -I$(dir) )

#make the command line switches for the predefined preprocessor macros.
VASIMDEFINESWITCHES := $(foreach def,$(VASIMDEFS),-D$(def))

#Make the command line switches for the vasim library directories
VASIMLIBPATHSWITCHES := $(foreach lib, $(VASIMLIBPATH), -L$(lib) )


#Make the command line switches for the vasim libraries
VASIMLIBSWITCHES := $(foreach lib, $(VASIMLIBS), -l$(lib) )

VASIMCC := $(CC) $(VASIMINCLUDEPATHSWITCHES) $(VASIMLIBPATHSWITCHES) $(VASIMLIBSWITCHES) $(VASIMDEFINESWITCHES)

%.vasim : %.vis.c stub.o
	echo $(VASIMINCLUDEPATHSWITCHES)
	$(VASIMCC) -c $< stub.o -o $@

%.vis.d :
	touch $@

stub.o: $(VASIMSTUBC)
	$(VASIMCC) -DLABEL_MAIN=Entry -c $< -o $@


INTERMEDIATEFILETYPES := $(INTERMEDIATEFILETYPES) .s .s.jumps .vis .vo .vis.c
