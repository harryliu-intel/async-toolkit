# standard setup
TOP = ..
SUBDIRS = bsim4
BIN_TARGETS = aspice aplot reorder_trace convert_trace debug_trace test_model rc_simplify
MAN_TARGETS = aspice.1 aplot.1
INSTALL_TARGETS = $(BIN_TARGETS)
include $(TOP)/Makefile.rules

# define shared aspice object files
ASPICE_OBJ = \
	partition.o simulate.o alint.o subcircuit.o devices.o \
	compare.o expression.o \
	model.o bsim4/bsim4.a \
	digital.o circuit.o digital_sim.o log.o tracelib.o \
	checkpoint.o parse.o dsimlib.o construct.o \
	parsenode.o

# define common header file dependencies
HEADERS = aspice.h circuit.h options.h tracelib.h digital_sim.h \
	  model.h

# aspice related programs
aspice: aspice.o $(ASPICE_OBJ) $(LIB_DIR)/castlib.a

test_model: test_model.o $(ASPICE_OBJ) $(LIB_DIR)/castlib.a

# trace processing programs
aplot: aplot.o tracelib.o $(LIB_DIR)/castlib.a

reorder_trace: reorder_trace.o tracelib.o $(LIB_DIR)/castlib.a

convert_trace: convert_trace.o tracelib.o $(LIB_DIR)/castlib.a

debug_trace: debug_trace.o tracelib.o $(LIB_DIR)/castlib.a

# other programs
rc_simplify: rc_simplify.o $(LIB_DIR)/castlib.a

# SPECIFIC RULES FOR ASPICE_OBJ .o FILES
partition.o: partition.c $(HEADERS)

circuit.o: circuit.c $(HEADERS)

compare.o: compare.c $(HEADERS)

devices.o: devices.c $(HEADERS)

digital_sim.o: digital_sim.c $(HEADERS)

log.o: log.c $(HEADERS)

digital.o: digital.c $(HEADERS)

expression.o: expression.c $(HEADERS)

simulate.o: simulate.c $(HEADERS)

alint.o: alint.c $(HEADERS)

subcircuit.o: subcircuit.c $(HEADERS)

model.o: model.c $(HEADERS)

tracelib.o: tracelib.c $(HEADERS)

checkpoint.o: checkpoint.c $(HEADERS)

construct.o: construct.c $(HEADERS)

parsenode.o: parsenode.c $(HEADERS)

bspline.o: bspline.c $(HEADERS)

parse.o: parse.c $(HEADERS)

dsimlib.o: dsimlib.c $(HEADERS)

# SPECIFIC RULES FOR OTHER .o FILES
aspice.o: aspice.c $(HEADERS)

test_model.o: test_model.c $(HEADERS)

aplot.o: aplot.c aplot.h tracelib.h

reorder_trace.o: reorder_trace.c tracelib.h

convert_trace.o: convert_trace.c tracelib.h

debug_trace.o: debug_trace.c tracelib.h

rc_simplify.o: rc_simplify.c
