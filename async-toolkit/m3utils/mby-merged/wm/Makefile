# Generate "Thin" White Model Adapter for Integration with VCS
#
PROTOBUF_HOME := $(shell ToolConfig.pl get_tool_path protobuf)
PROTOC        := $(PROTOBUF_HOME)/bin/protoc
PROTOBUF_LIB  := $(PROTOBUF_HOME)/lib
PROTOBUF_GENDIR := target/wm/protobuf

PROTOBUF_C    := /nfs/sc/disks/slx_1443/mwrighto/mby/work_root/protobuf-c-1.3.0_install
PROTOCC       := $(PROTOBUF_C)/bin/protoc-c


VCS_HOME := $(shell ToolConfig.pl get_tool_path vcsmx)
INCLUDES = -I $(PROTOBUF_HOME)/include -I $(PROTOBUF_GENDIR) -I $(VCS_HOME)/include


protobuf_gen: src/main/protobuf/whitemodel.proto
	mkdir -p target/wm/protobuf
	$(PROTOC) $< --cpp_out $(PROTOBUF_GENDIR)
	$(PROTOCC) $< --c_out $(PROTOBUF_GENDIR)

build: protobuf_gen
	g++ -c $(INCLUDES) $(PROTOBUF_GENDIR)/src/main/protobuf/whitemodel.pb.cc
	g++ -c $(INCLUDES) -L $(PROTOBUF_HOME)/lib -lprotobuf src/client/c/mbay_dpi_client.cpp

clean:
	rm -rf $(PROTOBUF_GENDIR)

