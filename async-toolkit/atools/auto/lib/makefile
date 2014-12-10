AUTO_IN= $(wildcard *.in)
AUTO_CPP= $(AUTO_IN:.in=.cpp)
INSTALLDIR=$(CAST_HOME)/lib/auto

install: $(INSTALLDIR) $(AUTO_CPP)
	(cd DEEP7M; make)
	(cd ADD7M; make)
	(cd ADD7M_FULLAUTO; make)
	(cd ADD7M_VCP; make)
	(cd ADD7M_VCP_NOM23; make)

$(INSTALLDIR):
	mkdir $(INSTALLDIR)

%.cpp: %.in
	cpp -traditional < $*.in > $(INSTALLDIR)/$*.in
