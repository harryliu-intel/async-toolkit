AUTO_IN= $(wildcard *.in)
AUTO_CPP= $(AUTO_IN:.in=.cpp)
INSTALLDIR=$(CAST_HOME)/lib/auto/ADD7M

install: $(INSTALLDIR) $(AUTO_CPP)
	-cp -f *.prs $(INSTALLDIR)
	-cp -f *.mag $(INSTALLDIR)

$(INSTALLDIR):
	mkdir $(INSTALLDIR)

%.cpp: %.in
	cpp -traditional < $*.in > $(INSTALLDIR)/$*.in
