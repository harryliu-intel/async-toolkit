FT_VERSION := 1.1.12
SYS := $(shell uname -a)
sysrh6 := $(findstring .el6., $(SYS))
svos := $(findstring .svos, $(SYS))
ifneq ($(sysrh6),)
FT_VERSION=1.0.4
else ifneq ($(svos),)
FT_VERSION=1.0.4
endif

.PHONY: default all install
default: all
all: libftd2xx.so 

install:
ifdef SVOS
	ln -sf /usr/lib64/libftd2xx.so.1.0.4 $(DESTDIR)/usr/lib64/libftd2xx.so
else
	sudo cp libftd2xx.so.$(FT_VERSION)          $(DESTDIR)/usr/lib64
	sudo ln -sf /usr/lib64/libftd2xx.so.$(FT_VERSION)       $(DESTDIR)/usr/lib64/libftd2xx.so
	sudo cp *.h                                 $(DESTDIR)/usr/include
	sudo cp *.h                                 $(DESTDIR)/usr/include/services/liteignite/
install: | $(DESTDIR)/usr/include/services/liteignite/
$(DESTDIR)/usr/include/services/liteignite/:
	-sudo mkdir -p $@
endif

clean:
	rm -f libftd2xx.so

clean_lib:
	sudo rm -f /usr/lib64/libftd2xx.so.1.1.12 /usr/local/lib64/libftd2xx.so.1.1.12 /usr/local/lib/libftd2xx.so.1.1.12
ifndef SVOS
	sudo rm -f /usr/lib64/libftd2xx.so /usr/lib64/libftd2xx.so.1.0.4 /usr/local/lib/libftd2xx.so /usr/local/lib/libftd2xx.so.1.0.4 \
	/usr/local/lib64/libftd2xx.so /usr/local/lib64/libftd2xx.so.1.0.4
endif
libftd2xx.so:
	ln -sf $(CURDIR)/libftd2xx.so.$(FT_VERSION) libftd2xx.so
