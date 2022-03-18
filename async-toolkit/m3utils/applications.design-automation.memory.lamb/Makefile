.PHONY: all clean cleanall quick

all: m3utils.done m3.done

quick: m3.done

m3utils.done:
	pushd m3utils && $(MAKE)
	touch $@

m3.done: m3utils.done
	pushd m3 && $(MAKE)
	touch $@

clean:
	rm -f m3.done
	pushd m3 && $(MAKE) clean
	rm -f m3utils.done
	pushd m3utils && $(MAKE) clean
