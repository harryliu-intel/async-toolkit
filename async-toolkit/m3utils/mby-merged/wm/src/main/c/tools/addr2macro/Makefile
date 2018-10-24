TARGET = addr2macro
CXXFLAGS = -std=c++11

ifeq ($(DEBUG),1)
CXXFLAGS += -O0 -g
else
CXXFLAGS += -O3
endif

$(TARGET): regs.h main.cpp
	$(CXX) $(CXXFLAGS) main.cpp -o $(TARGET)

clean:
	rm $(TARGET)

.PHONY: clean
