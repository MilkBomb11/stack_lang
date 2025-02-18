cc := g++
target := target/main
src = main.cpp
obj := $(wildcard *.o)
args := -std=c++20

all : $(target)

$(target) : $(src)
	$(cc) $(args) $(src) -o $(target)

.PHONY : clean
.PHONY : run

clean :
	rm $(obj) $(target)

run :
	./$(target) src