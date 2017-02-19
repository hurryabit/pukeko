CL=stack exec corelang --
GM=../gm/gm
HEAP=131072
STACK=8192

%.gm: %.cl
	$(CL) -g $<

%: %.gm
	$(GM) -g -h $(HEAP) -s $(STACK) $<

clean:
	rm -f *.gm

.PHONEY: clean fibs93 qsort isort minlet

.PRECIOUS: %.gm
