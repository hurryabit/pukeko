COMP = stack exec corelang --
NASM = nasm
LINK = clang

LIBS = -lc

HEAP_SIZE  = 0x01000000 # 16 MB
STACK_SIZE = 0x00100000 #  1 MB

RUNTIME = ../runtime.inc

SOURCES     = fibs93.cl qsort.cl isort.cl minlet.cl
GMACHINES   = $(SOURCES:.cl=.gm)
ASSEMBLES   = $(SOURCES:.cl=.asm)
OBJECTS     = $(SOURCES:.cl=.o)
EXECUTABLES = $(SOURCES:.cl=)
RUNTARGETS  = $(SOURCES:.cl=.run)

all: $(EXECUTABLES)

%.asm: %.cl
	$(COMP) $<

%.o: %.asm $(RUNTIME)
	$(NASM) -DHEAP_SIZE=$(HEAP_SIZE) -p $(RUNTIME) -f macho64 -o $@ $<

%: $.o
	$(LINK) -Wl,-stack_size,$(STACK_SIZE) -o $@ $< $(LIBS)

%.run: %
	./$<

clean:
	rm -f $(GMACHINES) $(ASSEMBLES) $(OBJECTS) $(EXECUTABLES)

.PHONEY: clean $(RUNTARGETS)

.PRECIOUS: %.asm
