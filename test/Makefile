COMP = stack exec corelang --
NASM = nasm
LINK = clang

# HEAP_SIZE  = 0x01000000 # 16 MB
HEAP_SIZE  = 0x00100000 #  1 MB
STACK_SIZE = 0x00100000 #  1 MB

RUNTIME = ../runtime.inc
LIBS = -lc

NASM_FLAGS = -DHEAP_SIZE=$(HEAP_SIZE) -p $(RUNTIME) -f macho64

SOURCES     = fibs93.cl qsort.cl isort.cl minlet.cl
GMACHINES   = $(SOURCES:.cl=.gm)
ASSEMBLES   = $(SOURCES:.cl=.asm)
OBJECTS     = $(SOURCES:.cl=.o)
EXECUTABLES = $(SOURCES:.cl=)
OUTPUTS     = $(SOURCES:.cl=.out)
TEMPOUTPUTS = $(SOURCES:.cl=.out.tmp)
CHECKS      = $(SOURCES:.cl=.check)

all: $(EXECUTABLES)

out: $(OUTPUTS)

check: $(CHECKS)

%.asm: %.cl
	$(COMP) $<

%.pp.asm: %.asm
	$(NASM) -DNOATAT $(NASM_FLAGS) -E $< | grep '^ *[^% ]' > $@

%.o: %.asm $(RUNTIME)
	$(NASM) -DDEBUG $(NASM_FLAGS) -o $@ $<

%: %.o
	$(LINK) -Wl,-stack_size,$(STACK_SIZE) -o $@ $< $(LIBS)

%.out %.out.tmp: %
	./$< > $@

%.check: %.out.tmp
	diff $(@:.check=.out) $<

clean:
	rm -f $(GMACHINES) $(ASSEMBLES) $(OBJECTS) $(EXECUTABLES) $(TEMPOUTPUTS)

.PHONEY: all clean check $(CHECKS)

.PRECIOUS: %.asm
