COMP = stack exec pukeko -- -l
NASM = nasm
LINK = clang
TIME = /usr/bin/time

HEAP_SIZE  = 0x30000000 # 768 MB
STACK_SIZE = 0x10000000 # 256 MB

RUNTIME = ../runtime.inc
LIBS = -lc

NASM_FLAGS = -DHEAP_SIZE=$(HEAP_SIZE) -p $(RUNTIME) -f macho64

SOURCES     = monad_io.pu \
							fibs.pu \
							qsort.pu \
							isort.pu \
							tsort.pu \
							queens.pu \
							minlet.pu \
							primes.pu \
							catalan.pu
LAMBDAS     = $(SOURCES:.pu=.ll)
GMACHINES   = $(SOURCES:.pu=.gm)
ASSEMBLES   = $(SOURCES:.pu=.asm)
OBJECTS     = $(SOURCES:.pu=.o) gc.o
EXECUTABLES = $(SOURCES:.pu=)
OUTPUTS     = $(SOURCES:.pu=.out)
TEMPOUTPUTS = $(SOURCES:.pu=.out.tmp)
CHECKS      = $(SOURCES:.pu=.check)

all: $(EXECUTABLES)

out: $(OUTPUTS)

check: $(CHECKS)

%.asm: %.pu prelude.pu
	$(COMP) $<

%.pp.asm: %.asm
	$(NASM) -DNOATAT $(NASM_FLAGS) -E $< | grep '^ *[^% ]' > $@

%.o: %.asm $(RUNTIME)
	$(NASM) -DDEBUG $(NASM_FLAGS) -o $@ $<

gc.o: ../gc.c
	clang -o $@ -c $<

%: %.o gc.o
	$(LINK) -Wl,-stack_size,$(STACK_SIZE) -o $@ gc.o $< $(LIBS)

monad_io.out monad_io.out.tmp: monad_io
	echo "  3  \n2  \n" | ./$< > $@

%.out %.out.tmp: %
	./$< > $@

%.check: %.out.tmp
	diff $(@:.check=.out) $<

clean:
	rm -f $(LAMBDAS) $(GMACHINES) $(ASSEMBLES) $(OBJECTS) $(EXECUTABLES) $(TEMPOUTPUTS)

.PHONEY: all clean check $(CHECKS)

.PRECIOUS: %.asm %.ll
