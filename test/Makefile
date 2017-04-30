COMP = stack exec pukeko -- -l
NASM = nasm
LINK = clang
TIME = /usr/bin/time

HEAP_SIZE  = 0x10000000 # 256 MB
STACK_SIZE = 0x04000000 #  64 MB

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
							catalan.pu \
							wildcard.pu \
							rmq.pu \
							sort_gen.pu \
							rmq_gen.pu
LAMBDAS     = $(SOURCES:.pu=.ll)
ASSEMBLES   = $(SOURCES:.pu=.asm)
OBJECTS     = $(SOURCES:.pu=.o) gc.o
EXECUTABLES = $(SOURCES:.pu=)
BENCHMARKS  = qsort.bench \
							tsort.bench \
							isort.bench \
							queens.bench \
							catalan.bench

all: $(EXECUTABLES)

asm: $(ASSEMBLES)

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

clean:
	rm -f $(LAMBDAS) $(ASSEMBLES) $(OBJECTS) $(EXECUTABLES) *.in

.PHONEY: all asm clean bench clean.bench

.PRECIOUS: %.asm %.ll

bench: $(BENCHMARKS)

sort400.in: sort_gen
	echo 400000 | ./$< > $@

sort20.in: sort_gen
	echo 20000 | ./$< > $@

qsort.bench: qsort sort400.in
	bench "./$< < sort400.in > /dev/null" > $@
	./$< < sort400.in > /dev/null 2>> $@

tsort.bench: tsort sort400.in
	bench "./$< < sort400.in > /dev/null" > $@
	./$< < sort400.in > /dev/null 2>> $@

isort.bench: isort sort20.in
	bench "./$< < sort20.in > /dev/null" > $@
	./$< < sort20.in > /dev/null 2>> $@

queens.bench: queens
	bench "echo 13 | ./$< > /dev/null" > $@
	echo 13 | ./$< > /dev/null 2>> $@

catalan.bench: catalan
	bench "echo 1666 | ./$< > /dev/null" > $@
	echo 1666 | ./$< > /dev/null 2>> $@

rmq.in: rmq_gen
	./$< > $@

rmq.bench: rmq rmq.in
	bench "./$< < rmq.in > /dev/null" > $@
	./$< < rmq.in > /dev/null 2>> $@

clean.bench:
	rm -f $(BENCHMARKS)
