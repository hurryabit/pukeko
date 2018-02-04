COMP = $(shell stack exec -- which pukeko)
OPTS =
NASM = nasm
LINK = clang
TIME = /usr/bin/time
BENCH = bench $(BENCH_OPTS)

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
							primes.pu \
							catalan.pu \
							wildcard.pu \
							rmq.pu \
							fix.pu \
							lambdalift.pu \
              aliasinline.pu \
							sort_gen.pu \
							rmq_gen.pu
LAMBDAS     = $(SOURCES:.pu=.pl)
TYPES       = $(SOURCES:.pu=.ti)
ASSEMBLES   = $(SOURCES:.pu=.asm)
OBJECTS     = $(SOURCES:.pu=.o) gc.o
EXECUTABLES = $(SOURCES:.pu=)
BENCHMARKS  = qsort.bench \
							tsort.bench \
							isort.bench \
							queens.bench \
							catalan.bench

all: $(EXECUTABLES)

types: $(TYPES)

asm: $(ASSEMBLES)

%.asm: %.pu std2/*.pu $(COMP)
	$(COMP) $(OPTS) -l $<

%.ti: %.pu std2/*.pu $(COMP)
	$(COMP) $(OPTS) -t $<

%.dot: %.pu std2/*.pu $(COMP)
	$(COMP) $(OPTS) -g $<

%.pdf: %.dot
	dot -o $@ -Tpdf $<

%.pp.asm: %.asm
	$(NASM) -DNOATAT $(NASM_FLAGS) -E $< | grep '^ *[^% ]' > $@

%.o: %.asm $(RUNTIME)
	$(NASM) -DDEBUG $(NASM_FLAGS) -o $@ $<

gc.o: ../gc.c
	clang -o $@ -c $<

%: %.o gc.o
	$(LINK) -Wl,-stack_size,$(STACK_SIZE) -o $@ gc.o $< $(LIBS)

clean:
	rm -f $(LAMBDAS) $(TYPES) $(ASSEMBLES) $(OBJECTS) $(EXECUTABLES) *.in

.PHONEY: all asm clean bench clean.bench types

.PRECIOUS: %.asm %.ll %.ti

bench: $(BENCHMARKS)

sort400.in: sort_gen
	echo 400000 | ./$< > $@

sort20.in: sort_gen
	echo 20000 | ./$< > $@

qsort.bench: qsort sort400.in
	$(BENCH) "./$< < sort400.in > /dev/null" > $@
	./$< < sort400.in > /dev/null 2>> $@

tsort.bench: tsort sort400.in
	$(BENCH) "./$< < sort400.in > /dev/null" > $@
	./$< < sort400.in > /dev/null 2>> $@

isort.bench: isort sort20.in
	$(BENCH) "./$< < sort20.in > /dev/null" > $@
	./$< < sort20.in > /dev/null 2>> $@

queens.bench: queens
	$(BENCH) "echo 13 | ./$< > /dev/null" > $@
	echo 13 | ./$< > /dev/null 2>> $@

catalan.bench: catalan
	$(BENCH) "echo 1666 | ./$< > /dev/null" > $@
	echo 1666 | ./$< > /dev/null 2>> $@

rmq.in: rmq_gen
	./$< > $@

rmq.bench: rmq rmq.in
	$(BENCH) "./$< < rmq.in > /dev/null" > $@
	./$< < rmq.in > /dev/null 2>> $@

clean.bench:
	rm -f $(BENCHMARKS)
