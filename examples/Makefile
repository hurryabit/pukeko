COMP = $(shell stack exec -- which pukeko)
OPTS =
NASM = nasm
LINK = clang
TIME = /usr/bin/time
BENCH = bench $(BENCH_OPTS)

HEAP_SIZE  = 0x10000000 # 256 MB
STACK_SIZE = 0x04000000 #  64 MB

ROOT = ..
RUNTIME_DIR := $(ROOT)
RUNTIME_INC := $(RUNTIME_DIR)/runtime.inc
LIBS = -lc

NASM_FLAGS := -DHEAP_SIZE=$(HEAP_SIZE) -P$(RUNTIME_INC)
LINK_FLAGS :=

OS := $(shell uname -s)
ifeq ($(OS), Darwin)
  NASM_FLAGS += -f macho64
  LINK_FLAGS += -Wl,-stack_size,$(STACK_SIZE)
else
  NASM_FLAGS += -f elf64
endif


SOURCES     = monad_io.pu \
							hello.pu \
							rev.pu \
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
							sort_gen.pu \
							rmq_gen.pu
ASSEMBLES   = $(SOURCES:.pu=.asm)
BYTECODES   = $(SOURCES:.pu=.pub)
OBJECTS     = $(SOURCES:.pu=.o) gc.o
EXECUTABLES = $(SOURCES:.pu=)
BENCHMARKS  = qsort.bench \
							qsort.ruceki.bench \
							tsort.bench \
							isort.bench \
							isort.ruceki.bench \
							queens.bench \
							queens.ruceki.bench \
							catalan.bench

all: $(EXECUTABLES)

asm: $(ASSEMBLES)

%.asm: %.pu std2/*.pu $(COMP)
	$(COMP) compile $(OPTS) -o $@ $<

%.pp.asm: %.asm
	$(NASM) -DNOATAT $(NASM_FLAGS) -E $< | grep '^ *[^% ]' > $@

%.pub: %.pu std2/*.pu $(COMP)
	$(COMP) bytecode $(OPTS) -o $@ $<

%.o: %.asm $(RUNTIME_INC)
	$(NASM) -DDEBUG $(NASM_FLAGS) -o $@ $<

gc.o: ../gc.c
	clang -o $@ -c $<

%: %.o gc.o
	$(LINK) $(LINK_FLAGS) -o $@ gc.o $< $(LIBS)

clean:
	rm -f $(ASSEMBLES) $(BYTECODES) $(OBJECTS) $(EXECUTABLES) *.in

.PHONEY: all asm clean bench clean.bench

bench: $(BENCHMARKS)

sort400.in: sort_gen
	echo 400000 | ./$< > $@

sort20.in: sort_gen
	echo 20000 | ./$< > $@

sort2.in: sort_gen
	echo 2000 | ./$< > $@

qsort.bench: qsort sort400.in
	$(BENCH) "./$< < sort400.in > /dev/null" > $@
	./$< < sort400.in > /dev/null 2>> $@

tsort.bench: tsort sort400.in
	$(BENCH) "./$< < sort400.in > /dev/null" > $@
	./$< < sort400.in > /dev/null 2>> $@

isort.bench: isort sort20.in
	$(BENCH) "./$< < sort20.in > /dev/null" > $@
	./$< < sort20.in > /dev/null 2>> $@

isort.ruceki.bench: isort.pub sort2.in
	$(BENCH) "ruceki $< < sort2.in > /dev/null" > $@
	ruceki $< < sort2.in > /dev/null 2>> $@

qsort.ruceki.bench: qsort.pub sort20.in
	$(BENCH) "ruceki $< < sort20.in > /dev/null" > $@
	ruceki $< < sort20.in > /dev/null 2>> $@

queens.bench: queens
	$(BENCH) "echo 13 | ./$< > /dev/null" > $@
	echo 13 | ./$< > /dev/null 2>> $@

queens.ruceki.bench: queens.pub
	$(BENCH) "echo 10 | ruceki $< > /dev/null" > $@
	echo 10 | ruceki $< > /dev/null 2>> $@

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
