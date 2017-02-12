#include <fstream>
#include <iomanip>
#include <iostream>
#include <list>
#include <map>
#include <sstream>
#include <vector>

#include <unistd.h>

using namespace std;

enum Inst {
  EVAL = 0,
  UNWIND,
  RETURN,
  EXIT,
  JUMP,       /* label */
  JUMPZERO,   /* label */
  LABEL,      /* label */
  PUSH,       /* long */
  PUSHINT,    /* long */
  PUSHGLOBAL, /* label */
  GLOBSTART,  /* label long */
  POP,        /* long */
  SLIDE,      /* long */
  UPDATE,     /* long */
  ALLOC,      /* long */
  MKAP,
  CONS0,      /* tag */
  CONS1,      /* tag */
  CONS2,      /* tag */
  HEAD,
  TAIL,
  NEG,
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  LES,
  LEQ,
  EQV,
  NEQ,
  GEQ,
  GTR,
  PRINT,
  ABORT
};

enum ArgType {
  NO_ARG,
  INT_ARG,
  LABEL_ARG,
  LABEL_INT_ARG
};

vector<pair<string, ArgType>> inst_table =
  { { "EVAL"      , NO_ARG },
    { "UNWIND"    , NO_ARG },
    { "RETURN"    , NO_ARG },
    { "EXIT"      , NO_ARG },
    { "JUMP"      , LABEL_ARG },
    { "JUMPZERO"  , LABEL_ARG },
    { "LABEL"     , LABEL_ARG },
    { "PUSH"      , INT_ARG },
    { "PUSHINT"   , INT_ARG },
    { "PUSHGLOBAL", LABEL_ARG },
    { "GLOBSTART" , LABEL_INT_ARG },
    { "POP"       , INT_ARG },
    { "SLIDE"     , INT_ARG },
    { "UPDATE"    , INT_ARG },
    { "ALLOC"     , INT_ARG },
    { "MKAP"      , NO_ARG },
    { "CONS0"     , INT_ARG },
    { "CONS1"     , INT_ARG },
    { "CONS2"     , INT_ARG },
    { "HEAD"      , NO_ARG },
    { "TAIL"      , NO_ARG },
    { "NEG"       , NO_ARG },
    { "ADD"       , NO_ARG },
    { "SUB"       , NO_ARG },
    { "MUL"       , NO_ARG },
    { "DIV"       , NO_ARG },
    { "MOD"       , NO_ARG },
    { "LES"       , NO_ARG },
    { "LEQ"       , NO_ARG },
    { "EQV"       , NO_ARG },
    { "NEQ"       , NO_ARG },
    { "GEQ"       , NO_ARG },
    { "GTR"       , NO_ARG },
    { "PRINT"     , NO_ARG },
    { "ABORT"     , NO_ARG }
  };

long code_size(Inst inst) {
  switch (ArgType(inst_table[inst].second)) {
  case NO_ARG:
    return 1;
  case LABEL_ARG:
  case INT_ARG:
    return 2;
  case LABEL_INT_ARG:
    return 3;
  }
}


class Parser {
private:
  ifstream file;
  map<string, pair<Inst, ArgType>> keyword_map;
  vector<string> labels;
  map<string, long> label_map;

  void error(string line) {
    cerr << "INVALID INPUT: " << line << endl;
    exit(EXIT_FAILURE);
  }

public:
  Parser(string filename) : file(filename) {
    for (size_t inst = 0; inst < inst_table.size(); ++inst) {
      pair<string, ArgType> inst_desc = inst_table[inst];
      keyword_map[inst_desc.first] = make_pair(Inst(inst), inst_desc.second);
    }
  }

  list<long> run() {
    list<long> result;
    string line;

    while (getline(file, line)) {
      istringstream input(line);
      string keyword;
      input >> keyword;

      if (keyword.empty()) {
      }
      else if (keyword_map.count(keyword) > 0) {
	pair<Inst, ArgType> inst_desc = keyword_map[keyword];
	result.push_back(inst_desc.first);
	ArgType arg_type = inst_desc.second;


	if (arg_type == LABEL_ARG || arg_type == LABEL_INT_ARG) {
	  string label;
	  input >> label;
	  if (label_map.count(label) > 0)
	    result.push_back(label_map[label]);
	  else {
	    long id = label_map.size();
	    result.push_back(id);
	    labels.push_back(label);
	    label_map[label] = id;
	  }
	}

	if (arg_type == INT_ARG || arg_type == LABEL_INT_ARG) {
	  long offset;
	  input >> offset;
	  result.push_back(offset);
	}

      }
      else {
	error(line);
      }
      if (input.fail())
	error(line);
    }

    return result;
  }

  const vector<string>& get_labels() const {
    return labels;
  }

  void reconstruct(const list<long>& code) {
    for (auto it = code.cbegin(); it != code.cend(); ++it) {
      auto inst = inst_table[*it];
      cout << *it << " " << inst.first;
      if (inst.second == LABEL_ARG || inst.second == LABEL_INT_ARG) {
	++it;
	cout << " " << labels[*it];
      }
      if (inst.second == INT_ARG || inst.second == LABEL_INT_ARG) {
	++it;
	cout << " " << *it;
      }
      cout << endl;
    }
  }
};

class GMachine {
private:
  vector<long> memory;
  long cptr, clim;
  long hptr, hbeg, hlim, hend; // (hlim - hptr) / 3 cells are left on the heap
  long sptr, bptr, slim;       // slim - sptr cells are left on the stack
  bool use_gc;
  long debug_level;
  map<int, string> sym_table;

  long ticks = 0;
  long allocations = 0;
  long max_stack = 0;
  long gc_runs = 0;
  long gc_free = 0;

  enum Tag {
    Nix = 0, // 0
    App,     // 1
    Int,     // 2
    Fun,     // 3
    Fwd,     // 4
    Con0,    // 5
    Con1,    // 6
    Con2     // 7
  };

public:
  GMachine(const list<long>& code, long heap_size, long stack_size, bool _use_gc,
	   long _debug_level) :
    memory(code.size() + heap_size + stack_size + 1, 0), use_gc(_use_gc),
    debug_level(_debug_level) {
    cptr = 1;
    for (auto inst : code) {
      memory[cptr] = inst;
      ++cptr;
    }
    clim = cptr;

    hbeg = clim;
    hend = hbeg + heap_size;
    hptr = hbeg;
    hlim = hend;

    bptr = hend;
    sptr = bptr - 1;
    slim = sptr + stack_size;
  }

private:
  void fail(string msg) {
    cerr << msg << endl;
    exit(EXIT_FAILURE);
  }

  // copy heap cell from scr to tgt
  void copy(long src, long tgt) {
    memory[tgt  ] = memory[src  ];
    memory[tgt+1] = memory[src+1];
    memory[tgt+2] = memory[src+2];
  }

  // copy heap cell from src in from-space to to-space during gc and leave fwd pointer
  long gc_copy(long src) {
    if (src < hbeg)
      return src;
    else if (memory[src] == Fwd)
      return memory[src+1];
    else {
      long tgt = hptr;
      hptr += 3;
      copy(src, tgt);
      memory[src] = Fwd;
      memory[src+1] = tgt;
      return tgt;
    }
  }

  void gc_handle(long qptr) {
    bool cpy1 = false, cpy2 = false;
    switch (Tag(memory[qptr])) {
    case Nix:
      fail("HANDLE NIX");
      break;
    case Fwd:
      fail("HANDLE FWD");
      break;
    case App:
      cpy1 = true;
      cpy2 = true;
      break;
    case Int:
    case Fun:
      break;
    case Con0:
    case Con1:
    case Con2:
      cpy1 = memory[qptr+1] != 0;
      if (cpy1)
	cpy2 = memory[qptr+2] != 0;
      break;
    default:
      fail("UNKNOWN TAG");
      break;
    }

    if (cpy1)
      memory[qptr+1] = gc_copy(memory[qptr+1]);
    if (cpy2)
      memory[qptr+2] = gc_copy(memory[qptr+2]);
  }

  void gc_stack_frame(long bptr, long sptr) {
    for (long src = sptr; src >= bptr; --src)
      memory[src] = gc_copy(memory[src]);
  }

  long heap_usage() const {
    if (hlim == hend)
      return hptr - (hbeg + hend) / 2;
    else
      return hptr - hbeg;
  }

  void gc() {
    long old_usage = heap_usage();
    if (hlim == hend) {
      hptr = hbeg;
      hlim = (hbeg + hend) / 2;
    }
    else {
      hptr = hlim;
      hlim = hend;
    }

    long qptr = hptr;

    // copy caf cells
    for (long fptr = clim; fptr < hbeg; fptr += 3)
      gc_handle(fptr);

    // copy cells reachable from current stack frame
    gc_stack_frame(bptr, sptr);

    // copy cells reachable from old stack frames
    long obptr = bptr;
    while (obptr - 2 >= hend) { // hend is the first stack cell
      long osptr = obptr - 3;
      obptr = memory[obptr-2];
      gc_stack_frame(obptr, osptr);
    }

    while (qptr < hptr) {
      gc_handle(qptr);
      qptr += 3;
    }

    long new_usage = heap_usage();
    // cerr << "[GC: " << old_usage << " -> " << new_usage << "]" << endl;
    gc_runs += 1;
    gc_free += (old_usage - new_usage) / 3;
  }

  void claim(long heap, long stck) {
    if (3*heap > hlim - hptr) {
      if (use_gc)
	gc();
      if (3*heap > hlim - hptr)
	fail("HEAP FULL");
    }
    if (sptr + stck > slim)
      fail("STACK OVERFLOW");
  }

  long alloc(Tag tag, long dat1, long dat2) {
    allocations += 1;
    long addr = hptr;
    hptr += 3;
    // This check should not be necessary. It's only purpose is to spot bugs.
    if (hptr > hlim)
      fail("UNDETECTED HEAP OVERFLOW");
    memory[addr]   = tag;
    memory[addr+1] = dat1;
    memory[addr+2] = dat2;
    return addr;
  }

  void push(long addr) {
    sptr += 1;
    // This check should not be necessary. It's only purpose is to spot bugs.
    if (sptr > slim)
      fail("UNDETECTED STACK OVERFLOW");
    memory[sptr] = addr;
    max_stack = max(max_stack, sptr - hend + 1);
  }

  void store() {
    claim(0, 2);
    long addr = memory[sptr];
    memory[sptr] = bptr;
    push(cptr);
    push(addr);
    bptr = sptr;
  }

  void restore() {
    long addr = memory[sptr];
    sptr -= 2;
    bptr = memory[sptr];
    cptr = memory[sptr+1];
    memory[sptr] = addr;
  }

  void calc_jumps(const vector<string>& labels) {
    map<long, long> table;
    for (cptr = 1; cptr < hptr; cptr += code_size(Inst(memory[cptr]))) {
      switch (Inst(memory[cptr])) {
      case LABEL:
      case GLOBSTART:
	table[memory[cptr+1]] = cptr;
	break;
      default:
	break;
      }
    }

    for (cptr = 1; cptr < hptr; cptr += code_size(Inst(memory[cptr]))) {
      long label;
      switch (Inst(memory[cptr])) {
      case JUMP:
      case JUMPZERO:
      case LABEL:
      case PUSHGLOBAL:
      case GLOBSTART:
	label = memory[cptr+1];
	if (table.count(label) > 0)
	  memory[cptr+1] = table[label];
	else
	  fail("UNKNOWN LABEL");
	break;
      default:
	break;
      }
    }

    for (int i = 0; i < labels.size(); ++i)
      sym_table[table[i]] = labels[i];
  }

  void alloc_cafs() {
    for (cptr = 1; cptr < hptr; cptr += code_size(Inst(memory[cptr]))) {
      if (memory[cptr] == GLOBSTART && memory[cptr+2] == 0) {
	memory[cptr+1] = hptr;
	alloc(Fun, 0, cptr);
      }
    }
    hbeg = hptr;
  }

  void setup_gc() {
    if (use_gc) {
      hlim = (hbeg + hend) / 2;
      hend -= (hend - hptr) % 2;
    }
  }

  void unwind() {
    long addr;
    while (memory[addr = memory[sptr]] == App) {
      ticks += 1;
      claim(0, 1);
      push(memory[addr+1]);
    }

    switch (Tag(memory[addr])) {
    case Nix: fail("UNWIND NIX"); break;
    case Fwd: fail("UNWIND FWD"); break;
      break;
    case Fun:
      if (memory[addr+1] <= sptr - bptr) // all arguments are present
	cptr = memory[addr+2];
      else {
	sptr = bptr;
	restore();
      }
      break;
    default:
      restore();
      break;
    }
  }

  void eval() {
    long addr = memory[sptr];
    switch (Tag(memory[addr])) {
    case Nix: fail("EVAL NIX"); break;
    case Fwd: fail("EVAL FWD"); break;
    case App:
      store();
      unwind();
      break;
    case Fun:
      if (memory[addr+1] == 0) { // fun is a caf
	store();
	cptr = memory[addr+2];
      }
      break;
    default:
      break;
    }
  }

  void step() {
    ticks += 1;
    Inst inst = Inst(memory[cptr]);
    cptr += 1;

    long k = 0;
    long arity = 0, t = 0;
    long addr = 0, adr1 = 0, adr2 = 0;
    long num1 = 0, num2 = 0;

    switch (inst) {
    case EVAL:
      eval();
      break;
    case UNWIND:
      unwind();
      break;
    case RETURN:
      sptr = bptr;
      restore();
      break;
    case JUMP:
      cptr = memory[cptr];
      break;
    case JUMPZERO:
      if (memory[memory[sptr]] == Con0)
	cptr = memory[cptr];
      else
	cptr += 1;
      sptr -= 1;
      break;
    case LABEL:
      cptr += 1;
      break;
    case PUSH:
      claim(0, 1);
      k = memory[cptr];
      cptr += 1;
      push(memory[sptr-k]);
      break;
    case PUSHINT:
      claim(1, 1);
      push(alloc(Int, memory[cptr], 0));
      cptr += 1;
      break;
    case PUSHGLOBAL:
      claim(1, 1);
      addr = memory[cptr]; // addr points to the corresponding GLOBSTART
      cptr += 1;
      arity = memory[addr+2];
      if (arity == 0)
	push(memory[addr+1]);
      else
	push(alloc(Fun, arity, addr));
      break;
    case GLOBSTART:
      arity = memory[cptr+1];
      cptr += 2;
      for (k = 1; k <= arity; ++k)
	memory[sptr-(k-1)] = memory[memory[sptr-k]+2];
      break;
    case POP:
      k = memory[cptr];
      cptr += 1;
      sptr -= k;
      break;
    case SLIDE:
      k = memory[cptr];
      cptr += 1;
      addr = memory[sptr];
      sptr -= k;
      memory[sptr] = addr;
      break;
    case UPDATE:
      k = memory[cptr];
      cptr += 1;
      addr = memory[sptr-k];
      copy(memory[sptr], addr);
      sptr -= 1;
      break;
    case ALLOC:
      claim(k, k);
      k = memory[cptr];
      cptr += 1;
      for (t = 0; t < k; ++t)
	push(alloc(Nix, 0, 0));
      break;
    case MKAP:
      claim(1, 0);
      adr1 = memory[sptr];
      sptr -= 1;
      adr2 = memory[sptr];
      memory[sptr] = alloc(App, adr1, adr2);
      break;
    case CONS0:
      claim(1, 1);
      t = memory[cptr];
      cptr += 1;
      push(alloc(Tag(Con0+t), 0, 0));
      break;
    case CONS1:
      claim(1, 0);
      t = memory[cptr];
      cptr += 1;
      addr = memory[sptr];
      memory[sptr] = alloc(Tag(Con0+t), addr, 0);
      break;
    case CONS2:
      claim(1, 0);
      t = memory[cptr];
      cptr += 1;
      adr1 = memory[sptr];
      sptr -= 1;
      adr2 = memory[sptr];
      memory[sptr] = alloc(Tag(Con0+t), adr1, adr2);
      break;
    case HEAD:
      memory[sptr] = memory[memory[sptr]+1];
      break;
    case TAIL:
      memory[sptr] = memory[memory[sptr]+2];
      break;
    case NEG:
      claim(1, 0);
      num1 = memory[memory[sptr]+1];
      memory[sptr] = alloc(Int, -num1, 0);
      break;
    case ADD:
    case SUB:
    case MUL:
    case DIV:
    case MOD:
      claim(1, 0);
      num1 = memory[memory[sptr]+1];
      sptr -= 1;
      num2 = memory[memory[sptr]+1];
      switch (inst) {
      case ADD:	num1 += num2; break;
      case SUB:	num1 -= num2; break;
      case MUL:	num1 *= num2; break;
      case DIV:	num1 /= num2; break;
      case MOD:	num1 %= num2; break;
      default:	fail("IMPOSSIBLE");
      }
      memory[sptr] = alloc(Int, num1, 0);
      break;
    case LES:
    case LEQ:
    case EQV:
    case NEQ:
    case GEQ:
    case GTR:
      claim(1, 0);
      num1 = memory[memory[sptr]+1];
      sptr -= 1;
      num2 = memory[memory[sptr]+1];
      switch (inst) {
      case LES: t = num1 <  num2; break;
      case LEQ:	t = num1 <= num2; break;
      case EQV:	t = num1 == num2; break;
      case NEQ: t = num1 != num2; break;
      case GEQ: t = num1 >= num2; break;
      case GTR:	t = num1 >  num2; break;
      default:	fail("IMPOSSIBLE");
      }
      memory[sptr] = alloc(Tag(Con0+t), 0, 0);
      break;
    case PRINT:
      num1 = memory[memory[sptr]+1];
      sptr -= 1;
      cout << num1 << endl;
      break;
    case EXIT:
      fail("STEP EXIT");
      break;
    case ABORT:
      fail("ABORT");
      break;
    default:
      fail("UNKNOWN INSTRUCTION");
      break;
    }
  }

  void follow(long addr, long level) {
    switch (Tag(memory[addr])) {
    case Nix:
      fail("FOLLOW NIX");
      break;
    case Fwd:
      fail("FOLLOW FWD");
      break;
    case App:
      if (level > 0) {
	cerr << "(";
	level -= 1;
	follow(memory[addr+1], level);
	cerr << " ";
	follow(memory[addr+2], level);
	cerr << ")";
      }
      else
	cerr << "(..)";
      break;
    case Int:
      cerr << memory[addr+1];
      break;
    case Fun:
      cerr << sym_table[memory[addr+2]];
      break;
    case Con0:
    case Con1:
    case Con2:
      if (level > 0) {
	level -= 1;
	cerr << "(Con" << (memory[addr]-Con0);
	if (memory[addr+1] != 0) {
	  cerr << " ";
	  follow(memory[addr+1], level);
	  if (memory[addr+2] != 0) {
	    cerr << " ";
	    follow(memory[addr+2], level);
	  }
	}
	cerr << ")";
      }
      else
	cerr << "(..)";
      break;
    default:
      cerr << endl << memory[addr] << endl;
      fail("INVALID TAG");
    }
  }

  void stack_trace() {
    if (debug_level > 0) {
      cerr << "============================================================" << endl;
      for (long sadr = bptr; sadr <= sptr; ++sadr) {
	cerr << setw(4) << sadr << ": ";
	follow(memory[sadr], debug_level);
	cerr << endl;
      }
      Inst inst = Inst(memory[cptr]);
      cerr << endl << "INSTRUCTION: " << inst_table[inst].first;
      for (long i = 1; i < code_size(inst); ++i)
	cerr << " " << memory[cptr+i];
      cerr << endl;
      cerr << "============================================================" << endl;
    }
  }

public:
  void exec(const vector<string>& labels) {
    calc_jumps(labels);
    alloc_cafs();
    setup_gc();

    cptr = 1;
    while (memory[cptr] != EXIT) {
      stack_trace();
      step();
    }
  }

  void print_stats() const {
    cerr << "Reductions:  " << setw(10) << ticks << endl;
    cerr << "Allocations: " << setw(10) << allocations << endl;
    cerr << "Stack depth: " << setw(10) << max_stack << endl;
    cerr << "GC runs:     " << setw(10) << gc_runs << endl;
  }
};

void usage(string prog) {
  cerr << "usage: "<< prog
       << " [-g] [-h HEAP_SIZE] [-s STACK_SIZE] [-d DEBUG_LEVEL] FILE\n" << endl;
  exit(EXIT_FAILURE);
}

int main (int argc, char** argv) {
  long heap_size = 3072, stack_size = 1024, debug_level = -1;
  bool use_gc = false;
  int curr_opt;
  string prog = argv[0];

  while ((curr_opt = getopt (argc, argv, "gh:s:d:")) != -1) {
    istringstream arg(optarg == nullptr ? "" : optarg);
    switch (curr_opt) {
    case 'h':
      arg >> heap_size;
      break;
    case 's':
      arg >> stack_size;
      break;
    case 'd':
      arg >> debug_level;
      break;
    case 'g':
      use_gc = true;
      break;
    default:
      usage(prog);
      break;
    }
    if (arg.fail())
      usage(prog);
  }

  if (optind != argc-1)
    usage(prog);

  Parser parser(argv[optind]);
  list<long> code = parser.run();

  GMachine gm(code, heap_size, stack_size, use_gc, debug_level);
  gm.exec(parser.get_labels());
  gm.print_stats();

  return 0;
}
