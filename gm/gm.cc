#include <fstream>
#include <iomanip>
#include <iostream>
#include <list>
#include <map>
#include <sstream>
#include <stack>
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
  { { "EVAL"		, NO_ARG },
    { "UNWIND"		, NO_ARG },
    { "RETURN"		, NO_ARG },
    { "EXIT"		, NO_ARG },
    { "JUMP"		, LABEL_ARG },
    { "JUMPZERO"	, LABEL_ARG },
    { "LABEL"		, LABEL_ARG },
    { "PUSH"		, INT_ARG },
    { "PUSHINT"		, INT_ARG },
    { "PUSHGLOBAL"	, LABEL_ARG },
    { "GLOBSTART"	, LABEL_INT_ARG },
    { "POP"		, INT_ARG },
    { "SLIDE"		, INT_ARG },
    { "UPDATE"		, INT_ARG },
    { "ALLOC"		, INT_ARG },
    { "MKAP"		, NO_ARG },
    { "CONS0"		, INT_ARG },
    { "CONS1"		, INT_ARG },
    { "CONS2"		, INT_ARG },
    { "HEAD"		, NO_ARG },
    { "TAIL"		, NO_ARG },
    { "NEG"		, NO_ARG },
    { "ADD"		, NO_ARG },
    { "SUB"		, NO_ARG },
    { "MUL"		, NO_ARG },
    { "DIV"		, NO_ARG },
    { "MOD"		, NO_ARG },
    { "LES"		, NO_ARG },
    { "LEQ"		, NO_ARG },
    { "EQV"		, NO_ARG },
    { "NEQ"		, NO_ARG },
    { "GEQ"		, NO_ARG },
    { "GTR"		, NO_ARG },
    { "PRINT"		, NO_ARG },
    { "ABORT"		, NO_ARG }
  };

long code_size(Inst inst) {
  switch (inst_table[inst].second) {
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
    cout << "Invalid input: " << line << endl;
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
      if (input.bad())
	error(line);
    }
        
    return result;
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
  long sptr, bptr, send, hptr, hbeg, hlim, hend, cptr;
  stack<pair<long,long>> dump;
    
  enum Tag { Nix = 0, App, Int, Fun, Fwd, Con0, Con1, Con2 };
    
  void fail(string msg) {
    cout << msg << endl;
    exit(EXIT_FAILURE);
  }
    
  long alloc(Tag tag, long dat1, long dat2) {
    long addr = hptr;
    hptr += 3;
    if (hptr > hend)
      fail("HEAP OVERFLOW");
    memory[addr] = tag;
    memory[addr+1] = dat1;
    memory[addr+2] = dat2;
    return addr;
  }
    
  void copy(long src, long tgt) {
    for (long i = 0; i < 3; ++i)
      memory[tgt+i] = memory[src+i];
  }
    
  void push(long addr) {
    sptr += 1;
    if (sptr > send)
      fail("STACK OVERFLOW");
    memory[sptr] = addr;
  }
    
  void store() {
    dump.push(make_pair(bptr, cptr));
    bptr = sptr;
  }
    
  void restore() {
    bptr = dump.top().first;
    cptr = dump.top().second;
    dump.pop();
  }
    
public:
  GMachine(const list<long>& code, long heap_size, long stack_size) :
    memory(code.size() + heap_size + stack_size + 1, 0), dump() {
    cptr = 1;
    for (auto inst : code) {
      memory[cptr] = inst;
      ++cptr;
    }
        
    hbeg = code.size() + 1;
    hend = hbeg + heap_size;
    hptr = hbeg;
    hlim = hend;
    bptr = hend;
    sptr = bptr - 1;
    send = sptr + stack_size;
  }
    
private:
  void calc_jumps() {
    map<long, long> table;
    for (cptr = 1; cptr < hptr; cptr += code_size(Inst(memory[cptr]))) {
      switch (memory[cptr]) {
      case LABEL:
      case GLOBSTART:
	table[memory[cptr+1]] = cptr;
	break;
      default: ;
      }
    }
        
    for (cptr = 1; cptr < hptr; cptr += code_size(Inst(memory[cptr]))) {
      long label;
      switch (memory[cptr]) {
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
      default: ;
      }
    }
  }
    
  void alloc_cafs() {
    for (cptr = 1; cptr < hptr; cptr += code_size(Inst(memory[cptr]))) {
      if (memory[cptr] == GLOBSTART && memory[cptr+2] == 0) {
	memory[cptr+1] = hptr;
	alloc(Fun, 0, cptr);
      }
    }
    hbeg = hptr;
    hlim = (hbeg + hend) / 2;
    if ((hend - hptr) % 2 == 1)
      hend -= 1;
  }
    
  void unwind() {
    long addr;
    while (memory[addr = memory[sptr]] == App) {
      push(memory[addr+1]);
    }
        
    switch (memory[addr]) {
    case Nix:
    case Fwd:
      fail("NIX/FWD");
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
    }
  }
    
  void eval() {
    long addr = memory[sptr];
    switch (memory[addr]) {
    case Nix:
    case Fwd:
      fail("NIX/FWD");
      break;
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
    Inst inst = Inst(memory[cptr]);
    cptr += 1;
        
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
      {
	long k = memory[cptr];
	cptr += 1;
	push(memory[sptr-k]);
      }
      break;
    case PUSHINT:
      push(alloc(Int, memory[cptr], 0));
      cptr += 1;
      break;
    case PUSHGLOBAL:
      {
	long addr = memory[cptr]; // addr points to the corresponding GLOBSTART
	cptr += 1;
	long arity = memory[addr+2];
	if (arity == 0)
	  push(memory[addr+1]);
	else
	  push(alloc(Fun, arity, addr));
      }
      break;
    case GLOBSTART:
      {
	long arity = memory[cptr+1];
	cptr += 2;
	for (long i = 1; i <= arity; ++i)
	  memory[sptr-(i-1)] = memory[memory[sptr-i]+2];
      }
      break;
    case POP:
      {
	long k = memory[cptr];
	cptr += 1;
	sptr -= k;
      }
      break;
    case SLIDE:
      {
	long k = memory[cptr];
	cptr += 1;
	long addr = memory[sptr];
	sptr -= k;
	memory[sptr] = addr;
      }
      break;
    case UPDATE:
      {
	long k = memory[cptr];
	cptr += 1;
	long addr = memory[sptr-k];
	copy(memory[sptr], addr);
	sptr -= 1;
      }
      break;
    case ALLOC:
      {
	long k = memory[cptr];
	cptr += 1;
	for (long i = 0; i < k; ++i)
	  push(alloc(Nix, 0, 0));
      }
      break;
    case MKAP:
      {
	long adr1 = memory[sptr];
	sptr -= 1;
	long adr2 = memory[sptr];
	memory[sptr] = alloc(App, adr1, adr2);
      }
      break;
    case CONS0:
      {
	long t = memory[cptr];
	cptr += 1;
	push(alloc(Tag(Con0+t), 0, 0));
      }
      break;
    case CONS1:
      {
	long t = memory[cptr];
	cptr += 1;
	long addr = memory[sptr];
	memory[sptr] = alloc(Tag(Con0+t), addr, 0);
      }
      break;
    case CONS2:
      {
	long t = memory[cptr];
	cptr += 1;
	long adr1 = memory[sptr];
	sptr -= 1;
	long adr2 = memory[sptr];      
	memory[sptr] = alloc(Tag(Con0+t), adr1, adr2);
      }
      break;
    case HEAD:
      memory[sptr] = memory[memory[sptr]+1];
      break;
    case TAIL:
      memory[sptr] = memory[memory[sptr]+2];
      break;
    case NEG:
      {
	long num = memory[memory[sptr]+1];
	memory[sptr] = alloc(Int, -num, 0);
      }
      break;
    case ADD:
    case SUB:
    case MUL:
    case DIV:
    case MOD:
      {
	long num1 = memory[memory[sptr]+1];
	sptr -= 1;
	long num2 = memory[memory[sptr]+1];
	switch (inst) {
	case ADD:
	  num1 += num2;
	  break;
	case SUB:
	  num1 -= num2;
	  break;
	case MUL:
	  num1 *= num2;
	  break;
	case DIV:
	  num1 /= num2;
	  break;
	case MOD:
	  num1 %= num2;
	  break;
	default:
	  fail("IMPOSSIBLE");
	}
	memory[sptr] = alloc(Int, num1, 0);
      }
      break;
    case LES:
    case LEQ:
    case EQV:
    case NEQ:
    case GEQ:
    case GTR:
      {
	long num1 = memory[memory[sptr]+1];
	sptr -= 1;
	long num2 = memory[memory[sptr]+1];
	bool res = false;
	switch (inst) {
	case LES:
	  res = num1 < num2;
	  break;
	case LEQ:
	  res = num1 <= num2;
	  break;
	case EQV:
	  res = num1 == num2;
	  break;
	case NEQ:
	  res = num1 != num2;
	  break;
	case GEQ:
	  res = num1 >= num2;
	  break;
	case GTR:
	  res = num1 > num2;
	  break;
	default:
	  fail("IMPOSSIBLE");
	}
	memory[sptr] = alloc(Tag(Con0+res), 0, 0);
      }
      break;
    case PRINT:
      {
	long num = memory[memory[sptr]+1];
	sptr -= 1;
	cout << "OUTPUT: " << num << endl;
      }
      break;
    case EXIT:
      fail("STEP EXIT");
      break;
    case ABORT:
      fail("ABORT");
      break;
    default:
      cout << inst << endl;
      fail("UNKNOWN INSTRUCTION");
      break;
    }
  }
    
  void follow(long addr, long level) {
    if (level > 0) {
      level -= 1;
      switch (memory[addr]) {
      case Nix:
      case Fwd:
	fail("NIX/Fwd");
	break;
      case App:
	cout << "(";
	follow(memory[addr+1], level);
	cout << " ";
	follow(memory[addr+2], level);
	cout << ")";
	break;
      case Int:
	cout << memory[addr+1];
	break;
      case Fun:
	cout << "f[" << memory[addr+1] << "]" << memory[addr+2];
	break;
      case Con0:
      case Con1:
      case Con2:
	cout << "(Con" << (memory[addr]-Con0);
	if (memory[addr+1] != 0) {
	  cout << " ";
	  follow(memory[addr+1], level);
	  if (memory[addr+2] != 0) {
	    cout << " ";
	    follow(memory[addr+2], level);
	  }
	}
	cout << ")";
	break;
      default:
	cout << endl << memory[addr] << endl;
	fail("INVALID TAG");
      }
    }
  }
    
  void stack_trace() {
    cout << "============================================================" << endl;
    for (long sadr = bptr; sadr <= sptr; ++sadr) {
      cout << setw(4) << sadr << ": ";
      follow(memory[sadr], 3);
      cout << endl;
    }
    Inst inst = Inst(memory[cptr]);
    cout << endl << "INSTRUCTION: " << inst_table[inst].first;
    for (long i = 1; i < code_size(inst); ++i)
      cout << " " << memory[cptr+i];
    cout << endl;
    cout << "============================================================" << endl;
  }
    
public:
  void exec() {
    cout << "starting exec" << endl;
    calc_jumps();
    cout << "calculated jumps" << endl;
    alloc_cafs();
    cout << "finished pre-processing" << endl;
        
    cptr = 1;
    while (memory[cptr] != EXIT) {
      // stack_trace();
      step();
    }
  }
};

void usage(char* prog) {
  printf("usage: %s [-h HEAP_SIZE] [-s STACK_SIZE] FILE\n", prog);
  exit(EXIT_FAILURE);
}

int main (int argc, char** argv) {
  long heap_size = 3072, stack_size = 1024;
  int curr_opt;
    
  while ((curr_opt = getopt (argc, argv, "h:s:")) != -1) {
    switch (curr_opt) {
    case 'h':
      heap_size = atoi(optarg);
      break;
    case 's':
      stack_size = atoi(optarg);
      break;
    default:
      usage(argv[0]);
    }
  }
    
  if (optind != argc-1)
    usage(argv[0]);
    
  Parser parser(argv[optind]);
  list<long> insts = parser.run();
    
  GMachine gm(insts, heap_size, stack_size);
  gm.exec();
    
  return 0;
}
