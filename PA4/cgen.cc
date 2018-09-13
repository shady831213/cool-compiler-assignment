
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <iomanip>

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;
//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);


static int label_cnt = 0;
static int alloc_label(int num) {
  int cur_label = label_cnt;
  label_cnt += num;
  return cur_label;
}
//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

typedef void (* emit_3ops) (char *dest, char *src1, char *src2, ostream& s);

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

static void emit_pop(char *reg, ostream& str)
{
  emit_addiu(SP,SP,4,str);
  emit_load(reg,0,SP,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

static void emit_enter_closure(ostream &s) {
  emit_addiu(SP,SP,-3 * WORD_SIZE,s);
  emit_store(RA,1,SP,s);
  emit_store(SELF,2,SP,s);
  emit_store(FP,3,SP,s);
  //FP to top of stack
  emit_addiu(FP,SP,4,s);  
}

static void emit_exit_closure(int nargs, ostream &s) {
  emit_load(RA, 1,SP,s);
  emit_load(SELF,2,SP,s);
  emit_load(FP, 3,SP,s);
  emit_addiu(SP,SP,(3+nargs) * WORD_SIZE,s);
}


static void emit_return_closure(int nargs, ostream &s) {
  emit_exit_closure(nargs, s);
  //return 
  emit_return(s);  
}

//return id address
static void emit_binding_id(IdEnv *env, Symbol name, ostream &s) {
  //after type checking, id must be there
  //if in stack, offset value is neg, else it is pos
  IdEnv *_env = new IdEnv();
  *_env = *env;
  int *offset;
  int layer_cnt = 1;

  offset = _env->probe(name);
  while(offset == NULL){
    _env->exitscope();
    offset = _env->probe(name);
    layer_cnt++;
  } 
  if (*offset > 0) {
    //emit_load(ACC, *offset, SELF, s);
    emit_addiu(ACC, SELF, *offset * WORD_SIZE, s);
  } else {
    int _label = alloc_label(1);
    emit_move(T3, FP, s);
    emit_load_imm(T2, layer_cnt, s);
    emit_move(T1, ZERO, s);
    emit_label_def(_label,s);
    emit_move(ACC, T3, s);
    emit_load(T3, 2, T3, s);
    emit_addiu(T1, T1, 1, s);
    emit_bne(T1, T2, _label, s);  
    //emit_load(ACC, -(*offset) + 2, ACC, s);
    emit_addiu(ACC, ACC, (-(*offset) + 2)  * WORD_SIZE, s);
  }
}

static void emit_copy(ostream &s) {
  emit_jal("Object.copy",s);
}

static void emit_slt(char *dest, char *src1, char *src2, ostream& s)
{ s << SLT << dest << " " << src1 << " " << src2 << endl; }

//dest must be diff from src1 and src2!!
static void emit_leq(char *dest, char *src1, char *src2, ostream& s)
{ 
  int _label = alloc_label(1);
  emit_addiu(dest, ZERO, 1, s);
  emit_beq(src1, src2, _label, s);
  emit_slt(dest, src1, src2, s);
  emit_label_def(_label,s);
}


static void emit_eval_and_copy(Expression e, IdEnv *env, CgenNodeP nd,  ostream &s) {
  e->code(env, nd, s);
  emit_copy(s);
}
static void emit_arith(Expression e1, Expression e2, IdEnv *env, CgenNodeP nd,  ostream &s, emit_3ops fn) {
  e1->code(env, nd, s);
  emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);
  emit_push(ACC,s);
  emit_eval_and_copy(e2, env, nd, s);
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);
  emit_pop(T1,s);
  fn(T1, T1, T2, s);
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
  if (cgen_Memmgr) {
    emit_addiu(A1, ACC, DEFAULT_OBJFIELDS * WORD_SIZE, s);
    emit_gc_assign(s);
  }
}

static void emit_compare(Expression e1, Expression e2, IdEnv *env, CgenNodeP nd,  ostream &s, emit_3ops fn) {
  int _label = alloc_label(1);
  e1->code(env, nd, s);
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_push(T1,s);
  e2->code(env, nd, s);
  emit_pop(T1,s);
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);
  fn(T3, T1, T2, s);
  emit_load_bool(ACC, falsebool, s);
  emit_beqz(T3, _label, s);
  emit_load_bool(ACC, truebool, s);
  emit_label_def(_label,s);
}

static void emit_void_object(char* filename, int line_num, char* handler, ostream &s) {
  int _label = alloc_label(1);
  emit_bne(ACC, ZERO, _label, s);
  emit_load_string(ACC, stringtable.lookup_string(filename),s);
  emit_load_imm(T1, line_num, s);
  emit_jal(handler, s);
  emit_label_def(_label,s);
}


static void emit_none_case(ostream &s) {
  emit_jal("_case_abort", s);
}


#define emit_load_label_ref(REF_TYPE, DST, SYM, STREAMM) \
  s << LA << DST << " "; \
  emit_##REF_TYPE##_ref(SYM, STREAMM); \
  s << endl; 

#define emit_jal_label_ref(REF_TYPE, SYM, STREAMM) \
  s << JAL; \
  emit_##REF_TYPE##_ref(SYM, STREAMM); \
  s << endl; 


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;
  emit_disptable_ref(Str, s);
      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 
  emit_disptable_ref(Int, s);
      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;
  emit_disptable_ref(Bool, s);
      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////
//My defininations
int CgenClassTable::stringclasstag()
{
  return probe(Str)->get_tag();
}

int CgenClassTable::intclasstag()
{
  return probe(Int)->get_tag();
}

int CgenClassTable::boolclasstag()
{
  return probe(Bool)->get_tag();
}

void CgenClassTable::tree_walk(CgenNodeP nd, Callbacks *cbs) {
  for (Callbacks *cb = cbs; cb != NULL; cb = cb->tl()) {
    CgenClassTableCallback *_cb = cb->hd();
    _cb->callback(nd);
  }
  for (List<CgenNode> * c = nd->get_children(); c != NULL; c = c->tl()) {
    tree_walk(c->hd(), cbs);
  }
}

void CgenClassTable::_build_class_info(CgenNodeP nd) {
  nd->set_tag(class_cnt);
  nd->set_max_child_tag(class_cnt);
  //if nd is leaf node, update all parents max_chaild_tag: for typcase
  if (nd->get_children() == NULL) {
    for (CgenNodeP p = nd->get_parentnd(); p != root()->get_parentnd(); p = p->get_parentnd()) {
      p->set_max_child_tag(class_cnt);
    }
  }
  class_cnt++;
  nd->build_env();
}

void CgenClassTable::_code_class_name_table(CgenNodeP nd) {
  StringEntry *name_symbol = stringtable.lookup_string(nd->get_name()->get_string());
  str << WORD;
  name_symbol->code_ref(str);
  str << endl;
}

void CgenClassTable::_code_class_protobj_table(CgenNodeP nd) {
  StringEntry *name_symbol = stringtable.lookup_string(nd->get_name()->get_string());
  str << WORD;
  emit_protobj_ref(nd->get_name(), str);
  str << endl;
  str << WORD;
  emit_init_ref(nd->get_name(), str);
  str << endl;
}

void CgenClassTable::_code_class_tables(CgenNodeP nd) {
  nd->code_protobj(str);
  nd->code_dispatch_tab(str);
}

void CgenClassTable::_code_class_methods(CgenNodeP nd) {
  nd->code_init(str);
  nd->code_methods(str);
}

void CgenClassTable::build_class_info() {
  Callbacks *cbs = new Callbacks(new CgenClassTableCallback(this, &CgenClassTable::_build_class_info), NULL);
  tree_walk(root(), cbs);
}


void CgenClassTable::code_class_name_table() {
  Callbacks *cbs = new Callbacks(new CgenClassTableCallback(this, &CgenClassTable::_code_class_name_table), NULL);
  str << CLASSNAMETAB << LABEL;
  tree_walk(root(), cbs);
}

void CgenClassTable::code_class_protobj_table() {
  Callbacks *cbs = new Callbacks(new CgenClassTableCallback(this, &CgenClassTable::_code_class_protobj_table), NULL);
  str << CLASSOBJTAB << LABEL;
  tree_walk(root(), cbs);  
}

void CgenClassTable::code_class_tables() {
  Callbacks *cbs = new Callbacks(new CgenClassTableCallback(this, &CgenClassTable::_code_class_tables), NULL);
  tree_walk(root(), cbs);  
}

void CgenClassTable::code_class_methods() {
  Callbacks *cbs = new Callbacks(new CgenClassTableCallback(this, &CgenClassTable::_code_class_methods), NULL);
  tree_walk(root(), cbs);  
}

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag() << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag() << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag() << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools()
{
  falsebool.code_def(str,boolclasstag());
  truebool.code_def(str,boolclasstag());
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag());
  inttable.code_string_table(str,intclasstag());
  code_bools();
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();
   build_class_info();

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

   inttable.add_string("0");

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

   stringtable.add_string("");

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  if (cgen_debug) cout << "coding class name and prototype objects table" << endl;
  code_class_name_table();
  code_class_protobj_table();

  if (cgen_debug) cout << "coding class tables" << endl;
  code_class_tables();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  if (cgen_debug) cout << "coding class methods" << endl;
  code_class_methods();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

}

CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus),
   attrs(new nil_node<attr_class *>()),
   methods(new nil_node<CgenMethodP>()),
   id_env(new IdEnv),
   method_env(new MethodEnv),
   class_table(ct)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
   id_env->enterscope();
   method_env->enterscope();
   for (int i= features->first(); features->more(i); i = features->next(i)) {
     if (features->nth(i)->is_attr()) {
       attrs = new append_node<attr_class *>(attrs, new single_list_node<attr_class *>((attr_class *)features->nth(i)));
     } else {
       methods = new append_node<CgenMethodP>(methods, new single_list_node<CgenMethodP>(new CgenMethod ((method_class *)features->nth(i), name)));
     }
   }
}

void CgenNode::build_env() {
  //merge attrs
  attrs = new append_node<attr_class *>(parentnd->get_attrs(), attrs);
  for (int i= attrs->first(); attrs->more(i); i = attrs->next(i)) {
    //offset base on object tables
    id_env->addid(attrs->nth(i)->get_name(), new int(i+DEFAULT_OBJFIELDS));
  }
  //merge methods
  list_node<CgenMethodP> *p_methods = new nil_node<CgenMethodP>();
  for (int i= parentnd->get_methods()->first(); parentnd->get_methods()->more(i); i = parentnd->get_methods()->next(i)) {
    int idx = get_method(parentnd->get_methods()->nth(i)->get_name());
    if (idx < 0) {
      //append
      p_methods = new append_node<CgenMethodP>(p_methods, new single_list_node<CgenMethodP>(parentnd->get_methods()->nth(i)->copy()));
    } else {
      //override
      p_methods = new append_node<CgenMethodP>(p_methods, new single_list_node<CgenMethodP>(methods->nth(idx)));
    }    
  }
  list_node<CgenMethodP> *c_methods = new nil_node<CgenMethodP>();
  for (int i= methods->first(); methods->more(i); i = methods->next(i)) {
    int idx = parentnd->get_method(methods->nth(i)->get_name());
    if (idx < 0) {
      //append
      c_methods = new append_node<CgenMethodP>(c_methods, new single_list_node<CgenMethodP>(methods->nth(i)));
    }
  }
  methods = new append_node<CgenMethodP>(p_methods, c_methods);

  //save offset
  for (int i= methods->first(); methods->more(i); i = methods->next(i)) {
    //offset base on dispatch tables
    id_env->addid(methods->nth(i)->get_name(), new int(i));
  }

  //save id_env
  for (int i= methods->first(); methods->more(i); i = methods->next(i)) {
    IdEnv *_id_env = new IdEnv();
    *_id_env = *id_env;
    _id_env->enterscope();
    Formals formals = methods->nth(i)->get_formals();
    for (int j = formals->first(); formals->more(j); j = formals->next(j)) {
      _id_env->addid(formals->nth(j)->get_name(), new int(-(formals->len() - j)));
    }
    method_env->addid(methods->nth(i)->get_name(), _id_env);
  }

}


void CgenNode::code_protobj(ostream &s) {
  // Add -1 eye catcher
  s << WORD << "-1" << endl;
  emit_protobj_ref(name, s);
  s << LABEL;
  s << WORD << tag << endl;
  s << WORD << DEFAULT_OBJFIELDS + attrs->len() << endl;
  s << WORD;
  emit_disptable_ref(name, s);
  s << endl;
  for (int i= attrs->first(); attrs->more(i); i = attrs->next(i)) {
    s << WORD;
    if (attrs->nth(i)->get_type_decl() == Int) {
      inttable.lookup_string("0")->code_ref(s);
    } else if (attrs->nth(i)->get_type_decl() == Str) {
      stringtable.lookup_string("")->code_ref(s);
    } else if (attrs->nth(i)->get_type_decl() == Bool) {
      falsebool.code_ref(s);
    } else {
      s << EMPTYSLOT;
    }
    s << endl;
  }
}
void CgenNode::code_dispatch_tab(ostream &s){
  emit_disptable_ref(name, s);
  s << LABEL;
  //self methods
  for (int i= methods->first(); methods->more(i); i = methods->next(i)) {
    s << WORD;
    emit_method_ref(methods->nth(i)->get_class_name(), methods->nth(i)->get_name(), s);
    s << endl;
  }
}

void CgenNode::code_init(ostream &s){
  emit_init_ref(name, s);
  s << LABEL;
  emit_enter_closure(s);
  emit_move(SELF, ACC, s);
  for (int i= attrs->first(); attrs->more(i); i = attrs->next(i)) {
    if (attrs->nth(i)->get_init()->get_type()) {
      attrs->nth(i)->get_init()->code(id_env, this, s);
      emit_store(ACC, DEFAULT_OBJFIELDS + i, SELF, s);
      if (cgen_Memmgr) {
	emit_addiu(A1, SELF, (DEFAULT_OBJFIELDS + i) * WORD_SIZE, s);
	emit_gc_assign(s);
      }
    }
  }
  emit_move(ACC, SELF, s);    
  emit_return_closure(0, s);
}

void CgenNode::code_methods(ostream &s){
  for (int i= methods->first(); methods->more(i); i = methods->next(i)) {
    if (!basic() && methods->nth(i)->get_class_name() == name) {
      emit_method_ref(name, methods->nth(i)->get_name(), s);
      s << LABEL;
      emit_enter_closure(s);
      emit_move(SELF, ACC, s);
      methods->nth(i)->get_expr()->code(get_id_env(methods->nth(i)->get_name()), this, s);
      emit_return_closure(methods->nth(i)->formals->len(), s);
    }
  }
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************
void assign_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  if (expr->get_type()) {
    expr->code(env, nd, s);
    emit_push(ACC,s);
    emit_binding_id(env, name, s);
    emit_pop(T1,s);
    if (cgen_Memmgr) {
      emit_move(A1, ACC, s);
    }
    emit_store(T1, 0, ACC, s);
    if (cgen_Memmgr) {
      emit_gc_assign(s);
    }
    emit_move(ACC, T1, s);
  }
}

void static_dispatch_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  //lookup method
  CgenNodeP disp_nd = nd->get_class_table()->probe(type_name);
  int *offset;
  offset = disp_nd->get_id_env()->probe(name);

  //parameters
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(env, nd, s);
    emit_push(ACC, s);
  }

  //object id
  expr->code(env,nd,s);

  emit_void_object(nd->get_filename()->get_string(), get_line_number(), "_dispatch_abort", s);

  //dispatch table
  emit_load_label_ref(protobj, T1, disp_nd->get_name(), s);
  emit_load(T1, DISPTABLE_OFFSET, T1, s);
  emit_load(T1, *offset, T1, s);

  //call method
  emit_jalr(T1, s);
}

void dispatch_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  //lookup method
  Symbol type = expr->get_type();
  int *offset;
  CgenNodeP disp_nd = type == SELF_TYPE ? nd : nd->get_class_table()->probe(type);
  offset = disp_nd->get_id_env()->probe(name);

  //parameters
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(env, nd, s);
    emit_push(ACC, s);
  }

  //object id
  expr->code(env,nd,s);
  
  //check error
  emit_void_object(nd->get_filename()->get_string(), get_line_number(), "_dispatch_abort", s);
  
  //dispatch table
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);
  emit_load(T1, *offset, T1, s);

  //call method
  emit_jalr(T1, s);
}

void cond_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  int _label = alloc_label(2);
  pred->code(env, nd, s);
  //if 
  emit_load_bool(T1, falsebool, s);  
  emit_beq(T1, ACC, _label, s);
  //then
  then_exp->code(env, nd, s);
  emit_branch(_label + 1, s);
  //else
  emit_label_def(_label,s);
  else_exp->code(env, nd, s);
  //done
  emit_label_def(_label + 1,s);
}

void loop_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  int _label = alloc_label(2);
  emit_label_def(_label,s);
  //condition
  pred->code(env, nd, s);
  emit_load_bool(T1, falsebool, s);  
  emit_beq(T1, ACC, _label+1, s);
  //body
  body->code(env, nd, s);
  emit_branch(_label, s);
  //done
  emit_label_def(_label + 1,s);
  //return void object
  emit_load_imm(ACC, EMPTYSLOT, s);
}

void typcase_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  int _label = alloc_label(cases->len()+2);
  expr->code(env, nd, s);

  //check error
  emit_void_object(nd->get_filename()->get_string(), get_line_number(), "_case_abort2", s);

  emit_push(ACC,s);
  //visit braches decreasingly by class tag
  int *visit = new int[cases->len()]();
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    //find max tag
    int max_tag = -1;
    int max_tag_idx;
    CgenNodeP max_tag_nd;
    for (int j = cases->first(); cases->more(j); j = cases->next(j)) {
      if (!visit[j] && nd->get_class_table()->probe(cases->nth(j)->get_type_decl())->get_tag() > max_tag) {
	max_tag_nd = nd->get_class_table()->probe(cases->nth(j)->get_type_decl());
	max_tag = max_tag_nd->get_tag();
	max_tag_idx = j;
      } 
    }
    visit[max_tag_idx] = 1;
    emit_load(T1, 0, ACC, s);
    emit_blti(T1, max_tag_nd->get_tag(), _label + i, s);
    emit_bgti(T1, max_tag_nd->get_max_child_tag(), _label + i, s);
    env->enterscope();
    env->addid(cases->nth(max_tag_idx)->get_name(), new int(-1));
    emit_enter_closure(s);
    cases->nth(max_tag_idx)->get_expr()->code(env, nd, s);
    emit_exit_closure(1, s);
    env->exitscope();
    //done
    emit_branch(_label + cases->len()+1, s);
    emit_label_def(_label+i,s);
  }
  //none case error
  emit_label_def(_label + cases->len(),s);
  emit_none_case(s);

  //done
  emit_label_def(_label + cases->len()+1,s);
  
}

void block_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->code(env, nd, s);
  }
}


void let_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  if (init->get_type()) {
    init->code(env, nd, s);    
  } else {
    if (type_decl == Int) {
      emit_load_int(ACC, inttable.lookup_string("0"), s);
    } else if (type_decl == Str) {
      emit_load_string(ACC, stringtable.lookup_string(""), s);
    } else if (type_decl == Bool) {
      emit_load_bool(ACC, falsebool, s);
    } else {
      emit_load_imm(ACC, EMPTYSLOT, s);
    }
  }
  emit_push(ACC,s);
  //call
  env->enterscope();
  env->addid(identifier, new int(-1));
  emit_enter_closure(s);
  body->code(env,nd,s);
  emit_exit_closure(1, s);
  env->exitscope();
}


void plus_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  emit_arith(e1, e2, env, nd, s, emit_add);
}

void sub_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  emit_arith(e1, e2, env, nd, s, emit_sub);
}

void mul_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  emit_arith(e1, e2, env, nd, s, emit_mul);
}

void divide_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  emit_arith(e1, e2, env, nd, s, emit_div);
}

void neg_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  emit_eval_and_copy(e1, env, nd, s);
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_neg(T1,T1,s);
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
  if (cgen_Memmgr) {
    emit_addiu(A1, ACC, DEFAULT_OBJFIELDS * WORD_SIZE, s);
    emit_gc_assign(s);
  }
}

void lt_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  emit_compare(e1, e2, env, nd, s, emit_slt);
}

void eq_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  int _label = alloc_label(1);
  e1->code(env, nd, s);
  emit_push(ACC,s);
  e2->code(env, nd, s);
  emit_pop(T1, s);
  emit_move(T2, ACC, s);
  emit_load_bool(ACC, truebool, s);
  emit_load_bool(A1, falsebool, s); 
 //if the pointers are equal , exit; otherwise, call equality_test
  emit_beq(T1, T2, _label, s);
  emit_jal("equality_test",s);
  emit_label_def(_label,s);
}

void leq_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  emit_compare(e1, e2, env, nd, s, emit_leq);
}

void comp_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  int _label = alloc_label(2);
  e1->code(env, nd, s);
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_beqz(T1, _label ,s);
  emit_load_bool(ACC, falsebool, s);  
  emit_branch(_label+1, s);    
  emit_label_def(_label,s);
  emit_load_bool(ACC, truebool, s);     
  emit_label_def(_label+1,s);
}

void int_const_class::code(IdEnv *env, CgenNodeP nd,  ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(IdEnv *env, CgenNodeP nd,  ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(IdEnv *env, CgenNodeP nd,  ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  //load type
  if (type_name == SELF_TYPE) {
    //deal with self
    //lookup class_obj_tab to find protobj and init
    int _label = alloc_label(1);
    emit_load_address(T1, CLASSOBJTAB, s);
    emit_load(T2, 0, SELF, s);
    emit_label_def(_label,s);
    emit_load(ACC, 0, T1, s);
    emit_load(T3, 0, ACC, s);
    emit_addiu(T1, T1, 2*WORD_SIZE, s);  
    emit_bne(T2, T3, _label, s);
    
    //save init func
    emit_load(T1, -1, T1, s);
    emit_push(T1, s);
    // call object copy
    emit_copy(s);
    //call init
    emit_pop(T1, s);
    emit_jalr(T1, s);
  } else {
    emit_load_label_ref(protobj, ACC, type_name, s);
    // call object copy
    emit_copy(s);
    // call init
    emit_jal_label_ref(init, type_name, s);
  }
}

void isvoid_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  int _label = alloc_label(1);
  e1->code(env,nd,s);
  emit_move(T1, ACC, s);
  emit_load_bool(ACC, truebool, s);
  emit_beqz(T1, _label, s);
  emit_load_bool(ACC, falsebool, s);  
  emit_label_def(_label,s);
}

void no_expr_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
}

void object_class::code(IdEnv *env, CgenNodeP nd,  ostream &s) {
  if (name == self) {
    emit_move(ACC, SELF, s);
    return;
  }
  emit_binding_id(env, name, s);
  emit_load(ACC, 0, ACC, s);
}


