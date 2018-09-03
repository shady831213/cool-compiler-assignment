

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <typeinfo>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
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



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr), class_map(new ClassMap()), children_map(new ChildrenMap()), env_map(new EnvMap()) {
  class_map->enterscope();
  children_map->enterscope();
  env_map->enterscope();
    /* Fill this in */
  install_basic_classes();
 
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    add_class(classes->nth(i));
    if (errors()) {
      return;
    }
  }
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    add_child(classes->nth(i));
    if (errors()) {
      return;
    }
  }
  build_env(Object, new Enviroment());
  if (get_class(Main) == NULL) {
    semant_error() << "Class Main is not defined." << endl;
    return;
  }
  if (get_env(Main)->lookup_method(main_meth) == NULL) {
    semant_error(get_class(Main)) << "No main method!" << endl;
  }
}

void ClassTable::build_env(Symbol class_, Enviroment *parent_env) {
  Enviroment *env = get_env(class_);
  *env = *parent_env;
  env->enterscope();
  get_class(class_)->build_env(this);
  Classes children = get_children(class_);
  if (children != NULL) {
    for (int i = children->first(); children->more(i); i = children->next(i)) {
      build_env(children->nth(i)->get_name(), env);
    }
  }
}


Class_ ClassTable::get_class(Symbol id) {
  return class_map->probe(id);
}

Class_ ClassTable::get_parent(Class_ class_) {
  return get_class(class_->get_parent());
}

Enviroment *ClassTable::get_env(Symbol id) {
  return env_map->probe(id);
}


Classes ClassTable::get_children(Symbol class_) {
  return children_map->probe(class_);
}

Symbol ClassTable::leastcommontype(Symbol class0, Symbol class1) {
  for (Class_ c = get_class(class0); c->get_name() != Object; c = get_parent(c)) {
    if (subtype(c->get_name(), class1)) {
      return c->get_name();
    }
  }
  return Object;
}


Symbol ClassTable::evaltype(Symbol scope, Symbol type) {
  if (type == SELF_TYPE) {
    return scope;
  }
  return type;
}


int ClassTable::subtype(Symbol class0, Symbol class1) {
  if (class1 == No_type) {
    return 1;
  }
  if (istype(class0, class1)) {
    return 1;
  }
  for (Class_ c = get_class(class1); c != NULL; c = get_parent(c)) {
    if (c->get_name() == class0) {
      return 1;
    }
  }
  return 0;
}

int ClassTable::istype(Symbol lhs, Symbol rhs) {
  return lhs == rhs;
}


int ClassTable::validtype(Symbol type) {
  return (class_map->probe(type) != NULL);
}

void ClassTable::add_class(Class_ class_) {
  if (get_class(class_->get_name()) == NULL && 
      class_->get_name() != No_class && 
      class_->get_name() != No_type && 
      class_->get_name() != SELF_TYPE) {
    class_map->addid(class_->get_name(), class_);
    env_map->addid(class_->get_name(), new Enviroment());
    return;
  }
  semant_error(class_) << "class " << class_->get_name() << "is redefined!" << endl;
}

void ClassTable::add_child(Class_ class_) {
  Class_ parent = get_class(class_->get_parent());
  if (parent != NULL && 
      parent->get_name() != Bool  && 
      parent->get_name() != Str && 
      parent->get_name() != Int) {
    Classes children = get_children(parent->get_name());
    if (children == NULL) {
      children_map->addid(parent->get_name(), single_Classes(class_));
    } else {
      children_map->addid(parent->get_name(), append_Classes(children, single_Classes(class_)));
    }

    return;
  }
  semant_error(class_) << "parent class " << class_->get_parent() << "is invalid!" << endl;
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);
    
    add_class(Object_class);
    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
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
	       filename);  

    add_class(IO_class);
    add_child(IO_class);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    add_class(Int_class);
    add_child(Int_class);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       

    add_class(Bool_class);
    add_child(Bool_class);

    Class_ Str_class =
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
	       filename);
    add_class(Str_class);
    add_child(Str_class);

}

void ClassTable::_dump(Symbol class_,ostream& stream, int n){
  cerr<<"-----"<<class_<<"-----"<<endl;
  get_class(class_)->dump(stream, n);
  get_env(class_)->dump();
  cerr<<"-----"<<class_<<"-----"<<endl;
  Classes children = get_children(class_);
  if (children != NULL) {
    for (int i = children->first(); children->more(i); i = children->next(i)) {
      _dump(children->nth(i)->get_name(), stream, n+2);
    }
  }
}
  
void ClassTable::dump(){
  _dump(Object, cerr, 0);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 


/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();
    
    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */
    if (semant_debug) {
      classtable->dump();
    }

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
      classes->nth(i)->type_check(classtable);
    }

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
    
}

void class__class::build_env(ClassTable *classtable) {
  classtable->get_env(name)->addid(self, SELF_TYPE);
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    features->nth(i)->build_env(name, classtable);
  }            
}


void attr_class::build_env(Symbol class_, ClassTable *classtable) {
  Enviroment *env = classtable->get_env(class_);
  if (env->lookup_type(name)!=NULL || env->probe_method(name)!=NULL) {
    classtable->semant_error(classtable->get_class(class_)) << name << " is redefined!" << endl;
    return;
  }
  env->addid(name, type_decl);
}


void method_class::build_env(Symbol class_, ClassTable *classtable) {
  Enviroment *env = classtable->get_env(class_);
  if (env->lookup_type(name)!=NULL || env->probe_method(name)!=NULL) {
    classtable->semant_error(classtable->get_class(class_)) << name << " is redefined!" << endl;
    return;
  }
  Symbols *parent_symbols = env->lookup_method(name);
  Symbols *symbols = NULL;
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    symbols = new Symbols(formals->nth(formals->len()-1-i)->get_type_decl(), symbols);
  }
  symbols = new Symbols(return_type, symbols);
  //check override
  if (parent_symbols != NULL) {
    Symbols *_parent_symbols = parent_symbols;
    Symbols *_symbols = symbols;
    while (_parent_symbols != NULL) {
      if (_parent_symbols->hd() != _symbols->hd()) {
	classtable->semant_error(classtable->get_class(class_)) << name << "illegal override!" << endl;
	return;
      }
      _parent_symbols = _parent_symbols->tl();
      _symbols = _symbols->tl();
    }
    if (_symbols != NULL) {
	classtable->semant_error(classtable->get_class(class_)) << name << "illegal override!" << endl;
	return;
    }
  }

  env->addid(name, symbols);
}


Symbol class__class::type_check(ClassTable *classtable){
  Enviroment *env = classtable->get_env(name);
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    features->nth(i)->type_check(name, classtable);
  }            
  return name;
}

Symbol attr_class::type_check(Symbol class_, ClassTable *classtable) {
  if (!classtable->validtype(classtable->evaltype(class_, type_decl))) {
    classtable->semant_error(classtable->get_class(class_)) << type_decl << " is invalid!" << endl;
    return Object;
  }
  if (!classtable->subtype(classtable->evaltype(class_, type_decl), classtable->evaltype(class_, init->type_check(class_, classtable)))) {
    classtable->semant_error(classtable->get_class(class_)) << "attr " << name << ": sematic error!" << endl;
    return Object;
  }
  return type_decl;
}


Symbol method_class::type_check(Symbol class_, ClassTable *classtable) {
  Enviroment *env = classtable->get_env(class_);

  if (!classtable->validtype(classtable->evaltype(class_, return_type))) {
    classtable->semant_error(classtable->get_class(class_)) << return_type << " is invalid!" << endl;
    return Object;
  }

  env->enterscope();
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    env->addid(formals->nth(i)->get_name(),formals->nth(i)->type_check(class_, classtable));
  }

  Symbol t0_prim = expr->type_check(class_, classtable);
  env->exitscope();

  if (return_type == SELF_TYPE && t0_prim != SELF_TYPE) {
    classtable->semant_error(classtable->get_class(class_)) << "method " << name << ": Non-conforming return type when SELF_TYPE is declared return type " << t0_prim << endl;
    return Object;
  }
  if (!classtable->subtype(classtable->evaltype(class_, return_type), classtable->evaltype(class_, t0_prim))) {
    classtable->semant_error(classtable->get_class(class_)) << "method " << name << ": sematic error!" << endl;
    return Object;
  }

  return return_type;
}

Symbol formal_class::type_check(Symbol class_, ClassTable *classtable) {
  Enviroment *env = classtable->get_env(class_);
  if (env->probe_type(name)!=NULL) {
    classtable->semant_error(classtable->get_class(class_)) << name << " is redefined!" << endl;
    return Object;
  }
  if (env->lookup_type(name) == SELF_TYPE) {
    classtable->semant_error(classtable->get_class(class_)) << name << " can not be self!" << endl;
    return Object;
  }
  if (!classtable->validtype(type_decl)) {
    classtable->semant_error(classtable->get_class(class_)) << type_decl << " is invalid!" << endl;
    return Object;
  }
  return type_decl;
}

Symbol assign_class::type_check(Symbol class_, ClassTable *classtable) {
  Enviroment *env = classtable->get_env(class_);
  Symbol t = env->lookup_type(name);
  if (t == NULL) {
    classtable->semant_error(classtable->get_class(class_)) << name << " is invalid!" << endl;
    return Object;
  }
  if (t == SELF_TYPE) {
    classtable->semant_error(classtable->get_class(class_)) << name << " can not be self!" << endl;
    return Object;
  }
  Symbol t_prim = expr->type_check(class_, classtable);
  if (!classtable->subtype(classtable->evaltype(class_,t), classtable->evaltype(class_,t_prim))) {
    classtable->semant_error(classtable->get_class(class_)) << name << ": sematic error!" << endl;
    return Object;
  }
  
  return set_type(t_prim)->get_type();
}

Symbol static_dispatch_class::type_check(Symbol class_, ClassTable *classtable) {
  Symbol t0 = expr->type_check(class_, classtable);
  if (!classtable->subtype(classtable->evaltype(class_, type_name), classtable->evaltype(class_, t0))) {
    classtable->semant_error(classtable->get_class(class_)) << name << ": type name is not match!" << endl;
    return Object;
  }
  Symbols *formal_types = classtable->get_env(type_name)->lookup_method(name);
  if (formal_types == NULL) {
    classtable->semant_error(classtable->get_class(class_)) << name << ": " << " is invalid!" << endl;
    return Object;
  }
  Symbol t_return = formal_types->hd();
  formal_types = formal_types->tl();

  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Symbol ti = actual->nth(i)->type_check(class_, classtable);
    if (formal_types == NULL) {
      classtable->semant_error(classtable->get_class(class_)) << name << ": " << " the number of parameters is not match!" << endl;
      return Object;
    }
    if (!classtable->subtype(classtable->evaltype(t0, formal_types->hd()), classtable->evaltype(class_, ti))) {
      classtable->semant_error(classtable->get_class(class_)) << name << ": " << i+1 << "th parameter is not match! expect: " << formal_types->hd() << " but actaul: " << ti << endl;
      return Object;
    }
    formal_types = formal_types->tl();
  }

  return set_type(classtable->evaltype(t0, t_return))->get_type();
}

Symbol dispatch_class::type_check(Symbol class_, ClassTable *classtable) {
  Symbol t0 = expr->type_check(class_, classtable);

  Symbols *formal_types = classtable->get_env(classtable->evaltype(class_, t0))->lookup_method(name);
  if (formal_types == NULL) {
    classtable->semant_error(classtable->get_class(class_)) << name << ": " << " is invalid!" << endl;
    return Object;
  }
  Symbol t_return = formal_types->hd();
  formal_types = formal_types->tl();

  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Symbol ti = actual->nth(i)->type_check(class_, classtable);
    if (formal_types == NULL) {
      classtable->semant_error(classtable->get_class(class_)) << name << ": " << " the number of parameters is not match!" << endl;
      return Object;
    }
    if (!classtable->subtype(classtable->evaltype(t0, formal_types->hd()), classtable->evaltype(class_, ti))) {
      classtable->semant_error(classtable->get_class(class_)) << name << ": " << i+1 << "th parameter is not match! expect: " << formal_types->hd() << " but actaul: " << ti << endl;
      return Object;
    }
    formal_types = formal_types->tl();
  }

  return set_type(classtable->evaltype(t0, t_return))->get_type();
}

Symbol cond_class::type_check(Symbol class_, ClassTable *classtable) {
  if (!classtable->istype(classtable->evaltype(class_, pred->type_check(class_, classtable)), Bool)) {
    classtable->semant_error(classtable->get_class(class_)) << pred << " is expect to be Bool!" << endl;
    return Object;
  }
  return set_type(classtable->leastcommontype(classtable->evaltype(class_, then_exp->type_check(class_, classtable)),
					      classtable->evaltype(class_, else_exp->type_check(class_, classtable))))->get_type();
}

Symbol loop_class::type_check(Symbol class_, ClassTable *classtable) {
  Symbol t1 = pred->type_check(class_, classtable);
  if (!classtable->istype(t1, Bool)) {
    classtable->semant_error(classtable->get_class(class_)) << pred << " is expect to be Bool!" << endl;
    return Object;
  }
  body->type_check(class_, classtable);
  return set_type(Object)->get_type();
}

Symbol branch_class::type_check(Symbol class_, ClassTable *classtable) {
  if (!classtable->validtype(type_decl)) {
    classtable->semant_error(classtable->get_class(class_)) << type_decl << " is invlaid!" << endl;
    return Object;
  }
  Enviroment *env = classtable->get_env(class_);
  env->enterscope();
  env->addid(name,type_decl);
  Symbol t = expr->type_check(class_, classtable);  
  env->exitscope();
  return t;
}

Symbol typcase_class::type_check(Symbol class_, ClassTable *classtable) {
  SymbolTable<Symbol ,Entry> *branch_type_map = new SymbolTable<Symbol ,Entry>();
  Symbol t_common = No_type;
  branch_type_map -> enterscope();
  expr->type_check(class_, classtable);
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    if (branch_type_map->probe(cases->nth(i)->get_type_decl())) {
      classtable->semant_error(classtable->get_class(class_)) << "Duplicate branch in case statement!" << endl;
      return Object;
    }
    t_common = classtable->leastcommontype(classtable->evaltype(class_,cases->nth(i)->type_check(class_, classtable)), t_common);
    branch_type_map->addid(cases->nth(i)->get_type_decl(), cases->nth(i)->get_name());
  }
  branch_type_map -> exitscope();
  return set_type(t_common)->get_type();
}

Symbol block_class::type_check(Symbol class_, ClassTable *classtable) {
  Symbol t = No_type;
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    t = body->nth(i)->type_check(class_, classtable);
  }
  return set_type(t)->get_type();
}

Symbol let_class::type_check(Symbol class_, ClassTable *classtable) {
  Enviroment *env = classtable->get_env(class_);
  if (!classtable->subtype(classtable->evaltype(class_, type_decl), classtable->evaltype(class_, init->type_check(class_, classtable)))) {
    classtable->semant_error(classtable->get_class(class_)) << identifier << " is not match!" << endl;
    return Object;
  }
  if (env->lookup_type(identifier) == SELF_TYPE) {
    classtable->semant_error(classtable->get_class(class_)) << identifier << " can not be self!" << endl;
    return Object;
  }
  env->enterscope();
  env->addid(identifier, type_decl);
  Symbol t = body->type_check(class_, classtable);
  env->exitscope();
  return set_type(t)->get_type();
}

Symbol plus_class::type_check(Symbol class_, ClassTable *classtable) {
  Symbol t1 = e1->type_check(class_, classtable);
  if (!classtable->istype(t1, Int)) {
    classtable->semant_error(classtable->get_class(class_)) << e1 << " is expect to be Int!" << endl;
    return Object;
  }
  Symbol t2 = e2->type_check(class_, classtable);
  if (!classtable->istype(t2, Int)) {
    classtable->semant_error(classtable->get_class(class_)) << e2 << " is expect to be Int!" << endl;
    return Object;
  }
  return set_type(Int)->get_type();
}

Symbol sub_class::type_check(Symbol class_, ClassTable *classtable) {
  Symbol t1 = e1->type_check(class_, classtable);
  if (!classtable->istype(t1, Int)) {
    classtable->semant_error(classtable->get_class(class_)) << e1 << " is expect to be Int!" << endl;
    return Object;
  }
  Symbol t2 = e2->type_check(class_, classtable);
  if (!classtable->istype(t2, Int)) {
    classtable->semant_error(classtable->get_class(class_)) << e2 << " is expect to be Int!" << endl;
    return Object;
  }
  return set_type(Int)->get_type();

}

Symbol mul_class::type_check(Symbol class_, ClassTable *classtable) {
  Symbol t1 = e1->type_check(class_, classtable);
  if (!classtable->istype(t1, Int)) {
    classtable->semant_error(classtable->get_class(class_)) << e1 << " is expect to be Int!" << endl;
    return Object;
  }
  Symbol t2 = e2->type_check(class_, classtable);
  if (!classtable->istype(t2, Int)) {
    classtable->semant_error(classtable->get_class(class_)) << e2 << " is expect to be Int!" << endl;
    return Object;
  }
  return set_type(Int)->get_type();

}

Symbol divide_class::type_check(Symbol class_, ClassTable *classtable) {
  Symbol t1 = e1->type_check(class_, classtable);
  if (!classtable->istype(t1, Int)) {
    classtable->semant_error(classtable->get_class(class_)) << e1 << " is expect to be Int!" << endl;
    return Object;
  }
  Symbol t2 = e2->type_check(class_, classtable);
  if (!classtable->istype(t2, Int)) {
    classtable->semant_error(classtable->get_class(class_)) << e2 << " is expect to be Int!" << endl;
    return Object;
  }
  return set_type(Int)->get_type();
}

Symbol neg_class::type_check(Symbol class_, ClassTable *classtable) {
  Symbol t1 = e1->type_check(class_, classtable);
  if (!classtable->istype(t1, Int)) {
    classtable->semant_error(classtable->get_class(class_)) << e1 << " is expect to be Int!" << endl;
    return Object;
  }
  return set_type(Int)->get_type();
}

Symbol lt_class::type_check(Symbol class_, ClassTable *classtable) {
  Symbol t1 = e1->type_check(class_, classtable);
  if (!classtable->istype(t1, Int)) {
    classtable->semant_error(classtable->get_class(class_)) << e1 << " is expect to be Int!" << endl;
    return Object;
  }
  Symbol t2 = e2->type_check(class_, classtable);
  if (!classtable->istype(t2, Int)) {
    classtable->semant_error(classtable->get_class(class_)) << e2 << " is expect to be Int!" << endl;
    return Object;
  }
  return set_type(Bool)->get_type();
}

Symbol eq_class::type_check(Symbol class_, ClassTable *classtable) {
  Symbol t1 = e1->type_check(class_, classtable);
  Symbol t2 = e2->type_check(class_, classtable);
  if ((classtable->istype(t1, Int) && !classtable->istype(t2, Int)) || 
      (!classtable->istype(t1, Int) && classtable->istype(t2, Int)) || 
      (classtable->istype(t1, Bool) && !classtable->istype(t2, Bool)) || 
      (!classtable->istype(t1, Bool) && classtable->istype(t2, Bool)) ||
      (classtable->istype(t1, Str) && !classtable->istype(t2, Str)) || 
      (!classtable->istype(t1, Str) && classtable->istype(t2, Str))) {
    classtable->semant_error(classtable->get_class(class_)) << t1 << " and " << t2 << " are not match!" << endl;
    return Object;
  }
  return set_type(Bool)->get_type();
}

Symbol leq_class::type_check(Symbol class_, ClassTable *classtable) {
  Symbol t1 = e1->type_check(class_, classtable);
  if (!classtable->istype(t1, Int)) {
    classtable->semant_error(classtable->get_class(class_)) << e1 << " is expect to be Int!" << endl;
    return Object;
  }
  Symbol t2 = e2->type_check(class_, classtable);
  if (!classtable->istype(t2, Int)) {
    classtable->semant_error(classtable->get_class(class_)) << e2 << " is expect to be Int!" << endl;
    return Object;
  }
  return set_type(Bool)->get_type();
}

Symbol comp_class::type_check(Symbol class_, ClassTable *classtable) {
  Symbol t1 = e1->type_check(class_, classtable);
  if (!classtable->istype(t1, Bool)) {
    classtable->semant_error(classtable->get_class(class_)) << e1 << " is expect to be Bool!" << endl;
    return Object;
  }
  return set_type(Bool)->get_type();
}

Symbol int_const_class::type_check(Symbol class_, ClassTable *classtable) {
  return set_type(Int)->get_type();
}

Symbol bool_const_class::type_check(Symbol class_, ClassTable *classtable) {
  return set_type(Bool)->get_type();
}

Symbol string_const_class::type_check(Symbol class_, ClassTable *classtable) {
  return set_type(Str)->get_type();
}

Symbol new__class::type_check(Symbol class_, ClassTable *classtable) {
  if (!classtable->validtype(classtable->evaltype(class_,type_name))) {
    classtable->semant_error(classtable->get_class(class_)) << type_name << " is invalid!" << endl;
    return Object;
  }
  return set_type(type_name)->get_type();
}

Symbol isvoid_class::type_check(Symbol class_, ClassTable *classtable) {
  e1->type_check(class_, classtable);
  return set_type(Bool)->get_type();
}

Symbol object_class::type_check(Symbol class_, ClassTable *classtable) {
  Enviroment *env = classtable->get_env(class_);
  Symbol t = env->lookup_type(name);
  if (t == NULL) {
    classtable->semant_error(classtable->get_class(class_)) << name << " is invalid!" << endl;
    return Object;
  }
  return set_type(t)->get_type();
}

Symbol no_expr_class::type_check(Symbol class_, ClassTable *classtable) {
  return set_type(No_type)->get_type();
}
