#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class Enviroment {
  typedef SymbolTable<Symbol ,Entry> TypeEnv; 
  typedef SymbolTable<Symbol ,Symbols> MethodEnv;  
private:
  TypeEnv *type_env;
  MethodEnv *method_env;
public:
 Enviroment():type_env(new TypeEnv()), method_env(new MethodEnv()) {}
  Enviroment &operator =(const Enviroment &s) { *type_env = *s.type_env;*method_env = *s.method_env; return *this; }

  void enterscope() {type_env->enterscope();method_env->enterscope();}
  void exitscope() {type_env->exitscope();method_env->exitscope();}
  void addid(Symbol s, Symbol i) {type_env->addid(s, i);}
  void addid(Symbol s, Symbols *i) {method_env->addid(s, i);}
  Symbol lookup_type (Symbol s) {return type_env->lookup(s);}
  Symbols *lookup_method (Symbol s) {return method_env->lookup(s);}
  Symbol probe_type (Symbol s) {return type_env->probe(s);}
  Symbols *probe_method (Symbol s) {return method_env->probe(s);}
  void dump() {
    cerr << "type_env:" << endl;
    type_env->dump();
    cerr << "method_env:" << endl;
    method_env->dump();
  }
};


class ClassTable {
  typedef SymbolTable<Symbol ,Class__class> ClassMap;
  typedef SymbolTable<Symbol ,Classes_class> ChildrenMap;
  typedef SymbolTable<Symbol , Enviroment> EnvMap; 
private:
  int semant_errors;
  ostream& error_stream;
  ClassMap *class_map;
  ChildrenMap *children_map;
  EnvMap *env_map;
  void install_basic_classes();
  void add_class(Class_ class_);
  void add_child(Class_ class_);
  void _dump(Symbol class_, ostream& stream, int n);
  void build_env(Symbol class_, Enviroment *parent_env);
public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  Class_ get_class(Symbol id);
  Class_ get_parent(Class_ class_);
  Enviroment *get_env(Symbol id);
  Classes get_children(Symbol class_);
  Symbol evaltype(Symbol scope, Symbol type);
  Symbol leastcommontype(Symbol class0, Symbol class1);
  int subtype(Symbol class0, Symbol class1);
  int istype(Symbol lhs, Symbol rhs);
  int validtype(Symbol type);
  void dump();
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};

#endif

