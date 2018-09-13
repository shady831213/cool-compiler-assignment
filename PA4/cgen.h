#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

typedef void (CgenClassTable::*Callback)(CgenNodeP nd);

class CgenClassTableCallback {
 private:
  CgenClassTableP class_table;
  Callback  cb;
 public:
 CgenClassTableCallback(CgenClassTableP _class_table, Callback _cb): class_table(_class_table), cb(_cb) {}
 void callback(CgenNodeP nd) { return (class_table->*cb)(nd);}
};


class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
typedef List<CgenClassTableCallback> Callbacks;
private:
   List<CgenNode> *nds;
   ostream& str;

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools();
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);

//My defination
   int class_cnt;
   int stringclasstag();
   int intclasstag();
   int boolclasstag();

   void tree_walk(CgenNodeP nd, Callbacks *cbs);

   void _build_class_info(CgenNodeP nd);
   void _code_class_name_table(CgenNodeP nd);
   void _code_class_protobj_table(CgenNodeP nd);
   void _code_class_tables(CgenNodeP nd);
   void _code_class_methods(CgenNodeP nd);


   void build_class_info();
   void code_class_name_table();
   void code_class_protobj_table();
   void code_class_tables();
   void code_class_methods();

public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenMethod : public method_class {
private: 
  Symbol class_name;
public:
 CgenMethod(method_class *m, Symbol c_name) :    
  method_class((const method_class &) *m),
    class_name(c_name){}
  
  CgenMethodP copy() {return new CgenMethod((method_class *)copy_Feature(), class_name);}
  void copy_from(CgenMethodP nd) {
    class_name = nd->get_class_name();
    name = nd->get_name();
    return_type = nd->get_return_type();
    formals = nd->get_formals();
    expr = nd->get_expr();
  }
  Symbol get_class_name(){return class_name;}
  void set_class_name(Symbol c_name){class_name = c_name;}

};

class CgenNode : public class__class {
typedef SymbolTable<Symbol ,IdEnv> MethodEnv;
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   list_node<attr_class *> *attrs;
   list_node<CgenMethodP> *methods;
   int       tag;
   int       max_child_tag;
   //if in stack, offset value is neg, else it is pos
   IdEnv *id_env;
   MethodEnv *method_env;

   CgenClassTableP class_table;
public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP ct);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   //My definations
   void set_tag(int _tag){tag = _tag;}
   void set_max_child_tag(int _tag){max_child_tag = _tag;}
   int get_tag(){return tag;}
   int get_max_child_tag(){return max_child_tag;}
   list_node<attr_class *> *get_attrs() { return attrs; }
   list_node<CgenMethodP> *get_methods() { return methods; }
   int get_method(Symbol method_name) {
     for (int i = methods->first(); methods->more(i); i = methods->next(i)) {
       if (methods->nth(i)->get_name() == method_name) {
	 return i;
       }
     }
     return -1;
   }
   CgenClassTableP  get_class_table() {return class_table;}
   IdEnv *get_id_env(){return id_env;}
   IdEnv *get_id_env(Symbol method_name) {return method_env->probe(method_name);}
   void build_env();
   void code_protobj(ostream&);	  
   void code_dispatch_tab(ostream&);		
   void code_init(ostream&);			
   void code_methods(ostream&);			
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

