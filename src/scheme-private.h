/* scheme-private.h */

#ifndef _SCHEME_PRIVATE_H
#define _SCHEME_PRIVATE_H

#include "scheme.h"
/*------------------ Ugly internals -----------------------------------*/
/*------------------ Of interest only to FFI users --------------------*/

enum scheme_port_kind { 
  port_free=0, 
  port_file=1, 
  port_string=2, 
  port_input=16, 
  port_output=32 
};

typedef struct port {
  unsigned char kind;
  union {
    struct {            
      FILE *file;
      int closeit;
#if USE_FILE_AND_LINE
      char *name;
      int line;
#endif
    } stdio;
    struct {
      char *start;
      char *past_the_end;
      char *curr;
    } string;
  } rep;
} port;

/* cell structure */
struct cell {
#if USE_PROTECT
  struct list plist;
  int pref;
#endif
  unsigned int _flag;
  union {
    struct {
      char   *_svalue;
      int   _length;
    } _string;
    num _number;
    port *_port;
    foreign_func _ff;
    struct {
      struct cell *_car;
      struct cell *_cdr;
    } _cons;
  } _object;
};

struct scheme {
/* arrays for segments */
        func_alloc malloc;
        func_dealloc free;
        
/* return code */
        int retcode;
        int tracing;
        
#define CELL_SEGSIZE    16*5000  /* # of cells in one segment */
#define CELL_NSEGMENT   10    /* # of segments for cells */
        char *alloc_seg[CELL_NSEGMENT];
        pointer cell_seg[CELL_NSEGMENT];
        int     last_cell_seg;
        
/* We use 4 registers. */
        pointer args;            /* register for arguments of function */
        pointer envir;           /* stack register for current environment */
        pointer code;            /* register for current code */
        pointer dump;            /* stack register for next evaluation */

        int interactive_repl;    /* are we in an interactive REPL? */

        struct cell _sink;
        pointer sink;            /* when mem. alloc. fails */
        struct cell _NIL;
        pointer NIL;             /* special cell representing empty cell */
        struct cell _HASHT;
        pointer T;               /* special cell representing #t */
        struct cell _HASHF;
        pointer F;               /* special cell representing #f */
        struct cell _EOF_OBJ;
        pointer EOF_OBJ;         /* special cell representing end-of-file object */
        pointer oblist;          /* pointer to symbol table */
        pointer global_env;      /* pointer to global environment */

/* global pointers to special symbols */
        pointer LAMBDA;               /* pointer to syntax lambda */
        pointer QUOTE;           /* pointer to syntax quote */

        pointer QQUOTE;               /* pointer to symbol quasiquote */
        pointer UNQUOTE;         /* pointer to symbol unquote */
        pointer UNQUOTESP;       /* pointer to symbol unquote-splicing */
        pointer FEED_TO;         /* => */
        pointer COLON_HOOK;      /* *colon-hook* */
        pointer ERROR_HOOK;      /* *error-hook* */
        pointer SHARP_HOOK;  /* *sharp-hook* */

        pointer free_cell;       /* pointer to top of free cells */
        long    fcells;          /* # of free cells */

        pointer inport;
        pointer outport;
        pointer save_inport;
        pointer loadport;

#define MAXFIL 64
        port load_stack[MAXFIL];     /* Stack of open files for port -1 (LOADing) */
        int nesting_stack[MAXFIL];
        int file_i;
        int nesting;

        char    gc_verbose;      /* if gc_verbose is not zero, print gc status */
        char    no_memory;       /* Whether mem. alloc. has failed */

#define LINESIZE 1024
        char    linebuff[LINESIZE];
        char    strbuff[256];

        FILE *tmpfp;
        int tok;
        int print_flag;
        pointer value;
        int op;

        void *ext_data;     /* For the benefit of foreign functions */
        long gensym_cnt;

        struct scheme_interface *vptr;
        void *dump_base;	 /* pointer to base of allocated dump stack */
        int dump_size;		 /* number of frames allocated for dump stack */

#if USE_PROTECT
        struct list protect;
        int ignore_protect;
#endif
        char inside; /* gmcnutt: flag to check recursive entry from C */

#if USE_CUSTOM_FINALIZE
        void (*custom_finalize)(scheme *sc, pointer pp);
#endif
};

#define cons(sc,a,b) _cons(sc,a,b,0)
#define immutable_cons(sc,a,b) _cons(sc,a,b,1)

int is_string(pointer p);
char *string_value(pointer p);
int is_number(pointer p);
num nvalue(pointer p);
long ivalue(pointer p);
double rvalue(pointer p);
int is_integer(pointer p);
int is_real(pointer p);
int is_character(pointer p);
long charvalue(pointer p);
int is_vector(pointer p);

int is_port(pointer p);

int is_pair(pointer p);
pointer pair_car(pointer p);
pointer pair_cdr(pointer p);
pointer set_car(pointer p, pointer q);
pointer set_cdr(pointer p, pointer q);

int is_symbol(pointer p);
char *symname(pointer p);
int hasprop(pointer p);

int is_syntax(pointer p);
int is_proc(pointer p);
int is_foreign(pointer p);
char *syntaxname(pointer p);
int is_closure(pointer p);
#ifdef USE_MACRO
int is_macro(pointer p);
#endif
pointer closure_code(pointer p);
pointer closure_env(pointer p);

int is_continuation(pointer p);
int is_promise(pointer p);
int is_environment(pointer p);
int is_immutable(pointer p);
void setimmutable(pointer p);
int scm_len(scheme *sc, pointer list);

/* convenience macros for extrenal code that uses scheme internals */
#define scm_protect(sc, cell) \
        (sc)->vptr->protect((sc), (cell))

#define scm_unprotect(sc, cell) \
        (sc)->vptr->unprotect((sc), (cell))

#define scm_mk_ptr(sc, val) \
        (sc)->vptr->mk_foreign_func((sc), (foreign_func)(val))

#define scm_mk_integer(sc, val) \
        (sc)->vptr->mk_integer((sc), (val))

#define scm_mk_symbol(sc, val) \
        (sc)->vptr->mk_symbol((sc), (val))

#define scm_mk_string(sc, val) \
        (sc)->vptr->mk_string((sc), (val))

#define scm_define(sc, sym, val) \
        (sc)->vptr->scheme_define((sc),  \
                                  (sc)->global_env,  \
                                  (sc)->vptr->mk_symbol(sc, (sym)), \
                                  (val))


#define scm_define_int(sc, sym, val) \
        scm_define(sc, sym, (sc)->vptr->mk_integer((sc), (val)))

#define scm_define_bool(sc, sym, val) \
        scm_define(sc, sym, (val) ? (sc)->T : (sc)->F)

#define scm_define_ptr(sc, sym, val) \
        scm_define(sc, sym, scm_mk_ptr(sc, val))

#define scm_is_pair(sc,arg) ((sc)->vptr->is_pair(arg))
#define scm_is_num(sc, arg) ((sc)->vptr->is_number(arg))
#define scm_is_int(sc, arg) ((sc)->vptr->is_integer(arg))
#define scm_is_real(sc, arg) ((sc)->vptr->is_real(arg))
#define scm_is_str(sc, arg) ((sc)->vptr->is_string(arg))
#define scm_is_sym(sc, arg) ((sc)->vptr->is_symbol(arg))
#define scm_is_ptr(sc, arg) ((sc)->vptr->is_foreign(arg))
#define scm_is_closure(sc, arg) ((sc)->vptr->is_closure(arg))

#define scm_car(sc, arg) ((sc)->vptr->pair_car(arg))
#define scm_cdr(sc, arg) ((sc)->vptr->pair_cdr(arg))

#define scm_str_val(sc, arg) ((sc)->vptr->string_value(arg))
#define scm_ptr_val(sc, arg) ((void*)(arg)->_object._ff)
#define scm_int_val(sc, arg) ((sc)->vptr->ivalue(arg))
#define scm_real_val(sc, arg) ((sc)->vptr->rvalue(arg))
#define scm_sym_val(sc, arg) ((sc)->vptr->symname(arg))
#define scm_closure_code(sc, arg) ((sc)->vptr->closure_code(arg))
#define scm_closure_env(sc, arg) ((sc)->vptr->closure_env(arg))

#define scm_set_cust_fin(sc, arg) ((sc)->vptr->setcustfin(arg))


#endif
