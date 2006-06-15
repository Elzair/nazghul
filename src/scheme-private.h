/* scheme-private.h */

#ifndef _SCHEME_PRIVATE_H
#define _SCHEME_PRIVATE_H

#include "scheme.h"
#include "gc.h"
/*------------------ Ugly internals -----------------------------------*/
/*------------------ Of interest only to FFI users --------------------*/

/* ADJ is enough slack to align cells in a TYPE_BITS-bit boundary */
#define ADJ 32
#define TYPE_BITS 5

/* cell flags */
#define T_MASKTYPE      31    /* 0000000000011111 */
#define FLAG1          512    /* 0000001000000000 */   /* extension for gc */
#define FLAG2         1024    /* 0000010000000000 */   /* extension for gc */
#define FLAG3         2048    /* 0000100000000000 */   /* extension for gc */
#define T_SYNTAX      4096    /* 0001000000000000 */
#define T_IMMUTABLE   8192    /* 0010000000000000 */
#define T_ATOM       16384    /* 0100000000000000 */   /* only for gc */
#define CLRATOM      49151    /* 1011111111111111 */   /* only for gc */
#define MARK         32768    /* 1000000000000000 */
#define UNMARK       32767    /* 0111111111111111 */

#define MAX_REF      255

/* macros for cell operations needed by gc */
#define typeflag(p)      ((p)->_flag)
#define type(p)          (typeflag(p)&T_MASKTYPE)
#define settype(p,v)     (typeflag(p)=((typeflag(p)&~T_MASKTYPE)|(v)))
#define car(p)           ((p)->_object._cons._car)
#define cdr(p)           ((p)->_object._cons._cdr)
#define is_atom(p)       (typeflag(p)&T_ATOM)
#define setatom(p)       typeflag(p) |= T_ATOM
#define clratom(p)       typeflag(p) &= CLRATOM
#define is_mark(p)       (typeflag(p)&MARK)
#define setmark(p)       typeflag(p) |= MARK
#define clrmark(p)       typeflag(p) &= UNMARK
#define ivalue_unchecked(p)       ((p)->_object._number.value.ivalue)
#define ref(p)           ((p)->ref)

enum scheme_port_kind { 
  port_free=0, 
  port_file=1, 
  port_string=2, 
  port_input=16, 
  port_output=32 
};

/* operator code */
enum scheme_opcodes { 
#define _OP_DEF(A,B,C,D,E,OP) OP, 
#include "opdefines.h" 
  OP_MAXDEFINED 
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
#if USE_REFCOUNT
  unsigned char ref;
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

#ifndef USE_SCHEME_STACK 
/* this structure holds all the interpreter's registers */ 
struct dump_stack_frame { 
  enum scheme_opcodes op; 
  pointer args; 
  pointer envir; 
  pointer code; 
}; 
#endif

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

gc_t *gc;

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
#endif
        char inside; /* gmcnutt: flag to check recursive entry from C */
#if USE_DEBUG
  int debug_nesting;
  pointer breaksym;
  pointer current_proc;
  pointer CURRENT_PROC;
  char dbg_next;
  char dbg_step;
#endif
        char *include_dir;
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
void finalize_cell(scheme *sc, pointer a);

#endif
