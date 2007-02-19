/* SCHEME.H */

#ifndef _SCHEME_H
#define _SCHEME_H

#include <stdio.h>

#define USE_INTERFACE 1

/*
 * Default values for #define'd symbols
 */
#ifndef STANDALONE       /* If used as standalone interpreter */
# define STANDALONE 0
#endif

#ifndef _MSC_VER 
# define USE_STRCASECMP 1 
# ifndef WIN32 /* needed for Cygwin */
#  define USE_STRLWR 1 
# else
#  define USE_STRLWR 0
# endif
# define SCHEME_EXPORT
#else 
# define USE_STRCASECMP 0 
# define USE_STRLWR 0 
# ifdef _SCHEME_SOURCE
#  define SCHEME_EXPORT __declspec(dllexport)
# else
#  define SCHEME_EXPORT __declspec(dllimport)
# endif
#endif

#if USE_NO_FEATURES
# define USE_MATH 0
# define USE_CHAR_CLASSIFIERS 0
# define USE_ASCII_NAMES 0
# define USE_STRING_PORTS 0
# define USE_ERROR_HOOK 0
# define USE_TRACING 0
# define USE_COLON_HOOK 0
# define USE_DL 0
# define USE_PLIST 0
# define USE_PROTECT 0
# define USE_SERIALIZE 0
# define USE_REENTER 0
#endif

#if USE_DL
# define USE_INTERFACE 1
#endif

#ifndef USE_PROTECT
# include "list.h"
# define USE_PROTECT 1
#endif

#ifndef USE_REENTER
# define USE_REENTER 1 /* works around script->C->script return issues */
#endif

#ifndef USE_FILE_AND_LINE
# define USE_FILE_AND_LINE 1
#endif

#ifndef USE_SERIALIZE
# define USE_SERIALIZE 1
#endif

#ifndef USE_MATH         /* If math support is needed */
# define USE_MATH 1
#endif

#ifndef USE_CHAR_CLASSIFIERS  /* If char classifiers are needed */
# define USE_CHAR_CLASSIFIERS 1
#endif

#ifndef USE_ASCII_NAMES  /* If extended escaped characters are needed */
# define USE_ASCII_NAMES 1
#endif

#ifndef USE_STRING_PORTS      /* Enable string ports */
# define USE_STRING_PORTS 1
#endif

#ifndef USE_TRACING
# define USE_TRACING 0
#endif

#ifndef USE_PLIST
# define USE_PLIST 0
#endif

/* To force system errors through user-defined error handling (see *error-hook*) */
#ifndef USE_ERROR_HOOK
# define USE_ERROR_HOOK 1
#endif

#ifndef USE_COLON_HOOK   /* Enable qualified qualifier */
# define USE_COLON_HOOK 1
#endif

#ifndef USE_STRCASECMP   /* stricmp for Unix */
# define USE_STRCASECMP 0
#endif

#ifndef USE_STRLWR
# define USE_STRLWR 1
#endif

#ifndef STDIO_ADDS_CR    /* Define if DOS/Windows */
# define STDIO_ADDS_CR 0
#endif

#ifndef INLINE
# define INLINE
#endif

#ifndef USE_INTERFACE
# define USE_INTERFACE 0
#endif

#ifndef USE_CELLDUMP
# define USE_CELLDUMP 1
#endif

/* Allow cells to be marked with a flag that will run a custom finalizer on
 * them. */
#ifndef USE_CUSTOM_FINALIZE
# define USE_CUSTOM_FINALIZE 1
#endif

typedef struct scheme scheme;
typedef struct cell *pointer;

typedef void * (*func_alloc)(size_t);
typedef void (*func_dealloc)(void *);

/* num, for generic arithmetic */
typedef struct num {
     char is_fixnum;
     union {
          long ivalue;
          double rvalue;
     } value;
} num;

SCHEME_EXPORT scheme *scheme_init_new();
SCHEME_EXPORT scheme *scheme_init_new_custom_alloc(func_alloc malloc, func_dealloc free);
SCHEME_EXPORT int scheme_init(scheme *sc);
SCHEME_EXPORT int scheme_init_custom_alloc(scheme *sc, func_alloc, func_dealloc);
SCHEME_EXPORT void scheme_deinit(scheme *sc);
void scheme_set_input_port_file(scheme *sc, FILE *fin);
void scheme_set_input_port_string(scheme *sc, char *start, char *past_the_end);
SCHEME_EXPORT void scheme_set_output_port_file(scheme *sc, FILE *fin);
void scheme_set_output_port_string(scheme *sc, char *start, char *past_the_end);
#ifdef USE_FILE_AND_LINE
SCHEME_EXPORT void scheme_load_named_file(scheme *sc, FILE *fin, const char *fname);
#endif
SCHEME_EXPORT void scheme_load_file(scheme *sc, FILE *fin);
SCHEME_EXPORT void scheme_load_string(scheme *sc, const char *cmd);
void scheme_apply0(scheme *sc, const char *procname);
SCHEME_EXPORT pointer scheme_apply1(scheme *sc, const char *procname, pointer);
void scheme_set_external_data(scheme *sc, void *p);
SCHEME_EXPORT void scheme_define(scheme *sc, pointer env, pointer symbol, pointer value);
//SCHEME_EXPORT void scheme_call(scheme *sc, pointer func, pointer env, pointer args);
SCHEME_EXPORT pointer scheme_call(scheme *sc, pointer func, pointer args);

#if USE_SERIALIZE
SCHEME_EXPORT void scheme_serialize(scheme *sc, pointer p, struct save *save);
#endif

#if USE_CUSTOM_FINALIZE
SCHEME_EXPORT void scheme_set_custom_finalize(scheme *, 
                                              void (*fx)(scheme *, pointer));
#endif

typedef pointer (*foreign_func)(scheme *, pointer);

pointer _cons(scheme *sc, pointer a, pointer b, int immutable);
pointer mk_integer(scheme *sc, long num);
pointer mk_real(scheme *sc, double num);
pointer mk_symbol(scheme *sc, const char *name);
pointer gensym(scheme *sc);
pointer mk_string(scheme *sc, const char *str);
pointer mk_counted_string(scheme *sc, const char *str, int len);
pointer mk_character(scheme *sc, int c);
pointer mk_foreign_func(scheme *sc, foreign_func f);
void putstr(scheme *sc, const char *s);
void celldump(scheme *sc, pointer p);

#if USE_INTERFACE
struct scheme_interface {
  void (*scheme_define)(scheme *sc, pointer env, pointer symbol, pointer value);
  pointer (*cons)(scheme *sc, pointer a, pointer b);
  pointer (*immutable_cons)(scheme *sc, pointer a, pointer b);
  pointer (*mk_integer)(scheme *sc, long num);
  pointer (*mk_real)(scheme *sc, double num);
  pointer (*mk_symbol)(scheme *sc, const char *name);
  pointer (*gensym)(scheme *sc);
  pointer (*mk_string)(scheme *sc, const char *str);
  pointer (*mk_counted_string)(scheme *sc, const char *str, int len);
  pointer (*mk_character)(scheme *sc, int c);
  pointer (*mk_vector)(scheme *sc, int len);
  pointer (*mk_foreign_func)(scheme *sc, foreign_func f);
  pointer (*find_slot_in_env)(scheme *sc, pointer env, pointer sym, int all);
  void (*putstr)(scheme *sc, const char *s);
  void (*putcharacter)(scheme *sc, int c);
  
  int (*is_string)(pointer p);
  char *(*string_value)(pointer p);
  int (*is_number)(pointer p);
  num (*nvalue)(pointer p);
  long (*ivalue)(pointer p);
  double (*rvalue)(pointer p);
  int (*is_integer)(pointer p);
  int (*is_real)(pointer p);
  int (*is_character)(pointer p);
  long (*charvalue)(pointer p);
  int (*is_vector)(pointer p);
  long (*vector_length)(pointer vec);
  void (*fill_vector)(pointer vec, pointer elem);
  pointer (*vector_elem)(pointer vec, int ielem);
  pointer (*set_vector_elem)(pointer vec, int ielem, pointer newel);
  int (*is_port)(pointer p);
  
  int (*is_pair)(pointer p);
  pointer (*pair_car)(pointer p);
  pointer (*pair_cdr)(pointer p);
  pointer (*set_car)(pointer p, pointer q);
  pointer (*set_cdr)(pointer p, pointer q);

  int (*is_symbol)(pointer p);
  char *(*symname)(pointer p);
  
  int (*is_syntax)(pointer p);
  int (*is_proc)(pointer p);
  int (*is_foreign)(pointer p);
  char *(*syntaxname)(pointer p);
  int (*is_closure)(pointer p);
  int (*is_macro)(pointer p);
  pointer (*closure_code)(pointer p);
  pointer (*closure_env)(pointer p);

  int (*is_continuation)(pointer p);
  int (*is_promise)(pointer p);
  int (*is_environment)(pointer p);
  int (*is_immutable)(pointer p);
  void (*setimmutable)(pointer p);
  void (*load_file)(scheme *sc, FILE *fin);
  void (*load_string)(scheme *sc, const char *input);

#if USE_PROTECT
  pointer (*protect)(scheme *sc, pointer p);
  pointer (*unprotect)(scheme *sc, pointer p);
#endif

#if USE_CUSTOM_FINALIZE
  void (*setcustfin)(pointer p);
#endif

  foreign_func (*ffvalue)(pointer arg); /* gmcnutt: new addition */
};
#endif

#endif

