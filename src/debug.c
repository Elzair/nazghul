#include "debug.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

// revisit: make these command-line options
int DEBUG = 1;
int VERBOSE = 1;

void dbg(char *fmt, ...)
{
        if (DEBUG) {
                va_list args;
                va_start(args, fmt);
                vprintf(fmt, args);
                va_end(args);
        }
}

void err(char *fmt, ...)
{
        va_list args;
        va_start(args, fmt);
        vfprintf(stderr, fmt, args);
        va_end(args);
        fprintf(stderr, "\n");
        exit(-1);
}

void info(char *fmt, ...)
{
        if (VERBOSE) {
                va_list args;
                va_start(args, fmt);
                vprintf(fmt, args);
                va_end(args);
        }
}

void warn(char *fmt, ...)
{
        va_list args;
        va_start(args, fmt);
        vprintf(fmt, args);
        va_end(args);
}

void vwarn(char *fmt, va_list args)
{
        vprintf(fmt, args);
}
