#include "debug.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

// revisit: make these command-line options
int DEBUG = 1;
int VERBOSE = 1;

void dbg(const char *fmt, ...)
{
        if (DEBUG) {
                va_list args;
                va_start(args, fmt);
                vprintf(fmt, args);
                va_end(args);
        }
}

void err(const char *fmt, ...)
{
        va_list args;
        va_start(args, fmt);
        vfprintf(stderr, fmt, args);
        va_end(args);
        fprintf(stderr, "\n");
        exit(-1);
}

void info(const char *fmt, ...)
{
        if (VERBOSE) {
                va_list args;
                va_start(args, fmt);
                vprintf(fmt, args);
                va_end(args);
        }
}

void warn(const char *fmt, ...)
{
        va_list args;
        va_start(args, fmt);
        vprintf(fmt, args);
        va_end(args);
}

void vwarn(const char *fmt, va_list args)
{
        vprintf(fmt, args);
}
