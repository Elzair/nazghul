dnl This file is part of the nazghul program
dnl
dnl Process this file with autogen.sh to produce a configure script.

dnl ---------------------------------------------------------------------------
dnl 1. Boilerplate
dnl      This section should include standard boilerplate code, such as the call 
dnl      to AC_INIT (which must be first), AM_INIT_AUTOMAKE, AC_CONFIG_HEADER, 
dnl      and perhaps AC_REVISION.
dnl ---------------------------------------------------------------------------

AC_PREREQ(2.59)

dnl   This macro performs essential initialization for the
dnl   generated `configure' script. An optional argument may provide the name of a
dnl   file from the source directory to ensure that the directory has been 
dnl   specified correctly.
AC_INIT(nazghul, 0.7.1, [nazghul-devel@lists.sourceforge.net])
AC_CONFIG_SRCDIR([src/nazghul.c])
AM_CONFIG_HEADER([config.h])

dnl AC_CANONICAL_HOST
dnl   This macro determines the type of the host system and sets the output 
dnl   variable `host', as well as other more obscure variables.
AC_CANONICAL_HOST

dnl AC_CANONICAL_TARGET
dnl   I'm not certain what this is. The automake book refers to 
dnl   AC_CANONICAL_SYSTEM, but not AC_CANONICAL_TARGET. A Google search only 
dnl   yields scads of mailing list reports about people having weird problems 
dnl   with this,
AC_CANONICAL_TARGET

dnl AM_INIT_AUTOMAKE(package, version, [nodefine])
dnl   This macro is used to do all the standard initialization required by 
dnl   Automake. It has two required arguments: the package name and the version 
dnl   number. This macro sets and calls AC_SUBST on the shell variables PACKAGE 
dnl   and VERSION. By default it also defines these variables (via 
dnl   AC_DEFINE_UNQUOTED). However, this macro also accepts an optional third 
dnl   argument which, if not empty, means that the AC_DEFINE_UNQUOTED calls for 
dnl   PACKAGE and VERSION should be suppressed.
AM_INIT_AUTOMAKE
AM_MAINTAINER_MODE

dnl ---------------------------------------------------------------------------
dnl 2. Options
dnl      The next section should include macros which add command-line options 
dnl      to configure, such as AC_ARG_ENABLE. It is typical to put support code
dnl      for the option in this section as well, if it is short enough.
dnl ---------------------------------------------------------------------------

dnl ---------------------------------------------------------------------------
dnl 3. Programs
dnl      Next it is traditional to check for programs that are either needed by
dnl      the configure process, the build process, or by one of the programs 
dnl      being built. This usually involves calls to macros like AC_CHECK_PROG
dnl      and AC_PATH_TOOL.
dnl ---------------------------------------------------------------------------

dnl AC_PROG_CC
dnl   This checks for the C compiler to use and sets the shell variable CC to the 
dnl   value. If the GNU C compiler is being used, this sets the shell variable GCC 
dnl   to `yes'. This macro sets the shell variable CFLAGS if it has not already 
dnl   been set. It also calls AC_SUBST on CC and CFLAGS.
AC_PROG_CC

dnl AC_PROG_CXX
dnl   This is like AC_PROG_CC, but it checks for the C++ compiler, and sets the
dnl   variables CXX, 
AC_PROG_CXX

dnl AC_PROG_LIBTOOL
dnl   This macro is the primary way to integrate Libtool support into `configure'.
dnl   If you are using Libtool, you should call this macro in `configure.in'. Among
dnl    other things, it adds support for the `--enable-shared' configure flag.
dnl AC_PROG_LIBTOOL

dnl AC_PROG_INSTALL
dnl   This looks for an install program and sets the output variables INSTALL, 
dnl   INSTALL_DATA, INSTALL_PROGRAM, and INSTALL_SCRIPT. This macro assumes that if
dnl   an install program cannot be found on the system, your package will have 
dnl   `install-sh' available in the directory chosen by AC_CONFIG_AUX_DIR.
dnl AC_PROG_INSTALL

AC_PROG_MAKE_SET

dnl ---------------------------------------------------------------------------
dnl 4. Libraries
dnl      Checks for libraries come before checks for other objects visible to C
dnl      (or C++, or anything else). This is necessary because some other checks
dnl      work by trying to link or run a program; by checking for libraries
dnl      first you ensure that the resulting programs can be linked.
dnl ---------------------------------------------------------------------------

dnl Check for SDL and SDL features, i.e. -lSDL
SDL_VERSION=1.2.3
AM_PATH_SDL($SDL_VERSION,
            :,
	    AC_MSG_ERROR([*** SDL version $SDL_VERSION not found!])
)

dnl ---------------------------------------------------------------------------
dnl 5. Headers
dnl      Next come checks for existence of headers.
dnl ---------------------------------------------------------------------------
AC_LANG([C])
AC_HEADER_STDC
AC_LANG([C++])
AC_CHECK_HEADERS([fcntl.h \
	malloc.h          \
	float.h           \
	limits.h])

dnl ---------------------------------------------------------------------------
dnl 6. Typedefs and structures
dnl    We do checks for typedefs after checking for headers for the simple
dnl    reason that typedefs appear in headers, and we need to know which headers
dnl    we can use before we look inside them.
dnl ---------------------------------------------------------------------------
PETI_ENABLED_DYNAMIC_LINKING
AC_C_CONST
AC_C_INLINE
AC_C_VOLATILE

dnl ---------------------------------------------------------------------------
dnl 7. Functions
dnl      Finally we check for functions. These come last because functions have
dnl      dependencies on the preceding items: when searching for functions,
dnl      libraries are needed in order to correctly link, headers are needed in
dnl      order to find prototypes (this is especially important for C++, which
dnl      has stricter prototyping rules than C), and typedefs are needed for
dnl      those functions which use or return types which are not built in.
dnl ---------------------------------------------------------------------------
AC_CHECK_FUNCS([malloc  \
	realloc         \
	select_argtypes \
	vprintf         \
	strerror        \
	strpbrk         \
	strstr          \
	atexit          \
	floor           \
	strcasecmp      \
	strdup          \
	realloc         \
	select          \
        strchr])

dnl Add in the SDL CFLAGS
CFLAGS="$CFLAGS $SDL_CFLAGS"

dnl Use the same flags for c++ as for c
CXXFLAGS="$CFLAGS"

dnl The preprocessor needs the same info too.
CXXCPP="$CXXCPP $CXXFLAGS"

dnl Set the compiler to c++ (for now)
CC="$CXX"

dnl Add libraries to build process
LIBS="$LIBS $SDL_LIBS"

dnl Check specifically for libpng, i.e. -lpng switch
AC_CHECK_LIB(png, png_create_write_struct)

AC_CHECK_HEADER([png.h],
               [],
               [AC_MSG_ERROR([*** png include file not found!
You should install development package.])])

dnl Check specifically for SDL_image library, i.e. -lSDL_image switch
AC_CHECK_LIB(SDL_image, IMG_ReadXPMFromArray)

AC_CHECK_HEADER([SDL_image.h],
               [],
               [AC_MSG_ERROR([*** SDL_image include files not found!
You should install development package.])])

dnl Check specifically for SDL_mixer library, i.e. -lSDL_mixer switch
AC_CHECK_LIB(SDL_mixer, Mix_OpenAudio)

AC_CHECK_HEADER([SDL_mixer.h],
               [],
               [AC_MSG_ERROR([*** SDL_mixer include files not found!
You should install development package.])])

dnl ---------------------------------------------------------------------------
dnl 8. Output
dnl      This is done by invoking AC_OUTPUT.
dnl ---------------------------------------------------------------------------

# User directories for saving game data
 
AC_DEFINE(NAZGHUL_USER_DIR, "/.nazghul",
           [.nazghul is for Unix users.])

AC_DEFINE(NAZGHUL_USER_DIR_WIN, "/_nazghul",
           [_nazghul is for Windows users.])

AC_DEFINE(HOME_ENV, "HOME",
           [Most likely your home directory is stored in $HOME.])

AC_DEFINE(MIN_SCRIPT_MAJOR, 0,
          [Minimum supported script version])

AC_DEFINE(MIN_SCRIPT_MINOR, 6,
          [Minimum supported script version])

AC_DEFINE(MIN_SCRIPT_RELEASE, 0,
          [Minimum supported script version])

AC_DEFINE(USE_SKILLS, 1, [Enable Yuse command and skills])
AC_DEFINE(USE_QUESTS, 1, [Enable quest log])

# Finally create all the generated files
AC_CONFIG_FILES([Makefile \
	m4/Makefile       \
	src/Makefile      \
	worlds/Makefile   \
	worlds/haxima-1.002/Makefile \
        worlds/haxima-1.002/music/Makefile])

AC_OUTPUT

cat <<EOF

$PACKAGE_NAME $PACKAGE_VERSION configured.

You can now type \`gmake' to compile $PACKAGE_NAME.
If \`gmake' is not available on your system, try \`make' instead.
EOF
