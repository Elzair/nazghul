AC_DEFUN([PETI_ENABLED_DYNAMIC_LINKING], [
AC_MSG_CHECKING(whether what binaries we shall create)
AC_ARG_ENABLE(dynamic-link,
[  --enable-dynamic-link   Create dynamically linked binaries (default)],
if test "$enableval" = "yes"; then
    AC_MSG_RESULT(dynamically linked)
else
    LDFLAGS="$LDFLAGS -static"
    AC_MSG_RESULT(statically linked)
fi,
AC_MSG_RESULT(dynamically linked))
])

