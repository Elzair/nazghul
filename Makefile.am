AUTOMAKE_OPTIONS = dist-shar dist-zip dist-tarZ

SUBDIRS = src worlds m4

DIST_SUBDIRS = $(SUBDIRS)

EXTRA_DIST = doc icons haxima.desktop

ACLOCAL_AMFLAGS = -I m4

@MAINT@alldist:
@MAINT@	${MAKE} distcheck
@MAINT@	${MAKE} dist-shar
@MAINT@	${MAKE} dist-zip
@MAINT@	${MAKE} dist-tarZ

MAINTAINERCLEANFILES = configure Makefile.in aclocal.m4 config.h.in \
		config.guess config.sub depcomp install-sh \
		missing mkinstalldirs

@MAINT@cvsclean: maintainer-clean
@MAINT@	@-rm -f `find . -name Makefile.in`
@MAINT@	@-rm -f libtool ltmain.sh acinclude.m4 */Makefile.in
@MAINT@	@-rm -f configure aclocal.m4 config.h.in stamp-h.in depcomp ylwrap
@MAINT@	@-rm -f config.guess config.sub config.cache config.log config.status
@MAINT@	@-rm -f mkinstalldirs missing install-sh COPYING ABOUT-NLS config.rpath
@MAINT@	@-rm -fr @PACKAGE@-@VERSION@* *~ */*~
@MAINT@	@echo "================================================="
@MAINT@	@echo "Don't forget your ChangeLog and NEWS entries ...."
@MAINT@	@echo "================================================="
