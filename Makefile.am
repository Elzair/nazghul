AUTOMAKE_OPTIONS = dist-shar dist-zip dist-tarZ

SUBDIRS = src worlds m4

DIST_SUBDIRS = $(SUBDIRS)

EXTRA_DIST = doc

ACLOCAL_AMFLAGS = -I m4

# build a distribution snapshot
# TODO:
#  add a snap documentation target
#  check to see if there is already a previous snapshot there,
#  and don't overwrite it!
@MAINT@snapshot: README-snapshot maintainer-clean
@MAINT@	@a=`date -R` ;\
@MAINT@	echo "Last built on $$a" >> README-snapshot
@MAINT@	@d=`pwd` ;\
@MAINT@	d=`basename $$d` ;\
@MAINT@	echo $$d ;\
@MAINT@	cd .. ;\
@MAINT@	tar -cvzf $$d-`date +%m%d%y`.tar.gz $$d ;\
@MAINT@	mv $$d-`date +%m%d%y`.tar.gz $$d/ ;\
@MAINT@	cd $$d

@MAINT@alldist:
@MAINT@	${MAKE} distcheck
@MAINT@	${MAKE} dist-shar
@MAINT@	${MAKE} dist-zip
@MAINT@	${MAKE} dist-tarZ

@MAINT@cvsclean: maintainer-clean
@MAINT@	@-rm -f `find . -name Makefile.in`
@MAINT@	@-rm -f libtool ltmain.sh acinclude.m4
@MAINT@	@-rm -f configure aclocal.m4 config.h.in stamp-h.in depcomp ylwrap
@MAINT@	@-rm -f config.guess config.sub config.cache config.log config.status
@MAINT@	@-rm -f mkinstalldirs missing install-sh COPYING ABOUT-NLS config.rpath
@MAINT@	@-rm -fr @PACKAGE@-@VERSION@* *~ */*~
@MAINT@	@echo "================================================="
@MAINT@	@echo "Don't forget your ChangeLog and NEWS entries ...."
@MAINT@	@echo "================================================="
