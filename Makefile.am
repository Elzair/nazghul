

SUBDIRS = src examples

DIST_SUBDIRS = $(SUBDIRS)

EXTRA_DIST = doc



# Rule to build tar-gzipped distribution package
# gmcnutt: added commands to retar w/o CVS directories
$(PACKAGE)-$(VERSION).tar.gz: dist
	GZIP=$(GZIP_ENV) $(TAR) zxf $@
	GZIP=$(GZIP_ENV) $(TAR) chozf  --exclude CVS $@ $(distdir)
	rm -rf $(distdir)

# Rule to build RPM distribution package
rpm: $(PACKAGE)-$(VERSION).tar.gz
	rpm -ta $(PACKAGE)-$(VERSION).tar.gz
