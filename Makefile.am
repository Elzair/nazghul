

SUBDIRS = src examples

DIST_SUBDIRS = $(SUBDIRS)

EXTRA_DIST = doc

# Rule to build tar-gzipped distribution package
$(PACKAGE)-$(VERSION).tar.gz: dist

# Rule to build RPM distribution package
rpm: $(PACKAGE)-$(VERSION).tar.gz
	rpm -ta $(PACKAGE)-$(VERSION).tar.gz
