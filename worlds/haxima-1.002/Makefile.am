haximadir   = $(datadir)/nazghul/haxima
bin_SCRIPTS = haxima.sh
CLEANFILES = haxima.sh
SUBDIRS = music

haxima.sh: Makefile
	echo '#!/bin/sh' > $@ 
	echo '"@bindir@/nazghul" -I "@datadir@/nazghul/haxima" -G "$$HOME/.haxima" "$$@"' >> $@

haxima_DATA = $(wildcard *.scm) $(wildcard *.png) $(wildcard *.wav)

EXTRA_DIST = $(haxima_DATA)
MAINTAINERCLEANFILES = Makefile.in

