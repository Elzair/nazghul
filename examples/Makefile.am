# gmcnutt: hack - not a lib, just a derived target that needs to get
# build... what's the right way to do this?
bin_SCRIPTS = mapfile
EXTRA_DIST = data dup_constants.h

# Special rule to build the mapfile
mapfile: forced_rebuild
	$(MAKE) -C data
	cp data/mapfile ./

forced_rebuild:
	rm -f mapfile

# Special rule to clean
clean:
	$(MAKE) -C data clean