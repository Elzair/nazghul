
all: force_mapfile_rebuild mapfile

# Perhaps 'make deps' could find all cpp dependences 
# and remove the need for 'force_mapfile_rebuild' ?
force_mapfile_rebuild:
	@echo "Forcing mapfile rebuild:"
	rm -f mapfile

mapfile: main.ghul
	@echo "Building mapfile..."
	cpp -P main.ghul > $@

clean:
	rm -rf *.o *.d *~ core nazghul '#'* .'#'*
