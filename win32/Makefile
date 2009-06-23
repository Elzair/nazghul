CFLAGS = -g -Wall -DDATADIR=. -DPREFIX=.

bin_PROGRAMS = nazghul

nazghul_SOURCES =		\
	angband.c \
        Arms.cpp \
        Arms.h \
        ascii.c \
        ascii.h \
        astar.c \
        astar.h \
        character.cpp \
        character.h \
        cmdwin.c \
        cmdwin.h \
        combat.c \
        combat.h \
        common.c \
        common.h \
        console.c \
        console.h \
        constants.h \
        Container.cpp \
        Container.h \
        cursor.cpp \
        cursor.h \
        debug.h \
	debug.c \
        dup_constants.h \
        event.c \
        event.h \
	escape.c \
	escape.h \
        Field.h \
	Field.cpp \
        floodfill.c \
        foogod.c \
        foogod.h \
        formation.h \
	formation.c \
        hash.c \
        hash.h \
        heap.c \
        heap.h \
        images.c \
        images.h \
        knapsack.c \
        knapsack.h \
        list.h \
        los.c \
        los.h \
        Makefile \
        map.c \
        map.h \
        nazghul.c \
        Party.cpp \
        Party.h \
        object.c \
        object.h \
        occ.c \
        occ.h \
        olist.c \
        olist.h \
        pinfo.h \
        place.c \
        place.h \
        play.c \
        player.cpp \
        player.h \
        play.h \
        Reagent.cpp \
        Reagent.h \
        sched.c \
        sched.h \
        screen.c \
        screen.h \
        sky.c \
        sky.h \
        sound.c \
        sound.h \
        species.c \
        species.h \
        sprite.c \
        sprite.h \
        status.c \
        status.h \
        terrain.c \
        terrain.h \
        terrain_map.c \
        terrain_map.h \
        tree.c \
        tree.h \
        vehicle.cpp \
        vehicle.h \
        wind.c \
        wind.h \
        wq.c \
        wq.h \
	Missile.cpp \
	Missile.h \
	cmd.c \
	cmd.h \
	clock.c \
	clock.h \
	vmask.h \
	vmask.c \
	ctrl.h \
	ctrl.c \
	scheme.c \
	scheme.h \
	scheme-private.h \
	opdefines.h \
	macros.h \
	session.c \
	session.h \
	closure.h \
	closure.c \
	kern.h \
	kern.c \
	dice.h \
	dice.c \
	gob.c \
	gob.h \
	magic.h \
	magic.c \
	effect.h \
	effect.c \
	conv.h \
	conv.c \
	ptable.c \
	ptable.h \
	mmode.c \
	mmode.h \
	log.h \
	log.c \
	dtable.h \
	dtable.c \
	hstack.h \
	hstack.c \
	Being.h \
	Being.cpp \
	factions.h \
	result.h \
	node.h \
	node.c \
	tick.h \
	tick.c \
	blender.h \
	dimensions.h dimensions.c \
	cfg.h cfg.c \
	menus.h menus.c \
	file.h file.c \
	screen_dims.h \
	skill.h skill.c \
	skill_set.h skill_set.c \
	skill_set_entry.h skill_set_entry.c \
	repstr.h repstr.c \
	templ.h templ.c \
	objectfreezer.h objectfreezer.c \
	kern_intvar.h kern_intvar.c \
	ztats.c \
	ztats.h \
	ztats_arms.c \
	ztats_arms.h \
	ztats_container_pane.c \
	ztats_container_pane.h \
	ztats_items.c \
	ztats_items.h \
	ztats_misc.c \
	ztats_misc.h \
	ztats_pane.c \
	ztats_pane.h \
	ztats_pm.c \
	ztats_pm.h \
	ztats_reagents.c \
	ztats_reagents.h \
	ztats_spells.c \
	ztats_spells.h \
	res.rc

nazghul_OBJS =		\
	angband.o \
        Arms.o \
        ascii.o \
        astar.o \
        character.o \
        cmdwin.o \
        combat.o \
        common.o \
        console.o \
        Container.o \
        cursor.o \
	debug.o \
        event.o \
	escape.o \
	Field.o \
        floodfill.o \
        foogod.o \
	formation.o \
        hash.o \
        heap.o \
        images.o \
        knapsack.o \
        los.o \
        map.o \
        nazghul.o \
        Party.o \
        object.o \
        occ.o \
        olist.o \
        place.o \
        play.o \
        player.o \
        Reagent.o \
        sched.o \
        screen.o \
        sky.o \
        sound.o \
        species.o \
        sprite.o \
        status.o \
        terrain.o \
        terrain_map.o \
        tree.o \
        vehicle.o \
        wind.o \
        wq.o \
	Missile.o \
	cmd.o \
	clock.o \
	vmask.o \
	ctrl.o \
	scheme.o \
	session.o \
	closure.o \
	kern.o \
	dice.o \
	gob.o \
	magic.o \
	effect.o \
	conv.o \
	ptable.o \
	mmode.o \
	log.o \
	dtable.o \
	hstack.o \
	Being.o \
	node.o \
	tick.o \
	cfg.o \
	dimensions.o \
	menus.o \
	file.o \
	skill.o \
	skill_set.o skill_set_entry.o \
	repstr.o \
	templ.o \
	objectfreezer.o \
	kern_intvar.o \
	ztats.o \
	ztats_arms.o \
	ztats_container_pane.o \
	ztats_items.o \
	ztats_misc.o \
	ztats_pane.o \
	ztats_pm.o \
	ztats_reagents.o \
	ztats_spells.o \
	res.o

INCLUDES = -I. -I.. -I/mingw/include -I/mingw/include/SDL
LIBS = -lSDL_image -lSDL_mixer -mwindows -lmingw32 -lSDLmain -lSDL.dll -llibpng -lz

.SUFFIXES: .c .cpp .o .obj .rc

nazghul: $(nazghul_SOURCES) $(nazghul_OBJS)
	g++ -o nazghul $(nazghul_OBJS) $(LIBS)

clean:
	rm -f *.o

.c.o:
	g++ $(CFLAGS) $(INCLUDES) -c $<

.cpp.o:
	g++ $(CFLAGS) $(INCLUDES) -c $<

.rc.o:
	windres -i $< -o $@
