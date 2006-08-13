AM_CXXFLAGS = -Wall -DDATADIR=\"@datadir@\" -DPREFIX=\"@prefix@\"
AM_CFLAGS = ${AM_CXXFLAGS}

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
	menus.h menus.c

INCLUDES = -I$(includedir)                                      \
        -I$(srcdir) -I$(top_srcdir)/include -I$(top_srcdir)     \
        -I$(top_builddir) -I$(top_builddir)/include -I.

LIBS = @LIBS@

MAINTAINERCLEANFILES = Makefile.in

