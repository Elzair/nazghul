/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2008 Gordon McNutt
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 *
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 */

#include "applet.h"
#include "cmd.h" /* for some types, may remove */
#include "cmdwin.h"
#include "cursor.h"
#include "log.h"
#include "map.h"
#include "place.h"
#include "session.h"
#include "terrain.h"
#include "terrain_editor.h"
#include "vmask.h"

struct terrain_editor_applet {
    struct applet base;
    struct place *place;
    int x, y;
};

/* Struct used by the terraform version of the movecursor function et al. It's
 * a superset of the standard movecursor_data and extends it with some extra
 * data. */
struct terraform_mode_keyhandler {
    struct place           * place;  // needed?
    struct terrain_map     * map;
    struct terrain_palette * palette;
};

typedef void (*v_fncptr_iiv_t) (struct place *, int x, int y, void * v);
typedef int (*i_fncptr_iiv_t) (struct place *, int x, int y, void * v);

static void print_terraform_help (void)
{
    log_msg("");
    log_msg(" PageUp/PageDn/ = Select terrain");
    log_msg("      Home/End    from palette");
    log_msg("    Arrow Keys  = Move cursor");
    log_msg("    SPACE/ENTER = Paint terrain");
    log_msg("              C = Copy from ground");
    log_msg("              F = Flood-Fill");
    log_msg("     1234567890 = Get from QuickKey");
    log_msg("CTRL-1234567890 = Set QuickKey");
    log_msg("            ESC = Exit Terraform mode");
    log_msg("              ? = This help text");
    log_msg("");
}

/*
 * emit_terraform_status - print the active terrain palette entry to the
 * console during terraform mode
 */
void emit_terraform_status (char * msg, struct terrain_palette * pp, 
                            struct terrain * tt)
{
    log_msg("[%s] %3d: %s '%s'", msg, pp->current_terrain_index, tt->tag, 
            tt->name);
}


/**
 * Fill a region with a terrain type (similar to image editor "fill" commands).
 */
static void cmd_terraform_fill(struct terrain *nt, struct terrain *ot, struct place *place, int x, int y)
{
    struct terrain *ct = place_get_terrain(place, x, y);

    /* base case 1: off-map */
    if (!ct)
        return;

    /* base case 2: current terrain does not match old terrain */
    if (ct != ot)
        return;

    /* recursive case - change current terrain to new terrain */
    place_set_terrain(place, x, y, nt);
    vmask_invalidate(place, x, y, 1, 1);

    /* recur on four neighbors */
    cmd_terraform_fill(nt, ot, place, x-1, y);
    cmd_terraform_fill(nt, ot, place, x+1, y);
    cmd_terraform_fill(nt, ot, place, x, y-1);
    cmd_terraform_fill(nt, ot, place, x, y+1);
}

/*
 * cmd_terraform_movecursor_and_do - key handler function for terraform mode
 */
static int cmd_terraform_movecursor_and_do(struct KeyHandler * kh, int key, int keymod)
{
    struct movecursor_data *movedat;
    struct terraform_mode_keyhandler *terdat;        
    struct terrain_palette * pp;
    struct terrain * tt;

    assert(kh);

    movedat = (struct movecursor_data *)kh->data;
    terdat = (struct terraform_mode_keyhandler *)movedat->data;
    pp     = terdat->palette;
  
    if (key == '\n' || key == SDLK_SPACE || key == SDLK_RETURN ||
        key == SDLK_LCTRL || key == SDLK_RCTRL) {
        int x = Session->crosshair->getX();
        int y = Session->crosshair->getY();
        if (movedat->each_target_func)
            movedat->each_target_func(Session->crosshair->getPlace(),
                                      x, y, terdat);
        return 0;  /* Keep on keyhandling */
    }

    if (keyIsDirection(key)) {
        int dir = keyToDirection(key);
        /* SAM: TODO: The Terraform cursor should not be allowed to go
         *            past the Viewport bounds...
         */
        Session->crosshair->move(directionToDx(dir), 
                                 directionToDy(dir));
        mapSetDirty();
        int x = Session->crosshair->getX();
        int y = Session->crosshair->getY();
        if (movedat->each_tile_func)
            movedat->each_tile_func(Session->crosshair->getPlace(),
                                    x, y, terdat);

        /* If the CTRL key is held down then also run the target
         * function to paint the tile. */
        if (keymod & KMOD_CTRL &&
            movedat->each_target_func)
            movedat->each_target_func(Session->crosshair->getPlace(),
                                      x, y, terdat);

        return 0;  /* Keep on keyhandling */
    }

    if (key == 'c') {
        /* Set the terrain beneath the cursor as the current "pen" */
        int index = -1;
        tt = place_get_terrain(Session->crosshair->getPlace(),
                               Session->crosshair->getX(),
                               Session->crosshair->getY());
        index = palette_get_terrain_index(pp, tt);
        if (index >= 0) {
            palette_set_current_terrain(pp, index);
            emit_terraform_status("Copy", pp, tt);
        }
        return 0;
    }

    if (key == 'f') {
        /* "Fill" using the 4-neighbors algorithm */
        tt = palette_current_terrain(pp);
        if (tt) {
            struct terrain *ot;
            ot = place_get_terrain(Session->crosshair->getPlace(),
                                   Session->crosshair->getX(),
                                   Session->crosshair->getY());
            if (tt != ot) {
                cmd_terraform_fill(tt, ot,
                                   Session->crosshair->getPlace(),
                                   Session->crosshair->getX(),
                                   Session->crosshair->getY());
                emit_terraform_status("Flood-Fill", pp, tt);
            }
            mapUpdate(0);
        }
        return 0;
    }

    if (key == '?') {
        print_terraform_help();
        return 0;
    }

    if (key == SDLK_PAGEUP) {
        // Page Up == Cycle back through terrain in palette
        palette_prev_terrain(pp);
        tt = palette_current_terrain(pp);
        emit_terraform_status("Prev", pp, tt);
        return 0;  /* Keep on keyhandling */
    }
    if (key == SDLK_PAGEDOWN) {
        // Page Down == Cycle forward through terrain in palette
        palette_next_terrain(pp);
        tt = palette_current_terrain(pp);
        emit_terraform_status("Next", pp, tt);
        return 0;  /* Keep on keyhandling */
    }
    if (key == SDLK_HOME) {
        // Home == Select first terrain in palette
        palette_first_terrain(pp);
        tt = palette_current_terrain(pp);
        emit_terraform_status("Frst", pp, tt);
        return 0;  /* Keep on keyhandling */
    }
    if (key == SDLK_END) {
        // End == Select last terrain in palette
        palette_last_terrain(pp);
        tt = palette_current_terrain(pp);
        emit_terraform_status("Last", pp, tt);
        return 0;  /* Keep on keyhandling */
    }

    if (key >= '0' && key <= '9') {
        // Number key 0..9 == get/set quick terrain
        int qt = key - '0';
    
        if ((keymod && KMOD_LCTRL) || (keymod && KMOD_RCTRL)) {
            // Control-NUM == set quick terrain to current:
            int index = palette_get_current_terrain_index(pp);
            palette_set_quick_terrain(pp, qt, index);
            tt = palette_current_terrain(pp);
            log_msg("[Set Quick %d] %3d: %s '%s'", qt, 
                    pp->current_terrain_index, tt->tag, tt->name);
            return 0; /* Keep on keyhandling */
        }
        // Plain NUM == set current terrain from quick terrain:
        int index = palette_get_quick_terrain_index(pp, qt);
        palette_set_current_terrain(pp, index);
        tt = palette_current_terrain(pp);
        log_msg("[Quick %d] %3d: %s '%s'", qt, 
                pp->current_terrain_index, tt->tag, tt->name);
        return 0;  /* Keep on keyhandling */
    }

    if (key == SDLK_ESCAPE) {
        movedat->abort = 1;
        return 1;  // Done (abort)
    }
    return 0;  /* Keep on keyhandling */
}


/**
 * cmd_terraform_cursor_func - 
 */
static int cmd_terraform_cursor_func(int ox, int oy, int *x, int *y,
                                     int range,
                                     v_fncptr_iiv_t each_tile_func,
                                     i_fncptr_iiv_t each_target_func,
                                     struct place * place)
{
        struct movecursor_data movedat;
        struct terraform_mode_keyhandler terdat;
        struct KeyHandler kh;
        struct MouseButtonHandler mbh;
        struct MouseMotionHandler mmh;

        /* Position the cursor */
        Session->crosshair->setRange(range);
        Session->crosshair->setViewportBounded(1);
        Session->crosshair->setOrigin(ox, oy);
        Session->crosshair->relocate(Place, *x, *y);
        mapSetDirty();
  
        /* Setup the key handler */
        terdat.map               = place->terrain_map;
        terdat.palette           = place->terrain_map->palette;
        movedat.each_tile_func   = each_tile_func;
        movedat.each_target_func = each_target_func;
        movedat.abort            = 0;
        movedat.jump             = 1;
        movedat.data             = &terdat;

        kh.fx   = cmd_terraform_movecursor_and_do;
        kh.data = &movedat;

        mbh.fx = mouse_button_cursor;
        mbh.data = &movedat;

        mmh.fx = mouse_motion_cursor;
        mmh.data = &movedat;

        /* Start interactive mode */
        eventPushMouseButtonHandler(&mbh);
        eventPushKeyHandler(&kh);
        cmdwin_spush("<target> (ESC to exit)");
        eventHandle();

        /* Done -  cleanup */
        cmdwin_pop();
        eventPopKeyHandler();
        eventPopMouseButtonHandler();
  
        *x = Session->crosshair->getX();
        *y = Session->crosshair->getY();
        Session->crosshair->remove();
        mapSetDirty();
  
        cmdwin_spush("Done.");
        log_msg("---Terraform Done---");

        return 0;
}

/**
 * cmd_dm_xray_look_at_xy - like look_at_XY() but unconditionally reports what
 * is there.
 */
static void cmd_dm_xray_look_at_xy(struct place *place, int x, int y, 
                                   void * data)
{
        if (!mapTileIsVisible(x, y) ) {
                log_begin("(Out of LOS) At XY=(%d,%d) you see ", x, y);
                place_describe(place, x, y, PLACE_DESCRIBE_ALL);
                log_end(NULL);
                return;
        }
        log_begin("At XY=(%d,%d) you see ", x, y);
        place_describe(place, x, y, PLACE_DESCRIBE_ALL);
        log_end(NULL);
}

/**
 * cmd_terraform_xy  - terraform this tile
 */
static int cmd_terraform_xy(struct place *place, int x, int y, void * data)
{
        struct terraform_mode_keyhandler * kh = 
                (struct terraform_mode_keyhandler *) data;
        struct terrain_map     * map = kh->map;
        struct terrain_palette * pp  = kh->palette;
        struct terrain         * tt  = palette_current_terrain(pp);

        terrain_map_fill(map, x, y, 1, 1, tt);
        vmask_invalidate(place, x, y, 1, 1);
        mapSetDirty();
        mapUpdate(0);
        return 0; /* keep on targeting */
}

/**
 * Start and run the terrain editor until player quits back to game.
 */
static void terrain_editor_applet_ops_run(struct applet *applet, SDL_Rect *dims, struct session *session)
{
    applet->dims = *dims;
    applet->session = session;

    cmdwin_clear();
    cmdwin_spush("Terraform");

    struct terrain_editor_applet *tea = (struct terrain_editor_applet*)applet;
    struct terrain_palette *palette = tea->place->terrain_map->palette;
    struct terrain *terrain = palette_current_terrain(palette);
    
    log_begin_group();
    log_msg("---Terraform---");
    log_msg("Place %s",     tea->place->tag  );
    log_msg("      \"%s\"", tea->place->name );
    log_msg("Map   %s",     tea->place->terrain_map->tag    );
    log_msg("Palette %s",   palette->tag);
    log_msg("");
    
    print_terraform_help();
    log_msg("Press '?' for Terraform command help.");
    
    emit_terraform_status("Trrn", palette, terrain);
    
    cmd_dm_xray_look_at_xy(tea->place, tea->x,tea->y, NULL);
    cmd_terraform_cursor_func(tea->x, tea->y, &tea->x, &tea->y, 99,
                              cmd_dm_xray_look_at_xy, 
                              cmd_terraform_xy,
                              tea->place);
    
    log_end_group();
    
}

static void terrain_editor_applet_ops_paint(struct applet *applet)
{

}

struct applet_ops terrain_editor_applet_ops = {
    terrain_editor_applet_ops_run,
    terrain_editor_applet_ops_paint,
    NULL, /* get_desired_height */
};

static struct terrain_editor_applet *terrain_editor_applet_new(struct place *place, int x, int y)
{
    struct terrain_editor_applet *tea = (struct terrain_editor_applet *)calloc(1, sizeof(*tea));
    if (tea) {
        tea->base.ops = &terrain_editor_applet_ops;
        tea->place = place;
        tea->x = x;
        tea->y = y;
    }
    return tea;
}

static void terrain_editor_applet_del(struct terrain_editor_applet *tea)
{
    free(tea);
}

void terrain_editor_run(struct place *place, int x, int y)
{
    struct terrain_editor_applet *tea = terrain_editor_applet_new(place, x, y);
    if (tea) {
        statusRunApplet(&tea->base);
        statusSetMode(ShowParty);
        terrain_editor_applet_del(tea);
    }
}
