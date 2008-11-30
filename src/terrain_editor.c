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
#include "sprite.h"
#include "terrain.h"
#include "terrain_editor.h"
#include "vmask.h"

struct terrain_editor_applet {
    struct applet base;
    struct place *place; /* place being edited */
    int x, y; /* cursor location in place (used?) */
    int max_cols; /* of the palette window */
    int max_rows; /* of the palette window */
    int top_index; /* index of terrain in ulc of status window */
    int max_top_index; /* where last entry is on the bottom of the palette window */
};

static void print_terraform_help (void)
{
    log_msg("");
    log_msg(" PageUp/PageDn/ = Scroll palette up/down");
    log_msg("      Home/End  = Scroll to begin/end");
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

/**
 * Terraform a tile.
 */
static void terrain_editor_change_xy(struct terrain_editor_applet *tea, int x, int y)
{
    struct terrain_map     * map = tea->place->terrain_map;
    struct terrain_palette * pp  = map->palette;
    struct terrain         * tt  = palette_current_terrain(pp);

    terrain_map_fill(map, x, y, 1, 1, tt); /* fixme: use place_set_terrain instead? */
    vmask_invalidate(tea->place, x, y, 1, 1);
    mapSetDirty();
    mapUpdate(0);
}

/**
 * Describe a tile in detail, DM mode.
 */
static void terrain_editor_look_at_xy(struct terrain_editor_applet *tea, int x, int y)
{
    if (!mapTileIsVisible(x, y) ) {
        log_begin("(Out of LOS) At XY=(%d,%d) you see ", x, y);
        place_describe(tea->place, x, y, PLACE_DESCRIBE_ALL);
        log_end(NULL);
        return;
    }
    log_begin("At XY=(%d,%d) you see ", x, y);
    place_describe(tea->place, x, y, PLACE_DESCRIBE_ALL);
    log_end(NULL);
}

/*
 * terrain_editor_key_handler - key handler function for terraform mode
 */
static int terrain_editor_key_handler(struct KeyHandler * kh, int key, int keymod)
{
    struct terrain_palette * pp;
    struct terrain * tt;
    struct session *session;

    assert(kh);

    DECL_CAST(struct terrain_editor_applet, tea, kh->data);

    pp = tea->place->terrain_map->palette;
    session = tea->base.session;

    if (key == '\n' || key == SDLK_SPACE || key == SDLK_RETURN || key == SDLK_LCTRL || key == SDLK_RCTRL) {
        int x = session->crosshair->getX();
        int y = session->crosshair->getY();
        terrain_editor_change_xy(tea, x, y);
        return 0;  /* Keep on keyhandling */
    }

    if (keyIsDirection(key)) {
        int dir = keyToDirection(key);
        /* SAM: TODO: The Terraform cursor should not be allowed to go
         *            past the Viewport bounds...
         */
        /* gjm: why not? */
        session->crosshair->move(directionToDx(dir), directionToDy(dir));
        mapSetDirty();
        int x = session->crosshair->getX();
        int y = session->crosshair->getY();
        terrain_editor_look_at_xy(tea, x, y);

        /* If the CTRL key is held down then also run the target function to
         * paint the tile. */
        if (keymod & KMOD_CTRL) {
            terrain_editor_change_xy(tea, x, y);
        }
        return 0;  /* Keep on keyhandling */
    }

    if (key == 'c') {
        /* Set the terrain beneath the cursor as the current "pen" */
        int index = -1;
        tt = place_get_terrain(session->crosshair->getPlace(),
                               session->crosshair->getX(),
                               session->crosshair->getY());
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
            ot = place_get_terrain(session->crosshair->getPlace(),
                                   session->crosshair->getX(),
                                   session->crosshair->getY());
            if (tt != ot) {
                cmd_terraform_fill(tt, ot,
                                   session->crosshair->getPlace(),
                                   session->crosshair->getX(),
                                   session->crosshair->getY());
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
        if (tea->top_index >= tea->max_cols) {
            tea->top_index -= tea->max_cols;
            statusRepaint();
        }
        return 0;  /* Keep on keyhandling */
    }
    if (key == SDLK_PAGEDOWN) {
        // Page Down == Cycle forward through terrain in palette
        palette_next_terrain(pp);
        tt = palette_current_terrain(pp);
        emit_terraform_status("Next", pp, tt);
        if (tea->top_index < tea->max_top_index) {
            tea->top_index += tea->max_cols;
            statusRepaint();
        }
        return 0;  /* Keep on keyhandling */
    }
    if (key == SDLK_HOME) {
        // Home == Select first terrain in palette
        palette_first_terrain(pp);
        tt = palette_current_terrain(pp);
        emit_terraform_status("Frst", pp, tt);
        tea->top_index = 0;
        statusRepaint();
        return 0;  /* Keep on keyhandling */
    }
    if (key == SDLK_END) {
        // End == Select last terrain in palette
        palette_last_terrain(pp);
        tt = palette_current_terrain(pp);
        emit_terraform_status("Last", pp, tt);
        tea->top_index = tea->max_top_index;
        statusRepaint();
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
        return 1; /* done */
    }

    return 0;  /* Keep on keyhandling */
}

/**
 * Handle mouse clicks on the map viewer. (mx, my) are the place coordinates
 * clicked.
 */
static bool terrain_editor_map_click(struct terrain_editor_applet *tea, int mx, int my)
{
#if 0
    /* gjm: let's try it without moving the crosshairs... */
    struct session *session = tea->base.session;
    class Cursor *crosshair = session->crosshair;
    int cx = crosshair->getX();
    int cy = crosshair->getY();

    /* Did the crosshair move? */
    if ((cx != mx) || (cy != my)) {

        /* Move the crosshair */
        crosshair->move(mx - cx, my - cy);
        mapSetDirty();

        /* Need to run our visitor function on each tile? */
        terrain_editor_look_at_xy(tea, mx, my);
    }
#endif
    terrain_editor_change_xy(tea, mx, my);

    return false;
}

/**
 * Handle mouse clicks on the palette viewer. (sx, sy) are the screen
 * coordinates clicked.
 */
static bool terrain_editor_palette_click(struct terrain_editor_applet *tea, int sx, int sy)
{
    struct terrain_palette *palette = tea->place->terrain_map->palette;

    /* Convert screen coordinates to row and column */
    int row = (sy - tea->base.dims.y) / TILE_H;
    int col = (sx - tea->base.dims.x) / TILE_W;

    /* Convert row and column to palette index */
    int index = tea->top_index + col + (row * tea->max_cols);
    
    /* Check */
    if (index < palette->num_entries) {
        palette_set_current_terrain(palette, index);
    }

    return false;
}

/*
 * terrain_editor_mouse_button_handler - mouse button handler function for terraform mode
 */
static bool terrain_editor_mouse_button_handler(struct MouseButtonHandler *mh, SDL_MouseButtonEvent *event)
{
    DECL_CAST(struct terrain_editor_applet, tea, mh->data);
    int mx = event->x;
    int my = event->y;
    
    /* Clicked on the map? */
    if (! mapScreenToPlaceCoords(&mx, &my)) {
        return terrain_editor_map_click(tea, mx, my);
    }
    
    /* Clicked on the palette window? */
    if (point_in_rect(event->x, event->y, &tea->base.dims)) {
        terrain_editor_palette_click(tea, event->x, event->y);
    }
    
    return false;
}


/**
 * Start and run the terrain editor until player quits back to game.
 */
static void terrain_editor_applet_ops_run(struct applet *applet, SDL_Rect *dims, struct session *session)
{
    /* Initialize the base applet */
    applet->dims = *dims;
    applet->session = session;

    /* Initialize the custom applet fields */
    DECL_CAST(struct terrain_editor_applet, tea, applet);
    tea->max_cols = dims->w / TILE_W;
    tea->max_rows = dims->h / TILE_H;
    tea->max_top_index = tea->place->terrain_map->palette->num_entries - (tea->max_cols * tea->max_rows);
    if (tea->max_top_index < 0) {
        tea->max_top_index = 0;
    }

    /* Initialize the status window */
    status_set_title("Terrain Editor");
    statusRepaint();

    /* Initialize the cmdwin window */
    cmdwin_clear();
    cmdwin_spush("Terraform");
    cmdwin_spush("<target> (ESC to exit)");

    /* Initialize the console */
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
    terrain_editor_look_at_xy(tea, tea->x, tea->y);

    /* Position the cursor */
    session->crosshair->setRange(99);
    session->crosshair->setViewportBounded(1);
    session->crosshair->setOrigin(tea->x, tea->y);
    session->crosshair->relocate(tea->place, tea->x, tea->y);
    mapSetDirty();
  
    /* Setup the key handler */
    struct KeyHandler kh;
    kh.fx = terrain_editor_key_handler;
    kh.data = tea;
    eventPushKeyHandler(&kh);

    /* Setup the mouse handler */
    struct MouseButtonHandler mbh;
    mbh.fx = terrain_editor_mouse_button_handler;
    mbh.data = tea;
    eventPushMouseButtonHandler(&mbh);

    /* Enter interactive mode */
    eventHandle();

    /* Done -  cleanup */
    cmdwin_pop();
    eventPopKeyHandler();
    eventPopMouseButtonHandler();
    session->crosshair->remove();
    mapSetDirty();
  
    cmdwin_spush("Done.");
    log_msg("---Terraform Done---");

    
    log_end_group();
    
}

/**
 * Called by statusRepaint(), this repaints the status window only, the map and
 * console continue to be updated in their usual way.
 */
static void terrain_editor_applet_ops_paint(struct applet *applet)
{
    DECL_CAST(struct terrain_editor_applet, tea, applet);

    int y = applet->dims.y;
    struct terrain_palette_entry *tpe = palette_entry(tea->place->terrain_map->palette, tea->top_index);
    if (tpe) {
        for (int row = 0; tpe && (row < tea->max_rows); row++) {
            int x = applet->dims.x;
            for (int col = 0; tpe && (col < tea->max_cols); col++) {
                sprite_paint(tpe->terrain->sprite, 0, x, y);
                tpe = palette_entry_next(tea->place->terrain_map->palette, tpe);
                x += TILE_W;
            }
            y += TILE_H;
        }
    }
    screenUpdate(&applet->dims);
    status_repaint_title();    
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
