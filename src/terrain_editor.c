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
    struct list cmds; /* list of commands (for undo/redo) */
    struct place *place; /* place being edited */
    struct terrain *terrain; /* currently selected terrain to paint with */
    struct list *lastcmd; /* last command executed */
    struct terrain_map *origmap; /* clone of original map (for undo/redo) */
    struct terrain *quick[10]; /* quick terrain selection table */
    int x, y; /* cursor location in place (at least initially) */
    int max_cols, max_rows; /* dimensions of the palette window */
    int top_index; /* index of terrain in ulc of palette window */
    int max_top_index; /* value for which last entry is on the bottom of the palette window */
};

typedef enum {
    CMD_PAINT = 0,
    CMD_FILL = 1
} terrain_editor_cmd_code_t;

struct terrain_editor_cmd_paint {
    int x, y;
    struct terrain *terrain;    
};

struct terrain_editor_cmd_fill {
    int x, y;
    struct terrain *oldter, *newter;    
};

struct terrain_editor_cmd {
    struct list list;
    terrain_editor_cmd_code_t code;
    union {
        struct terrain_editor_cmd_paint paint;
        struct terrain_editor_cmd_fill fill;
    } parms;
};

static void print_terraform_help (void)
{
    /* fixme: no longer visible because the status window is enlarged; move to
     * a 'help' pane */
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
void emit_terraform_status (char * msg, struct terrain * tt)
{
    log_msg("[%s]: %s '%s'", msg, tt->tag, tt->name);
}

static void terrain_editor_cmd_del(struct terrain_editor_cmd *cmd)
{
    free(cmd);
}

static void terrain_editor_cmd_fill_exec(struct terrain *nt, struct terrain *ot, struct place *place, int x, int y)
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
    terrain_editor_cmd_fill_exec(nt, ot, place, x-1, y);
    terrain_editor_cmd_fill_exec(nt, ot, place, x+1, y);
    terrain_editor_cmd_fill_exec(nt, ot, place, x, y-1);
    terrain_editor_cmd_fill_exec(nt, ot, place, x, y+1);
}

static void terrain_editor_cmd_exec(struct terrain_editor_applet *tea, struct terrain_editor_cmd *cmd)
{
    switch (cmd->code) {
    case CMD_PAINT:
    {
        struct terrain *ter = cmd->parms.paint.terrain;
        int x = cmd->parms.paint.x;
        int y = cmd->parms.paint.y;

        place_set_terrain(tea->place, x, y, ter);
        vmask_invalidate(tea->place, x, y, 1, 1);
    }
    break;
    case CMD_FILL:
    {
        struct terrain *oldter = cmd->parms.fill.oldter;
        struct terrain *newter = cmd->parms.fill.newter;
        int x = cmd->parms.fill.x;
        int y = cmd->parms.fill.y;

        terrain_editor_cmd_fill_exec(newter, oldter, tea->place, x, y);
    }
    break;
    default:
        assert(0);
        break;
    }
}

static struct terrain_editor_cmd *terrain_editor_cmd_paint_new(int x, int y, struct terrain *terrain)
{
    struct terrain_editor_cmd *cmd = (struct terrain_editor_cmd *)calloc(1, sizeof(*cmd));
    if (!cmd) {
        return NULL;
    }
    cmd->code = CMD_PAINT;
    cmd->parms.paint.x = x;
    cmd->parms.paint.y = y;
    cmd->parms.paint.terrain = terrain;
    return cmd;
}

static struct terrain_editor_cmd *terrain_editor_cmd_fill_new(int x, int y, struct terrain *oldter, struct terrain *newter)
{
    struct terrain_editor_cmd *cmd = (struct terrain_editor_cmd *)calloc(1, sizeof(*cmd));
    if (!cmd) {
        return NULL;
    }
    cmd->code = CMD_FILL;
    cmd->parms.fill.x = x;
    cmd->parms.fill.y = y;
    cmd->parms.fill.oldter = oldter;
    cmd->parms.fill.newter = newter;
    return cmd;
}

static void terrain_editor_del_cmd_list(struct terrain_editor_applet *tea, struct list *elem)
{
    while (elem != &tea->cmds) {
        struct terrain_editor_cmd *cmd = list_entry(elem, struct terrain_editor_cmd, list);
        elem = elem->next;
        list_remove(&cmd->list);
        terrain_editor_cmd_del(cmd);
    }

}

static void terrain_editor_submit_cmd(struct terrain_editor_applet *tea, struct terrain_editor_cmd *cmd)
{
    /* if there are commands in the 'redo' part of the command list then delete
     * them */
    terrain_editor_del_cmd_list(tea, tea->lastcmd->next);

    /* enqueue command */
    list_add_tail(&tea->cmds, &cmd->list);

    /* advance last command pointer */
    tea->lastcmd = &cmd->list;

    /* exec command */
    terrain_editor_cmd_exec(tea, cmd);
    mapSetDirty();
    mapUpdate(0);
}

static void terrain_editor_undo(struct terrain_editor_applet *tea)
{
    /* if no commands queued then done */
    if (tea->lastcmd == &tea->cmds) {
        return;
    }

    /* delete current map */
    terrain_map_unref(tea->place->terrain_map);

    /* restore original map */
    tea->place->terrain_map = terrain_map_clone(tea->origmap, tea->origmap->tag);
    assert(tea->place->terrain_map);

    vmask_flush_all();

    /* replay commands up to last */
    struct list *elem = tea->cmds.next;
    while (elem != tea->lastcmd) {
        struct terrain_editor_cmd *cmd = list_entry(elem, struct terrain_editor_cmd, list);
        elem = elem->next;
        terrain_editor_cmd_exec(tea, cmd);
    }

    /* back up last command pointer */
    tea->lastcmd = tea->lastcmd->prev;

    mapSetDirty();        
    mapUpdate(0);

}

static void terrain_editor_redo(struct terrain_editor_applet *tea)
{
    /* if no more commands in list then done */
    if (tea->lastcmd->next == &tea->cmds) {
        return;
    }

    /* advance last command pointer */
    tea->lastcmd = tea->lastcmd->next;

    /* replay last command */
    struct terrain_editor_cmd *cmd = list_entry(tea->lastcmd, struct terrain_editor_cmd, list);
    terrain_editor_cmd_exec(tea, cmd);

    mapSetDirty();        
    mapUpdate(0);

}

/**
 * Submit a command to set a tile to the current terrain.
 */
static void terrain_editor_req_paint(struct terrain_editor_applet *tea, int x, int y)
{
    if (tea->terrain != place_get_terrain(tea->place, x, y)) {
        struct terrain_editor_cmd *cmd = terrain_editor_cmd_paint_new(x, y, tea->terrain);
        terrain_editor_submit_cmd(tea, cmd);
    }
}

/**
 * Submit a command to fill a region with the current terrain.
 */
static void terrain_editor_req_fill(struct terrain_editor_applet *tea, int x, int y)
{
    struct terrain *oldter = place_get_terrain(tea->place, x, y);
    if (tea->terrain != oldter) {
        struct terrain_editor_cmd *cmd = terrain_editor_cmd_fill_new(x, y, oldter, tea->terrain);
        terrain_editor_submit_cmd(tea, cmd);
    }
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
    DECL_CAST(struct terrain_editor_applet, tea, kh->data);
    struct session *session = tea->base.session;

    printf("key=%x keymod=%x\n", key, keymod);

    if (key == '\n' || key == SDLK_SPACE || key == SDLK_RETURN) {
        int x = session->crosshair->getX();
        int y = session->crosshair->getY();
        terrain_editor_req_paint(tea, x, y);
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
            terrain_editor_req_paint(tea, x, y);
        }
        return 0;  /* Keep on keyhandling */
    }

    if (key >= '0' && key <= '9') {
        // Number key 0..9 == get/set quick terrain
        int qt = key - '0';
    
        if ((keymod && KMOD_LCTRL) || (keymod && KMOD_RCTRL)) {
            // Control-NUM == set quick terrain to current:
            tea->quick[qt] = tea->terrain;
            log_msg("[Set Quick %d]: %s '%s'", qt, tea->terrain->tag, tea->terrain->name);
            return 0;
        } else {
            // Plain NUM == set current terrain from quick terrain:
            if (!tea->quick[qt]) {
                log_msg("[Quick %d]: empty!");
            } else {
                tea->terrain = tea->quick[qt];
                log_msg("[Quick %d]: %s '%s'", qt, tea->terrain->tag, tea->terrain->name);
            }
        }
        return 0;
    }

    switch (key) {
    case 'c':
        /* Set the terrain beneath the cursor as the current "pen" */
        tea->terrain = place_get_terrain(session->crosshair->getPlace(),
                                         session->crosshair->getX(),
                                         session->crosshair->getY());
        emit_terraform_status("Copy", tea->terrain);
        return 0;

    case 'f':
        terrain_editor_req_fill(tea, session->crosshair->getX(), session->crosshair->getY());
        emit_terraform_status("Flood-Fill", tea->terrain);
        return 0;

    case KEY_CTRL_R:
        terrain_editor_redo(tea);
        return 0;        

    case KEY_CTRL_Z:
        terrain_editor_undo(tea);
        return 0;

    case '?':
        print_terraform_help();
        return 0;

    case SDLK_PAGEUP:
        if (tea->top_index >= tea->max_cols) {
            tea->top_index -= tea->max_cols;
            statusRepaint();
        }
        return 0;

    case SDLK_PAGEDOWN:
        if (tea->top_index < tea->max_top_index) {
            tea->top_index += tea->max_cols;
            statusRepaint();
        }
        return 0;

    case SDLK_HOME:
        tea->top_index = 0;
        statusRepaint();
        return 0;

    case SDLK_END:
        tea->top_index = tea->max_top_index;
        statusRepaint();
        return 0;

    case SDLK_ESCAPE:
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
#if 1
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
    terrain_editor_req_paint(tea, mx, my);

    return false;
}

/**
 * terrain_editor_next - return the next terrain in the session terrain list or
 * NULL if this is the last.
 */
static terrain *terrain_editor_next(struct terrain_editor_applet *tea, struct terrain *ter)
{
    struct list *head = &tea->base.session->terrains;
    struct list *elem = ter->session_list.next;
    if (elem == head) {
        return NULL;
    }
    return list_entry(elem, struct terrain, session_list);
}

/**
 * terrain_editor_lookup - find the terrain at the given index in the session
 * terrain list and return it, or NULL if the index is beyond the end of the
 * list.
 */
static terrain *terrain_editor_lookup(struct terrain_editor_applet *tea, int index)
{
    struct list *head = &tea->base.session->terrains;
    struct list *elem = head->next;
    while (index && (elem != head)) {
        index--;
        elem = elem->next;
    }
    if (index) {
        return NULL;
    }

    return list_entry(elem, struct terrain, session_list);
}

/**
 * Handle mouse clicks on the palette viewer. (sx, sy) are the screen
 * coordinates clicked.
 */
static bool terrain_editor_palette_click(struct terrain_editor_applet *tea, int sx, int sy)
{
    /* Convert screen coordinates to row and column */
    int row = (sy - tea->base.dims.y) / TILE_H;
    int col = (sx - tea->base.dims.x) / TILE_W;

    /* Convert row and column to palette index */
    int index = tea->top_index + col + (row * tea->max_cols);
    
    /* Lookup */
    struct terrain *newter = terrain_editor_lookup(tea, index);
    if (newter) {
        tea->terrain = newter;
        emit_terraform_status("Set ", tea->terrain);
    }

    return false;
}

/*
 * terrain_editor_mouse_button_handler - mouse button handler function for
 * terraform mode
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

/*
 * terrain_editor_mouse_motion_handler - mouse motion handler function for
 * terraform mode
 */
static bool terrain_editor_mouse_motion_handler(struct MouseMotionHandler *mh, SDL_MouseMotionEvent *event)
{
    DECL_CAST(struct terrain_editor_applet, tea, mh->data);
    int mx = event->x;
    int my = event->y;
    int dragging = event->state & SDL_BUTTON(1);

    
    /* Clicked on the map? */
    if (dragging && ! mapScreenToPlaceCoords(&mx, &my)) {
        return terrain_editor_map_click(tea, mx, my);
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
    assert(! list_empty(&session->terrains));
    tea->terrain = list_entry(session->terrains.next, struct terrain, session_list);
    tea->max_top_index = list_len(&session->terrains) - (tea->max_cols * tea->max_rows);
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
    log_begin_group();
    log_msg("---Terraform---");
    log_msg("Place %s",     tea->place->tag  );
    log_msg("      \"%s\"", tea->place->name );
    log_msg("Map   %s",     tea->place->terrain_map->tag    );
    log_msg("");
    
    print_terraform_help();
    log_msg("Press '?' for Terraform command help.");
    emit_terraform_status("Set ", tea->terrain);
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

    /* Setup the mouse click handler */
    struct MouseButtonHandler mbh;
    mbh.fx = terrain_editor_mouse_button_handler;
    mbh.data = tea;
    eventPushMouseButtonHandler(&mbh);

    /* Setup the mouse movement handler */
    struct MouseMotionHandler mmh;
    mmh.fx = terrain_editor_mouse_motion_handler;
    mmh.data = tea;
    eventPushMouseMotionHandler(&mmh);

    /* Enter interactive mode */
    eventHandle();

    /* Done -  cleanup */
    cmdwin_pop();
    eventPopKeyHandler();
    eventPopMouseButtonHandler();
    eventPopMouseMotionHandler();
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
    struct terrain *ter = terrain_editor_lookup(tea, tea->top_index);
    for (int row = 0; ter && (row < tea->max_rows); row++) {
        int x = applet->dims.x;
        for (int col = 0; ter && (col < tea->max_cols); col++) {
            sprite_paint(ter->sprite, 0, x, y);
            ter = terrain_editor_next(tea, ter);
            x += TILE_W;
        }
        y += TILE_H;
    }
    screenUpdate(&applet->dims);
    status_repaint_title();    
}

struct applet_ops terrain_editor_applet_ops = {
    terrain_editor_applet_ops_run,
    terrain_editor_applet_ops_paint,
    NULL, /* get_desired_height */
};

static void terrain_editor_applet_del(struct terrain_editor_applet *tea)
{
    /* delete the command list */
    terrain_editor_del_cmd_list(tea, tea->cmds.next);

    /* unref the clone of the original map */
    if (tea->origmap) {
        terrain_map_unref(tea->origmap);
    }

    free(tea);
}

static struct terrain_editor_applet *terrain_editor_applet_new(struct place *place, int x, int y)
{
    struct terrain_editor_applet *tea = (struct terrain_editor_applet *)calloc(1, sizeof(*tea));
    if (!tea) {
        return NULL;
    }

    list_init(&tea->cmds);
    tea->lastcmd = &tea->cmds;
    tea->base.ops = &terrain_editor_applet_ops;
    tea->place = place;
    tea->x = x;
    tea->y = y;

    if (!(tea->origmap = terrain_map_clone(tea->place->terrain_map, tea->place->terrain_map->tag))) {
        terrain_editor_applet_del(tea);
        return NULL;
    }

    terrain_map_ref(tea->origmap);


    return tea;
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
