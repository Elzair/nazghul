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
#include "cmdwin.h"
#include "cursor.h"
#include "event.h"
#include "foogod.h"
#include "log.h"
#include "map.h"
#include "place.h"
#include "session.h"
#include "sprite.h"
#include "terrain.h"
#include "terrain_editor.h"
#include "vmask.h"

/**
 * The generic status pane.
 */
struct terrain_editor_pane {
    struct list list;
    struct terrain_editor_applet *app;
    void (*paint)(struct terrain_editor_pane *pane);
    int (*key_handler)(struct terrain_editor_pane *pane, int key, int keymod);
    bool (*mouse_click_handler)(struct terrain_editor_pane *pane, int sx, int sy);
};

/**
 * The terrain palette status pane.
 */
struct terrain_editor_palette {
    struct terrain_editor_pane base;
    int top_index; /* index of terrain in ulc of palette window */
    int max_top_index; /* value for which last entry is on the bottom of the palette window */
    int max_cols, max_rows; /* dimensions of the palette window */
};

/**
 * Status window applet structure for the terrain editor. The terrain editor
 * keeps a list of panes which appear in the status window. For example, the
 * terrain palette is one, the help screen another, and there may be one in the
 * future for selecting different editing tools, etc.
 */
struct terrain_editor_applet {
    struct applet base;
    struct list panes; /* list of status panes */
    struct list cmds; /* list of commands (for undo/redo) */
    struct terrain_editor_palette *pal; /* keep a pointer for ops_run init */
    struct terrain_editor_pane *pane; /* current pane */
    struct place *place; /* place being edited */
    struct terrain *terrain; /* currently selected terrain to paint with */
    struct list *lastcmd; /* last command executed */
    struct terrain_map *origmap; /* clone of original map (for undo/redo) */
    struct terrain *quick[10]; /* quick terrain selection table */
    int x, y; /* cursor location in place (at least initially) */
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

/*
 * emit_terraform_status - print the active terrain palette entry to the
 * console during terraform mode
 */
void emit_terraform_status (const char * msg, struct terrain * tt)
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

static int terrain_editor_palette_key_handler(struct terrain_editor_pane *pane, int key, int keymod)
{
    DECL_CAST(struct terrain_editor_palette, pal, pane);

    switch (key) {
    case SDLK_PAGEUP:
        if (pal->top_index >= pal->max_cols) {
            pal->top_index -= pal->max_cols;
            statusRepaint();
        }
        return 0;

    case SDLK_PAGEDOWN:
        if (pal->top_index < pal->max_top_index) {
            pal->top_index += pal->max_cols;
            statusRepaint();
        }
        return 0;

    case SDLK_HOME:
        pal->top_index = 0;
        statusRepaint();
        return 0;

    case SDLK_END:
        pal->top_index = pal->max_top_index;
        statusRepaint();
        return 0;
    }

    return 0;
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

    case KEY_SHIFT_EAST:
    {
        struct list *list = tea->pane->list.next;
        if (list == &tea->panes) {
            list = list->next;
        }
        assert(list != &tea->panes);
        tea->pane = outcast(list, struct terrain_editor_pane, list);
        statusRepaint();
    }
    break;

    case KEY_SHIFT_WEST:
    {
        struct list *list = tea->pane->list.prev;
        if (list == &tea->panes) {
            list = list->prev;
        }
        assert(list != &tea->panes);
        tea->pane = outcast(list, struct terrain_editor_pane, list);
        statusRepaint();
    }
    break;

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

    case SDLK_ESCAPE:
        return 1; /* done */
    }

    /* pass it to the pane handler */
    if (tea->pane->key_handler) {
        return tea->pane->key_handler(tea->pane, key, keymod);
    }

    return 0;
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
static bool terrain_editor_palette_mouse_click_handler(struct terrain_editor_pane *pane, int pane_x, int pane_y)
{
    DECL_CAST(struct terrain_editor_palette, pal, pane);

    /* Convert pane pixel coordinates to row and column */
    int row = pane_y / TILE_H;
    int col = pane_x / TILE_W;

    /* Convert row and column to palette index */
    int index = pal->top_index + col + (row * pal->max_cols);
    
    /* Lookup */
    struct terrain *newter = terrain_editor_lookup(pane->app, index);
    if (newter) {
        pane->app->terrain = newter;
        emit_terraform_status("Set ", pane->app->terrain);
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
        tea->pane->mouse_click_handler(tea->pane, event->x - tea->base.dims.x, event->y - tea->base.dims.y);
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
    tea->pal->max_cols = dims->w / TILE_W;
    tea->pal->max_rows = dims->h / TILE_H;
    assert(! list_empty(&session->terrains));
    tea->terrain = list_entry(session->terrains.next, struct terrain, session_list);
    tea->pal->max_top_index = list_len(&session->terrains) - (tea->pal->max_cols * tea->pal->max_rows);
    if (tea->pal->max_top_index < 0) {
        tea->pal->max_top_index = 0;
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
    
    emit_terraform_status("Set ", tea->terrain);
    terrain_editor_look_at_xy(tea, tea->x, tea->y);

    /* Position the cursor */
    session->crosshair->setRange(99);
    session->crosshair->setViewportBounded(1);
    session->crosshair->setOrigin(tea->x, tea->y);
    session->crosshair->relocate(tea->place, tea->x, tea->y);
    mapSetDirty();

    /* Initialize the foogod hints */
    foogodSetHintText("SHIFT+\200\201=scroll ESC=exit");
    foogodSetMode(FOOGOD_HINT);        
  
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
    foogodSetMode(FOOGOD_DEFAULT);
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
 * Paint the help screen
 */
static void terrain_editor_help_paint(struct terrain_editor_pane *pane)
{
    static const char *text[] = {
        "  PageUp/PageDn/ = Scroll palette up/down",
        "       Home/End  = Scroll to begin/end",
        "     Arrow Keys  = Move cursor",
        "     SPACE/ENTER = Paint terrain",
        " CTRL-Arrow Keys = Paint continuously",
        "               C = Copy from ground",
        "               F = Flood-fill",
        "SHIFT-Arrow Keys = Flip/scroll status",
        "      1234567890 = Get from QuickKey",
        " CTRL-1234567890 = Set QuickKey",
        "          CTRL-Z = undo",
        "          CTRL-R = redo",
        "             ESC = Exit back to game",
        NULL
    };

    SDL_Rect rect = pane->app->base.dims;
    rect.h = ASCII_H;
    rect.y += ASCII_H;

    /* Naively assume we don't have to worry about wrapping/scrolling... since
     * we probably won't unless the list starts getting long. */

    for (int i = 0; text[i]; i++) {
        screenPrint(&rect, 0, text[i]);
        rect.y += ASCII_H;
    }

    status_set_title("Editor: Commands");
}

/**
 * Paint the terrain palette pane.
 */
static void terrain_editor_palette_paint(struct terrain_editor_pane *pane)
{
    DECL_CAST(struct terrain_editor_palette, pal, pane);
    SDL_Rect *dims = &pane->app->base.dims;
    int y = dims->y;
    struct terrain *ter = terrain_editor_lookup(pane->app, pal->top_index);
    for (int row = 0; ter && (row < pal->max_rows); row++) {
        int x = dims->x;
        for (int col = 0; ter && (col < pal->max_cols); col++) {
            sprite_paint(ter->sprite, 0, x, y);
            ter = terrain_editor_next(pane->app, ter);
            x += TILE_W;
        }
        y += TILE_H;
    }
    screenUpdate(dims);

    status_set_title("Editor: Palette");
}

/**
 * Called by statusRepaint(), this repaints the status window only, the map and
 * console continue to be updated in their usual way.
 */
static void terrain_editor_applet_ops_paint(struct applet *applet)
{
    DECL_CAST(struct terrain_editor_applet, tea, applet);
    tea->pane->paint(tea->pane);
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

    /* delete the panes */
    struct list *list = tea->panes.next;
    while (list != &tea->panes) {
        struct terrain_editor_pane *pane = outcast(list, struct terrain_editor_pane, list);
        list = list->next;
        free(pane);
    }

    /* unref the clone of the original map */
    if (tea->origmap) {
        terrain_map_unref(tea->origmap);
    }

    free(tea);
}

static struct terrain_editor_applet *terrain_editor_applet_new(struct place *place, int x, int y)
{
    struct terrain_editor_pane *help;

    /* Create the base instance */
    struct terrain_editor_applet *tea = (struct terrain_editor_applet *)calloc(1, sizeof(*tea));
    if (!tea) {
        return NULL;
    }

    list_init(&tea->panes);
    list_init(&tea->cmds);
    tea->lastcmd = &tea->cmds;
    tea->base.ops = &terrain_editor_applet_ops;
    tea->place = place;
    tea->x = x;
    tea->y = y;

    if (!(tea->origmap = terrain_map_clone(tea->place->terrain_map, tea->place->terrain_map->tag))) {
        goto abort;
    }

    terrain_map_ref(tea->origmap);

    /* Add the help pane */
    help = (struct terrain_editor_pane *)calloc(1, sizeof(*help));
    if (!help) {
        goto abort;
    }

    help->paint = terrain_editor_help_paint;
    help->key_handler = NULL;
    help->mouse_click_handler = NULL;
    help->app = tea;
    list_add_tail(&tea->panes, &help->list);

    /* Add the terrain palette pane */
    tea->pal = (struct terrain_editor_palette *)calloc(1, sizeof(*(tea->pal)));
    if (!tea->pal) {
        goto abort;
    }

    tea->pal->base.paint = terrain_editor_palette_paint;
    tea->pal->base.key_handler = terrain_editor_palette_key_handler;
    tea->pal->base.mouse_click_handler = terrain_editor_palette_mouse_click_handler;
    tea->pal->base.app = tea;
    list_add_tail(&tea->panes, &tea->pal->base.list);

    tea->pane = help;

    return tea;

 abort:
    terrain_editor_applet_del(tea);
    return NULL;
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
