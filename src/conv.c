//
// nazghul - an old-school RPG engine
// Copyright (C) 2002, 2003 Gordon McNutt
//
// This program is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 2 of the License, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// more details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
// Suite 330, Boston, MA 02111-1307 USA
//
// Gordon McNutt
// gmcnutt@users.sourceforge.net
//

#include "conv.h"
#include "event.h"
#include "cmdwin.h"
#include "common.h"
#include "object.h"
#include "closure.h"
#include "log.h"
#include "session.h"

#include <ctype.h>
#include <stdio.h>

#define MAX_KEYWORD_SZ 16

static int C_room, C_len;
static char C_query[64], *C_ptr;
static int conv_done;

static int get_player_query(struct KeyHandler *kh, int key, int keymod)
{
	if (key == CANCEL) {
		while (C_ptr > C_query) {
			C_ptr--;
			*C_ptr = 0;
			cmdwin_pop();
			C_room++;
		}
		return 1;
	}

	if (key == '\n') {
		return 1;
	}

	if (key == '\b') {
		if (C_ptr != C_query) {
			C_ptr--;
			*C_ptr = 0;
			C_room++;
			cmdwin_pop();
		}
		return 0;
	}

	if (isprintable(key) 
            && C_room) {
		cmdwin_push("%c", key);
		*C_ptr++ = key;
		C_room--;
	}

	return 0;
}

static void set_query(char *str)
{
	snprintf(C_query, sizeof(C_query) - 1, "%s", str);
	C_query[MAX_KEYWORD_SZ] = 0;
	C_len = strlen(C_query);
}


void conv_end()
{
        conv_done = 1;
}

void conv_enter(Object *npc, Object *pc, struct closure *conv)
{
	struct KeyHandler kh;

        assert(conv);

        session_run_hook(Session, conv_start_hook, "pp", pc, npc);

        conv_done = 0;
	kh.fx = get_player_query;
	set_query("hail");

	for (;;) {

                /* Truncate the query to 4 characters */
                C_query[4] = 0;

                /* Query the NPC */
                closure_exec(conv, "ypp", C_query, npc, pc);

		if (conv_done)
			break;

		/*** Setup for next query ***/

		memset(C_query, 0, sizeof(C_query));
		C_room = sizeof(C_query) - 1;
		C_ptr = C_query;

		cmdwin_clear();
		cmdwin_push("Say: ");

		/*** Get next query ***/

		eventPushKeyHandler(&kh);
		eventHandle();
		eventPopKeyHandler();

		C_query[MAX_KEYWORD_SZ] = 0;
		C_len = strlen(C_query);
                if (! C_len)
                        sprintf(C_query, "bye");
		log_msg("^c+%c%s:^c- %s", CONV_PC_COLOR, 
                        pc->getName(), C_query);

		/*** Check if player ended conversation ***/

		if (Quit)
			break;

		if (strlen(C_query) == 0)
			set_query("BYE");

		if (!strcasecmp(C_query, "BYE")) {
			conv_end();
		}

	}

	cmdwin_clear();
	cmdwin_repaint();

        session_run_hook(Session, conv_end_hook, "pp", pc, npc);
        
}
int isprintable(int c)
{
        /* Looks like ctype's isprint() doesn't always behave the same way. On
         * some systems it was letting c<32 go through, causing an assert in
         * ascii.c. */
        return ((c >= 32)
                && (c < 127)
                && (c != '%') /* printf special char */
                && (c != '^') /* ascii.c special char */
                );
}

