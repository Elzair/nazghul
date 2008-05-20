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

/**
 * This is where you add a new hook to the kernel. session.h #includes this to
 * create the id's for its hook array. kern.c uses this to map scheme strings
 * to hook id's via a table. The only other thing you need to do for a hook is
 * add a call to run it from the appropriate place in the kernel source.
 *
 * Note: by convention the comments following each line show the args the hook
 * proc expects.
 *
 * Note: do NOT use #define guards for this header, since kern.c includes it
 * twice via session.h.
 */

SESSION_DECL_HOOK(session_start_hook), /* player party */
SESSION_DECL_HOOK(new_game_start_hook), /* player party */
SESSION_DECL_HOOK(camping_turn_start_hook), /* player party */
SESSION_DECL_HOOK(conv_start_hook), /* pc, npc */
SESSION_DECL_HOOK(music_change_hook), /* player party */
SESSION_DECL_HOOK(combat_change_hook), /* player party */
SESSION_DECL_HOOK(str_based_attack_query), /* character */
SESSION_DECL_HOOK(dex_based_attack_query), /* character */
SESSION_DECL_HOOK(damage_bonus_query), /* character */
SESSION_DECL_HOOK(defense_bonus_query), /* character */
