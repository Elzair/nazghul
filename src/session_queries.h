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
 * This is where you add a new query to the kernel. session.h #includes this to
 * create the id's for its query array. kern.c uses this to map scheme strings
 * to query id's via a table. The only other thing you need to do for a query is
 * add a call to run it from the appropriate place in the kernel source.
 *
 * Note: by convention the comments following each line show the args the query
 * proc expects.
 *
 * Note: do NOT use #define guards for this header, since kern.c includes it
 * twice via session.h.
 */

SESSION_DECL_QUERY(str_based_attack_query), /* character */
SESSION_DECL_QUERY(dex_based_attack_query), /* character */
SESSION_DECL_QUERY(damage_bonus_query), /* character */
SESSION_DECL_QUERY(defense_bonus_query), /* character */
