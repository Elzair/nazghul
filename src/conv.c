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
#include "play.h"
#include "Loader.h"
#include "common.h"
#include "console.h"
//#include "NpcParty.h"
#include "event.h"
#include "player.h"
#include "foogod.h"
#include "cmdwin.h"
#include "Mech.h"
#include "wq.h"
#include "lexer.h"		// for parse error msgs
#include "combat.h"
#include "cmd.h"
#include "object.h"
#include "vmask.h"

#include <ctype.h>
#include <stdio.h>

#define MARKUP 4

#define MAX_KEYWORD_SZ 16

/* Reserved parameter IDs */
#define PID_AMOUNT   -1
#define PID_GOLD     -2
#define PID_FOOD     -3
#define PID_ACTIVITY -4
#define MIN_PARM_ID   PID_ACTIVITY
#define MAX_PARM_ID   63

#define MAX_FLAG_ID     0xff

// hack:
extern void *lookupTag(char *tag, int tid);

static int C_room, C_len;
static char C_query[64], *C_ptr;
static int C_parms[MAX_PARM_ID + 1];
static unsigned int C_flags[(MAX_FLAG_ID + sizeof(unsigned int) - 1) /
			    sizeof(unsigned int)];

struct response *load_response_chain(class Loader * loader);

void execute_response_chain(struct response *resp, struct conv *conv)
{
	while (resp) {
		if (resp->fx) {
			resp->fx(resp, conv);
			mapUpdate(REPAINT_IF_DIRTY);
		}
		resp = resp->next;
	}
}

static void say(struct response *resp, struct conv *conv)
{
	if (conv->speaker)
		consolePrint("%s: %s\n", conv->speaker->getName(), resp->msg);
	else
		consolePrint("%s\n", resp->msg);
}

static void attack(struct response *resp, struct conv *conv)
{
	conv->consequence = CONV_COMBAT;
	conv->done = true;
	player_party->clearAlignment(conv->speaker->getAlignment());
}

static void prompt_amount(struct response *resp, struct conv *conv)
{
	cmdwin_clear();
	cmdwin_print("Give-");
	conv->amount = select_quantity(-1);
	consolePrint("You give %d\n", conv->amount);
}

static void set_amount(struct response *resp, struct conv *conv)
{
	conv->amount = resp->amount;
}


static void give_item(struct response *resp, struct conv *conv)
{
	// Note: for now ignore the speaker's inventory. Not realistic, but
	// much simpler both here and in the load script.
	player_party->add_to_inventory(resp->item, resp->amount);
	consolePrint("You receive %d %s\n", resp->amount,
		     resp->item->getName());
}

static void take_item(struct response *resp, struct conv *conv)
{
	struct inv_entry *ie;
	ie = player_party->search_inventory(resp->item);
	if (ie && ie->count > ie->ref) {
		player_party->remove_from_inventory(ie, min(resp->amount,
							    ie->count -
							    ie->ref));
	}
}

static void buy(struct response *resp, struct conv *conv)
{
	struct KeyHandler kh;
	struct ScrollerContext sc;
	struct trade_info *trade;
	int quantity, cost, max_q;

	statusSetTradeInfo(resp->n_trades, resp->trades);
	statusSetMode(Trade);

	sc.selector = TradeItem;
	kh.fx = scroller;
	kh.data = &sc;

	for (;;) {

		// *** selection ***

		sc.selection = NULL;

		cmdwin_clear();
		cmdwin_print("Buy-<select/ESC>");
		eventPushKeyHandler(&kh);
		eventHandle();
		eventPopKeyHandler();
		cmdwin_backspace(strlen("<select/ESC>"));

		trade = (struct trade_info *) sc.selection;

		if (!trade) {
			cmdwin_print("none!");
			break;
		}

		cmdwin_print("%s-", trade->name);

		if (player_party->gold < trade->cost) {
			int dummy;
			cmdwin_print("not enough gold! <hit any key>");
			getkey(&dummy, anykey);
			continue;
		}
		// *** quantity ***

		max_q = player_party->gold / trade->cost;

		cmdwin_mark();
		quantity = select_quantity(max_q);
		cmdwin_erase_back_to_mark();

		if (quantity == 0) {
			cmdwin_print("none!");
			continue;
		}

		quantity = min(quantity, max_q);
		cmdwin_print("%d-", quantity);

		cost = quantity * trade->cost;

		// *** trade ***

		player_party->gold -= cost;
		player_party->add_to_inventory((class ObjectType *) trade->data,
					       quantity);
		cmdwin_print("ok");
		consolePrint("You buy %d %s%s for %d gold\n", quantity,
			     trade->name, quantity > 1 ? "s" : "", cost);
		foogodRepaint();
	}

	statusSetMode(ShowParty);
}

static int fill_sell_list(struct response *resp, struct trade_info *trades)
{
	struct list *elem;
	struct inv_entry *ie;
	int i, j = 0;

	list_for_each(&player_party->inventory, elem) {
		ie = outcast(elem, struct inv_entry, list);
		for (i = 0; i < resp->n_trades; i++) {
			if (resp->trades[i].data == ie->type &&
			    ie->count > ie->ref &&
			    resp->trades[i].cost / MARKUP) {
				trades[j] = resp->trades[i];
				trades[j].cost /= MARKUP;
				trades[j].quantity = ie->count - ie->ref;
				trades[j].show_quantity = 1;
				j++;
				break;
			}
		}
	}

	return j;
}

static void sell(struct response *resp, struct conv *conv)
{
	// A bit trickier than the "Buy" scenario. A merchant will only buy
	// items that it is willing to turn around and sell at a profit. When
	// it comes time to select an item to sell the user should only see the
	// list of items in player inventory which the merchant is willing to
	// buy. So here we need to build that list and feed it to the status
	// viewer.

	int n_trades = 0;
	struct trade_info *trades;
	struct KeyHandler kh;
	struct ScrollerContext sc;
	struct trade_info *trade;

	// Allocate the trade list.
	trades = new struct trade_info[resp->n_trades];
	if (!trades) {
		consolePrint("%s: I don't need anything\n",
			     conv->speaker->getName());
		return;
	}
	// Fill out the list
	n_trades = fill_sell_list(resp, trades);
	statusSetTradeInfo(n_trades, trades);
	statusSetMode(Trade);

	sc.selector = TradeItem;
	kh.fx = scroller;
	kh.data = &sc;

	for (;;) {

		struct inv_entry *ie;
		int quantity, max_q;

		sc.selection = NULL;

		cmdwin_clear();
		cmdwin_print("Sell-<select or ESC>");
		eventPushKeyHandler(&kh);
		eventHandle();
		eventPopKeyHandler();
		cmdwin_backspace(strlen("<select or ESC>"));

		trade = (struct trade_info *) sc.selection;

		if (!trade) {
			cmdwin_print("none!");
			break;
		}

		cmdwin_print("%s-", trade->name);

		ie = player_party->search_inventory((class ObjectType *) trade->
						    data);
		assert(ie);
		assert(ie->ref < ie->count);

		// quantity

		max_q = ie->count - ie->ref;

		cmdwin_mark();
		quantity = select_quantity(max_q);
		cmdwin_erase_back_to_mark();

		if (quantity == 0) {
			cmdwin_print("none!");
			continue;
		}

		quantity = min(quantity, max_q);
		cmdwin_print("%d-", quantity);

		// make the trade
		player_party->remove_from_inventory(ie, quantity);
		player_party->gold += quantity * trade->cost;
		foogodRepaint();

		cmdwin_print("ok");
		consolePrint("You sell %d %s%s for %d gold\n", quantity,
			     trade->name, quantity > 1 ? "s" : "",
			     quantity * trade->cost);

		// refresh the sell list
		n_trades = fill_sell_list(resp, trades);
		statusSetTradeInfo(n_trades, trades);
		statusUpdateTradeInfo(n_trades, trades);
	}

	statusSetMode(ShowParty);

	delete trades;
}

static bool get_buy_or_sell_key(struct KeyHandler *kh, int key, int keymod)
{
	int *val = (int *) kh->data;

	switch (key) {
	case 'b':
	case 'B':
		*val = 'b';
		return true;
	case 's':
	case 'S':
		*val = 's';
		return true;
	case CANCEL:
		*val = 'x';
		return true;
	default:
		return false;
	}
}

static void trade(struct response *resp, struct conv *conv)
{
	int key;

	statusSetTradeInfo(resp->n_trades, resp->trades);

	for (;;) {
		cmdwin_clear();
		cmdwin_print("Buy or sell-<B/S/ESC>");
		getkey(&key, get_buy_or_sell_key);

		switch (key) {
		case 'b':
			buy(resp, conv);
			break;
		case 's':
			sell(resp, conv);
			break;
		default:
			cmdwin_backspace(strlen("<B/S>"));
			cmdwin_print("none!");
			return;
		}
	}
}

static void service(struct response *resp, struct conv *conv)
{
	struct KeyHandler kh;
	struct ScrollerContext sc;
	struct trade_info *trade;
	class Character *pc = NULL;

	statusSetTradeInfo(resp->n_trades, resp->trades);
	statusSetMode(Trade);

	sc.selector = TradeItem;
	kh.fx = scroller;
	kh.data = &sc;

	for (;;) {

		// *** Select a service ***

		sc.selection = NULL;

		cmdwin_clear();
		cmdwin_print("Buy-<select/ESC>");
		eventPushKeyHandler(&kh);
		eventHandle();
		eventPopKeyHandler();
		cmdwin_backspace(strlen("<select/ESC>"));

		trade = (struct trade_info *) sc.selection;

		if (!trade) {
			cmdwin_print("none!");
			break;
		}

		cmdwin_print("%s-", trade->name);

		if (player_party->gold < trade->cost) {
			int dummy;
			cmdwin_print("not enough gold! <hit any key>");
			getkey(&dummy, anykey);
			continue;
		}
		// *** Select a target (if necessary)

		switch ((int) trade->data) {
		case SRV_HEAL:
		case SRV_CURE:
		case SRV_RESURRECT:
			pc = select_party_member();
			if (!pc) {
				continue;
			}
			cmdwin_print("%s-", pc->getName());
			break;
		default:
			break;
		}

		// *** Perform the service ***

		switch ((int) trade->data) {
		case SRV_HEAL:
                        assert(pc != NULL);
			pc->heal(pc->getMaxHp());
			break;
		case SRV_CURE:
                        assert(pc != NULL);
			pc->cure();
			break;
		case SRV_RESURRECT:
                        assert(pc != NULL);
			pc->resurrect();
			break;
		default:
			assert(0);
		}

		cmdwin_print("ok");
		consolePrint("You %s %s for %d gold\n", trade->name,
			     pc->getName(), trade->cost);

		// *** Pay the price ***

		player_party->gold -= trade->cost;
		foogodRepaint();
	}

	statusSetMode(ShowParty);
}

static void turn_away(struct response *resp, struct conv *conv)
{
	consolePrint("%s turns away\n", conv->speaker->getName());
	conv->done = true;
}

static void take(struct response *resp, struct conv *conv)
{
	int *val;
	bool pos = false;

	switch (resp->parm_id) {
	case PID_AMOUNT:
		val = &conv->amount;
		break;
	case PID_GOLD:
		val = &player_party->gold;
		pos = true;
		break;
	case PID_FOOD:
		val = &player_party->food;
		pos = true;
		break;
	default:
		val = &C_parms[resp->parm_id];
	}

	*val -= conv->amount;
	if (pos && *val < 0)
		*val = 0;
	foogodRepaint();

}

static void change_parm(struct response *resp, struct conv *conv)
{
	int *val;

	switch (resp->parm_id) {
	case PID_AMOUNT:
		val = &conv->amount;
		break;
	case PID_GOLD:
		val = &player_party->gold;
		break;
	case PID_FOOD:
		val = &player_party->food;
		break;
	default:
		val = &C_parms[resp->parm_id];
	}

	*val += resp->amount;
	foogodRepaint();
}

static void set_flag(struct response *resp, struct conv *conv)
{
	int flag;
	unsigned int *set;

	if (resp->flag_id > MAX_CFLAG) {
		flag = resp->flag_id - MAX_CFLAG;
		set = C_flags;	// global flag set
	} else {
		flag = resp->flag_id;
		set = conv->flags;	// per-conversation flag set
	}

	set[flag / sizeof(unsigned int)] |=
	    (1 << (flag % sizeof(unsigned int)));
}

static void clear_flag(struct response *resp, struct conv *conv)
{
	int flag;
	unsigned int *set;

	if (resp->flag_id > MAX_CFLAG) {
		// This is a "global" flag visible outside the scope of the
		// conversation.
		flag = resp->flag_id - MAX_CFLAG;
		set = C_flags;
	} else {
		// This is a "local" flag visible only within this conversation
		// (but persistent across conversation end and begin).
		flag = resp->flag_id;
		set = conv->flags;
	}

	set[flag / sizeof(unsigned int)] &=
	    ~(1 << (flag % sizeof(unsigned int)));
}

static void join(struct response *resp, struct conv *conv)
{
	if (!conv->speaker->joinPlayer())
		consolePrint("No room in party!\n");
	else
		conv->done = true;
}

static bool get_player_query(struct KeyHandler *kh, int key, int keymod)
{
	if (key == CANCEL) {
		while (C_ptr > C_query) {
			C_ptr--;
			*C_ptr = 0;
			cmdwin_backspace(1);
			C_room++;
		}
		return true;
	}

	if (key == '\n') {
		consoleNewline();
		return true;
	}

	if (key == '\b') {
		if (C_ptr != C_query) {
			C_ptr--;
			*C_ptr = 0;
			C_room++;
			cmdwin_backspace(1);
		}
		return false;
	}

	if (isprint(key) && C_room) {
		cmdwin_print("%c", key);
		*C_ptr++ = key;
		C_room--;
	}

	return false;
}

static struct response *lookup_response(struct conv *conv)
{
	struct response *resp;
	int i;

	/* Check the native query-response pairs */
	for (i = 0; i < conv->n_qr_pairs; i++) {
		if (!strncasecmp(C_query, conv->qr_pairs[i].query, C_len)) {
			return conv->qr_pairs[i].response;
		}
	}

	/* Recursively search the imported conversations */
	for (i = 0; i < conv->n_imports; i++) {
		resp = lookup_response(conv->imports[i]);
		if (resp)
			return resp;
	}

	/* No match found */
	return 0;
}

static void set_query(char *str)
{
	snprintf(C_query, sizeof(C_query) - 1, "%s", str);
	C_query[MAX_KEYWORD_SZ] = 0;
	C_len = strlen(C_query);
}

static bool load_imports(struct Loader *loader, int *n_imports,
			 struct conv ***imports)
{
	char *tag;
	struct conv *import;
	int index;

	/* base case */
	if (loader->matchToken('}')) {
		*imports = new struct conv *[*n_imports];
		if (!*imports)
			return false;
		memset(*imports, 0, *n_imports * sizeof(struct conv *));
		return true;
	}

	/* recursive case */
	if (!loader->getWord(&tag))
		return false;

	import = (struct conv *) loader->lookupTag(tag, CONVERSATION_TYPE_ID);
	free(tag);
	if (!import || import->magic != CONVERSATION_TYPE_ID)
		return false;

	index = *n_imports;
	(*n_imports)++;
	if (!load_imports(loader, n_imports, imports))
		return false;

	(*imports)[index] = import;
	return true;
}

static bool load_qr_pairs(struct Loader *loader, int *n_pairs,
			  struct qr_pair **pairs)
{
	char *tag;
	char *key;
	struct response *response;
	int index;

	/* base case */
	if (loader->matchToken('}')) {
		*pairs = new struct qr_pair[*n_pairs];
		if (!*pairs)
			return false;
		memset(*pairs, 0, *n_pairs * sizeof(struct qr_pair));
		return true;
	}

	/* recursive case */
	if (!loader->getWord(&key))
		return false;

	if (!loader->getWord(&tag)) {
		free(key);
		return false;
	}

	response = (struct response *) loader->lookupTag(tag, RESPONSE_TYPE_ID);
	free(tag);
	if (!response || response->magic != RESPONSE_TYPE_ID) {
                loader->setError("Invalid RESP tag '%s'", tag);
		free(key);
		return false;
	}

	index = *n_pairs;
	(*n_pairs)++;
	if (!load_qr_pairs(loader, n_pairs, pairs)) {
		free(key);
		return false;
	}

	(*pairs)[index].query = key;
	(*pairs)[index].response = response;
	return true;
}

static bool load_trades(struct Loader *loader, int *n_trades,
			struct trade_info **trades)
{
	char *tag;
	int index, cost;
	class ObjectType *item;

	// base case
	if (loader->matchToken('}')) {
		*trades = new struct trade_info[*n_trades];
		if (!trades)
			return false;
		memset(*trades, 0, *n_trades * sizeof(struct trade_info));
		return true;
	}
	// recursive case
	if (!loader->getWord(&tag))
		return false;

	item = (class ObjectType *) loader->lookupTag(tag, OBJECT_TYPE_ID);
	free(tag);
	if (!item)
		return false;

	if (!loader->getInt(&cost))
		return false;

	index = *n_trades;
	(*n_trades)++;

	if (!load_trades(loader, n_trades, trades))
		return false;

	(*trades)[index].sprite = item->getSprite();
	(*trades)[index].name = item->getName();
	(*trades)[index].data = item;
	(*trades)[index].cost = cost;
	(*trades)[index].show_sprite = 1;

	return true;
}

static bool load_services(struct Loader *loader, int *n_trades,
			  struct trade_info **trades)
{
	int index, cost, service;
	char *name = 0;

	// base case
	if (loader->matchToken('}')) {
		*trades = new struct trade_info[*n_trades];
		if (!trades)
			return false;
		memset(*trades, 0, *n_trades * sizeof(struct trade_info));
		return true;
	}
	// recursive case
	if (!loader->getInt(&service) ||
	    !loader->getString(&name) || !loader->getInt(&cost)) {
		if (name)
			free(name);
		return false;
	}
	// catch unknown services at load time
	if (service < SRV_MIN || service > SRV_MAX)
		return false;

	index = *n_trades;
	(*n_trades)++;

	if (!load_services(loader, n_trades, trades))
		return false;

	(*trades)[index].name = name;
	(*trades)[index].data = (void *) service;
	(*trades)[index].cost = cost;

	return true;
}

static struct response *response_create(void)
{
	struct response *resp;

	resp = new struct response;
	if (!resp)
		return 0;
	memset(resp, 0, sizeof(struct response));
	resp->magic = RESPONSE_TYPE_ID;
	return resp;
}

void response_chain_destroy(struct response *resp)
{
	if (resp->dtor) {
		resp->dtor(resp);
	} else {
		if (resp->tag)
			free(resp->tag);
		if (resp->next)
			response_chain_destroy(resp->next);
		if (resp->msg)
			free(resp->msg);
	}
	delete resp;
}

static bool is_member(class Character * c, void *data)
{
	struct response *resp = (struct response *) data;
	struct api_check_member_parms *parms = &resp->parms.check_member;

	if (c->tag && !strcmp(c->tag, parms->ch_tag)) {
		resp->result = true;
		return true;
	}
	return false;
}

struct mech_alarm_cb_data {
	class Mech *mech;
	int signal;
};

static void mech_alarm_cb(struct wq_job *job, struct list *wq)
{
	struct mech_alarm_cb_data *parms;

	parms = (struct mech_alarm_cb_data *) job->data;
	parms->mech->activate(parms->signal);
	delete parms;
	free(job);
}

static void api_set_mech_alarm(struct response *resp, struct conv *conv)
{
	struct api_set_mech_alarm_parms *parms = &resp->parms.mech_alarm;
	if (!conv->mech)
		return;
	struct mech_alarm_cb_data *data = new struct mech_alarm_cb_data;
	if (!data)
		return;
	data->mech = conv->mech;
	data->signal = parms->signal;
	wqCreateJob(&TurnWorkQueue, Turn + parms->turns, 0, data,
		    mech_alarm_cb);
}

static void api_destroy_set_mech_alarm(struct response *resp)
{
	// nothing
}

static bool api_parse_set_mech_alarm(class Loader * loader,
				     struct response *resp)
{
	struct api_set_mech_alarm_parms *parms = &resp->parms.mech_alarm;

	if (!loader->getInt(&parms->turns) || !loader->getInt(&parms->signal))
		return false;
	resp->fx = api_set_mech_alarm;
	resp->dtor = api_destroy_set_mech_alarm;
	return true;
}

// api_blit_map --------------------------------------------------------------

static void api_blit_map(struct response *resp, struct conv *conv)
{
	struct api_blit_map_parms *parms = &resp->parms.blit_map;

	terrain_map_blit(parms->dst->terrain_map,
			 parms->dst_x,
			 parms->dst_y,
			 parms->src,
			 parms->src_x, parms->src_y, parms->w, parms->h);

        // ---------------------------------------------------------------------
        // Because we may have changed the line-of-sight properties of the
        // tiles we just blitted, we have to tell the vmask cache to invalidate
        // all vmasks in the surrounding area.
        // ---------------------------------------------------------------------

        vmask_invalidate(parms->dst, parms->dst_x, parms->dst_y, parms->w, parms->h);
	mapSetDirty();
}

static void api_destroy_blit_map(struct response *resp)
{
	struct api_blit_map_parms *parms = &resp->parms.blit_map;
        if (parms->place_tag) {
                free(parms->place_tag);
                parms->place_tag = 0;
        }
        if (parms->map_tag) {
                free(parms->map_tag);
                parms->map_tag = 0;
        }
}

static bool api_bind_blit_map(struct response *resp, class Loader *loader)
{
	struct api_blit_map_parms *parms = &resp->parms.blit_map;
        SDL_Rect rect;

        // bind the place tag
        parms->dst = (struct place*)loader->lookupTag(parms->place_tag, 
                                                        PLACE_ID);
        if (!parms->dst) {
                loader->setError("Error binding blit_map: '%s' is not "
                                 "a valid PLACE tag\n", 
                                 parms->place_tag);
                return false;
        }

        // bind the map tag
        parms->src = 
                (struct terrain_map*)loader->lookupTag(parms->map_tag, 
                                                     MAP_ID);
        if (!parms->src) {
                loader->setError("Error binding blit_map: '%s' is not "
                                 "a valid MAP tag\n",
                                 parms->map_tag);
                return false;
        }

        // check if the src rect is within the map
        rect.x = 0;
        rect.y = 0;
        rect.w = parms->src->w;
        rect.h = parms->src->h;
        if (!point_in_rect(parms->src_x, parms->src_y, &rect)) {
                loader->setError("Error binding blit_map: upper left "
                                 "corner (%d, %d) not in MAP %s\n", 
                                 parms->src_x, parms->src_y, parms->map_tag);
                return false;
        }
        if (!point_in_rect(parms->src_x + parms->w - 1, 
                           parms->src_y + parms->h - 1, 
                           &rect)) {
                loader->setError("Error binding blit_map: lower right "
                                 "corner (%d, %d) not in MAP %s\n", 
                                 parms->src_x + parms->w - 1, 
                                 parms->src_y + parms->h - 1, 
                                 parms->map_tag);
                return false;
        }

        // check if the dst rect is within the place
        rect.x = 0;
        rect.y = 0;
        rect.w = place_w(parms->dst);
        rect.h = place_h(parms->dst);
        if (!point_in_rect(parms->dst_x, parms->dst_y, &rect)) {
                loader->setError("Error binding blit_map: upper left "
                                 "corner (%d, %d) not in PLACE %s\n", 
                                 parms->dst_x, parms->dst_y, parms->place_tag);
                return false;
        }
        if (!point_in_rect(parms->dst_x + parms->w - 1, 
                           parms->dst_y + parms->h - 1, 
                           &rect)) {
                loader->setError("Error binding blit_map: lower right "
                                 "corner (%d, %d) not in PLACE %s\n", 
                                 parms->dst_x + parms->w - 1,
                                 parms->dst_y + parms->h - 1, 
                                 parms->place_tag);
                return false;
        }

        return true;
}


static bool api_parse_blit_map(class Loader * loader, struct response *resp)
{
	struct api_blit_map_parms *parms = &resp->parms.blit_map;

	// parse
	if (!loader->getWord(&parms->place_tag) ||
	    !loader->getInt(&parms->dst_x) ||
	    !loader->getInt(&parms->dst_y) ||
	    !loader->getWord(&parms->map_tag) ||
	    !loader->getInt(&parms->src_x) ||
	    !loader->getInt(&parms->src_y) ||
	    !loader->getInt(&parms->w) ||
	    !loader->getInt(&parms->h) || !loader->getInt(&parms->rot))
		goto fail;

	// bind tags later
#if 0
#ifdef LATE_BIND_BLIT_MAP
	// note: have to do a two-pass bind to get the place, since places are
	// usually specified after mechs. For now default to the current place
	// at runtime.
	if (!(parms->dst = (struct place *) loader->lookupTag(ptag, PLACE_ID))) {
		loader->setError("Error parsing blit_map: '%s' is not a "
				 "valid PLACE tag", ptag);
		goto fail;
	}
#endif
	if (!(parms->src =
	      (struct terrain_map *) loader->lookupTag(mtag, MAP_ID))) {
		loader->setError("Error parsing blit_map: '%s' is not a "
				 "valid MAP tag", mtag);
		goto fail;
	}
#endif

	resp->fx = api_blit_map;
	resp->dtor = api_destroy_blit_map;
        resp->bind = api_bind_blit_map;

	return true;

      fail:
        api_destroy_blit_map(resp);
	return false;
}

// api_send_signal -----------------------------------------------------------

static void api_send_signal(struct response *resp, struct conv *conv)
{
	struct api_send_signal_parms *parms = &resp->parms.send_signal;
	class Mech *mech;

	if (parms->mech_tag) {
		mech = (class Mech *) lookupTag(parms->mech_tag, MECH_ID);
		if (!mech)
			return;
	} else if (conv->mech->port) {
		mech = conv->mech->port;
	} else {
		return;
	}

	mech->activate(parms->signal);
	mapUpdate(REPAINT_IF_DIRTY);
}

static void api_destroy_send_signal(struct response *resp)
{
	struct api_send_signal_parms *parms = &resp->parms.send_signal;
	free(parms->mech_tag);
}

static bool api_parse_send_signal(class Loader * loader, struct response *resp)
{
	struct api_send_signal_parms *parms = &resp->parms.send_signal;

	// Try to get a tag. If we don't get one then we'll assume that the
	// target mech is "this" - in other words this send_signal call is
	// occurring within the context of a mechs action block.
	loader->getWord(&parms->mech_tag);

	if (!loader->getInt(&parms->signal))
		return false;

	resp->fx = api_send_signal;
	resp->dtor = api_destroy_send_signal;
	return true;
}

static void api_check_member(struct response *resp, struct conv *conv)
{
	struct api_check_member_parms *parms = &resp->parms.check_member;

	player_party->for_each_member(is_member, resp);
	conv->result = resp->result;
	execute_response_chain(conv->result ? parms->yes : parms->no, conv);
}

static void api_destroy_check_member(struct response *resp)
{
	struct api_check_member_parms *parms = &resp->parms.check_member;
	free(parms->ch_tag);
	response_chain_destroy(parms->yes);
	response_chain_destroy(parms->no);
}

static bool api_parse_check_member(class Loader * loader, struct response *resp)
{
	struct api_check_member_parms *parms = &resp->parms.check_member;

	// Note: I can't resolve the tag right now because characters are
	// defined AFTER conversations (it's more common for characters to
	// refere to conversations than vice versa)
	if (!loader->getWord(&parms->ch_tag) ||
	    !(parms->yes = load_response_chain(loader))) {
		free(parms->ch_tag);
		return false;
	}

	if (!(parms->no = load_response_chain(loader))) {
		free(parms->ch_tag);
		response_chain_destroy(parms->yes);
		return false;
	}

	resp->fx = api_check_member;
	resp->dtor = api_destroy_check_member;
	return true;
}

static void api_check_flag(struct response *resp, struct conv *conv)
{
	int flag;
	unsigned int *set;
	struct api_check_flag_parms *parms = &resp->parms.check_flag;

	if (parms->fid > MAX_CFLAG) {
		flag = parms->fid - MAX_CFLAG;
		set = C_flags;
	} else {
		flag = parms->fid;
		set = conv->flags;
	}

	conv->result = set[flag / sizeof(unsigned int)] &
	    (1 << (flag % sizeof(unsigned int)));

	execute_response_chain(conv->result ? parms->yes : parms->no, conv);
}

static void api_destroy_check_flag(struct response *resp)
{
	struct api_check_flag_parms *parms = &resp->parms.check_flag;
	response_chain_destroy(parms->yes);
	response_chain_destroy(parms->no);
}

static bool api_parse_check_flag(class Loader * loader, struct response *resp)
{
	struct api_check_flag_parms *parms = &resp->parms.check_flag;

	if (!loader->getInt(&parms->fid) ||
	    parms->fid > MAX_FLAG_ID ||
	    !(parms->yes = load_response_chain(loader)))
		return false;

	if (!(parms->no = load_response_chain(loader))) {
		response_chain_destroy(parms->yes);
		return false;
	}
	resp->fx = api_check_flag;
	resp->dtor = api_destroy_check_flag;
	return true;
}

static void api_check_item(struct response *resp, struct conv *conv)
{
	struct api_check_item_parms *parms = &resp->parms.check_item;
        struct inv_entry *ie = player_party->search_inventory(parms->obj);
        int val = ie ? ie->count : 0;
        
	switch (parms->operation) {
	case '=':
		conv->result = (val == parms->val);
		break;
	case '<':
		conv->result = (val < parms->val);
		break;
	case '>':
		conv->result = (val > parms->val);
		break;
	default:
		assert(0);
	}        

	execute_response_chain(conv->result ? parms->yes : parms->no, conv);
}

static void api_destroy_check_item(struct response *resp)
{
	struct api_check_item_parms *parms = &resp->parms.check_item;
	response_chain_destroy(parms->yes);
	response_chain_destroy(parms->no);
}

static bool api_parse_check_item(class Loader * loader, struct response *resp)
{
	struct api_check_item_parms *parms = &resp->parms.check_item;
	char *tag;

	if (!loader->getWord(&tag))
		return false;
	parms->obj = (class ObjectType *) loader->lookupTag(tag,
							    OBJECT_TYPE_ID);
	free(tag);

	loader->getToken(&parms->operation);
	if ((parms->operation != '=' &&
	     parms->operation != '<' &&
	     parms->operation != '>') ||
	    !loader->getInt(&parms->val))
		return false;

	if (!parms->obj || !(parms->yes = load_response_chain(loader)))
		return false;
	if (!(parms->no = load_response_chain(loader))) {
		response_chain_destroy(parms->yes);
		return false;
	}

	resp->fx = api_check_item;
	resp->dtor = api_destroy_check_item;
	return true;

}

static void api_check_parm(struct response *resp, struct conv *conv)
{
	int val;
	struct api_check_parm_parms *parms = &resp->parms.check_parm;

	switch (parms->id) {
	case PID_AMOUNT:
		val = conv->amount;
		break;
	case PID_GOLD:
		val = player_party->gold;
		break;
	case PID_FOOD:
		val = player_party->food;
		break;
	case PID_ACTIVITY:
		val = conv->speaker->getActivity();
		break;
	default:
		val = C_parms[parms->id];
	}

	switch (parms->operation) {
	case '=':
		conv->result = (val == parms->val);
		break;
	case '<':
		conv->result = (val < parms->val);
		break;
	case '>':
		conv->result = (val > parms->val);
		break;
	default:
		assert(0);
	}

	execute_response_chain(conv->result ? parms->yes : parms->no, conv);
}

static void api_destroy_check_parm(struct response *resp)
{
	struct api_check_parm_parms *parms = &resp->parms.check_parm;
	response_chain_destroy(parms->yes);
	response_chain_destroy(parms->no);
}

static bool api_parse_check_parm(class Loader * loader, struct response *resp)
{
	struct api_check_parm_parms *parms = &resp->parms.check_parm;

	if (!loader->getInt(&parms->id) ||
	    parms->id < MIN_PARM_ID || parms->id > MAX_PARM_ID)
		return false;

	loader->getToken(&parms->operation);
	if ((parms->operation != '=' &&
	     parms->operation != '<' &&
	     parms->operation != '>') ||
	    !loader->getInt(&parms->val) ||
	    !(parms->yes = load_response_chain(loader)))
		return false;

	if (!(parms->no = load_response_chain(loader))) {
		response_chain_destroy(parms->yes);
		return false;
	}

	resp->fx = api_check_parm;
	resp->dtor = api_destroy_check_parm;
	return true;
}

static void api_ui_yes_no(struct response *resp, struct conv *conv)
{
	int yesno;
	struct api_ui_yes_no_parms *parms = &resp->parms.yes_no;
	cmdwin_clear();
	cmdwin_print("Reply-<Y/N>");
	getkey(&yesno, yesnokey);
	cmdwin_backspace(strlen("<Y/N>"));
	if (yesno == 'y') {
		cmdwin_print("yes");
		consolePrint("You say: Yes\n");
		conv->result = (yesno == 'y');
		execute_response_chain(parms->yes, conv);
	} else {
		cmdwin_print("no");
		consolePrint("You say: No\n");
		conv->result = (yesno == 'n');
		execute_response_chain(parms->no, conv);
	}
}

static void api_destroy_ui_yes_no(struct response *resp)
{
	response_chain_destroy(resp->parms.yes_no.yes);
	response_chain_destroy(resp->parms.yes_no.no);
}

static bool api_parse_ui_yes_no(class Loader * loader, struct response *resp)
{
	struct api_ui_yes_no_parms *parms = &resp->parms.yes_no;
	if (!(parms->yes = load_response_chain(loader)))
		return false;
	if (!(parms->no = load_response_chain(loader))) {
		response_chain_destroy(parms->yes);
		return false;
	}
	resp->fx = api_ui_yes_no;
	resp->dtor = api_destroy_ui_yes_no;
	return true;
}

// api_create_object ----------------------------------------------------------

static void api_destroy_create_object(struct response *resp)
{
	struct api_create_object_parms *parms = &resp->parms.create_object;
        if (parms->obj_type_tag) {
                free(parms->obj_type_tag);
                parms->obj_type_tag = 0;
        }
        if (parms->place_tag) {
                free(parms->place_tag);
                parms->place_tag = 0;
        }
}

static bool api_bind_create_object(struct response *resp, class Loader *loader)
{
	struct api_create_object_parms *parms = &resp->parms.create_object;
        SDL_Rect rect;

        // lookup the object type
        parms->obj_type = 
                (class ObjectType*)loader->lookupTag(parms->obj_type_tag, 
                                                     OBJECT_TYPE_ID);
        if (!parms->obj_type) {
                loader->setError("Error binding create_object: '%s' is not "
                                 "a valid OBJECT TYPE tag\n",
                                 parms->obj_type_tag);
                return false;
        }

        // lookup the place
        parms->place = (struct place*)loader->lookupTag(parms->place_tag, 
                                                        PLACE_ID);
        if (!parms->place) {
                loader->setError("Error binding create_object: '%s' is not "
                                 "a valid PLACE tag\n", 
                                 parms->place_tag);
                return false;
        }

        // check if the target rect is within the place
        rect.x = 0;
        rect.y = 0;
        rect.w = place_w(parms->place);
        rect.h = place_h(parms->place);
        if (!point_in_rect(parms->x, parms->y, &rect)) {
                loader->setError("Error binding create_object: upper left "
                                 "corner (%d, %d) not in place %s\n", 
                                 parms->x, parms->y, parms->place_tag);
                return false;
        }
        if (!point_in_rect(parms->x + parms->w - 1, 
                           parms->y + parms->h - 1, &rect)) {
                loader->setError("Error binding create_object: lower right "
                                 "corner (%d, %d) not in place %s\n", 
                                 parms->x + parms->w - 1, 
                                 parms->y + parms->h - 1, 
                                 parms->place_tag);
                return false;
        }

        return true;
}

static void api_create_object(struct response *resp, struct conv *conv)
{
	struct api_create_object_parms *parms = &resp->parms.create_object;
        class Object *obj = NULL;
        int i;

        assert(parms->prob);
        assert(parms->obj_type);
        assert(parms->place);

        // roll for success
        if ((rand() % (int)(1 / parms->prob)))
                return;

        // for each object
        for (i = 0; i < parms->obj_count; i++) {

                // create the object
                obj = parms->obj_type->createInstance();
                if (!obj)
                        return;
                
                // place the object
                obj->relocate(parms->place, parms->x, parms->y);
        }
}

static bool api_parse_create_object(class Loader * loader, 
                                    struct response *resp)
{
	struct api_create_object_parms *parms = &resp->parms.create_object;

	// parse
	if (!loader->getWord(&parms->obj_type_tag) ||
	    !loader->getInt(&parms->obj_count) ||
            !loader->getWord(&parms->place_tag) ||
	    !loader->getInt(&parms->x) ||
	    !loader->getInt(&parms->y) ||
	    !loader->getInt(&parms->w) ||
	    !loader->getInt(&parms->h) ||
	    !loader->getFloat(&parms->prob))
		goto fail;


        // bind tags later
        
	resp->fx = api_create_object;
	resp->dtor = api_destroy_create_object;
        resp->bind = api_bind_create_object;
	return true;

 fail:
        api_destroy_create_object(resp);
	return false;
}

// api_create_npc_party -------------------------------------------------------

static void api_destroy_create_npc_party(struct response *resp)
{
	struct api_create_npc_party_parms *parms = 
                &resp->parms.create_npc_party;
        if (parms->obj_type_tag) {
                free(parms->obj_type_tag);
                parms->obj_type_tag = 0;
        }
        if (parms->place_tag) {
                free(parms->place_tag);
                parms->place_tag = 0;
        }
}

static bool api_bind_create_npc_party(struct response *resp, 
                                      class Loader *loader)
{
	struct api_create_npc_party_parms *parms = 
                &resp->parms.create_npc_party;
        SDL_Rect rect;

        // lookup the object type
        parms->obj_type = 
                (class ObjectType*)loader->lookupTag(parms->obj_type_tag, 
                                                     NPCPARTY_TYPE_ID);
        if (!parms->obj_type) {
                loader->setError("Error binding create_npc_party: '%s' is not "
                                 "a valid PARTY tag\n",
                                 parms->obj_type_tag);
                return false;
        }

        // lookup the place
        parms->place = (struct place*)loader->lookupTag(parms->place_tag, 
                                                        PLACE_ID);
        if (!parms->place) {
                loader->setError("Error binding create_npc_party: '%s' is not "
                                 "a valid PLACE tag\n", 
                                 parms->place_tag);
                return false;
        }

        // check if the target rect is within the place
        rect.x = 0;
        rect.y = 0;
        rect.w = place_w(parms->place);
        rect.h = place_h(parms->place);
        if (!point_in_rect(parms->x, parms->y, &rect)) {
                loader->setError("Error binding create_npc_party: upper left "
                                 "corner (%d, %d) not in place %s\n", 
                                 parms->x, parms->y, parms->place_tag);
                return false;
        }
        if (!point_in_rect(parms->x + parms->w, parms->y + parms->h, &rect)) {
                loader->setError("Error binding create_npc_party: lower right "
                                 "corner (%d, %d) not in place %s\n", 
                                 parms->x + parms->w, parms->y + parms->h, 
                                 parms->place_tag);
                return false;
        }

        return true;
}

static void api_create_npc_party(struct response *resp, struct conv *conv)
{
	struct api_create_npc_party_parms *parms = 
                &resp->parms.create_npc_party;
        class NpcParty *party = NULL;


        assert(parms->prob);
        assert(parms->obj_type);
        assert(parms->place);

        // roll for success
        if ((rand() % (int)(1 / parms->prob)))
                return;

        // ---------------------------------------------------------------------
        // Allocate the new party
        // ---------------------------------------------------------------------

        party = (class NpcParty *)parms->obj_type->createInstance();
        assert(party);

        // ---------------------------------------------------------------------
        // Initialize the new party.
        // ---------------------------------------------------------------------

        party->setAlignment(parms->align);
        party->createMembers();

        // ---------------------------------------------------------------------
        // Add it to the specified place.
        // ---------------------------------------------------------------------

        party->relocate(parms->place, parms->x, parms->y);
}

static bool api_parse_create_npc_party(class Loader * loader, 
                                       struct response *resp)
{
	struct api_create_npc_party_parms *parms = 
                &resp->parms.create_npc_party;

	// parse
	if (!loader->getWord(&parms->obj_type_tag) ||
            !loader->getBitmask(&parms->align) ||
            !loader->getWord(&parms->place_tag) ||
	    !loader->getInt(&parms->x) ||
	    !loader->getInt(&parms->y) ||
	    !loader->getInt(&parms->w) ||
	    !loader->getInt(&parms->h) ||
	    !loader->getFloat(&parms->prob))
		goto fail;


        // bind tags later
        
	resp->fx = api_create_npc_party;
	resp->dtor = api_destroy_create_npc_party;
        resp->bind = api_bind_create_npc_party;
	return true;

 fail:
        api_destroy_create_npc_party(resp);
	return false;
}

static struct api_entry {
	char *name;
	 bool(*parse) (class Loader *, struct response *);
} api_tbl[] = {
	{ "send_signal",      api_parse_send_signal    },
        { "set_alarm",        api_parse_set_mech_alarm },
        { "blit_map",         api_parse_blit_map       },
        { "create_object",    api_parse_create_object  },
        { "create_npc_party", api_parse_create_npc_party  },
};

struct response *load_response_chain(class Loader * loader)
{
	struct response *head, *resp, **presp;
	unsigned int i;

	/*** Allocate the first response clause ***/

	if (!(head = response_create()))
		return 0;

	if (!loader->matchToken('{'))
		goto fail;

	presp = &head;

	do {

		/*** Allocate & chain another clause if nec. ***/

		resp = *presp;

		if (!resp)
			resp = *presp = response_create();
		if (!resp)
			goto fail;

		/*** Parse the response ***/

		// look it up in the table of valid calls and call its parse
		// routine
		for (i = 0; i < array_sz(api_tbl); i++) {
			if (loader->matchWord(api_tbl[i].name)) {
				if (!api_tbl[i].parse(loader, resp))
					goto fail;
				break;
			}
		}

		if (i < array_sz(api_tbl))
                        goto link;

		if (loader->matchWord("SAY") || loader->matchWord("print")) {
			if (!loader->getString(&resp->msg))
				goto fail;
			resp->fx = say;
		} else if (loader->matchWord("GET_YES_NO")) {
			if (!api_parse_ui_yes_no(loader, resp))
				goto fail;
		} else if (loader->matchWord("ATTACK")) {
			resp->fx = attack;
		} else if (loader->matchWord("GET_AMOUNT")) {
			resp->fx = prompt_amount;
		} else if (loader->matchWord("SET_AMOUNT")) {
			if (!loader->getInt(&resp->amount))
				goto fail;
			resp->fx = set_amount;
		} else if (loader->matchWord("CHECK_PARM")) {
			if (!api_parse_check_parm(loader, resp))
				goto fail;
		} else if (loader->matchWord("GIVE_ITEM")) {
			char *tag;
			if (!loader->getWord(&tag))
				goto fail;
			resp->item = (class ObjectType *) loader->
			    lookupTag(tag, OBJECT_TYPE_ID);
			// fixme: need to confirm the magic # here
			free(tag);
			if (!resp->item)
				goto fail;
			if (!loader->getInt(&resp->amount))
				goto fail;
			resp->fx = give_item;
		} else if (loader->matchWord("CHECK_ITEM")) {
			if (!api_parse_check_item(loader, resp))
				goto fail;
		} else if (loader->matchWord("TAKE_ITEM")) {
			char *tag;
			if (!loader->getWord(&tag))
				goto fail;
			resp->item = (class ObjectType *) loader->
			    lookupTag(tag, OBJECT_TYPE_ID);
			// fixme: need to confirm the magic # here
			free(tag);
			if (!resp->item)
				goto fail;
			if (!loader->getInt(&resp->amount))
				goto fail;
			resp->fx = take_item;
		} else if (loader->matchWord("TRADE")) {
			if (!loader->matchToken('{'))
				goto fail;
			if (!load_trades(loader, &resp->n_trades,
					 &resp->trades))
				goto fail;
			resp->fx = trade;
		} else if (loader->matchWord("SERVICE")) {
			if (!loader->matchToken('{'))
				goto fail;
			if (!load_services(loader, &resp->n_trades,
					   &resp->trades))
				goto fail;
			resp->fx = service;
		} else if (loader->matchWord("TURN_AWAY")) {
			resp->fx = turn_away;
		} else if (loader->matchWord("TAKE")) {
			if (!loader->getInt(&resp->parm_id) ||
			    resp->parm_id < MIN_PARM_ID ||
			    resp->parm_id > MAX_PARM_ID)
				goto fail;
			resp->fx = take;
		} else if (loader->matchWord("CHANGE_PARM")) {
			if (!loader->getInt(&resp->parm_id) ||
			    resp->parm_id < MIN_PARM_ID ||
			    resp->parm_id > MAX_PARM_ID ||
			    !loader->getInt(&resp->amount))
				goto fail;
			resp->fx = change_parm;
		} else if (loader->matchWord("SET_FLAG")) {
			if (!loader->getInt(&resp->flag_id)
			    || resp->flag_id > MAX_FLAG_ID)
				goto fail;
			resp->fx = set_flag;
		} else if (loader->matchWord("CLEAR_FLAG")) {
			if (!loader->getInt(&resp->flag_id)
			    || resp->flag_id > MAX_FLAG_ID)
				goto fail;
			resp->fx = clear_flag;
		} else if (loader->matchWord("CHECK_FLAG")) {
			if (!api_parse_check_flag(loader, resp))
				goto fail;
		} else if (loader->matchWord("JOIN")) {
			resp->fx = join;
		} else if (loader->matchWord("CHECK_MEMBER")) {
			if (!api_parse_check_member(loader, resp))
				goto fail;
		} else if (loader->matchToken('}')) {
			// allow blank clauses
			break;
		} else {
			loader->setError("Unknown API call: %s\n",
					 loader->lexer->lexeme);
			goto fail;
		}

        link:
		presp = &resp->next;
	} while (!loader->matchToken('}'));

	return head;

      fail:
	response_chain_destroy(head);
	return 0;

}

struct response *convLoadResponse(class Loader * loader)
{
	char *tag;
	struct response *chain;

	if (!loader->getWord(&tag))
		return NULL;

	if (!(chain = load_response_chain(loader))) {
		free(tag);
		return NULL;
	}

	chain->tag = tag;

	return chain;
}

struct conv *convLoadConversation(class Loader * loader)
{
	struct conv *conv;

	conv = new struct conv;
	if (!conv)
		return 0;
	memset(conv, 0, sizeof(struct conv));
	conv->magic = CONVERSATION_TYPE_ID;

	/* tag { INHERIT { */
	if (!loader->getWord(&conv->tag) ||
	    !loader->matchToken('{') ||
	    !loader->matchWord("IMPORT") || !loader->matchToken('{'))
		goto fail;

	if (!load_imports(loader, &conv->n_imports, &conv->imports))
		goto fail;

	/* NATIVE { */
	if (!loader->matchWord("NATIVE") || !loader->matchToken('{'))
		goto fail;

	/* native pairs */
	if (!load_qr_pairs(loader, &conv->n_qr_pairs, &conv->qr_pairs))
		goto fail;

	/* } */
	if (!loader->matchToken('}'))
		goto fail;

	return conv;

      fail:
	delete conv;
	return 0;
}

enum conv_result convEnter(struct conv *conv)
{
	struct KeyHandler kh;
	struct response *resp;

	conv->consequence = CONV_OK;
	conv->amount = 0;
	conv->result = false;
	conv->done = false;

	kh.fx = get_player_query;

	set_query("HAIL");
	resp = lookup_response(conv);

	for (;;) {

		/*** Process response(s) ***/

		execute_response_chain(resp, conv);

		/*** Check if npc ended conversation ***/

		if (conv->done)
			break;

		/*** Setup for next query ***/

		memset(C_query, 0, sizeof(C_query));
		C_room = sizeof(C_query) - 1;
		C_ptr = C_query;

		cmdwin_clear();
		cmdwin_print("Say: ");

		/*** Get next query ***/

		eventPushKeyHandler(&kh);
		eventHandle();
		eventPopKeyHandler();

		C_query[MAX_KEYWORD_SZ] = 0;
		C_len = strlen(C_query);
		consolePrint("You say: %s\n", C_query);

		/*** Check if player ended conversation ***/

		if (Quit)
			break;

		if (strlen(C_query) == 0)
			set_query("BYE");

		if (!strcasecmp(C_query, "BYE")) {
			conv->done = true;
		}

		/*** Get NPC response ***/

		resp = lookup_response(conv);

		if (!resp) {
			set_query("DEFAULT");
			resp = lookup_response(conv);
		}

		if (!resp) {
			consolePrint("No reply.\n");
		}
	}

	cmdwin_clear();
	cmdwin_repaint();
	return conv->consequence;
}

int convInit(void)
{
	memset(C_parms, 0, sizeof(C_parms));
	return 0;
}

bool convLoadParms(struct Loader * loader)
{
	int index;

	while (!loader->matchToken('}')) {
		if (!loader->getInt(&index) ||
		    index > MAX_PARM_ID || !loader->getInt(&C_parms[index]))
			return false;
	}
	return true;
}
