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
#ifndef talk_h
#define talk_h

#include "list.h"
#include "status.h"

#define MAX_CFLAG 127
#define CFLAG_WORDS (MAX_CFLAG + sizeof(unsigned int) - 1)/sizeof(unsigned int)

struct api_ui_yes_no_parms {
	struct response *yes;
	struct response *no;
};

struct api_check_parm_parms {
	int id;
	char operation;
	int val;
	struct response *yes;
	struct response *no;
};

struct api_check_item_parms {
	class ObjectType *obj;
	struct response *yes;
	struct response *no;
};

struct api_check_flag_parms {
	int fid;
	struct response *yes;
	struct response *no;
};

struct api_check_member_parms {
	char *ch_tag;
	struct response *yes;
	struct response *no;
};

struct api_send_signal_parms {
	char *mech_tag;
	int signal;
};

struct api_set_mech_alarm_parms {
	int signal;
	int turns;
	class Mech *mech;	// hack?
};

struct api_blit_map_parms {
	struct place *dst;
	int dst_x;
	int dst_y;
	struct terrain_map *src;
	int src_x;
	int src_y;
	int w;
	int h;
	int rot;
};

struct response {
	int magic;		/* RESPONSE_TYPE_ID */
	char *tag;
	struct list list;
	struct response *next;
	void (*fx) (struct response *, struct conv *);
	void (*dtor) (struct response *);
	char *msg;
	int n_trades;
	struct trade_info *trades;
	bool result;
	int amount;
	int parm_id;
	int flag_id;
	class ObjectType *item;
	union {
		struct api_ui_yes_no_parms yes_no;
		struct api_check_parm_parms check_parm;
		struct api_check_item_parms check_item;
		struct api_check_flag_parms check_flag;
		struct api_check_member_parms check_member;
		struct api_send_signal_parms send_signal;
		struct api_set_mech_alarm_parms mech_alarm;
		struct api_blit_map_parms blit_map;
	} parms;
};

struct qr_pair {
	char *query;
	struct response *response;
};

enum conv_result {
	CONV_OK,
	CONV_COMBAT,
};

struct conv {
	int magic;		/* CONVERSATION_TYPE_ID */
	char *tag;		/* used by loader */
	struct list list;	/* used by loader */
	int amount;		/* result of last amount prompt */
	bool result;		/* result of last yes/no prompt or check */
	enum conv_result consequence;	/* returned by convEnter */
	int n_imports;		/* Number of "imported" conversations */
	struct conv **imports;	/* Pointers to "imported" conversations */
	int n_qr_pairs;		/* Number of "native" query-response pairs */
	struct qr_pair *qr_pairs;	/* "Native" query-response pairs */
	class NpcParty *speaker;	/* NPC the player is talking to */
	class Mech *mech;	/* hack? if this conversation is owned by a
				 * mech */
	bool done;
	unsigned int flags[CFLAG_WORDS];
};

extern struct response *convLoadResponse(struct Loader *loader);
extern struct conv *convLoadConversation(struct Loader *loader);
extern conv_result convEnter(struct conv *conv);
extern bool convLoadParms(struct Loader *loader);
extern int convInit(void);

// hack: expose this to Mech.cpp
extern void execute_response_chain(struct response *resp, struct conv *conv);
extern struct response *load_response_chain(class Loader * loader);
extern void response_chain_destroy(struct response *resp);

#endif
