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
#ifndef player_h
#define player_h

#include "list.h"
#include "object.h"
#include "map.h"
#include "character.h"
#include "pinfo.h"
#include "clock.h" /* for alarm clock */
#include "place.h" // for struct location

#include <string.h>

enum party_control {
        PARTY_CONTROL_ROUND_ROBIN = 0,
        PARTY_CONTROL_FOLLOW,
        PARTY_CONTROL_SOLO,
};

enum move_result {
        move_ok = 0,
        move_off_map,
        move_null_place,
        move_occupied,
        move_enter_combat,
        move_enter_auto_portal,
        move_impassable,
        move_enter_moongate,
        move_player_quit,
};

struct move_info {
        struct place *place;
        int x;
        int y;
        int dx;
        int dy;
        int turns; // consumed by movement
        class Party *npc_party;
        class Portal *portal;
        class Moongate *moongate;
};

class player_party : public Party {
 
      public:
        player_party();
	virtual ~ player_party();

        // overloaded Object methods:
        virtual bool addToInventory(class Object *object);
        virtual struct sprite *getSprite(void);
        virtual char *getName(void);
        virtual bool isVisible();
        virtual void clearAlignment(int alignment);
        virtual void describe(int count);
        virtual void relocate(struct place *place, int x, int y);
        virtual void exec(struct exec_context *context);
        virtual void damage(int amount);
        virtual void poison();
        virtual int getSpeed(void);
        virtual void decActionPoints(int points);
        virtual int getAlignment();
        virtual void beginResting(int hours);
        virtual bool isResting();
        virtual void beginCamping(class Character *guard, int hours);
        virtual void endCamping();
        virtual void ambushWhileCamping();
        virtual void endResting();
        virtual int getPmask(void);
	virtual int getVisionRadius();
        virtual void addView();
        virtual int getLight();
        virtual void changePlaceHook();

        void distributeMembers(struct place *new_place, int new_x, int new_y, int new_dx, int new_dy);
        MoveResult move(int dx, int dy);
	void add_to_inventory(class ObjectType * data, int quantity);
        void remove_from_inventory(struct inv_entry *ie, int quantity);
	struct inv_entry *search_inventory(class ObjectType * type);
        void enter_portal(void);
	enum MoveResult try_to_enter_town_from_edge(class Portal * portal, int dx, int dy);
        void ready_arms(struct object *object);
        bool allDead(void);
        bool immobilized(void);
	void for_each_member(bool(*fx) (class Character *, void *data),
			     void *data);
        void board_vehicle(void);
        class Character *get_leader(void);
	virtual void removeMember(class Character *);
        virtual bool addMember(class Character *);
        void add_spell(struct spell *spell, int quantity);
	bool enter_moongate(class Moongate * srcGate, int x, int y);
        char *get_movement_description();
        char *get_movement_sound();
	void enter_moongate(class Moongate * moongate);
	enum MoveResult try_to_enter_moongate(class Moongate * src_gate);
        enum move_result check_move_to(struct move_info *info);
        virtual void paint(int sx, int sy);
        virtual struct formation *get_formation();
        int get_num_living_members(void);
        class Character *get_first_living_member(void);
        void throw_out_of_bed();
        int getTurnCount();
        class Character *getMemberAtIndex(int index);
        void removeMembers();
        void setCombatExitDestination(struct location *loc);
        void getCombatExitDestination(struct location *loc);
        void clearCombatExitDestination();
        void unCharmMembers();
        void setCamping(bool val);
        bool isCamping();
        enum party_control getPartyControlMode();
        void enableFollowMode();
        void enableRoundRobinMode();
        void enableSoloMode(class Character *solo);
        void setLeader(class Character *character);
        bool rendezvous(struct place *place, int x, int y);
        int getContext(void);

        struct sprite *sprite;
        class Vehicle *vehicle;
        int turns;
        char *mv_desc;
        char *mv_sound;

        struct list inventory;
        int nArms;
        int nReagents;
        int nSpells;
        int nItems;
        int nAmmo;
        int light;
        int food;
        bool onMap;
        int alignment;
        int gold;
        struct formation *formation;
        struct terrain_map *campsite_map;
        struct formation *campsite_formation;
        struct position_info pinfo;

 protected:
        virtual void applyExistingEffects();

        void try_to_enter_portal(class Portal *portal);
        enum MoveResult try_to_move_off_map(struct move_info *info);
        bool turn_vehicle(void);
        void chooseNewLeader();
        void disableCurrentMode();

        clock_alarm_t wakeup_alarm;
        clock_alarm_t rest_alarm;
        bool          resting;
        int           speed;
        int           turn_count;

        struct location combat_exit_destination;

        bool camping;
        class Character *camp_guard;

        enum party_control control_mode;
        class Character *leader;
        class Character *solo_member;
        class Character *active_member;
        void (*ctrl)(class player_party*);
};

extern class player_party *player_party;
extern int player_init(void);

#endif				// player_h
