/* Copyright (c) 2003 Gordon McNutt */
#ifndef player_h
#define player_h

#include "list.h"
#include "object.h"
#include "map.h"
#include "character.h"
#include "pinfo.h"

#include <string.h>

#define MAX_N_PC 8

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
        class NpcParty *npc_party;
        class Portal *portal;
        class Moongate *moongate;
};

class player_party:public Object {

      public:
        player_party();
	virtual ~ player_party();

        // overloaded Object methods:
        virtual struct sprite *getSprite(void);
        virtual char *getName();
        virtual bool isVisible();
        virtual void describe();
        virtual void relocate(struct place *place, int x, int y);

        unsigned char get_pmask(void);
        bool move(int dx, int dy, bool teleport);
	void add_to_inventory(class ObjectType * data, int quantity);
        void remove_from_inventory(struct inv_entry *ie, int quantity);
	struct inv_entry *search_inventory(class ObjectType * type);
        void enter_portal(void);
	bool try_to_enter_town_from_edge(class Portal * portal, int dx, int dy);
        int get_speed(void);
        void fire_vehicle_weapon(int dx, int dy);
        void ready_arms(struct object *object);
        bool all_dead(void);
	void for_each_member(bool(*fx) (class Character *, void *data),
			     void *data);
        void recompute_los(void);
        void board_vehicle(void);
        class Character *get_leader(void);
	bool add_to_party(class Character * c);
        int get_room_in_party(void);
        void advance_turns(void);
        void add_spell(struct spell *spell, int quantity);
	bool enter_moongate(class Moongate * srcGate, int x, int y);
        char *get_movement_description();
        char *get_movement_sound();
	// enum move_result move_to(struct move_info *info);
	// enum move_result enter_moongate(struct move_info *info);
	void enter_moongate(class Moongate * moongate);
        void move_to_combat(struct combat_info *info);
	bool try_to_enter_moongate(class Moongate * src_gate);
        enum move_result check_move_to(struct move_info *info);

        virtual void paint(int sx, int sy);
	virtual void hit_by_ordnance(class OrdnanceType * ordnance);
        virtual struct formation *get_formation();

        int dx, dy;
        struct sprite *sprite;
        int speed;
        unsigned char pmask;
        class Vehicle *vehicle;
        int turns;
        char *mv_desc;
        char *mv_sound;
        int n_pc;
        class Character *pc[MAX_N_PC];
        class Character *leader;
        struct list inventory;
        int nArms;
        int nReagents;
        int nSpells;
        int nItems;
        int nAmmo;
        int light;
        int food;
        struct mview *view;
        bool onMap;
        int alignment;
        int gold;
        int context;
        struct formation *formation;
        struct terrain_map *campsite_map;
        struct formation *campsite_formation;
        bool camping;
        struct position_info pinfo;

 protected:
        bool try_to_enter_portal(class Portal *portal, int dx, int dy);
        bool try_to_move_off_map(struct move_info *info);
        void move_to_wilderness_combat(struct combat_info *cinfo);
        bool turn_vehicle(void);
};

extern class player_party *player_party;
extern int player_init(void);

// useful fxs for playerForEach:
extern bool apply_damage(class Character * pm, void *amount);
extern bool apply_poison(class Character * pm, void *unused);

#endif				// player_h
