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

#include "Container.h"
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
        PARTY_CONTROL_SOLO
};

enum move_result {
        move_ok = 0,
        move_off_map,
        move_null_place,
        move_occupied,
        move_enter_combat,
        move_enter_auto_portal,
        move_impassable,
        move_player_quit,
        move_enter_subplace,
        move_not_in_vehicle
};

struct move_info {
        struct place *place;
        int x;
        int y;        
        int dx;
        int dy;
        int px; // parent-x for wilderness combat
        int py; // parent-y for wilderness combat
        int turns; // consumed by movement
        class Party *npc_party;
        struct place *subplace;
};

class PlayerParty : public Party {
 
      public:
        PlayerParty(char *tag,
                     struct sprite *sprite,
                     char *mv_desc, sound_t *mv_sound,
                     int food, int gold, 
                     struct formation *formation, 
                     struct terrain_map *camping_map,
                     struct formation *camping_formation);
	virtual ~ PlayerParty();

        // overloaded Object methods:
	virtual void addExperience(int delta);
        virtual bool addToInventory(class Object *object);
        virtual bool hasInInventory(class ObjectType *type);
        virtual struct sprite *getSprite(void);
        virtual const char *getName(void);
        virtual bool isVisible();
        virtual void describe();
        virtual void exec();
        virtual void damage(int amount);
        virtual void beginResting(int hours);
        virtual bool isResting();
        virtual void beginCamping(class Character *guard, int hours);
        virtual void endCamping();
        virtual void ambushWhileCamping();
        virtual void endResting();
	virtual int getVisionRadius();
        virtual void addView();
        virtual int getLight();
        virtual void changePlaceHook();
        virtual bool isPlayerControlled();

        void beginLoitering(int hours);
        void endLoitering();
        void forceAbortLoitering();
        bool isLoitering();
        void startSession(void);
        void distributeMembers(struct place *new_place, int new_x, int new_y, 
                               int new_dx, int new_dy);
        MoveResult move(int dx, int dy);
	enum MoveResult try_to_enter_subplace_from_edge(struct place *town,
                                                        int dx, int dy);
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
        char *get_movement_description();
        sound_t *get_movement_sound();
        enum move_result check_move_to(struct move_info *info);
        virtual void paint(int sx, int sy);
        virtual struct formation *get_formation();
        int get_num_living_members(void);
        class Character *get_first_living_member(void);
        void throw_out_of_bed();
        class Character *getMemberAtIndex(int index);
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
        virtual void setOnMap(bool val);        
        virtual void save(struct save *save);
        virtual class Container *getInventory();

        void chooseNewLeader();
        void advanceTurns(int turns);
        void setTurnsToNextMeal(int turns);
        void setTurnsToNextRestCredit(int turns);

        struct sprite *sprite;
        int turns;
        char *mv_desc;

        /* Inventory management */
        Container *inventory;
        void setInventoryContainer(Container *val);
        virtual bool addFood(int quantity);
        virtual bool addGold(int quantity);
        virtual bool add(ObjectType *type, int amount);
        virtual bool takeOut(ObjectType *type, int amount);
        void unrefInventoryObject(ObjectType *type);
        void refInventoryObject(ObjectType *type);
        bool canSeeLocation(struct place *place, int x, int y);

        int food;
        bool onMap;
        int gold;
        struct formation *formation;
        struct terrain_map *campsite_map;
        struct formation *campsite_formation;
        void sortReadiedItems(class Character * member);

 protected:
        enum MoveResult try_to_move_off_map(struct move_info *info);
        bool turn_vehicle(void);
        void disableCurrentMode();
        struct place *getPlaceFromMembers();

        clock_alarm_t wakeup_alarm;
        clock_alarm_t rest_alarm;
        bool          resting;
        int           speed;

        bool camping;
        class Character *camp_guard;

        enum party_control control_mode;
        class Character *leader;
        class Character *solo_member;
        class Character *active_member;
        void (*ctrl)(class PlayerParty*);
        int turns_to_next_rest_credit;
        int turns_to_next_meal;
        sound_t *mv_sound;
        bool loitering;

 private:
        PlayerParty();

};

//extern class player_party *player_party;
extern void player_dtor(void *val);
extern void player_save(struct save *save, void *val);

#endif				// player_h
