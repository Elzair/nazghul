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
#ifndef character_h
#define character_h

#include "object.h"
#include "Arms.h"
#include "species.h"

#undef RAM_ATTACK

// Weird C++-isms ensue at compile-time if I try to include combat.h directly
// here...
extern void combatKIA(class Character *);

struct ready_spell {
	struct list list;
	struct spell *spell;
};

struct point {
	int x, y;
};

struct TypicalObjectInfo {
	int probability;
	class ObjectType *type;
	int n_max;
};

class Character:public Object {
      public:

	enum ReadyResult {
		Readied,
		NoAvailableSlot,
		WrongType,
		TooHeavy,
	};

	enum MoveResult {
		MovedOk,
		ExitedMap,
		EngagedEnemy,
		WasOccupied,
		WasImpassable,
		SlowProgress,
		SwitchedOccupants,
                CouldNotSwitchOccupants,
                OffMap, /* and no parent map (dungeons) */
	};

	Character();
	virtual ~ Character();

	virtual int getType();
	virtual char *getName();
	virtual int getHp();
	virtual int getMaxHp();
	virtual int getOrder();
	virtual void addExperience(int delta);
	virtual int getExperience();
	virtual unsigned char getStrength();
	virtual unsigned char getIntelligence();
	virtual unsigned char getDexterity();
	virtual unsigned char getLevel();
	virtual struct mview *getView();
	virtual int getPmask();
	virtual int getArmourClass();
	virtual char *getWoundDescription();
	virtual class Character *getAttackTarget();
	virtual class ArmsType *getNextArms();
	virtual class ArmsType *getNextWeapon();
	virtual class ArmsType *getCurrentWeapon();
	virtual int getLight();
	virtual int getVisionRadius();
	virtual int getSpeed();
	virtual int getMana();
	virtual int getMaxMana();
	virtual struct sprite *getSprite();
	virtual int getFleeDx();
	virtual int getFleeDy();
	virtual int getAlignment();
	virtual int getRestCredits(void);
        virtual char *get_damage_sound();
        virtual char *get_movement_sound();

	virtual bool isType(int classID);
	virtual bool isDead();
	virtual bool isPoisoned();
	virtual bool isAsleep();
	virtual bool isOnMap();
	virtual bool isIncapacitated();
	virtual bool isPlayerControlled();
	virtual bool hasReadied(class ArmsType * arms);
	virtual bool isSolo();
	virtual int  hasAmmo(class ArmsType * weapon);
	virtual bool isFleeing();
	virtual bool isHostile(int alignment);
	virtual bool wasElevated(void);
	virtual bool isVisible();
	virtual bool isShaded();

	virtual void changeArmourClass(int val);
	virtual void changeHp(int delta);
	virtual void changeSleep(bool val);
	virtual void changeLight(int delta);
	virtual void changeMana(int delta);
	virtual void addRestCredits(int delta_hours);

	virtual void setName(char *name);
	virtual void setOrder(int order);
	virtual void setPoison(bool val);
	virtual void setHp(int hp);
	virtual void setPlayerControlled(bool val);
	virtual void setKilledNotifier(void (*cb) (class Character * c));
	virtual void setAttackTarget(class Character * target);
	virtual bool setSolo(bool val);
	virtual void setFleeing(bool val);
	virtual void setAlignment(int val);
	virtual void setCombat(bool val);
	virtual void setRestCredits(int hours);
	virtual void setElevated(bool val);
	virtual void setVisible(bool val);

	bool initStock(struct species * species, struct occ * occ,
		       struct sprite * sprite, char *name, int order,
		       int alignment);
	virtual void awaken(void);
	virtual int attack(int damage);
	virtual void kill();
	virtual void remove();
	virtual enum ReadyResult ready(class ArmsType * arms);
	virtual bool unready(class ArmsType * arms);
	virtual enum MoveResult move(int dx, int dy);
	virtual enum Character::MoveResult flee();
	virtual void attackTerrain(int x, int y);
	virtual void useAmmo();
	virtual void rejuvenate();
	virtual void initItems();
	virtual void armThyself();
	virtual bool needToRearm();
	virtual void heal();
	virtual void cure();
	virtual void resurrect();
	virtual void rest(int hours);
	virtual bool load(struct Loader *loader);
	virtual class Character *clone(class Character *);
	virtual void describe(int count);
	virtual void relocate(struct place *place, int x, int y);

	virtual class ArmsType *enumerateArms();
	virtual class ArmsType *enumerateWeapons();

	char *tag;
	struct list plist;	// party list
	struct list llist;	// load list
	class NpcParty *party;
	struct conv *conv;
	struct sched *sched;
	struct species *species;
	struct occ *occ;
	bool is_clone;
	int light;
	class Character *quarry;

	struct astar_node *path;	// Added when I rewrote party
	// rendezvous

      protected:
	bool initCommon(void);
	virtual bool isAttackTargetInRange();

	char *name;
	int hm;

        int hp_mod;
        int hp_mult;
        int mp_mod;
        int mp_mult;

        int hit_mod;
        int def_mod;
        int dam_mod;
        int arm_mod;

	int xp;
	int order;
	int hp;
	bool sleep;
	int ac;
	int armsIndex;
	int str;
	int intl;
	int dex;
	int mana;
	int lvl;
	bool poison;
	bool playerControlled;
	bool solo;
	struct mview *view;
	void (*killedNotifier) (class Character * c);
	class ArmsType *currentArms;
	class Character *target;
	class ArmsType **rdyArms;
	bool fleeing;
	int fleeX, fleeY;
	int burden;
	int alignment;
	bool inCombat;
        char *damage_sound;

	// *** NPC fields
	virtual void dropRdyArms();
	virtual bool dropItems();
	class Container *container;
	bool rearm;

	// *** PC fields
	struct sprite *sprite;

	int n_rest_credits;
	bool elevated;
	int visible;
};

#endif
