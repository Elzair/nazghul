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
	virtual bool isType(int classID) {
		if (classID == CHARACTER_ID)
			return true;
		return Object::isType(classID);
	}
	virtual int getType() {
		return CHARACTER_ID;
	}

	Character();
	virtual ~ Character();
	bool initStock(struct species * species, struct occ * occ,
		       struct sprite * sprite, char *name, int order,
		       int alignment);

	virtual char *getName() {
		return name;
	}
	virtual int getHp() {
		return hp;
	}
	virtual int getMaxHp() {
		return lvl * HP_PER_LVL;
	}
	virtual int getOrder() {
		return order;
	}
	virtual void addExperience(int delta);
	virtual int getExperience() {
		return xp;
	}
	virtual unsigned char getStrength() {
		return str;
	}
	virtual unsigned char getIntelligence() {
		return intl;
	}
	virtual unsigned char getDexterity() {
		return dex;
	}
	virtual unsigned char getLevel() {
		return lvl;
	}
	virtual struct mview *getView() {
		return view;
	}
	virtual bool isDead() {
		return (hp == 0);
	}
	virtual bool isPoisoned() {
		return poison;
	}
	virtual bool isAsleep() {
		return sleep;
	}
	virtual bool isOnMap() {
		return getPlace() != 0;
	}
	virtual bool isIncapacitated() {
		return (!isOnMap() || isDead() || isAsleep());
	}
	virtual int getPmask() {
		return species->pmask;
	}
	virtual void changeArmourClass(int val);
	virtual int getArmourClass() {
		return ac;
	}
	virtual void setPoison(bool val) {
		poison = val;
	}
	virtual void setHp(int hp) {
		this->hp = hp;
	}
	virtual void changeHp(int delta);
	virtual void changeSleep(bool val);
	virtual void awaken(void);
	virtual int attack(int damage) {
		if (isDead())
			return 0;
		damage -= getArmourClass();
		if (damage < 0)
			damage = 0;
		changeHp(-damage);
		addExperience(XP_PER_DEFEND);
		return damage;
	}
	virtual void kill();
	virtual void remove();
	enum ReadyResult {
		Readied,
		NoAvailableSlot,
		WrongType,
		TooHeavy,
	};
	virtual enum ReadyResult ready(class ArmsType * arms);
	virtual bool unready(class ArmsType * arms);
	enum MoveResult {
		MovedOk,
		ExitedMap,
		EngagedEnemy,
		WasOccupied,
		WasImpassable,
		SlowProgress,
		SwitchedOccupants,
	};
	virtual enum MoveResult move(int dx, int dy);
#ifdef RAM_ATTACK
	virtual void meleeAttack(class Character * target);
#endif
	virtual bool isPlayerControlled() {
		return playerControlled;
	}
	virtual void setPlayerControlled(bool val) {
		playerControlled = val;
	}
	virtual char *getWoundDescription();
	virtual void setKilledNotifier(void (*cb) (class Character * c)) {
		killedNotifier = cb;
	}
	virtual class Character *getAttackTarget();
	virtual void setAttackTarget(class Character * target) {
		this->target = target;
	}
	virtual class ArmsType *enumerateArms();
	virtual class ArmsType *getNextArms();
	virtual class ArmsType *enumerateWeapons();
	virtual class ArmsType *getNextWeapon();
	virtual class ArmsType *getCurrentWeapon() {
		return currentArms;
	}
	virtual bool hasReadied(class ArmsType * arms);
	virtual bool setSolo(bool val);
	virtual bool isSolo() {
		return solo;
	}
	virtual bool attackTarget(class Character * target);
	virtual bool hasAmmo(class ArmsType * weapon);
	virtual void changeLight(int delta);
	virtual int getLight() {
		return light;
	}
	virtual int getVisionRadius() {
		return species->vr;
	}
	virtual int getSpeed() {
		return species->spd;
	}

	virtual int getMana() {
		return mana;
	}
	virtual int getMaxMana();
	virtual void changeMana(int delta);

	virtual struct sprite *getSprite();

	virtual void setFleeing(bool val);
	virtual bool isFleeing() {
		return fleeing;
	}
	virtual enum Character::MoveResult flee();
	virtual int getFleeDx() {
		return fleeX;
	}
	virtual int getFleeDy() {
		return fleeY;
	}

	virtual void attackTerrain(int x, int y);
	virtual void useAmmo();
	virtual void setName(char *name) {
		this->name = strdup(name);
	}
	virtual void setOrder(int order) {
		this->order = order;
	}
	virtual int getAlignment() {
		return alignment;
	}
	virtual void setAlignment(int val) {
		alignment = val;
	}
	virtual bool isHostile(int alignment) {
		return ((this->alignment & alignment) == 0);
	}
	virtual void setCombat(bool val) {
		inCombat = val;
	}

	virtual void rejuvenate();

	virtual void initItems();
	virtual void armThyself();
	virtual bool needToRearm();

	virtual void heal();
	virtual void cure();
	virtual void resurrect();

	virtual void setRestCredits(int hours);
	virtual void addRestCredits(int delta_hours);
	virtual int getRestCredits(void);
	virtual void rest(int hours);

	virtual bool load(struct Loader *loader);

	virtual bool wasElevated(void) {
		return elevated;
	}
	virtual void setElevated(bool val) {
		elevated = val;
	}
	virtual bool isVisible();
	virtual bool isShaded();
	virtual void setVisible(bool val);
	virtual class Character *clone(class Character *);
	virtual void describe();
	virtual void relocate(struct place *place, int x, int y);
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
