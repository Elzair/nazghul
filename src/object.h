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
#ifndef object_h
#define object_h

#include "clock.h"
#include "list.h"
#include "olist.h"
#include "closure.h"
#include "console.h"		// SAM: for DEBUG purposes...
#include "sound.h"

#include <assert.h>


// Wrappers for session_add() and session_rm() that do the ref-counting.
#define session_add_obj(session, obj, dtor, save, start)                      \
        do {                                                                  \
                obj_inc_ref((obj));                                           \
                (obj)->handle = session_add((session), (obj), (dtor), (save), \
                                            (start));                         \
        } while (0)

#define session_rm_obj(session, obj)                                          \
        do {                                                                  \
                session_rm((session), (obj)->handle);                         \
                obj_dec_ref((obj));                                           \
        } while (0)

/* Hack: until movement modes implemented in objects */
#define obj_mmode(obj) ((obj)->getMovementMode())

/* Hooks id's */
#define OBJ_HOOK_START_OF_TURN 0  /* after ap assigned at start of turn      */
#define OBJ_HOOK_ADD_HOOK      1  /* whan a new effects is added             */
#define OBJ_HOOK_DAMAGE        2  /* when the object takes damage            */
#define OBJ_HOOK_KEYSTROKE     3  /* when PC is given a command              */
#define OBJ_HOOK_NIL           4  /* never (effect just exists)              */
#define OBJ_HOOK_ON_DEATH      5  /* character just died                     */
#define OBJ_HOOK_READY_EQUIP   6  /* PC readies                              */
#define OBJ_HOOK_UNREADY_EQUIP 7  /* PC unreadies                            */
#define OBJ_HOOK_MOVE_DONE     8  /* character just moved                    */
#define OBJ_HOOK_ATTACK_DONE   9  /* character just attacked                 */
#define OBJ_HOOK_CAST_DONE     10 /* character just cast a spell             */
#define OBJ_HOOK_DROP_DONE     11 /* character just dropped something        */
#define OBJ_HOOK_YUSE_DONE     12 /* character just yused a skill            */
#define OBJ_HOOK_GET_DONE      13 /* character just got something            */
#define OBJ_HOOK_HANDLE_DONE   14 /* character just handled something        */
#define OBJ_HOOK_OPEN_DONE     15 /* character just opened something         */
#define OBJ_HOOK_READY_DONE    16 /* character finished equipping            */
#define OBJ_HOOK_TALK_DONE     17 /* character finished talking              */
#define OBJ_HOOK_USE_DONE      18 /* character finished using something      */
#define OBJ_HOOK_MIX_DONE      19 /* character finished using something      */
#define OBJ_HOOK_KAMP_START    20 /* character about to start camping        */
#define OBJ_NUM_HOOKS          21 /* total number of object hooks            */

/* Relocation flags. Used to avoid triggers in special cases. */
#define REL_NOSTEP    (1<<0)                     /* don't trigger "step"     */
#define REL_NOSENSE   (1<<1)                     /* don't trigger "sense"    */
#define REL_NOTRIG    (REL_NOSTEP|REL_NOSENSE)   /* don't trigger anything   */

// Note: if you change the layers you'll probably need to change the save
//       file
// Proper rendering depends on keeping these in order!
enum layer {
	null_layer       = 0,
        tfeat_layer      = 1,
	mech_layer       = 2,
	portal_layer     = 3,
	vehicle_layer    = 4,
        bed_layer        = 5,
	container_layer  = 6,
	item_layer       = 7,
	field_layer      = 8,
	being_layer      = 9,
	projectile_layer = 10,
	cursor_layer     = 11
};

enum control_mode {
        CONTROL_MODE_AUTO = 0,
        CONTROL_MODE_PLAYER,
        CONTROL_MODE_IDLE,
        CONTROL_MODE_FOLLOW,
        CONTROL_MODE_TASK
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
        OffMap, // and no parent
        NotFollowMode,
        CantRendezvous,
        NotApplicable,
        ChangedFacing,
        AvoidedHazard,
        OutOfRange,
        NoDestination,
        UserCanceled,
        StationaryObject,
        NotInVehicle
};

struct inv_entry {
	struct list list;
	struct list auxlist;
	int count;
	int ref;
	class ObjectType *type;
};

struct filter {
        bool (*fx)(struct inv_entry *ie, void *fdata);
        void *fdata;
};

typedef struct hook_entry {
        struct list list;
        struct effect *effect;
        struct gob *gob;
        clock_alarm_t expiration;
        int flags;
        char started : 1; /* unsaved flag */
} hook_entry_t;

class ObjectType {

      public:
	virtual bool isType(int classID);
	virtual int getType();
	ObjectType();
        ObjectType(const char *tag, const char *name, struct sprite *sprite, 
                   enum layer layer);
	virtual ~ObjectType();
	virtual bool init(char *tag, char *name, enum layer layer, 
                          struct sprite * sprite);
        virtual void setSprite(struct sprite *sprite);
	virtual const char *getTag();
	virtual const char *getName();
	virtual struct sprite *getSprite();
	virtual enum layer getLayer();
	virtual class Object *createInstance();
	virtual bool isVisible();
	virtual void describe(Object *obj);
        virtual int getSpeed();
        virtual int getMaxHp();
   virtual void setMovementMode(struct mmode *mmode);
	virtual struct mmode *getMovementMode(); 
	
        // This might turn out to be too vague. We'll see.
        virtual int getRequiredActionPoints();

        // This version of describe() can't run any hooks; it's used for
        // inventory descriptions, where there are no object instances.
        void describeType(int count);

        bool isUsable();    // items, etc
        bool isReadyable(); // arms
        bool isMixable();   // reagents
        bool isCastable();  // spells
        bool canExec();     // mechs, etc
        bool canGet();
        bool canOpen();
        bool canStep();
        bool canHandle();
        bool canSense();
        bool canXamine();
        bool canAttack();
        bool canOnAttack();
        bool canEnter();
        bool canBump(); // attempted entry onto same tile
        bool canHitLocation(); // weapon hitting a target location
        bool canBuy(); // has a hook for 'buy'
        bool canSearch(); // has a hook for 'search'
        bool isQuestItem();

        // These return the result of closure_exec:
        int use(Object *user);
        int exec(Object *obj);
        int get(Object *obj, Object *getter);
        int open(Object *obj, Object *opener);
        int step(Object *obj, Object *stepper);
        int sense(Object *obj, Object *stepper);
        int xamine(Object *obj, Object *xaminer);		
        int handle(Object *obj, Object *handler);
        int attack(Object *obj, Object *attacker);
        int onAttack(Object *obj, Object *attacker);
        int enter(Object *obj, Object *enterer);
        int cast(Object *caster);
        int bump(Object *obj, Object *bumper);
        int hitLocation(Object *obj, Object *attacker, Object *target, struct place *place, int x, int y, int dam);
        int buy(Object *buyer, int q);
        int search(Object *obj, Object *searcher);

        closure_t *getGifc();
        void setGifc(closure_t *gifc, int cap);

        void setPluralName(char *val);
        void setGob(struct gob *gob);
        struct gob * getGob();
        void setQuestItemFlag(bool val);


      protected:
	char *tag;
	char *name;
	struct sprite *sprite;
	enum layer layer;
        int speed;
        int required_action_points;
        int max_hp;

        /* ghulscript-interface (gifc) */
        closure_t *gifc;
        int gifc_cap;
        struct gob *gob;
        struct mmode *movementMode;

 private:
        bool hasDescribeHook();
        void runDescribeHook(Object *obj);

        char *pluralName;
        char *getPluralName();
        bool questItemFlag;
};

class Object {

      public:
	virtual bool isType(int classID);
	virtual int getType();

	Object(class ObjectType * type); // preferred constructor

        Object();
	virtual ~Object();
	virtual void init(int x, int y, struct place *place,
			  class ObjectType * type);
	virtual void init(class ObjectType * type);

        virtual sound_t *getDamageSound();
        virtual sound_t *get_movement_sound();
        virtual int getActivity();
        virtual enum control_mode getControlMode();
        virtual int getCount();
        virtual int getHp();
	virtual enum layer getLayer(void);
	virtual int getLight();
        virtual int getMaxHp();
	virtual const char *getName(void);
	virtual class ObjectType *getObjectType();
	virtual struct place *getPlace();
        virtual int getRequiredActionPoints();
        virtual int getSpeed();
        virtual struct mview *getView();
	virtual int getVisionRadius();
	virtual int getX();
	virtual int getY();
        virtual int getDx();
        virtual int getDy();

        virtual bool isCompanionOf(class Object *other);
	virtual bool isDestroyed();
        virtual bool isOnMap();
        virtual bool isDead();
	virtual bool isSelected();
        virtual bool isTurnEnded();
        virtual bool isCameraAttached();
        virtual bool isPlayerPartyMember();
        virtual bool isPlayerControlled();
        virtual bool canWanderTo(int x, int y);

        virtual void addView();
        virtual void rmView();
        virtual void updateView();
        virtual bool addToInventory(class Object *object);
        virtual bool hasInInventory(class ObjectType *type);
        virtual void attachCamera(bool val);
	virtual void heal(int amount);
        virtual void save(struct save *save);
        virtual void setLight(int val);
        virtual void setOnMap(bool val);
	virtual void setX(int x);
	virtual void setY(int y);
	virtual void changeX(int dx);
	virtual void changeY(int dy);
        virtual void setCount(int count);
	virtual void setPlace(struct place *place);
	virtual void select(bool val);
	virtual void destroy();
	virtual void relocate(struct place *newplace, int newx, int newy, 
                              int flags = 0,
                              struct closure *place_switch_hook = NULL);
	virtual void remove();
        virtual void start();
	virtual bool isVisible();
        virtual void setVisible(bool val);
	virtual bool isSubmerged();
        virtual void setSubmerged(bool val);
	virtual bool isShaded();
	virtual void describe();
		virtual void examine();
	virtual void paint(int sx, int sy);
        virtual class Object *clone();
	virtual bool joinPlayer(void);     
	virtual void synchronize();
        virtual void exec();
        virtual int getActionPointsPerTurn();
        virtual void applyEffect(closure_t *effect);
        virtual int getActionPoints();
        virtual void resetActionPoints();
        virtual void burn();
        virtual void sleep();
        virtual void damage(int amount);
        virtual void inflictDamage(int amount,class Character *attacker);
        virtual void decActionPoints(int points);
        virtual void setActionPoints(int amount);
        virtual void endTurn();
        virtual void startTurn();
        virtual void setControlMode(enum control_mode);
        virtual bool putOnMap(struct place *place, int x, int y, int r, 
                              int flags /* PFLAG_* (see place.h) */);
        virtual void setView(struct mview *view);
        virtual void changePlaceHook();
        virtual MoveResult move(int dx, int dy);

        // Hook/effect API.
        virtual void hookForEach(int hook_id, 
                                 int (*cb)(struct hook_entry *entry, 
                                           void *data),
                                 void *data);
        virtual bool addEffect(struct effect *effect, struct gob *gob);
        virtual void restoreEffect(struct effect *effect, struct gob *gob, 
                                   int flags, clock_alarm_t expiration);
        virtual void runHook(int hook_id, const char *fmt, ...);
        virtual void saveHooks(struct save *save);
        virtual bool removeEffect(struct effect *effect);
        

        // Virtual container ops
        virtual bool addFood(int quantity);
        virtual bool addGold(int quantity);
        virtual bool add(ObjectType *type, int amount);
        virtual bool takeOut(ObjectType *type, int amount);        

        // ghulscript-object (gob) access.
        void setGob(struct gob *gob);
        struct gob * getGob();

        // State variables affected by script execution
        virtual void setSprite(struct sprite *sprite);
	virtual struct sprite *getSprite(); 

        virtual void setOpacity(bool opaque);
	virtual bool isOpaque();
        virtual bool tryToRelocateToNewPlace(struct place *place, 
                                             int x, int y,
                                             struct closure *cutscene);

        // Proxies into script signals
        bool canEnter();
        bool canStep();
		bool canSense();
        void step(Object *stepper);
		void sense(Object *stepper);
        void attack(Object *attacker);
        void onAttack(Object *user);
        void enter(Object *enterer);

        // Conversation interface
        void setConversation(struct conv *conv);
        virtual struct conv *getConversation();
        virtual Object *getSpeaker();

        virtual bool isTemporary();
        virtual void setTemporary(bool val);

        virtual int getMovementCost(int pclass);
        virtual struct mmode *getMovementMode();
        virtual void setMovementMode(struct mmode *mmode);
        virtual bool isPassable(int pclass);
        virtual void setPclass(int val);
        virtual int getPclass();
        virtual bool isStationary();

        int getTTL(void);
        // These two might destroy the object so make them class methods:
        static void setTTL(class Object *obj, int val);
        static void decrementTTL(class Object *obj);

        bool ignoresTimeStop();
        void setIgnoreTimeStop(bool val);
        
        struct node *clink; // points back to node in container's list

	char *tag;
	struct list list;	// for the loader, not the place

        struct node *turn_list; /* points back to node in place's turn list */

        // The session handle for removing/checking the orphan list
        void *handle;

        // The session id of the last save.
        int saved;

        /* The reference count. Use the obj_inc_ref() and obj_dec_ref() macros
         * to change this. obj_dec_ref() will destroy the object when this hits
         * zero. */
        int refcount;

        bool setFacing(int facing);
        int getFacing();
        struct sprite *getPortrait();
        void setPortrait(struct sprite *sprite);

      protected:
        virtual void setup();

	class ObjectType * type;
	int x;
	int y;
        int dx;
        int dy;
        int count;
	struct place *place;
	bool selected;
	bool destroyed;
        int action_points;
        enum control_mode control_mode;
        bool camera_attached;
	int hp;
        bool is_on_map;
	struct conv *conv;
        struct mview *view;
        int light;
        bool temporary;

        struct hook_list {
                struct list list;
                int lock;
        } hooks[OBJ_NUM_HOOKS];

        // (Possibly null) pointer to this object's corresponding ghulscript
        // object.
        struct gob *gob;

        // Used to shadow the ObjectType sprite for objects whose sprite is
        // determined by their gob state (like mechs).
        struct sprite *current_sprite;
        bool opacity;

        // Used for invisibility;
        int visible;

        // Forces addEffect() to add the given effect without allowing the
        // "add-hook-hook" effects to prevent it. Used by start().
        bool forceEffect;

        // For fields, mechs and other objects that affect passability:
        int pclass;

        // Time to live: if > 0, this is decremented whenever the object
        // executes a turn. When it falls to zero the object is removed from
        // the map, which will destroy it once nothing else references
        // it. Defaults to -1 (ie, infinite).
        int ttl;

 private:
        bool surreptitiouslyRemove();
        bool started;
        void triggerSense(struct place *tilePlace, int tileX, int tileY);
        void triggerStep(struct place *tilePlace, int tileX, int tileY);
        void triggerOnTileEntry(struct place *tilePlace, int tileX, int tileY,
                                int flags);
        void triggerOnTileExit(struct place *tilePlace, int tileX, int tileY,
                               int flags);
        int facing;
        bool ignoreTimeStop;

        // Each object has its own sprite_frame, which breaks up the
        // "synchronized dancing" problem. It also allows us to animate only
        // player-controlled characters during Time Stop.
        int sprite_frame;

        // Used for critters that can go underwater.
        bool submerged;

        struct sprite *portrait;
};

#include "macros.h"
BEGIN_DECL

extern void obj_inc_ref(Object *obj);
extern void obj_dec_ref(Object *obj);
#define obj_dec_ref_safe(obj) do { if ((obj)) obj_dec_ref(obj); } while (0)

END_DECL

#endif				// object_h
