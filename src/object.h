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

#include <assert.h>

// Macros to manage an object's reference count and to destroy it when it has
// no more references.
#define obj_inc_ref(obj) ((obj)->refcount++)
#define obj_dec_ref(obj)                                                      \
        do {                                                                  \
                assert((obj)->refcount >= 0);                                 \
                (obj)->refcount--;                                            \
        } while (0)


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

/* Hooks
 *
 * OBJ_HOOK_START_OF_TURN
 *      Runs at the start of every turn after the object is given its action 
 *      points for the turn. Passes the object as the sole parameter to the
 *      exec routine. For example, a paralysis effect might set the action 
 *      points to zero so the object cannot move on its turn.
 *
 * OBJ_HOOK_ADD_HOOK
 *      Runs whenever a new effect is added. Passes the effect being added as
 *      the parameter to the exec routine. This can be used to block the effect
 *      from being added. Immunity effects are implemented this way.
 *
 * OBJ_HOOK_DAMAGE
 *      Runs whenever an object takes damage. Passes in the object.
 *
 */
#define OBJ_HOOK_START_OF_TURN 0
#define OBJ_HOOK_ADD_HOOK      1
#define OBJ_HOOK_DAMAGE        2
#define OBJ_NUM_HOOKS          3
#define OBJ_MAX_CONDITIONS     8

// Note: if you change the layers you'll probably need to change the save
//       file
// Proper rendering depends on keeping these in order!
enum layer {
	null_layer       = 0,
	mech_layer       = 1,
	portal_layer     = 2,
	vehicle_layer    = 3,
        bed_layer        = 4,
	container_layer  = 5,
	item_layer       = 6,
	field_layer      = 7,
	being_layer      = 8,
	projectile_layer = 9,
	cursor_layer     = 10,
};

enum control_mode {
        CONTROL_MODE_AUTO = 0,
        CONTROL_MODE_PLAYER,
        CONTROL_MODE_IDLE,
        CONTROL_MODE_FOLLOW,
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
        AvoidedPortal,
        AvoidedHazard,
        OutOfRange,
        NoDestination,
        UserCanceled
};

struct inv_entry {
	struct list list;
	struct list auxlist;
	int count;
	int ref;
	class ObjectType *type;
};

struct exec_context {
        struct place *place;
        int combat:1;
        int quicken:1;
        int time_stop:1;
};

typedef struct hook_entry {
        struct list list;
        struct effect *effect;
        struct gob *gob;
        clock_alarm_t expiration;
        int flags;
} hook_entry_t;

class ObjectType {

      public:
	virtual bool isType(int classID);
	virtual int getType();
	ObjectType();
        ObjectType(char *tag, char *name, struct sprite *sprite, 
                   enum layer layer);
	virtual ~ObjectType();
	virtual bool init(char *tag, char *name, enum layer layer, 
                          struct sprite * sprite);
        virtual void setSprite(struct sprite *sprite);
	virtual char *getTag();
	virtual char *getName();
	virtual struct sprite *getSprite();
	virtual enum layer getLayer();
	virtual class Object *createInstance();
	virtual bool isVisible();
	virtual void describe(int count);
        virtual int getSpeed();
        virtual int getMaxHp();

        // This might turn out to be too vague. We'll see.
        virtual int getRequiredActionPoints();

	struct list list;

        bool isUsable();    // items, etc
        bool isReadyable(); // arms
        bool isMixable();   // reagents
        bool isCastable();  // spells
        bool canExec();     // mechs, etc
        bool canGet();
        bool canOpen();
        bool canStep();
        bool canHandle();
        bool canAttack();
        bool canEnter();

        void use(Object *user);
        void exec(Object *obj);
        void get(Object *obj, Object *getter);
        void open(Object *obj, Object *opener);
        void step(Object *obj, Object *stepper);
        void handle(Object *obj, Object *handler);
        void attack(Object *obj, Object *attacker);
        void enter(Object *obj, Object *enterer);
        void cast(Object *caster);
        closure_t *getGifc();
        void setGifc(closure_t *gifc, int cap);

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

        virtual char *get_movement_sound();
        virtual int getActivity();
        virtual int getAlignment();
        virtual enum control_mode getControlMode();
        virtual int getCount();
        virtual int getHp();
	virtual enum layer getLayer(void);
	virtual int getLight();
        virtual int getMaxHp();
	virtual char *getName(void);
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
        virtual bool isHostile(int alignment);
	virtual bool isSelected();
        virtual bool isTurnEnded();
        virtual bool isCameraAttached();
        virtual bool isCharmed();
        virtual bool isNativelyHostile(int alignment);
        virtual bool isPlayerPartyMember();
        virtual bool isPlayerControlled();
        virtual bool canWanderTo(int x, int y);

        virtual void addView();
        virtual void rmView();
        virtual void updateView();
        virtual bool addToInventory(class Object *object);
        virtual void attachCamera(bool val);
        virtual void charm(int alignment);
        virtual void clearAlignment(int alignment);
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
        virtual void unCharm();
	virtual void destroy();
	virtual void relocate(struct place *newplace, int newx, int newy, 
                              bool noStep = false, 
                              struct closure *place_switch_hook = NULL);
	virtual void remove();
        virtual void start();
	virtual bool isVisible();
        virtual void setVisible(bool val);
	virtual bool isShaded();
	virtual void describe();
	virtual void paint(int sx, int sy);
        virtual class Object *clone();
	virtual bool joinPlayer(void);     
	virtual void synchronize();
        virtual void exec(struct exec_context *context);
        virtual int getActionPointsPerTurn();
        virtual void applyEffect(closure_t *effect);
        virtual int getActionPoints();
        virtual void burn();
        virtual void sleep();
        virtual void damage(int amount);
        virtual void decActionPoints(int points);
        virtual void endTurn();
        virtual void startTurn();
        virtual void setControlMode(enum control_mode);
        virtual bool putOnMap(struct place *place, int x, int y, int r);
        virtual void setView(struct mview *view);
        virtual void changePlaceHook();
        virtual MoveResult move(int dx, int dy);

        // Condition API - used to report condition in the status window
        virtual char *getCondition();
        virtual void setDefaultCondition();

        // Hook/effect API.
        static int nameToHookId(char *hook_name);
        static char * hookIdToName(int hook_id);
        virtual void hookForEach(int hook_id, 
                                 int (*cb)(struct hook_entry *entry, 
                                           void *data),
                                 void *data);
        virtual bool addEffect(struct effect *effect, struct gob *gob);
        virtual void restoreEffect(struct effect *effect, struct gob *gob, int flags, clock_alarm_t expiration);
                                   
        virtual void runHook(int hook_id);
        virtual void saveHooks(struct save *save);
        virtual bool removeEffect(struct effect *effect);
        

        // Virtual container ops
        virtual bool addFood(int quantity);
        virtual bool add(ObjectType *type, int amount);
        virtual bool takeOut(ObjectType *type, int amount);        

        // ghulscript-object (gob) access.
        void setGob(struct gob *gob);
        struct gob * getGob();

        // State variables affected by script execution
        virtual void setSprite(struct sprite *sprite);
	virtual struct sprite *getSprite(); 
        virtual void setPmask(int pmask);
        virtual int getPmask();
        virtual void setOpacity(bool opaque);
	virtual bool isOpaque();
        virtual bool tryToRelocateToNewPlace(struct place *place, 
                                             int x, int y,
                                             struct closure *cutscene);

        // Proxies into script signals
        bool canEnter();
        bool canStep();
        void step(Object *stepper);
        void attack(Object *attacker);
        void enter(Object *enterer);

        // Conversation interface
        void setConversation(closure_t *conv);
        bool canTalk();
        void talk(char *query, Object *asker);

        virtual bool isTemporary();
        virtual void setTemporary(bool val);


        
	struct olist container_link;

	char *tag;
	struct list list;	// for the loader, not the place
        struct list turn_list; /* for processing each object in a turn */

        // The session handle for removing/checking the orphan list
        void *handle;

        // The session id of the last save.
        int saved;

        /* The reference count. Use the obj_inc_ref() and obj_dec_ref() macros
         * to change this. obj_dec_ref() will destroy the object when this hits
         * zero. */
        int refcount;

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
	closure_t *conv;
        struct mview *view;
        int light;
        bool temporary;

        struct hook_list {
                struct list list;
                int lock;
        } hooks[OBJ_NUM_HOOKS];

        // Conditions are used to report the single-character conditions shown
        // in the status window. E.g., good = 'G', dead = 'D', etc. With the
        // exception of 'G' and 'D' the kernel is agnostic about their values
        // and the script controls them.
        char condition[OBJ_MAX_CONDITIONS + 1];

        // (Possibly null) pointer to this object's corresponding ghulscript
        // object.
        struct gob *gob;

        // Used to shadow the ObjectType sprite for objects whose sprite is
        // determined by their gob state (like mechs).
        struct sprite *current_sprite;
        int pmask;
        bool opacity;

        // Used for invisibility;
        int visible;

        // Forces addEffect() to add the given effect without allowing the
        // "add-hook-hook" effects to prevent it. Used by start().
        bool forceEffect;
};

#endif				// object_h
