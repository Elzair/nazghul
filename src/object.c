//
// nazghul - an old-school RPG engine
// Copyright (C) 2002, 2003 Gordon McNutt
//e
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
#include "gob.h"
#include "object.h"
#include "session.h"
#include "place.h"
#include "character.h"
#include "map.h"
#include "sprite.h"
#include "screen.h"
#include "console.h"
#include "sound.h"
#include "player.h"
#include "terrain.h"
#include "vmask.h"
#include "Field.h"
#include "dice.h"
#include "effect.h"
#include "mmode.h"
#include "combat.h"  // for combat_get_state()
#include "log.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*****************************************************************************/

//
// These GIFC_CAN_* bits need to match the script:
//
#define GIFC_CAN_GET          (1<<0)
#define GIFC_CAN_USE          (1<<1)
#define GIFC_CAN_EXEC         (1<<2)
#define GIFC_CAN_OPEN         (1<<3)
#define GIFC_CAN_HANDLE       (1<<4)
#define GIFC_CAN_STEP         (1<<5)
#define GIFC_CAN_ATTACK       (1<<6)
#define GIFC_CAN_MIX          (1<<7)
#define GIFC_CAN_ENTER        (1<<8)
#define GIFC_CAN_CAST         (1<<9)
#define GIFC_CAN_BUMP         (1<<10)
#define GIFC_CAN_HIT_LOCATION (1<<11)
#define GIFC_CAN_BUY          (1<<12)
#define GIFC_CAN_SEARCH       (1<<13)
#define GIFC_CAN_SENSE        (1<<14)
#define GIFC_CAN_XAMINE       (1<<15)
#define GIFC_CAN_DESCRIBE     (1<<16)
#define GIFC_CAN_ON_ATTACK     (1<<17)

ObjectType::ObjectType()
{
        assert(false);
}

ObjectType::ObjectType(const char *tag, const char *sname, struct sprite *sprite_, 
                       enum layer layer_)
        : sprite(sprite_), layer(layer_), speed(0), required_action_points(0), 
          max_hp(0), gifc(NULL), gifc_cap(0), gob(NULL), pluralName(NULL)
{
	this->tag = strdup(tag);
        assert(this->tag);

        if (sname) {
                this->name = strdup(sname);
                assert(this->name);
        } else {
                this->name = 0;
        }
}

ObjectType::~ObjectType()
{
	if (tag)
		free(tag);
	if (name)
		free(name);
        if (gifc)
                closure_unref(gifc);
        if (pluralName)
                free(pluralName);
        if (gob)
                gob_unref(gob);
}

void ObjectType::setPluralName(char *val)
{
        if (pluralName)
                free(pluralName);
        if (val)
                pluralName=strdup(val);
        else
                pluralName=NULL;
}

char *ObjectType::getPluralName()
{
        return pluralName;
}

bool ObjectType::init(char *tag, char *name, enum layer layer,
		      struct sprite *sprite)
{
	this->tag = strdup(tag);
	this->name = strdup(name);
	this->sprite = sprite;
	this->layer = layer;
	return (this->tag != 0 && this->name != 0);
}

class Object *ObjectType::createInstance()
{
	return new Object(this);
}

void ObjectType::setSprite(struct sprite *sprite)
{
	this->sprite = sprite;        
}

static int endsWith(const char *word, const char *end)
{
        int wlen=strlen(word)-1;
        int elen=strlen(end)-1;

        if (wlen<elen)
                return 0;
        
        while (elen>=0) {
                if (word[wlen--]!=end[elen--])
                        return 0;
        }

        return 1;
}

void ObjectType::describeType(int count)
{
	if (1 == count) {
		if (isvowel(name[0]))
			log_continue("an ");
		else
			log_continue("a ");
		log_continue("%s", getName());
	} else if (getPluralName()) {
                log_continue("some %s (%d)", getPluralName(), count);
        } else {
                if (endsWith(name, "s")
                    || endsWith(name, "sh"))
                        log_continue("some %ses (%d)", getName(), count);
                else
                        log_continue("some %ss (%d)", getName(), count);
	}
}

void ObjectType::describe(Object *obj)
{
        if (hasDescribeHook()) {
                runDescribeHook(obj);
                return;
        }

        describeType(obj->getCount());
}

bool ObjectType::isType(int classID) 
{
        return (classID == OBJECT_TYPE_ID);
}

int ObjectType::getType()
{
        return OBJECT_TYPE_ID;
}

const char *ObjectType::getTag()
{
        return tag;
}

const char *ObjectType::getName()
{
        return name;
}

struct sprite *ObjectType::getSprite()
{
        return sprite;
}

enum layer ObjectType::getLayer()
{
        return layer;
}

bool ObjectType::isVisible()
{
        return true;
}

int ObjectType::getSpeed()
{
        return speed;
}

int ObjectType::getRequiredActionPoints()
{
        return required_action_points;
}

int ObjectType::getMaxHp()
{
        return max_hp;
}

//////////////////////////////////////////////////////////////////////////////
//
// hook_list api
//
//////////////////////////////////////////////////////////////////////////////
#define hook_list_init(hl) do { list_init(&(hl)->list); (hl)->lock = 0; } while (0)
#define hook_list_add(hl,en) (list_add(&(hl)->list, (en)))
#define hook_list_lock(hl) (++(hl)->lock)
#define hook_list_unlock(hl) ((hl)->lock--)
#define hook_list_first(hl) ((hl)->list.next)
#define hook_list_end(hl) (&(hl)->list)
#define hook_list_empty(hl) (list_empty(&(hl)->list))
#define hook_list_for_each(hl,ptr) list_for_each(&(hl)->list, (ptr))
#define hook_list_locked(hl) ((hl)->lock)
#define hook_list_trylock(hl) (hook_list_locked(hl) ? 0 : hook_list_lock(hl))
#define hook_list_tryunlock(hl,lock) ((lock) ? hook_list_unlock(hl) : 0)

//////////////////////////////////////////////////////////////////////////////
//
// hook_entry api
//
//////////////////////////////////////////////////////////////////////////////

#define HEF_INVALID (1<<0)
#define HEF_DETECTED (1<<1)
#define HEF_STARTED  (1<<2)

#define hook_entry_invalidate(he) ((he)->flags |= HEF_INVALID)
#define hook_entry_detected(he) ((he)->flags & HEF_DETECTED)
#define hook_entry_detect(he) ((he)->flags |= HEF_DETECTED)
#define hook_entry_gob(he) ((he)->gob->p)
#define hook_entry_started(he) ((he)->started)
#define hook_entry_set_started(he) ((he)->started=1)

hook_entry_t *hook_entry_new(struct effect *effect, struct gob *gob)
{
        hook_entry_t *entry;

        entry = (hook_entry_t*)calloc(1, sizeof(*entry));
        assert(entry);
        list_init(&entry->list);
        entry->effect = effect;
        entry->gob = gob;
        gob_ref(gob);
        
        if (effect_will_expire(effect)) {
                clock_alarm_set(&entry->expiration, effect->duration);
        }

        //dbg("hook_entry_new: %p\n", entry);
        return entry;
}

void hook_entry_del(hook_entry_t *entry)
{
        //dbg("hook_entry_del: %p\n", entry);
        if (entry->gob)
                gob_unref(entry->gob);
        free(entry);
}

void hook_entry_save(hook_entry_t *entry, struct save *save)
{
        // Note: saved effects are loaded in kern.c:kern_mk_char() & attached
        // via restoreEffect().

        save->enter(save, "(list\n");
        save->write(save, "%s\n", entry->effect->tag);
        if (entry->gob)
                gob_save(entry->gob, save);
        else
                save->write(save, "nil\n");
        save->write(save, "%d\n", entry->flags);
        clock_alarm_save(entry->expiration, save);
        save->exit(save, ")\n");
}

static inline int hook_entry_is_invalid(hook_entry_t *entry)
{
        return ((entry->flags & HEF_INVALID) ||
                (effect_will_expire(entry->effect) &&
                 clock_alarm_is_expired(&entry->expiration)));
}

//////////////////////////////////////////////////////////////////////////////
//
// Object class methods
//
//////////////////////////////////////////////////////////////////////////////

void Object::init(int x, int y, struct place *place, class ObjectType * type)
{
        // fixme: obsolete?
	this->type = type;
	setX(x);
	setY(y);
	setPlace(place);
}

void Object::init(class ObjectType * type)
{
	this->type = type;
}

bool Object::tryToRelocateToNewPlace(struct place *newplace, 
                                     int newx, int newy,
                                     struct closure *cutscene)
{
        obj_inc_ref(this);
        place_remove_object(getPlace(), this);

        if (cutscene) {
                
                mapUpdate(0);
                closure_exec(cutscene, NULL);
                
        }

        setPlace(newplace);
        setX(newx);
        setY(newy);
        place_add_object(getPlace(), this);
        obj_dec_ref(this);
        changePlaceHook();
        return true;
}

////
// Trigger the topmost sense mechanism on a tile, using this as the subject
//
// @param tilePlace place the tile is in
// @param tileX coord of tile
// @param tileY coord of tile
//
void Object::triggerSense(struct place *tilePlace, int tileX, int tileY)
{
        Object *mech = place_get_object(tilePlace, tileX, tileY, mech_layer);
        if (mech 
            && mech != this 
            && mech->getObjectType()->canSense()) {
                mech->getObjectType()->sense(mech, this);
        }
}

////
// Trigger the topmost step mechanism on a tile, using this as the subject
//
// @param tilePlace place the tile is in
// @param tileX coord of tile
// @param tileY coord of tile
//
void Object::triggerStep(struct place *tilePlace, int tileX, int tileY)
{
        Object *mech = place_get_object(tilePlace, tileX, tileY, mech_layer);
        if (mech 
            && mech != this 
            && mech->getObjectType()->canStep()) {
                mech->getObjectType()->step(mech, this);
        }
}

////
// Run the step and sense triggers on tile entry using this as the subject,
// flags permitting.
//
// @param tilePlace place the tile is in
// @param tileX coord of tile
// @param tileY coord of tile
// @param flags relocation flags which specify what types of triggers to avoid
//
void Object::triggerOnTileEntry(struct place *tilePlace, int tileX, int tileY,
                                int flags)
{
        bool sense = !(flags & REL_NOSENSE);
        bool step = !(flags & REL_NOSTEP);

        if (sense) {
                triggerSense(tilePlace, tileX, tileY);
        }

        if (step) {
                triggerStep(tilePlace, tileX, tileY);
        }
}

////
// Run the sense trigger on tile exit using this as the subject, flags
// permitting.
//
// @param tilePlace place the tile is in
// @param tileX coord of tile
// @param tileY coord of tile
// @param flags relocation flags which specify what types of triggers to avoid
//
void Object::triggerOnTileExit(struct place *tilePlace, int tileX, int tileY,
                               int flags)
{
        bool sense = ! (flags & REL_NOSENSE);

        if (sense) {
                triggerSense(tilePlace, tileX, tileY);
        }
}

void Object::relocate(struct place *newplace, int newx, int newy, int flags,
                      struct closure *place_switch_hook)
{
        int volume;
        int distance;
        struct place *foc_place;
        int foc_x, foc_y;
        struct place *oldPlace = getPlace(); // remember for tile exit
        int oldX = getX(); // remember for tile exit
        int oldY = getY(); // remember for tile exit

        assert(newplace);

        if (isOnMap()) {

                assert(getPlace());

                if (newplace == getPlace()) {

                        // Moving from one tile to another in the same place.
                        if (place_switch_hook) {

                                // A cut scene was specified (this happens for
                                // moongate entry, for example). Remove the
                                // object, play the cut scene, and put the
                                // object back down at the new location.
                                mapUpdate(0);
                                setOnMap(false);
                                rmView();
                                obj_inc_ref(this);
                                place_remove_object(getPlace(), this);

                                closure_exec(place_switch_hook, NULL);

                                setPlace(newplace);
                                setX(newx);
                                setY(newy);
                                place_add_object(newplace, this);
                                obj_dec_ref(this);
                                setOnMap(true);
                                addView();

                        } else {

                                place_move_object(newplace, this, newx, newy);
                                setX(newx);
                                setY(newy);
                        }

                } else {

                        // Place-to-place movement, where the object is on the
                        // map. This is a special case for character objects so
                        // use an overloadable method to implement it.
                        if (! tryToRelocateToNewPlace(newplace, newx, newy,
                                                      place_switch_hook)) {
                                return;
                        }

                        // This object may no longer be on a map as a result of
                        // the above call. If so then finish processing.
                        if (! isOnMap()) {

                                // Run the exit triggers before returning
                                if (oldPlace) {
                                        triggerOnTileExit(oldPlace, oldX, oldY, flags);
                                }

                                return;
                        }
                }

        } else {

                // Place-to-place movement, where the object is off-map in the
                // old place. I assume by default it will be on-map in the new
                // place and let changePlaceHook() fix things up if necessary.
                setPlace(newplace);
                setX(place_wrap_x(newplace, newx));
                setY(place_wrap_y(newplace, newy));
                place_add_object(getPlace(), this);
                setOnMap(true);
                addView();
                changePlaceHook();

        }

        // Run the exit triggers.
        if (oldPlace) {
                triggerOnTileExit(oldPlace, oldX, oldY, flags);
        }
        
        mapSetDirty();

        // It's possible that changePlaceHook() removed this object from the
        // map. This certainly happens when the player party moves from town to
        // wilderness, for example. In this case I probably want to skip all of
        // what follows. I ABSOLUTELY want to skip the call to updateView() at
        // the end, and in fact I changed updateView() to assert if this object
        // is not on the map.
        if (! isOnMap())
                return;

        // Attenuate movement sound based on distance from the camera's focal
        // point.
        volume = SOUND_MAX_VOLUME;
        mapGetCameraFocus(&foc_place, &foc_x, &foc_y);
        if (foc_place == getPlace()) {
                distance = place_flying_distance(foc_place, foc_x, foc_y, 
                                                 getX(), getY());
                if (distance > 1)
                        volume = (volume * (20 - distance))/20;
                if (volume > 0)
                	sound_play(get_movement_sound(), volume);
        }

        // If the camera is attached to this object then update it to focus on
        // the object's new location.
        if (isCameraAttached()) {
                mapCenterCamera(getX(), getY());
        }
        
        updateView();

	// Run the entry triggers.
	triggerOnTileEntry(getPlace(), getX(), getY(), flags);
}

void Object::setOnMap(bool val)
{
        is_on_map = val;
}

void Object::remove()
{
	obj_inc_ref(this);
	if (isOnMap())
	{
		struct place *oldPlace = getPlace(); // remember for tile exit
		int oldX = getX(); // remember for tile exit
		int oldY = getY(); // remember for tile exit
		setOnMap(false);
		rmView();
		
		place_remove_object(getPlace(), this);
		
		// Run the exit triggers.
		if (oldPlace)
		{
			triggerOnTileExit(oldPlace, oldX, oldY, 0);
		}
		
                if (isOpaque()) {
                        vmask_flush_all();
                }

		// Note: do NOT call setPlace(NULL) here. When the player party
		// object is removed from the map it still needs to "know" what
		// place the members are in.
	}
	endTurn();
	attachCamera(false);
	obj_dec_ref(this);
}

void Object::paint(int sx, int sy)
{
	struct sprite *sprite = getSprite();
	if (sprite) {
                int origFacing = sprite_get_facing(sprite);
                sprite_set_facing(sprite, facing);
                if (TimeStop
                    && isPlayerControlled()) {
                        sprite_paint(sprite, 
                                     sprite_frame + Session->time_stop_ticks, 
                                     sx, sy);
                } else {
                        sprite_paint(sprite, sprite_frame, sx, sy);
                }
                sprite_set_facing(sprite, origFacing);
        }
}

void Object::describe()
{
        assert(getObjectType()); // else implement this method in subclass
        getObjectType()->describe(this);
        if (!isVisible()) {
                log_continue(" (invisible)");
        }
        if (isSubmerged()) {
                log_continue(" (submerged)");
        }
}

void Object::examine()
{
        assert(getObjectType()); // else implement this method in subclass
        describe();

        //todo: dont have examiner to pass in to ifc
        if (getObjectType()->canXamine()) {
                log_end(":");
                getObjectType()->xamine(this, this);
                log_begin("");
        }
}

sound_t *Object::get_movement_sound()
{
        return NULL_SOUND;
}

class Object *Object::clone()
{
        // gmcnutt: added support for an optional quantity field for placed
        // objects.

        class Object *obj;

        obj = getObjectType()->createInstance();
        obj->setPlace(getPlace());
        obj->setX(getX());
        obj->setY(getY());

        // FIXME: should assign the new object a unique script tag

        return obj;
}

//////////////////////////////////////////////////

bool Object::isType(int classID) 
{
        return (classID == OBJECT_ID);
}
int Object::getType() 
{
        return OBJECT_ID;
}

Object::Object()
{
        type = NULL;
        setup();
}

Object::Object(class ObjectType * type) 
{
        this->type = type;
        setup();
}

void Object::setup()
{
        int i;

        clink     = NULL;
        turn_list = NULL;

        for (i = 0; i < OBJ_NUM_HOOKS; i++) {
                hook_list_init(&hooks[i]);
        }

        x               = -1;
        y               = -1;
        place           = NULL;
        selected        = false;
        destroyed       = false;
        tag             = 0;
        action_points   = 0; /* FIXME: assumes no debt */
        control_mode    = CONTROL_MODE_AUTO;
        camera_attached = false;
        hp              = 0;
        is_on_map       = false;
        conv            = NULL;
        view            = NULL;
        saved           = 0;
        handle          = 0;
        refcount        = 0;
        count           = 1;
        gob             = NULL;
        current_sprite  = NULL;
        opacity         = false;
        light           = 0;
        temporary       = false;
        forceEffect     = false;
        pclass          = PCLASS_NONE;
        ttl             = -1; // everlasting by default
        started         = false;
        facing          = SPRITE_DEF_FACING;
        ignoreTimeStop  = false;
        submerged       = false;
        portrait        = NULL;

        if (getObjectType() && ! getObjectType()->isVisible())
                visible = 0;
        else
                visible = 1;

        // Four is the typical max number of frames, so using more will not
        // help (it won't hurt either, sprites.c will ensure the frame is
        // within what the sprite will actually support).
        sprite_frame = rand() % 4;
}

Object::~Object()
{
        int i;

        if (refcount) {
                dbg("refcount=%d\n", refcount);
                assert(! refcount);
        }

        //dbg("destroying %d %08lx %s\n", refcount, this, getName());

        if (handle) {
                session_rm(Session, handle);
                handle = 0;
        }
        if (tag) {
                free(tag);
                tag = 0;
        }

        if (gob)
                gob_del(gob);

        if (getView()) {
                rmView();
                mapDestroyView(getView());
                setView(NULL);                
        }                

        if (conv) {
                conv_unref(conv);
        }

        // For each type of hook...
        for (i = 0; i < OBJ_NUM_HOOKS; i++) {

                // Shouldn't be destroying the object while one of its hook
                // lists is locked.
                assert(! hook_list_locked(&hooks[i]));

                // This is a hack to workaround a bug due to a design flaw. A
                // request has been logged to fix the design flaw [SF
                // 1568398]. It will be a "deep" fix, and I expect it to add
                // lots of new bugs, so I'm going to see if this relatively
                // easy change will get us by a bit longer.
                //
                // The bug is this: we land here in the process of a
                // session_del() call. Some of our effects have already been
                // destroyed. In hookForEach() it will inspect some of these
                // effects, not knowing that they are destroyed, and cause a
                // crash. So instead of using hookForEach() I'm going to
                // destroy the lists by hand right here without calling any
                // hook removal closures or anything like that.
                struct list *lptr;
                struct hook_list *hl;
                hl = &hooks[i];
                lptr = hook_list_first(hl);
                while (lptr != hook_list_end(hl)) {
                        hook_entry_t *he = outcast(lptr, hook_entry_t, list);
                        lptr = lptr->next;
                        list_remove(&he->list);
                        hook_entry_del(he);
                }
        }
}

int Object::getX()
{
        return x;
}

int Object::getY()
{
        return y;
}

struct place *Object::getPlace()
{
        return place;
}

struct sprite *Object::getSprite()
{
        if (current_sprite)
                return current_sprite;
        if (type)
                return type->getSprite();
        return NULL;
}

bool Object::isSelected()
{
        return selected;
}

enum layer Object::getLayer(void)
{
        // subtle: ~Being runs, calls ~Object, calls hookForEach to delete all
        // the hooks, but there's an invalid hook which gets it's rm closure
        // invoked. In that closure it calls kern-obj-is-being, which lands us
        // here instead of Being::getLayer() because of where we are in the
        // destructor chain, and Being's have no object type... hopefully since
        // the object is being destroyed it doesn't really matter what the
        // layer is.
        if (getObjectType())
                return getObjectType()->getLayer();
        else
                return null_layer;
}

const char *Object::getName(void)
{
        if (type)
                return type->getName();
        return "<no type>";
}

class ObjectType *Object::getObjectType()
{
        return type;
}

bool Object::isDestroyed()
{
        return destroyed;
}


void Object::setX(int x)
{
        this->x = x;
}

void Object::setY(int y)
{
        this->y = y;
}

void Object::changeX(int dx)
{
        this->x += dx;
}

void Object::changeY(int dy)
{
        this->y += dy;
}

void Object::setPlace(struct place *place)
{
        this->place = place;
}

void Object::select(bool val)
{
        if (val == isSelected())
                return;

        if (val) {
                mapSetSelected(this);
        } else if (isSelected()) {
                mapSetSelected(NULL);
        }

        selected = val;
        mapUpdateTile(getPlace(), getX(), getY());        
        //mapUpdate(0);
}

void Object::destroy()
{
        destroyed = true;
        if (isSelected())
                select(false);
        remove();
}

int Object::getLight()
{
        return light;
}

void Object::exec()
{
        startTurn();
        if (getObjectType()->canExec())
                getObjectType()->exec(this);
        endTurn(); // warn: might destroy this!
        Object::decrementTTL(this); // might destroy the object!
}

void Object::synchronize()
{
}

bool Object::isVisible()
{
        //return getObjectType()->isVisible();
        return visible > 0;
}

void Object::setVisible(bool val)
{
	if (val)
		visible++;
	else {
		visible--;
                if (visible < 0 && getName()) {
                        printf("%s: %d\n", getName(), visible);
                }
        }
}

bool Object::isSubmerged()
{
        return submerged;
}

void Object::setSubmerged(bool val)
{
        submerged = val;
}

bool Object::isShaded()
{
        return isSubmerged();
}

void Object::setOpacity(bool val)
{
        // If the opacity is changing then invalidate the view mask cache in
        // the surrounding area.
        if (val != opacity && isOnMap())
                vmask_invalidate(getPlace(), getX(), getY(), 1, 1);                

        opacity = val;
}

bool Object::isOpaque()
{
        return opacity;
}

bool Object::joinPlayer()
{
        return false;
}

int Object::getActivity()
{
        return 0;
}

int Object::getActionPointsPerTurn()
{
        int baseAP = (int)(getSpeed() * session_get_time_accel());

        // If 'Quicken' is in effect then give player-controlled objects bonus
        // action points per turn.
        if (Quicken > 0 && isPlayerControlled()) {
                return baseAP * 2;
        }

        // If 'TimeStop' is in effect then give action points ONLY to
        // player-controlled objects.
        if (TimeStop && ! isPlayerControlled()) {
                return 0;
        }

        return baseAP;
}

void Object::applyEffect(closure_t *effect)
{
        closure_exec(effect, "p", this);
}

void Object::burn()
{
}

void Object::sleep()
{
}

sound_t *Object::getDamageSound()
{
        return NULL_SOUND;
}

void Object::damage(int amount)
{
        // Paint the red "*" damage symbol over the character's icon on the map
        if (isOnMap()) {
                mapPaintDamage(getX(), getY());        
                sound_play(getDamageSound(), SOUND_MAX_VOLUME);
        }

        runHook(OBJ_HOOK_DAMAGE, 0);
}

void Object::inflictDamage(int amount, class Character *attacker)
{
    damage(amount);
}

int Object::getActionPoints()
{
        return action_points;
}

void Object::decActionPoints(int points)
{
        setActionPoints(action_points - points);
}

void Object::endTurn()
{
	if (action_points > 0)
	{
		setActionPoints(0);
	}
}

void Object::startTurn()
{
        setActionPoints(action_points + getActionPointsPerTurn());
        runHook(OBJ_HOOK_START_OF_TURN, 0);
}

int Object::getSpeed()
{
        return getObjectType()->getSpeed();
}

int Object::getRequiredActionPoints()
{
        return getObjectType()->getRequiredActionPoints();
}

bool Object::isOnMap()
{
        return is_on_map;
}

bool Object::isDead()
{
        return false;
}

enum control_mode Object::getControlMode()
{
        return control_mode;
}

void Object::setControlMode(enum control_mode mode)
{
    control_mode = mode;
}

void Object::attachCamera(bool val)
{
        if (camera_attached == val)
                return;

        camera_attached = val;

        if (val)
                mapAttachCamera(this);
        else
                mapDetachCamera(this);
}

bool Object::isCameraAttached()
{
        return camera_attached;
}

bool Object::isTurnEnded()
{
        return (getActionPoints() <= 0 ||
                isDead() ||
                Quit);
}

bool Object::isPlayerPartyMember()
{
        return false;
}

bool Object::addToInventory(class Object *object)
{
        return false;
}

bool Object::hasInInventory(class ObjectType *object)
{
        return false;
}

void Object::heal(int amount)
{
        amount = min(amount, getMaxHp() - hp);
        hp += amount;
}

bool Object::isPlayerControlled()
{
        return false;
}

int Object::getMaxHp()
{
        return getObjectType()->getMaxHp();
}

int Object::getHp()
{
        return hp;
}

bool Object::isCompanionOf(class Object *other)
{
        return false;
}

int Object::getPclass()
{
        return pclass;
}

void Object::setPclass(int val)
{
        pclass = val;
}

int Object::getMovementCost(int pclass)
{        
        if (pclass == PCLASS_NONE)
                return 0;

        struct mmode *mmode = getMovementMode();
        if (! mmode)
                return PTABLE_IMPASSABLE;

        return ptable_get(session_ptable(), mmode->index, pclass);
}

bool Object::isPassable(int pclass)
{
        return (getMovementCost(pclass) != PTABLE_IMPASSABLE);
}

bool Object::putOnMap(struct place *new_place, int new_x, int new_y, int r,
                      int flags)
{
        // --------------------------------------------------------------------
        // Put an object on a map. If possible, put it at (new_x, new_y). If
        // that's not possible then put it at some other (x, y) such that:
        // 
        // o (x, y) is with radius r of (new_x, new_y)
        //
        // o The object can find a path from (x, y) to (new_x, new_y)
        //
        // If no such (x, y) exists then return false without placing the
        // object.
        // --------------------------------------------------------------------

        char *visited;
        char *queued;
        bool ret = false;
        int i;
        int rx;
        int ry;
        int *q_x;
        int *q_y;
        int index;
        int q_head;
        int q_tail;
        int q_size;
        int x_offsets[] = { -1, 1, 0, 0 };
        int y_offsets[] = { 0, 0, -1, 1 };
        
        printf("Putting %s near (%d %d)\n", getName(), new_x, new_y);

        // --------------------------------------------------------------------
        // Although the caller specified a radius, internally I use a bounding
        // box. Assign the upper left corner in place coordinates. I don't
        // *think* I have to worry about wrapping coordinates because all the
        // place_* methods should do that internally.
        // --------------------------------------------------------------------

        rx = new_x - (r / 2);
        ry = new_y - (r / 2);

        // --------------------------------------------------------------------
        // Initialize the 'visited' table and the coordinate search queues. The
        // queues must be large enough to hold all the tiles in the bounding
        // box, plus an extra ring around it. The extra ring is for the case
        // where we enqueue the neighbors of tiles that are right on the edge
        // of the bounding box. We won't know the neighbors are bad until we
        // pop them off the queue and check them.
        // --------------------------------------------------------------------

        q_size = r * r;

        visited = (char*)calloc(sizeof(char), q_size);
        if (NULL == visited)
                return false;
                
        q_x = (int*)calloc(sizeof(int), q_size);
        if (NULL == q_x) {
                goto free_visited;
        }

        q_y = (int*)calloc(sizeof(int), q_size);
        if (NULL == q_y) {
                goto free_q_x;
        }

        queued = (char*)calloc(sizeof(char), q_size);
        if (NULL == queued)
                goto free_q_y;

        // --------------------------------------------------------------------
        // Enqueue the preferred location to start the search.
        // --------------------------------------------------------------------

#define INDEX(x,y) (((y)-ry) * r + ((x)-rx))

        q_head        = 0;
        q_tail        = 0;
        q_x[q_tail]   = new_x;
        q_y[q_tail]   = new_y;
        index         = INDEX(new_x, new_y);
        assert(index >= 0);
        assert(index < q_size);
        queued[index] = 1;
        q_tail++;

        // --------------------------------------------------------------------
        // Run through the search queue until it is exhausted or a safe
        // position has been found.
        // --------------------------------------------------------------------

        while (q_head != q_tail) {

                // ------------------------------------------------------------
                // Dequeue the next location to check.
                // ------------------------------------------------------------

                new_x = q_x[q_head];
                new_y = q_y[q_head];
                q_head++;

                printf("Checking (%d,%d)...", new_x, new_y);

                // ------------------------------------------------------------
                // Has the location already been visited? (If not then mark it
                // as visited now).
                // ------------------------------------------------------------
                
                index = INDEX(new_x, new_y);
                assert(index >= 0);
                assert(index < q_size);

                if (0 != visited[index]) {
                        printf("already checked\n");
                        continue;
                }
                visited[index] = 1;

                // ------------------------------------------------------------
                // Is the location off the map or impassable?
                // ------------------------------------------------------------
                
                if (place_off_map(new_place, new_x, new_y) ||
                    ! place_is_passable(new_place, new_x, new_y, this, flags)){
                        continue;
                }

                // ------------------------------------------------------------
                // Is the location occupied or hazardous?
                // ------------------------------------------------------------

                if ((! (flags & PFLAG_IGNOREBEINGS) &&
                     place_is_occupied(new_place, new_x, new_y)) ||
                    (! (flags & PFLAG_IGNOREHAZARDS) &&
                     place_is_hazardous(new_place, new_x, new_y))) {

                        printf("occupied or hazardous\n");

                        // ----------------------------------------------------
                        // This place is not suitable, but its neighbors might
                        // be. Put them on the queue.
                        // ----------------------------------------------------

                        for (i = 0; i < array_sz(x_offsets); i++) {

                                int neighbor_x;
                                int neighbor_y;
                                int neighbor_index;

                                neighbor_x = new_x + x_offsets[i];
                                neighbor_y = new_y + y_offsets[i];

                                // --------------------------------------------
                                // Is the neighbor outside the search radius?
                                // --------------------------------------------

                                if (neighbor_x < rx        ||
                                    neighbor_y < ry        ||
                                    neighbor_x >= (rx + r) ||
                                    neighbor_y >= (ry + r)) {
                                        continue;
                                }
                                
                                // --------------------------------------------
                                // Has the neighbor already been queued?
                                // --------------------------------------------

                                neighbor_index = INDEX(neighbor_x, neighbor_y);
                                assert(neighbor_index >= 0);
                                assert(neighbor_index < (q_size));

                                if (queued[neighbor_index])
                                        continue;

                                // --------------------------------------------
                                // Enqueue the neighbor
                                // --------------------------------------------

                                assert(q_tail < (q_size));

                                q_x[q_tail] = neighbor_x;
                                q_y[q_tail] = neighbor_y;
                                q_tail++;
                                queued[neighbor_index] = 1;
                        }
                        
                        continue;
                }

                // ------------------------------------------------------------
                // I've found a good spot, and I know that I can pathfind back
                // to the preferred location from here because of the manner in
                // which I found it.
                //
                // Note: we don't want to activate step triggers while doing
                // this. One example of why this is bad is if we get here by
                // coming through a moongate on a town map. If the moongate is
                // in a state where it leads to itself we could end up
                // re-entering it with the relocate call below if we allowed
                // stepping.
                // ------------------------------------------------------------

                printf("OK!\n");
                relocate(new_place, new_x, new_y, REL_NOSTEP, NULL);
                ret = true;

                goto done;
        }

        // --------------------------------------------------------------------
        // Didn't find anyplace suitable. Return false. If the caller wants to
        // force placement I'll leave it to their discretion.
        // --------------------------------------------------------------------

        printf("NO PLACE FOUND!\n");

 done:
        free(queued);
 free_q_y:
        free(q_y);
 free_q_x:
        free(q_x);
 free_visited:
        free(visited);

        return ret;

}

struct mview *Object::getView()
{
        return view;
}

int Object::getVisionRadius()
{
        return 0;
}

void Object::addView()
{
        if (NULL != getView()) {
                mapAddView(getView());
                updateView();
        }
}

void Object::rmView()
{
        if (NULL != getView()) {
                mapRmView(getView());
        }
}

void Object::updateView()
{
        assert(isOnMap());

        if (NULL != getView()) {
                mapCenterView(getView(), getX(), getY());
                mapSetRadius(getView(), min(getVisionRadius(), MAX_VISION_RADIUS));
                mapSetDirty();
        }
}

void Object::setView(struct mview *new_view)
{
        view = new_view;
}

void Object::changePlaceHook()
{
}

int Object::getDx()
{
        return dx;
}

int Object::getDy()
{
        return dy;
}
bool Object::canWanderTo(int newx, int newy)
{
        return true;
}
enum MoveResult Object::move(int dx, int dy)
{
        return NotApplicable;
}

void Object::save(struct save *save)
{
        // Create the object within a 'let' block
        save->enter(save, "(let ((kobj (kern-mk-obj %s %d\n", 
                    getObjectType()->getTag(), 
                    getCount());
        saveHooks(save);
        save->write(save, ")))\n");

        // Assign the tag.
        if (tag) {
                save->write(save, "(kern-tag '%s kobj)\n", tag);
        }

        // Save the gob binding.
        if (getGob()) {
                save->enter(save, "(bind kobj\n");
                gob_save(getGob(), save);
                save->exit(save, ")\n");
        }

        // Save time-to-live.
        if (getTTL() != -1) {
                save->write(save, "(kern-obj-set-ttl kobj %d)\n",
                            getTTL());
        }

        // Set the custom sprite.
        if (current_sprite) {
                save->enter(save, "(kern-obj-set-sprite kobj\n");
                sprite_save(current_sprite, save);
                save->exit(save, ")\n");
        }

        // Set the facing
        if (SPRITE_DEF_FACING != facing) {
                save->write(save, "(kern-obj-set-facing kobj %d)\n", facing);
        }

        // Set the ignore-time-stop flag
        if (ignoreTimeStop) {
                save->write(save, "(kern-obj-set-ignore-time-stop kobj #t)\n");
        }

        // Set the submerged flag
        if (submerged) {
                save->write(save, "(kern-obj-set-submerged kobj #t)\n");
        }

        // Close the 'let' block, returning kobj as the last thing evaluated.
        save->exit(save, "kobj)\n");

}


#define VALID_HOOK_ID(id) ((id) >= 0 && (id) < OBJ_NUM_HOOKS)

void Object::hookForEach(int hook_id, 
                         int (*cb)(struct hook_entry *entry, void *data),
                         void *data)
{
        struct list *elem;
        struct hook_list *hl;
        int locked;

        //dbg("hookForEach entry\n");

        assert(VALID_HOOK_ID(hook_id));
        hl = &hooks[hook_id];

        // Lock the hook list to prevent any removals or entry deletions while
        // we're running it.
        locked = hook_list_trylock(hl);

        elem = hook_list_first(hl);
        while (elem != hook_list_end(hl)) {
        
                hook_entry_t *entry;

                entry = outcast(elem, hook_entry_t, list);
                elem = elem->next;

                // Check if the entry is invalid. Invalid entries are entries
                // that somebody tried to remove while we had the hook list
                // locked. Since we have the lock, we can remove/delete them
                // now.
                if (hook_entry_is_invalid(entry)) {
                        if (locked) {
                                //dbg("hookForEach: delete %p\n", entry);
                                if (entry->effect->rm)
                                        closure_exec(entry->effect->rm, "lp", 
                                                     hook_entry_gob(entry), 
                                                     this);
                                list_remove(&entry->list);
                                hook_entry_del(entry);
                        }
                        continue;
                }

                // Invoke the callback on the hook entry... this is the part
                // that does anything interesting. If it returns non-zero then
                // we skip running the rest of the hooks.
                if (cb(entry, data))
                        break;
                        
                if (isDestroyed())
                        break;
        }

        hook_list_tryunlock(hl, locked);
        //dbg("hookForEach exit\n");

}

static int object_start_effect(struct hook_entry *entry, void *data)
{
        if (entry->effect->restart
            && ! hook_entry_started(entry)) {
                hook_entry_set_started(entry);
                closure_exec(entry->effect->restart, "lp", hook_entry_gob(entry),
                             data);
        }
        return 0;
}

void Object::start(void)
{
        int i;

        // Don't start effects multiple times.
        if (started) {
                return;
        }
        started = true;

        forceEffect = true;
        for (i = 0; i < OBJ_NUM_HOOKS; i++) {
                hookForEach(i, object_start_effect, this);
        }
        forceEffect = false;
}

struct add_hook_hook_data {
        struct effect *effect;
        char reject : 1;
};

int object_run_add_hook_hook(hook_entry_t *entry, void *data)
{
        struct add_hook_hook_data *context;
        context = (struct add_hook_hook_data *)data;
        if (entry->effect->exec &&
            closure_exec(entry->effect->exec, "lp", hook_entry_gob(entry),
                         context->effect))
                context->reject = 1;
        return context->reject;
}

int object_find_effect(hook_entry_t *entry, void *data)
{
        struct add_hook_hook_data *context;
        context = (struct add_hook_hook_data *)data;
        if (entry->effect == context->effect)
                context->reject = 1;
        return context->reject;
}

bool Object::addEffect(struct effect *effect, struct gob *gob)
{
        hook_entry_t *entry;
        struct add_hook_hook_data data;
        int hook_id = effect->hook_id;
        
        assert(VALID_HOOK_ID(effect->hook_id));

        // Hack: NPC's don't go through a keystroke handler. For these,
        // substitute the start-of-turn-hook for the keystroke-hook.
        if (effect->hook_id == OBJ_HOOK_KEYSTROKE &&
            ! isPlayerControlled())
                hook_id = OBJ_HOOK_START_OF_TURN;

        // Use the same data structure to search for the effect and to check
        // for countereffects.
        data.effect = effect;
        data.reject = 0;

        // For non-cumulative effects Check if the effect is already applied.
        if (! effect->cumulative) {
                hookForEach(hook_id, object_find_effect, &data);
                if (data.reject)
                        return false;
        }

        // If we're starting up the object then "force" effects to be applied,
        // without checking for immunities. This works around the
        // script-kernel-script recursion that will otherwise occur, and which
        // will make summoning creatures with native effects not work from the
        // script.
        if (! forceEffect) {

                // Run the add-hook entries to see if any of these will block
                // this new entry from being added. This is how immunities are
                // implemented, BTW.
                hookForEach(OBJ_HOOK_ADD_HOOK, object_run_add_hook_hook, 
                            &data);

                if (data.reject) {
                        return false;
                }
        }

        // Run the "apply" procedure of the effect if it has one.
        if (effect->apply) {
                closure_exec(effect->apply, "lp", gob? gob->p : NULL, this);
        }

        entry = hook_entry_new(effect, gob);
        hook_entry_set_started(entry);

        // Roll to see if the character detects the effect (it won't show up in
        // stats if not)
        if (dice_roll("1d20") > effect->detect_dc) {
                hook_entry_detect(entry);
        }

        hook_list_add(&hooks[hook_id], &entry->list);

        // gmcnutt: I saw a crash on reload because we ran through this code
        // before the player party was created in the new session, and the
        // status window tried to access it because of this next call. I don't
        // think we need to be updating status for every object, anyway, only
        // party members.
        if (isPlayerControlled()) {
                statusRepaint();
        }

        return true;
}

void Object::restoreEffect(struct effect *effect, struct gob *gob, int flags, 
                           clock_alarm_t expiration)
{
        hook_entry_t *entry;

        assert(VALID_HOOK_ID(effect->hook_id));

        // Note: do NOT run the "apply" procedure of the effect here (already
        // tried this - causes script recursion while loading which as we know
        // aborts the load prematurely). Instead the "restart" procedure will
        // be run as part of our start() method, called on all objects near the
        // end of session_load().

        entry = hook_entry_new(effect, gob);
        entry->flags = flags;
        entry->expiration = expiration;
        hook_list_add(&hooks[effect->hook_id], &entry->list);

}

struct object_run_hook_entry_data {
        Object *obj;
        const char *fmt;
        va_list args;
};

static int object_run_hook_entry(struct hook_entry *entry, void *data)
{
        struct object_run_hook_entry_data *info;
        info = (struct object_run_hook_entry_data *)data;

        if (entry->effect->exec)
                return closure_execlpv(entry->effect->exec, 
                                       hook_entry_gob(entry),
                                       info->obj,
                                       info->fmt, 
                                       info->args);
        return 0;
}

void Object::runHook(int hook_id, const char *fmt, ...)
{
        struct object_run_hook_entry_data data;

        data.obj = this;
        data.fmt = fmt;
        va_start(data.args, fmt);
        hookForEach(hook_id, object_run_hook_entry, &data);
        va_end(data.args);
}

void Object::saveHooks(struct save *save)
{
        int i;

        save->write(save, ";; hooks\n");
        save->enter(save, "(list\n");
        for (i = 0; i < OBJ_NUM_HOOKS; i++) {
                struct list *elem;
                hook_list_for_each(&hooks[i], elem) {
                        hook_entry_t *entry;
                        entry = outcast(elem, hook_entry_t, list);
                        hook_entry_save(entry, save);
                }
        }
        save->exit(save, ")\n");
}

bool Object::removeEffect(struct effect *effect)
{
        struct list *elem;
        struct hook_list *hl;
        int hook_id = effect->hook_id;

        assert(VALID_HOOK_ID(effect->hook_id));
        
        // Hack: NPC's don't go through a keystroke handler. For these,
        // substitute the start-of-turn-hook for the keystroke-hook.
        if (effect->hook_id == OBJ_HOOK_KEYSTROKE &&
            ! isPlayerControlled())
                hook_id = OBJ_HOOK_START_OF_TURN;

        hl = &hooks[hook_id];

        elem = hook_list_first(hl);
        while (elem != hook_list_end(hl)) {
                hook_entry_t *entry;

                entry = outcast(elem, hook_entry_t, list);
                elem = elem->next;

                if (hook_entry_is_invalid(entry))
                        // Already pending removal.
                        continue;

                if (effect == entry->effect) {

                        // If the hook list is locked we can't remove/delete
                        // the entry, but if we mark it invalid then the
                        // runHooks() method will eventually clean it up.
                        if (hook_list_locked(hl)) {
                                hook_entry_invalidate(entry);
                        } else {
                                if (entry->effect->rm)
                                        closure_exec(entry->effect->rm, "lp", 
                                                     hook_entry_gob(entry), 
                                                     this);
                                list_remove(&entry->list);
                                hook_entry_del(entry);
                        }

                        statusRepaint();

                        return true;
                }
        }

        return false;
}

int Object::getCount()
{
        return count;
}

void Object::setCount(int c)
{
        count = c;
}

bool ObjectType::isUsable()
{
        return (gifc_cap & GIFC_CAN_USE);
}

bool ObjectType::isReadyable()
{
        return isType(ARMS_TYPE_ID);
}

bool ObjectType::isMixable()
{
        return (gifc_cap & GIFC_CAN_MIX);
}

bool ObjectType::isCastable()
{
        return (gifc_cap & GIFC_CAN_CAST);
}

bool ObjectType::canExec()
{
        return (gifc_cap & GIFC_CAN_EXEC);
}

bool ObjectType::canStep()
{
        return (gifc_cap & GIFC_CAN_STEP);
}

bool ObjectType::canSense()
{
        return (gifc_cap & GIFC_CAN_SENSE);
}

bool ObjectType::canXamine()
{
        return (gifc_cap & GIFC_CAN_XAMINE);
}

bool ObjectType::canAttack()
{
        return (gifc_cap & GIFC_CAN_ATTACK);
}

bool ObjectType::canEnter()
{
        return (gifc_cap & GIFC_CAN_ENTER);
}

bool ObjectType::canGet()
{
        // Hack: arms types not converted over to use gifc's yet
        return (gifc_cap & GIFC_CAN_GET);
}

bool ObjectType::canBuy()
{
        return (gifc_cap & GIFC_CAN_BUY);
}

bool ObjectType::canSearch()
{
        return (gifc_cap & GIFC_CAN_SEARCH);
}

bool ObjectType::canOpen()
{
        return (gifc_cap & GIFC_CAN_OPEN);
}

bool ObjectType::canBump()
{
        return (gifc_cap & GIFC_CAN_BUMP);
}

int ObjectType::open(Object *obj, Object *opener)
{
        return closure_exec(gifc, "ypp", "open", obj, opener);
}

int ObjectType::bump(Object *obj, Object *bumper)
{
        return closure_exec(gifc, "ypp", "bump", obj, bumper);
}

bool ObjectType::canHandle()
{
        return (gifc_cap & GIFC_CAN_HANDLE);
}

int ObjectType::handle(Object *obj, Object *handler)
{
        return closure_exec(gifc, "ypp", "handle", obj, handler);
}

bool ObjectType::canHitLocation()
{
        return (gifc_cap & GIFC_CAN_HIT_LOCATION);
}

int ObjectType::hitLocation(Object *obj, Object *attacker, Object *target, struct place *place, int x, int y, int dam)
{
        return closure_exec(gifc, "yppppddd", "hit-loc", obj, attacker, target, place, x, y, dam);
}

int ObjectType::step(Object *obj, Object *stepper)
{
        return closure_exec(gifc, "ypp", "step", obj, stepper);
}

int ObjectType::sense(Object *obj, Object *stepper)
{
        return closure_exec(gifc, "ypp", "sense", obj, stepper);
}

int ObjectType::xamine(Object *obj, Object *xaminer)
{
        return closure_exec(gifc, "ypp", "xamine", obj, xaminer);
}

int ObjectType::attack(Object *obj, Object *stepper)
{
        return closure_exec(gifc, "ypp", "attack", obj, stepper);
}

bool ObjectType::canOnAttack()
{
        return (gifc_cap & GIFC_CAN_ON_ATTACK);
}

int ObjectType::onAttack(Object *obj, Object *stepper)
{
        return closure_exec(gifc, "yp", "on-attack", stepper);
}

int ObjectType::enter(Object *obj, Object *stepper)
{
        return closure_exec(gifc, "ypp", "enter", obj, stepper);
}

int ObjectType::exec(Object *obj)
{
        return closure_exec(gifc, "yp", "exec", obj);
}

int ObjectType::use(Object *user)
{
        return closure_exec(gifc, "ypp", "use", this, user);
}

int ObjectType::cast(Object *caster)
{
        return closure_exec(gifc, "yp", "cast", caster);
}

int ObjectType::get(Object *obj, Object *getter)
{
        return closure_exec(gifc, "ypp", "get", obj, getter);
}

int ObjectType::buy(Object *buyer, int q)
{
        return closure_exec(gifc, "ypd", "buy", buyer, q);
}

int ObjectType::search(Object *obj, Object *searcher)
{
        return closure_exec(gifc, "ypp", "search", obj, searcher);
}

closure_t *ObjectType::getGifc()
{
        return gifc;
}

void ObjectType::setGifc(closure_t *g, int cap)
{
        // out with the old
        if (gifc) {
                closure_unref(gifc);
                gifc = NULL;
                gifc_cap = 0;
        }

        // in with the new
        if (g) {
                closure_ref(g);
                gifc = g;
                gifc_cap = cap;
        }
}

void ObjectType::setGob(struct gob *g)
{
        if (gob) {
                gob_unref(gob);
                gob = 0;
        }

        if (g) {
                gob = g;
                gob_ref(g);
        }
}

struct gob * ObjectType::getGob()
{
        return gob;
}

bool ObjectType::hasDescribeHook()
{
        return (gifc_cap & GIFC_CAN_DESCRIBE);
}

void ObjectType::runDescribeHook(Object *obj)
{
        closure_exec(gifc, "ypd", "describe", obj, obj->getCount());
}

bool ObjectType::isQuestItem()
{
        return questItemFlag;
}

void ObjectType::setQuestItemFlag(bool val)
{
        questItemFlag = val;
}

struct mmode *ObjectType::getMovementMode()
{
   return movementMode;
}

void ObjectType::setMovementMode(struct mmode *mmode)
{
	movementMode = mmode;
}

/////////////////////////////////////////////////////////////////////////////////////////////
// Object

bool Object::add(ObjectType *type, int amount)
{
        return false; // subclasses will overload
}

bool Object::takeOut(ObjectType *type, int amount)
{
        return false; // subclasses will overload
}

bool Object::addFood(int amount)
{
        return false; // subclasses will overload
}

bool Object::addGold(int amount)
{
        return false; // subclasses will overload
}

void Object::setGob(struct gob *g)
{
        gob = g;
}

struct gob * Object::getGob()
{
        return gob;
}

void Object::setSprite(struct sprite *sprite)
{
        current_sprite = sprite;
}

void Object::step(Object *stepper)
{        
        if (! getObjectType() ||
            ! getObjectType()->canStep())
                return;

        getObjectType()->step(this, stepper);
}

void Object::sense(Object *stepper)
{        
        if (! getObjectType() ||
            ! getObjectType()->canSense())
                return;

        getObjectType()->sense(this, stepper);
}

void Object::attack(Object *stepper)
{        
        if (! getObjectType() ||
            ! getObjectType()->canAttack())
                return;

        getObjectType()->attack(this, stepper);
}

void Object::onAttack(Object *user)
{        
        if (! getObjectType() ||
            ! getObjectType()->canOnAttack())
                return;

        getObjectType()->onAttack(this, user);
}

struct conv *Object::getConversation()
{
        return conv;
}

void Object::setConversation(struct conv *val)
{
        // out with the old
        if (conv) {
                conv_unref(conv);
                conv = NULL;
        }

        // in with the new
        if (val) {
                conv_ref(val);
                conv = val;
        }
}

bool Object::canEnter()
{
        return (getObjectType() && getObjectType()->canEnter());
}

void Object::enter(Object *enterer)
{
        getObjectType()->enter(this, enterer);
}

bool Object::canStep()
{
        return (getObjectType() && getObjectType()->canStep());
}

bool Object::canSense()
{
        return (getObjectType() && getObjectType()->canSense());
}

void Object::setLight(int val)
{
        light = val;
        if (light < 0)
                light = 0;
}

bool Object::isTemporary()
{
        return temporary;
}

void Object::setTemporary(bool val)
{
        temporary = val;
}

struct mmode *Object::getMovementMode()
{
        return getObjectType()->getMovementMode();
}

void Object::setMovementMode(struct mmode *mmode)
{
        // nop
}

Object *Object::getSpeaker()
{
        return this;
}

void Object::resetActionPoints()
{
        setActionPoints(0);
}

void Object::setActionPoints(int amount)
{
        action_points = amount;
        if (isPlayerControlled()) {
                statusRepaint();
        }
}

void obj_inc_ref(Object *obj)
{
        obj->refcount++;
#if 0
        if (obj->getName() && ! strcmp(obj->getName(), "player party"
                                       /*"The Wanderer"*/)) {
                printf("obj_inc_ref: %d\n", obj->refcount);
        }
#endif
}

void obj_dec_ref(Object *obj)
{
        assert((obj)->refcount >= 0);
        (obj)->refcount--;
#if 0
        if (obj->getName() && ! strcmp(obj->getName(), "player party"
                                       /*"The Wanderer"*/)) {
                printf("obj_dec_ref: %d\n", obj->refcount);
        }
#endif
        if (! obj->refcount)
                delete obj;
}

int Object::getTTL(void) { return ttl; }

bool Object::surreptitiouslyRemove()
{
        // crasher fix: check for player party; this may be called on boot if
        // the load file has (kern-obj-set-ttl kobj 0). That scenario happens
        // when an object's ttl is expired but the player is still in LOS, and
        // then the player saves the game.

        if (!isOnMap()) {
                return false;
        }

        if (player_party
            && getPlace()==player_party->getPlace()
            && (place_flying_distance(player_party->getPlace(),
                                      player_party->getX(),
                                      player_party->getY(),
                                      getX(),
                                      getY())
                < player_party->getVisionRadius())
            && place_in_los(player_party->getPlace(),
                            player_party->getX(),
                            player_party->getY(),
                            getPlace(),
                            getX(),
                            getY())) {
                return false;
        }

        remove();
        return true;
}

void Object::setTTL(class Object *obj, int val)
{
        obj->ttl = val;
        if (!obj->ttl) {
                obj->surreptitiouslyRemove(); // may destroy obj!
        }
}

void Object::decrementTTL(class Object *obj)
{
        // don't decrement if everlasting
        if (-1==obj->getTTL())
                return;

        if (0==obj->getTTL()) {
                obj->surreptitiouslyRemove(); // may destroy obj!
                return;
        }

        obj->setTTL(obj, obj->getTTL() - 1); // may destroy obj!
}

bool Object::isStationary()
{
        return false;
}

bool Object::setFacing(int val)
{
	if (!sprite_can_face(getSprite(), val))
		return false;
	facing = val;
	return true;
}

int Object::getFacing()
{
	return facing;
}

bool Object::ignoresTimeStop()
{
        return ignoreTimeStop;
}

void Object::setIgnoreTimeStop(bool val)
{
        ignoreTimeStop = val;
}

struct sprite *Object::getPortrait()
{ 
        return portrait; 
}

void Object::setPortrait(struct sprite *sprite) 
{ 
        portrait = sprite; 
}
