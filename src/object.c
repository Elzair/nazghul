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
#include "object.h"
#include "util.h"
#include "place.h"
#include "character.h"
#include "map.h"
#include "sprite.h"
#include "screen.h"
#include "Loader.h"
#include "console.h"
#include "sound.h"
#include "player.h"
#include "terrain.h"
#include "Field.h"

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*****************************************************************************/

ObjectType::ObjectType()
{
        list_init(&this->list);
        speed                  = 0;
        required_action_points = 0;
        max_hp                 = 0;
}

ObjectType::~ObjectType()
{
	if (tag)
		free(tag);
	if (name)
		free(name);
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
	class Object *obj = new Object();
	if (obj)
		obj->init(this);
	return obj;
}

bool ObjectType::load(class Loader * loader)
{
	char *sprite_tag = 0;

	if (!loader->getWord(&tag) ||
	    !loader->matchToken('{') ||
	    !loader->matchWord("name") ||
	    !loader->getString(&name) ||
	    !loader->matchWord("sprite") || !loader->getWord(&sprite_tag))
		return false;

	sprite = (struct sprite *) loader->lookupTag(sprite_tag, SPRITE_ID);
	if (!sprite) {
		loader->setError("Invalid SPRITE tag '%s'", sprite_tag);
		free(sprite_tag);
		return false;
	}
	free(sprite_tag);

	// layer is set by subclass
	// '}' is parsed by subclass

	return true;
}

void ObjectType::describe(int count)
{
	char *name = getName();
	if (count == 1) {
		if (isvowel(name[0]))
			consolePrint("an ");
		else
			consolePrint("a ");
		consolePrint(name);
	} else {
		consolePrint("some ");
		consolePrint("%ss (%d)", name, count);
	}
}

bool ObjectType::isType(int classID) 
{
        return (classID == OBJECT_TYPE_ID);
}

int ObjectType::getType()
{
        return OBJECT_TYPE_ID;
}

char *ObjectType::getTag()
{
        return tag;
}

char *ObjectType::getName()
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

bool ObjectType::bindTags(class Loader * loader)
{
        return true;
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
/*****************************************************************************/

void Object::init(int x, int y, struct place *place, class ObjectType * type)
{
	this->type = type;
	setX(x);
	setY(y);
	container_link.key = type->getLayer();
	setPlace(place);
}

void Object::init(class ObjectType * type)
{
	container_link.key = type->getLayer();
	this->type = type;
}

void Object::relocate(struct place *newplace, int newx, int newy)
{
        int volume;
        int distance;
        struct place *foc_place;
        int foc_x, foc_y;

        if (!getPlace()) {
                setPlace(newplace);
                setX(newx);
                setY(newy);
                place_add_object(getPlace(), this);
                addView();
                changePlaceHook();
        } else if (newplace == getPlace()) {
                place_move_object(newplace, this, newx, newy);
                setX(newx);
                setY(newy);
        } else {
		place_remove_object(getPlace(), this);
                setPlace(newplace);
                setX(newx);
                setY(newy);
                place_add_object(getPlace(), this);
                changePlaceHook();
        }

        volume = SOUND_MAX_VOLUME;

        // ---------------------------------------------------------------------
        // Attenuate movement sound based on distance from the camera's focal
        // point.
        // ---------------------------------------------------------------------

        mapGetCameraFocus(&foc_place, &foc_x, &foc_y);
        if (foc_place == getPlace()) {
                distance = place_flying_distance(foc_place, foc_x, foc_y, getX(), getY());
                if (distance > 1)
                        volume /= (distance/2);
                soundPlay(get_movement_sound(), volume);
        }

        // ---------------------------------------------------------------------
        // If the camera is attached to this object then update it to focus on
        // the object's new location.
        // ---------------------------------------------------------------------

        if (isCameraAttached()) {
                mapCenterCamera(getX(), getY());
        }
                
        updateView();

}

void Object::remove()
{
	if (isOnMap()) {
		place_remove_object(getPlace(), this);
		setPlace(0);
                rmView();
	}
        endTurn();
        attachCamera(false);
}

bool Object::load(class Loader * loader)
{
	loader->getWord(&script_tag);

	if (!loader->getInt(&x) || 
            !loader->getInt(&y))
		return false;
	return true;
}

void Object::paint(int sx, int sy)
{
	struct sprite *sprite = getSprite();
	if (sprite)
		spritePaint(sprite, 0, sx, sy);
}

void Object::describe(int count)
{
        getObjectType()->describe(count);
        if (!isVisible())
                consolePrint(" (invisible)");
}

char *Object::get_movement_sound()
{
        return 0;
}

class Object *Object::clone()
{
        // gmcnutt: added support for an optional quantity field for placed
        // objects.

        class Object *obj;

        obj = getObjectType()->createInstance();
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
        setup();
}

Object::Object(class ObjectType * type) 
{
        this->type = type;
        setup();
}

void Object::setup()
{
        list_init(&this->container_link.list);
        list_init(&this->turn_list);

        x               = -1;
        y               = -1;
        place           = NULL;
        selected        = false;
        destroyed       = false;
        script_tag      = 0;
        action_points   = 0; /* FIXME: assumes no debt */
        control_mode    = CONTROL_MODE_AUTO;
        camera_attached = false;
        hp              = 0;
        is_on_map       = false;
        conv            = NULL;
        view            = NULL;
}

Object::~Object()
{
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
        return type->getSprite();
}

bool Object::isSelected()
{
        return selected;
}

enum layer Object::getLayer(void)
{
        return (enum layer) container_link.key;
}

char *Object::getName(void)
{
        return type->getName();
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
        is_on_map = (place != NULL);

        if (place &&
            0 == strcmp(place->name, "The Great Wild"))
                printf("Set %s to place %s\n", getName(), place ? place->name : "null");
}

void Object::select(bool val)
{
        selected = val;
        mapUpdate(0);
}

void Object::destroy()
{
        destroyed = true;
        remove();
}

int Object::getLight()
{
        return 0;
}

void Object::exec(struct exec_context *context)
{
}

void Object::synchronize()
{
}

bool Object::isVisible()
{
        return getObjectType()->isVisible();
}

bool Object::isShaded()
{
        return false;
}

bool Object::is_opaque()
{
        return false;
}

int Object::getAlignment()
{
        return 0;
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
        // ---------------------------------------------------------------------
        // If 'Quicken' is in effect then give player-controlled objects bonus
        // action points per turn.
        // ---------------------------------------------------------------------

        if (Quicken > 0 && isPlayerControlled()) {
                return getSpeed() * Quicken;
        }

        // ---------------------------------------------------------------------
        // If 'TimeStop' is in effect then give action points ONLY to
        // player-controlled objects.
        // ---------------------------------------------------------------------

        if (TimeStop && ! isPlayerControlled()) {
                return 0;
        }

        return getSpeed();
}

void Object::applyPerTurnEffects()
{
	struct terrain *terrain;
        class Field *field;
        int effects = 0;

        /* apply any pre-existing effects */
        applyExistingEffects();

        if (isDead())
                return;

        /* collect terrain effects */
	terrain = place_get_terrain(getPlace(), getX(), getY());
        effects |= terrain->effects;

        /* collect field effects */
        field = (class Field *)place_get_object(getPlace(), getX(),
                                                getY(), field_layer);
        if (field != NULL)
                effects |= field->getObjectType()->getEffects();

        /* apply the collected effects */
        if (effects & TERRAIN_BURN) {
                burn();
        }
        if (effects & TERRAIN_POISON) {
                poison();
        }
        if (effects & EFFECT_SLEEP) {
                sleep();
        }

}

void Object::burn()
{
}

void Object::poison()
{
}

void Object::sleep()
{
}

void Object::damage(int amount)
{
}

void Object::applyExistingEffects()
{
}

int Object::getActionPoints()
{
        return action_points;
}

void Object::decActionPoints(int points)
{
        action_points -= points;
}

void Object::endTurn()
{
        if (action_points > 0)
                action_points = 0;
}

void Object::startTurn()
{
        action_points += getActionPointsPerTurn();
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

bool Object::isHostile(int alignment)
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

void Object::charm(int alignment)
{

}

bool Object::isCharmed()
{
        return false;
}

bool Object::isNativelyHostile(int alignment)
{
        return false;
}

bool Object::isPlayerPartyMember()
{
        return false;
}

void Object::unCharm()
{

}

bool Object::addToInventory(class Object *object)
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

struct conv *Object::getConversation()
{
        return conv;
}

void Object::clearAlignment(int alignment)
{
}

int Object::getPmask(void)
{
        return 0;
}

bool Object::putOnMap(struct place *new_place, int new_x, int new_y, int r)
{
        // ---------------------------------------------------------------------
        // Put an object on a map. If possible, put it at (new_x, new_y). If
        // that's not possible then put it at some other (x, y) such that:
        // 
        // o (x, y) is with radius r of (new_x, new_y)
        //
        // o The object can find a path from (x, y) to (new_x, new_y)
        //
        // If no such (x, y) exists then return false without placing the
        // object.
        // ---------------------------------------------------------------------

        char *visited;
        bool ret = false;
        int i;
        int rx;
        int ry;
        int *q_x;
        int *q_y;
        int index;
        int q_head;
        int q_tail;
        int x_offsets[] = { -1, 1, 0, 0 };
        int y_offsets[] = { 0, 0, -1, 1 };
        
        printf("Putting %s near (%d %d)\n", getName(), new_x, new_y);

        // ---------------------------------------------------------------------
        // Althouth the caller specified a radius, internally I use a bounding
        // box. Assign the upper left corner in place coordinates. I don't
        // *think* I have to worry about wrapping coordinates because all the
        // place_* methods should do that internally.
        // ---------------------------------------------------------------------

        rx = new_x - (r / 2);
        ry = new_y - (r / 2);

        // ---------------------------------------------------------------------
        // Initialize the 'visited' table and the coordinate search queues.
        // ---------------------------------------------------------------------

        visited = (char*)calloc(sizeof(char), r * r);
        if (NULL == visited)
                return false;
        
        q_x = (int*)calloc(sizeof(int), r * r);
        if (NULL == q_x) {
                goto free_visited;
        }

        q_y = (int*)calloc(sizeof(int), r * r);
        if (NULL == q_y) {
                goto free_q_x;
        }

        // ---------------------------------------------------------------------
        // Enqueue the preferred location to start the search.
        // ---------------------------------------------------------------------

        q_head      = 0;
        q_tail      = 0;
        q_x[q_tail] = new_x;
        q_y[q_tail] = new_y;
        q_tail++;

        // ---------------------------------------------------------------------
        // Run through the search queue until it is exhausted or a safe
        // position has been found.
        // ---------------------------------------------------------------------

        while (q_head != q_tail) {

                // -------------------------------------------------------------
                // Dequeue the next location to check.
                // -------------------------------------------------------------

                new_x = q_x[q_head];
                new_y = q_y[q_head];
                q_head++;

                printf("Checking (%d,%d)...", new_x, new_y);

                // -------------------------------------------------------------
                // Is the location outside the search radius?
                // -------------------------------------------------------------

                if (new_x < rx        ||
                    new_y < ry        ||
                    new_x >= (rx + r) ||
                    new_y >= (ry + r)) {
                        printf("outside the radius\n");
                        continue;
                }

                // -------------------------------------------------------------
                // Has the location already been visited? (If not then mark it
                // as visited now).
                // -------------------------------------------------------------
                
                index = (new_y - ry) * r + (new_x - rx);
                if (0 != visited[index]) {
                        printf("already checked\n");
                        continue;
                }
                visited[index] = 1;

                // -------------------------------------------------------------
                // Is the location off the map or impassable?
                // -------------------------------------------------------------

                if (place_off_map(new_place, new_x, new_y) ||
                    ! place_is_passable(new_place, new_x, new_y, getPmask(), 0)) {
                        printf("off-map or impassable\n");
                        continue;
                }

                // -------------------------------------------------------------
                // Is the location occupied or hazardous?
                // -------------------------------------------------------------

                if (place_is_occupied(new_place, new_x, new_y) ||
                    place_is_hazardous(new_place, new_x, new_y)) {

                        printf("occupied or hazardous\n");

                        // -----------------------------------------------------
                        // This place is not suitable, but its neighbors might
                        // be. Put them on the queue.
                        // -----------------------------------------------------

                        for (i = 0; i < array_sz(x_offsets); i++) {
                                assert(q_tail < (r * r));
                                q_x[q_tail] = new_x + x_offsets[i];
                                q_y[q_tail] = new_y + y_offsets[i];
                                q_tail++;
                        }
                        
                        continue;
                }

                // -------------------------------------------------------------
                // I've found a good spot, and I know that I can pathfind back
                // to the preferred location from here because of the manner in
                // which I found it.
                //
                // REVISIT: Would relocate() work just as well in place of the
                //          following code?
                //
                // -------------------------------------------------------------

                printf("OK!\n");

#if 0
                setX(new_x);
                setY(new_y);
                setPlace(new_place);
                place_add_object(place, this);

                if (isPlayerControlled()) {
                        mapAddView(getView());
                        mapCenterView(getView(), getX(), getY());
                        mapSetRadius(getView(), min(getVisionRadius(), MAX_VISION_RADIUS));;
                        mapRecomputeLos(getView());
                }
#else
                relocate(new_place, new_x, new_y);
#endif
                ret = true;

                goto done;
        }

        // ---------------------------------------------------------------------
        // Didn't find anyplace suitable. Return false. If the caller wants to
        // force placement I'll leave it to their discretion.
        // ---------------------------------------------------------------------

        printf("NO PLACE FOUND!\n");

 done:
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
        if (NULL != getView()) {
                mapCenterView(getView(), getX(), getY());
                mapSetRadius(getView(), min(getVisionRadius(), MAX_VISION_RADIUS));
                mapRecomputeLos(getView());
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
