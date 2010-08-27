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
#include "Party.h"
#include "dice.h"
#include "place.h"
#include "player.h"
#include "combat.h"
#include "species.h"
#include "occ.h"
#include "wind.h"
#include "vehicle.h"
#include "console.h"
#include "formation.h"
#include "ctrl.h"
#include "session.h"
#include "sched.h"
#include "log.h"
#include "event.h"
#include "cmd.h"
#include "cmdwin.h"
#include "factions.h"
#include "sprite.h"

#include <stdio.h>

#define DIRLOC(dir,place,coord) { \
        if ((dir) < 0) \
                (coord) = place_w((place)) - 1; \
        else if (! (dir)) \
                (coord) = place_w((place)) / 2; \
        else \
                (coord) = 0; \
}

// Convenience macro for iterating over party members:
#define FOR_EACH_MEMBER(e,c)                                               \
        for ((e) = node_next(&members), (c) = (class Character *)(e)->ptr; \
        (e) != &members;                                                   \
        (e) = node_next(e), (c) = (class Character *)(e)->ptr)


Party::Party()
        : vehicle(NULL)
          , size(0)
          , formation(NULL)
          , wandering(false)
          , ctrl(ctrl_party_ai)
{
	node_init(&members);
        memset(&pinfo, 0, sizeof(pinfo));
}

static bool party_remove_member(class Character *ch, void *data)
{
        ((Party*)data)->removeMember(ch);
        return false;
}

Party::~Party()
{
        /* Dereference all the members by removing them from the party. If no
         * other container references them then they will be automatically
         * destoyed. */
        forEachMember(party_remove_member, this);
        obj_dec_ref_safe(vehicle);
}

bool Party::isType(int classID)
{
        return (classID == PARTY_ID);
                return true;
        return Object::isType(classID);
}

int Party::getType()
{
        return PARTY_ID;
}

int Party::getVisionRadius()
{
        int maxVal=0;
        struct node *entry;
        class Character *member;

        FOR_EACH_MEMBER(entry, member) {
                if (!member->isDead()) {
                        int val  = member->getVisionRadius();
                        maxVal = max(val, maxVal);
                }
        }

        return maxVal;
}

int Party::getSize(void)
{
        return size;
}


bool Party::turn_vehicle(void)
{
	int cost = 0;

	// Three possible outcomes:
	// 
	// 1. We do NOT turn any vehicle, therefore we want to continue
	// processing the move() command which calls us and we do NOT want to
	// consume any turns (e.g., no vehicle, or horse turning north/south).
	// 
	// 2. We DO turn a vehicle, but it does not cost us anything (e.g.,
	// horse turning east/west).
	// 
	// 3. We do turn a vehicle and it cost us a turn (e.g., ship).

	if (!vehicle || !vehicle->turn(dx, dy, &cost) || !vehicle->mustTurn())
		return false;

        action_points -= cost;

	return true;
}

bool Party::attackPlayer(int dx, int dy)
{
        // Subtle: check if the player party is on the map. This
        // catches the case where the player has just engaged another
        // npc party in combat on this turn. I don't want this npc
        // party to move to that spot because then when the player
        // party exits combat they will be on top of this npc party.
        if (! player_party->isOnMap())
                return false;

        struct move_info info;
        struct combat_info cinfo;

        memset(&info, 0, sizeof(info));
        
         
		/* Check for a diagonal attack. */
		if (dx && dy)
		{
			int xgoodness=0;
			int ygoodness=0;
			if (place_is_passable(getPlace(), getX() + dx, getY(), 
					this, 0))
			{
				if (place_is_hazardous(getPlace(), getX() + dx, getY()))
				{
					xgoodness=1;
				}
				else
				{
					xgoodness=2;	
				}
			}
			if ((xgoodness<2) && place_is_passable(getPlace(), getX(), getY() + dy, 
				this, 0))
			{
				if (place_is_hazardous(getPlace(), getX() + dx, getY()))
				{
					ygoodness=1;
				}
				else
				{
					ygoodness=2;	
				}				
			}
			if (xgoodness && xgoodness > ygoodness)
			{
				//changing x is better				
				info.x = getX() + dx;
				info.y = getY();
				info.dx = 0;
				info.dy = dy;
			}
			else if (ygoodness)
			{
				//changing y is better	
				info.x = getX();
				info.y = getY() + dy;
				info.dy = 0;
				info.dx = dx;
			}
			else
			{
				//cant actually get there!
				return false;	
			}
		}
		else
		{
			info.x = getX();
			info.y = getY();
			info.dx = dx;
			info.dy = dy;
		}

        info.place = getPlace();
        info.px = player_party->getX();
        info.py = player_party->getY();
        info.npc_party = this;

        memset(&cinfo, 0, sizeof(cinfo));
        cinfo.defend = true;
        cinfo.move = &info;

        combat_enter(&cinfo);
        endTurn();
        return true;
}

MoveResult Party::move(int dx, int dy)
{
	struct place *newplace;
	int newx;
	int newy;
	struct place *oldplace;
	int oldx;
	int oldy;
	class Object *mech;

	this->dx = dx;
	this->dy = dy;

	/* Check if the party is in a vehicle that must turn its facing before
	 * moving */
	if (turn_vehicle())
		return ChangedFacing;

	/* Remember old (current) coordinates */
	oldplace = getPlace();
	oldx = getX();
	oldy = getY();

	/* Setup new coordinates */
	newx = place_wrap_x(oldplace, oldx + dx);
	newy = place_wrap_y(oldplace, oldy + dy);
	newplace = oldplace;

	/* Walking off the edge of a map */
	if (place_off_map(oldplace, newx, newy)) {
		return OffMap;
	}

	/* Check if the player is there. */
	if (newx == player_party->getX() && 
            newy == player_party->getY()) {

		/* If this party is hostile to the player then begin combat */
		if (are_hostile(this, player_party)
                    && attackPlayer(dx, dy)) {
                        return EngagedEnemy;

		}

                return WasOccupied;
	}

	/* Check if another entity is already there */
	if (place_is_occupied(oldplace, newx, newy)) {
		return WasOccupied;
	}

	/* Check for a vehicle. */
	class Vehicle *veh = place_get_vehicle(newplace, newx, newy);
	if (veh && (vehicle || veh->getOccupant())) {
		return WasOccupied;
	}

	/* Check passability */
	if (!place_is_passable(oldplace, newx, newy, this,
                               PFLAG_MOVEATTEMPT)) {
		return WasImpassable;
	}

        /* When wandering, don't wander over terrain hazards. When pathfinding,
         * assume that braving the hazard is the best course. */
	if (wandering && place_is_hazardous(newplace, newx, newy))
                return AvoidedHazard;


	// Check for a mech (not for passability, for sending the STEP
        // signal)

	mech = place_get_object(getPlace(), newx, newy, mech_layer);
        if (mech) {
                // Bugfix 1411788: avoid step triggers; dungeon step triggers
                // are relocating the NPC party into non-wilderness places,
                // corrupting them (note: the code here used to call
                // mech->step(this), not only did this invoke the step but it
                // was redundant, because the relocate() call checks for step
                // trigger mechs)
                return AvoidedHazard;
        }

	relocate(newplace, newx, newy);

	action_points -= place_get_diagonal_movement_cost(getPlace(), oldx, oldy, getX(), getY(), this,0);

        return MovedOk;
}

bool Party::gotoSpot(int mx, int my)
{
	// Common routine used by work() and commute().
	struct astar_node *path;
	struct astar_node *next;
	struct astar_search_info as_info;
	int dx;
	int dy;
        enum MoveResult ret = NotApplicable;

        if (isStationary())
                return StationaryObject;

	/* Look for a path. */
	memset(&as_info, 0, sizeof(as_info));
	as_info.x0 = getX();
	as_info.y0 = getY();
	as_info.x1 = mx;
	as_info.y1 = my;
	as_info.flags = PFLAG_IGNOREMECHS;
	path = place_find_path(Place, &as_info, this);

	if (!path)
		return false;

        //dump_path(path);

	/* The first node in the path is the starting location. Get the next
	 * step. */
	next = path->next;
	if (next) {

		/* Get the movement vector */
		dx = next->x - getX();
		dy = next->y - getY();

		/* Attempt to move */
		ret = move(dx, dy);
                        
	}

	/* Cleanup */
	astar_path_destroy(path);

	return (ret == MovedOk || ret == EngagedEnemy);
}

bool Party::attack_with_ordnance(int d)
{
	class ArmsType *ordnance;
	int dx, dy;
	bool ret;

	if (!vehicle || !(ordnance = vehicle->getOrdnance()))
		return false;

	// Check if the player is in range.
	if (d > ordnance->getRange()) {
		return false;
	}
	// Get the normalized vector to the player.
        place_get_direction_vector(getPlace(),
                                   getX(), getY(), 
                                   player_party->getX(), player_party->getY(),
                                   &dx, &dy);
	clamp(dx, -1, 1);
	clamp(dy, -1, 1);

	// Check if the player is on a major axes (assumes we must fire in a
	// straight line -- always true for now).
	if (player_party->getY() == getY()) {

		// If necessary, turn the vehicle to broadside the player (this
		// assumes we must use a broadside, again always true for now).
		// If we do turn return true to end this turn.
		if (vehicle->getFacing() != NORTH &&
		    vehicle->getFacing() != SOUTH) {
			int cost;
			vehicle->turn(0, 1, &cost);
			action_points -= cost;
			return true;
		}

		ret = vehicle->fire_weapon(dx, dy, this);
		assert(ret);	// to remind me if I change some assumptions
		return true;
	}
	// Ditto but for the other axis.
	if (player_party->getX() == getX()) {
		if (vehicle->getFacing() != EAST &&
		    vehicle->getFacing() != WEST) {
			int cost;
			vehicle->turn(1, 0, &cost);
			return true;
		}
		ret = vehicle->fire_weapon(dx, dy, this);
		assert(ret);	// to remind me if I change some assumptions
		return true;
	}
	// In range but no lined up on an axis. For now just return and let out
	// strategy be to close with the player's ship. In future I might want
	// to try and go to find and move toward the nearest axis point.
	return false;
}


void Party::exec()
{        
        assert(!isDestroyed());

        startTurn();

        while (action_points > 0 && !isDestroyed()) {

                int initial_points = action_points;
                ctrl(this);
                /* If we didn't use any action points then we're idle and need
                 * to break to prevent an endless loop. */
                if (action_points == initial_points)
                        break;
	}

        endTurn();
        Object::decrementTTL(this); // might destroy this!
}


void Party::forEachMember(bool (*fx) (class Character *, void *), void *data)
{
	struct node *elem;

	elem = node_next(&members);
	while (elem != &members) {
		class Character *c;

		c = (class Character *)elem->ptr;
                elem = node_next(elem);

		if (fx(c, data))
			return;
	}
}


void Party::forEachReverseMember(bool (*fx) (class Character *, void *), void *data)
{
	struct node *elem;

	elem = node_prev(&members);
	while (elem != &members) {
		class Character *c;

		c = (class Character *)elem->ptr;
                elem = node_prev(elem);

		if (fx(c, data))
			return;
	}
}

static bool party_destroy_and_remove_member(class Character * c, void *data)
{
	class Party *party = (class Party *) data;

	c->destroy();
	party->removeMember(c);

	return false;
}

void Party::destroy()
{
        // Note: this is a case of destroying an object in a container:
	disembark();

	forEachMember(party_destroy_and_remove_member, this);
	assert(node_list_empty(&members));
	Object::destroy();	// removes it
}

void Party::removeMember(class Character * c)
{
        struct node *node;

        /* Convenienve pointer to node */
        node = c->plnode;
        
        /* Should be valid */
        assert(node);

        /* Unlink the node from the member list */
	node_remove(node);

        /* Break the link from the char back to the node */
        c->plnode = NULL;

        /* Break the link from the char back to the party */
	c->party = NULL;

        /* Reduce party size counter */
	size--;

        /* Release the node */
        node_unref(node);

        /* Release the char */
        obj_dec_ref(c);
}

bool Party::addMember(class Character * c)
{
        struct node *node;

        /* Add ref to char to prevent destruction */
        obj_inc_ref(c);

        /* Make a new list node for the member list */
        node = node_new(c);

        /* Link the new member in at the END of the list (otherwise, in the
         * case of the player party, the order shown in status gets screwed
         * up) */
        node_add_tail(&members, node);

        /* Point the member back to its node (for fast removal) */
        c->plnode = node;

        /* Point the member back to its party */
        c->party = this;

        /* Set the character's order in the party */
        c->setOrder(size);

        /* Increase the party size counter */
        size++;

        /* Make the member loyal to the party */
        c->setBaseFaction(getBaseFaction());

        return true;
}

static bool add_to_player_party(class Character * c, void *data)
{
	// Note: I'll leave the party set as-is. It does no harm and might do
	// some good later.  Note: The order will be forgotten (changed to
	// match player party order).

	if (!c->joinPlayer())
		assert(false);
	return false;
}

bool Party::joinPlayer(void)
{
	remove();
	forEachMember(add_to_player_party, 0);
	return true;
}

void Party::paint(int sx, int sy)
{
	if (vehicle)
		vehicle->paint(sx, sy);
	else
		Object::paint(sx, sy);
}

struct sprite *Party::getSprite()
{
	if (vehicle)
		return vehicle->getSprite();

	return Object::getSprite();
}

void Party::disembark()
{
	if (vehicle) {
		assert(getPlace());
		vehicle->setOccupant(0);
                if (!vehicle->isDestroyed()) {
                        vehicle->relocate(getPlace(), getX(), getY());
                }
                obj_dec_ref(vehicle);
		vehicle = NULL;
	}
}


int Party::getSpeed()
{
	if (vehicle)
		return vehicle->getSpeed();

        int minVal=255; // something big, whatever
        struct node *entry;
        class Character *member;

        FOR_EACH_MEMBER(entry, member) {
                if (!member->isDead()) {
                        int val  = member->getSpeed();
                        minVal = min(val, minVal);
                }
        }

        return minVal;

}

struct damage_member_info {
	int damage;
	bool any_alive;
};

static bool damage_member(class Character * member, void *data)
{
	struct damage_member_info *dm_info = (struct damage_member_info *) data;

	// apply damage
	member->damage(dm_info->damage);

	// check if dead and remove from party
	if (member->isDead()) {
		member->party->removeMember(member);
		return false;
	}
	// otherwise at least one still alive
	dm_info->any_alive = true;

	return false;
}

void Party::damage(int damage)
{
	struct damage_member_info dm_info;

        Object::damage(damage);

	// First apply damage to the vehicle. If the vehicle is destroyed then
	// destroy the party, too.
	if (vehicle) {
		vehicle->damage(damage);
                
                // If the vehicle was destroyed by the above damage, it has
                // already called destroy() on its occupants (that's us right
                // here!) and we've already disembarked. So there's really
                // nothing to do.
                return;

	}

	// Apply damage to all party members. If they all die then the party is
	// destroyed, too.
	dm_info.damage = damage;
	dm_info.any_alive = false;
	forEachMember(damage_member, &dm_info);
	if (!dm_info.any_alive) {
		destroy();
	}
}

void Party::distributeMembers()
{
        // -------------------------------------------------------------------
        // Emulate what I currently do for the player party.
        // -------------------------------------------------------------------

        // -------------------------------------------------------------------
        // The combat alg requires me to fill out a "position info" structure
        // based on the player party destination.
        // -------------------------------------------------------------------
        
        combat_fill_position_info(&pinfo, getPlace(), getX(), getY(), dx, dy, false);

        // -------------------------------------------------------------------
        // Set the party formation to a sane default.
        // -------------------------------------------------------------------

        if (NULL == pinfo.formation)
                pinfo.formation = formation_get_default();

        // -------------------------------------------------------------------
        // Party members must be placed such that they can pathfind back to the
        // party. This minimizes the chance of a party member getting stranded
        // (which in turn will strand the whole party in that place).
        // -------------------------------------------------------------------

        pinfo.find_party = true;

        // -------------------------------------------------------------------
        // Remove the party from the current place before distributing members.
        // -------------------------------------------------------------------

        remove();
        mapSetDirty();

        // -------------------------------------------------------------------
        // Use the combat algorithm to place each member. Currently this will
        // never fail, in the degenerate case all party members will end up
        // "stranded" on top of the destination tile.
        // -------------------------------------------------------------------

        forEachMember(combat_place_character, &pinfo);

}

struct formation *Party::get_formation()
{
	if (vehicle && vehicle->get_formation())
		return vehicle->get_formation();
	return formation;
}

void Party::describe()
{
        assert(Session->subject);
        const char *diplstr = diplomacy_string(this, Session->subject);
        if (isvowel(diplstr[0]))
                log_continue("an ");
        else
                log_continue("a ");

        log_continue("%s %s", diplstr, getName());

        if (vehicle) {
                log_continue(" in ");
                vehicle->describe();
        }
}

static bool member_examine(class Character *member, void *data)
{
        log_begin("");
        member->examine();
        log_end("");
        return false;
}

void Party::examine()
{
	describe();
        log_end(":");
	forEachMember(member_examine, this);
        log_begin("");
	//todo: more details?
}


static bool get_member_movement_sound(class Character * member, void *data)
{
        sound_t **sound = (sound_t **)data;
	*sound = member->get_movement_sound();
        return data != 0;
}


sound_t *Party::get_movement_sound()
{
        sound_t *sound = NULL_SOUND;

	if (vehicle)
		return vehicle->get_movement_sound();
        forEachMember(get_member_movement_sound, &sound);
        return sound;
}

static bool member_burn(class Character *member, void *data)
{
        member->burn();
        return false;
}

static bool member_sleep(class Character *member, void *data)
{
        member->sleep();
        return false;
}

void Party::burn()
{
        forEachMember(member_burn, NULL);
        if (allDead())
                destroy();
}

void Party::sleep()
{
        forEachMember(member_sleep, NULL);
        if (allDead())
                destroy();
}

static bool member_check_if_alive(class Character *member, void *data)
{
        if (!member->isDead()) {
                *((bool*)data) = false;
                return true;
        }
        return false;
}

bool Party::allDead()
{
        bool dead = true;
        forEachMember(member_check_if_alive, &dead);
        return dead;
}

void Party::switchOrder(class Character *ch1, class Character *ch2)
{
        int tmp;
        node_switch(ch1->plnode, ch2->plnode);
        tmp = ch1->getOrder();
        ch1->setOrder(ch2->getOrder());
        ch2->setOrder(tmp);
}

void Party::setPlace(struct place *place)
{
        struct node *entry;
        class Character *member;
        Object::setPlace(place);
        if (this!=player_party
            && !place_is_wilderness(place)) {
                warn("putting %s in non-wilderness %s\n",
                     getName(), place->name);
                assert(0);
        }
        FOR_EACH_MEMBER(entry, member) {
                member->setPlace(place);
        }
}

void Party::setX(int x)
{
        struct node *entry;
        class Character *member;
        Object::setX(x);
        FOR_EACH_MEMBER(entry, member) {
                member->setX(x);
        }
}

void Party::setY(int y)
{
        struct node *entry;
        class Character *member;
        Object::setY(y);
        FOR_EACH_MEMBER(entry, member) {
                member->setY(y);
        }
}

bool Party::addEffect(struct effect *effect, struct gob *gob)
{
        struct node *entry;
        class Character *member;
        bool result = false;

        // NOTE: in the future we'll probably want to distinguish between
        // start-of-char-turn and start-of-party-turn for characters. Also,
        // we'll want to specify if the hook should really apply to the party
        // object or to its members.
        FOR_EACH_MEMBER(entry, member)
                result = member->addEffect(effect, gob) || result;
        
        return result;
}

bool Party::removeEffect(struct effect *effect)
{
        struct node *entry;
        class Character *member;
        bool result = false;

        FOR_EACH_MEMBER(entry, member) {
                result = member->removeEffect(effect) || result;
        }
        
        return result;
}

void Party::startTurn()
{
        struct node *entry;
        class Character *member;

        Object::startTurn();
        if (isDestroyed())
                return;

        // NOTE: in the future we'll probably want to distinguish between
        // start-of-char-turn and start-of-party-turn for characters. Also, to
        // be authentic we really should iterate over this in proportion to the
        // map scale.
        FOR_EACH_MEMBER(entry, member)
                member->runHook(OBJ_HOOK_START_OF_TURN, 0);

        if (allDead())
                destroy();

}

void Party::applyEffect(closure_t *effect)
{
        struct node *entry;
        class Character *member;

        FOR_EACH_MEMBER(entry, member)
                member->applyEffect(effect);
}

void Party::save(struct save *save)
{
        struct node *entry;
        class Character *member;

        save->enter(save, "(let ((kparty (kern-mk-party)))\n");
        if (getName())
                save->write(save, "(kern-being-set-name kparty \"%s\")\n", getName());
        save->write(save, "(kern-obj-set-sprite kparty %s)\n", 
                    sprite_get_tag(getSprite()));
        save->write(save, "(kern-being-set-base-faction kparty %d)\n", getBaseFaction());
        if (vehicle) {
                save->enter(save, "(kern-party-set-vehicle kparty");
                vehicle->save(save);
                save->exit(save, ") ;; end kern-party-set-vehicle\n");
        }
        FOR_EACH_MEMBER(entry, member) {
                save->enter(save, "(kern-party-add-member kparty\n");
                member->save(save);
                save->exit(save, ") ;; end kern-party-add-member\n");
        }
        if (getTTL() != -1) {
                save->write(save, "(kern-obj-set-ttl kparty %d)\n", getTTL());
        }
        save->write(save, "kparty\n");
        save->exit(save, ") ;; end let\n");
}

static bool member_remove(class Character *member, void *data)
{
        member->remove();
        return false;
}

void Party::removeMembers()
{
        forEachMember(member_remove, NULL);
}

static bool memberStart(class Character *member, void *data)
{
        member->start();
        return false;
}

void Party::start()
{
        forEachMember(memberStart, NULL);
}

int Party::getMovementCost(int pclass)
{
        struct node *entry;
        class Character *member;
        int maxCost = 0;

        if (vehicle)
                return vehicle->getMovementCost(pclass);

        FOR_EACH_MEMBER(entry, member) {
                if (!member->isDead()) {
                        int cost = member->getMovementCost(pclass);
                        maxCost = max(cost, maxCost);
                }
        }

        return maxCost;
}

bool Party::isStationary()
{
        struct node *entry;
        class Character *member;

        FOR_EACH_MEMBER(entry, member) {
                if (!member->isDead()) {
                        if (member->isStationary())
                                return true;
                }
        }

        return false;
}

class Character *Party::getMemberByOrder(int order)
{
        struct node *entry;
        class Character *member;

        FOR_EACH_MEMBER(entry, member) {
                if (! order)
                        return member;
                order--;
        }

        return NULL;
}

Object *Party::getSpeaker()
{
        struct node *entry;
        class Character *member;
        struct stat_list_entry *statlist;
        int list_sz = 0;
        class Character *selected = NULL;
        enum StatusMode orig_stat_mode;
	struct KeyHandler kh;
	struct ScrollerContext sc;

        // Allocate an array of status list entries big enough for the entire
        // party (this is probably more than we need, but it's only temporary).
        statlist = (struct stat_list_entry*)
                calloc(getSize(), sizeof(struct stat_list_entry));
        assert(statlist);


        // For each party member that has a conversation, add it to the list.
        FOR_EACH_MEMBER(entry, member) {

            // Fix for crasher: 'conv' is a member of this object, so don't use
            // it as a temp variable!
            if (!member->getConversation()) {
                continue;
            }

                statlist[list_sz].sprite = member->getSprite();
                snprintf(statlist[list_sz].line1, STAT_LIST_CHARS_PER_LINE,
                         member->getName());
                statlist[list_sz].data = member;
                list_sz++;
        }

        // Remember the current stat mode so we can restore it.
        orig_stat_mode = statusGetMode();

        // Check if nobody has a conversation.
        if (! list_sz)
                goto done;

        // Check if only one has a conversation.
        if (list_sz == 1) {
                selected = (class Character*)statlist[0].data;
                goto done;
        }

        // The player has to choose. Poke the list into the status state.
        statusSetGenericList("Choose Speaker", list_sz, statlist);

        // Switch the status mode over to list selection.
        statusSetMode(GenericList);

        // Setup a keyhandler for handling the scrolling
	sc.selector  = Generic;
	sc.selection = NULL;
	kh.fx        = scroller;
	kh.data      = &sc;

        // Push the handler and wait for the player to make a selection.
	eventPushKeyHandler(&kh);
	cmdwin_spush("<select>");
	eventHandle();
	cmdwin_pop();
	eventPopKeyHandler();

	statusRepaint();

	selected = (class Character *) sc.selection;
        
 done:
        statusSetMode(orig_stat_mode);
        free(statlist);
        return selected;
}

void Party::setVehicle(class Vehicle *val)
{
        if (vehicle) {
                vehicle->setOccupant(0);
                obj_dec_ref(vehicle);
        }
        vehicle=val;
        if (val) {
                val->setOccupant(this);
                obj_inc_ref(val);
        }
}

class Vehicle *Party::getVehicle()
{
        return vehicle;
}

static bool party_absorb_member_apdebt(class Character * c, void *data)
{
	int *maxapdebt = (int *) data;

	int apdebt = c->getActionPoints();
	if (apdebt < 0)
	{
		c->resetActionPoints();
		if (apdebt < *maxapdebt)
		{
			*maxapdebt = apdebt;	
		}	
	}

	return false;
}

void Party::absorbMemberAPDebt()
{
	int maxapdebt=0;
	forEachMember(party_absorb_member_apdebt, &maxapdebt);
	decActionPoints(0-maxapdebt/10); // 0-... because AP debt is negative
}

class Container *Party::getInventory()
{
        return NULL; // subclass(es) may override
}
