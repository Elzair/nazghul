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
#include "place.h"
#include "portal.h"
#include "util.h"
#include "player.h"
#include "combat.h"
#include "Loader.h"
#include "species.h"
#include "occ.h"
#include "wind.h"
#include "Mech.h"
#include "vehicle.h"
#include "console.h"
#include "formation.h"
#include "ctrl.h"

#include <stdio.h>

#define DIRLOC(dir,place,coord) { \
        if ((dir) < 0) \
                (coord) = place_w((place)) - 1; \
        else if (! (dir)) \
                (coord) = place_w((place)) / 2; \
        else \
                (coord) = 0; \
}

// -----------------------------------------------------------------------------
//
// PartyType
//
// -----------------------------------------------------------------------------

PartyType::PartyType():formation(NULL), i_group(-1), n_groups(0), 
                             groups(NULL),
                             pmask(0), vrad(0)
{
        sleep_sprite = NULL;
}

PartyType::~PartyType()
{
	if (groups != NULL)
		delete groups;
}

bool PartyType::init(class Character * ch)
{
	char name[64];

	// Use the species and occupation as the name of the party.
	snprintf(name, sizeof(name), "%s %s", ch->species->name, 
                 ch->occ ? ch->occ->name : "");

	if (!ObjectType::init("fake_tag", name, being_layer, ch->getSprite()))
		return false;

	pmask = ch->getPmask();
	speed = ch->getSpeed();
	vrad = ch->getVisionRadius();
	visible = ch->isVisible();
        sleep_sprite = ch->species->sleep_sprite;
	return true;
}

struct GroupInfo *PartyType::enumerateGroups(void)
{
	i_group = -1;
	return getNextGroup();
}

struct GroupInfo *PartyType::getNextGroup(void)
{
	i_group++;
	if (i_group < n_groups)
		return &groups[i_group];
	return NULL;
}

class Object *PartyType::createInstance()
{
	class Party *obj = new Party();
	if (!obj)
                return NULL;

        obj->init(this);

	return obj;
}

static struct GroupInfo *load_group_info(class Loader * loader, int *n)
{
	char *sptag = 0, *octag = 0, *stag = 0;
	struct GroupInfo *groups = 0, tmp;
	int index;

	// base case
	if (loader->matchToken('}')) {
		groups = new struct GroupInfo[*n];
		if (!groups)
			loader->setError("Memory allocation failed");
		return groups;
	}
	// recursive case
	memset(&tmp, 0, sizeof(tmp));

	if (!loader->getWord(&sptag) ||
	    !loader->getWord(&octag) ||
	    !loader->getWord(&stag) || !loader->getInt(&tmp.n_max))
		return 0;

	tmp.species = (struct species *) loader->lookupTag(sptag, SPECIES_ID);
	if (!tmp.species) {
		loader->setError("Invalid SPECIES tag '%s'", sptag);
		goto done;
	}

	if (strcmp(octag, "null")) {
		tmp.occ = (struct occ *) loader->lookupTag(octag, OCC_ID);
		if (!tmp.occ) {
			loader->setError("Invalid OCC tag '%s'", octag);
			goto done;
		}
	}

	tmp.sprite = (struct sprite *) loader->lookupTag(stag, SPRITE_ID);
	if (!tmp.sprite) {
		loader->setError("Invalid SPRITE tag '%s'", stag);
		goto done;
	}

	index = *n;
	(*n)++;

	groups = load_group_info(loader, n);
	if (groups)
		groups[index] = tmp;

      done:
	if (sptag)
		free(sptag);
	if (octag)
		free(octag);
	if (stag)
		free(stag);

	return groups;

}

bool PartyType::load(class Loader * loader)
{
	char *tmp_tag = 0;

	if (!loader->getWord(&tag) ||
	    !loader->matchToken('{') ||
	    !loader->matchWord("name") ||
	    !loader->getString(&name) ||
	    !loader->matchWord("sprite") || !loader->getWord(&tmp_tag))
		return false;

	// sprite lookup
	sprite = (struct sprite *) loader->lookupTag(tmp_tag, SPRITE_ID);
	if (!sprite) {
		loader->setError("Invalid sprite tag '%s'", tmp_tag);
		free(tmp_tag);
		return false;
	}
	free(tmp_tag);

	// formation parse
	if (!loader->matchWord("formation") || !loader->getWord(&tmp_tag))
		return false;

	// formation lookup
	if (strcmp(tmp_tag, "null")) {
		formation = (struct formation *) loader->
		    lookupTag(tmp_tag, FORMATION_TYPE_ID);
		if (!formation) {
			loader->setError("Error loading %s: invalid FORMATION "
					 "tag %s", tag, tmp_tag);
			free(tmp_tag);
			return false;
		}
	}
	free(tmp_tag);

	if (!loader->matchWord("groups") || !loader->matchToken('{'))
		return false;

	groups = load_group_info(loader, &n_groups);
	if (!groups)
		return false;

	if (!ObjectType::init(tag, name, being_layer, sprite))
		return false;

	pmask = 0xFFFFFFFF;
	speed = MAX_SPEED;
	vrad = 0;

	// Default to invisible but if any species are visible then change it.
	visible = false;

	for (int i = 0; i < n_groups; i++) {

		// Passability is the intersection of the set of all group
		// passabilities
		pmask &= groups[i].species->pmask;

		// Speed is set by the slowest group
		speed = min(speed, groups[i].species->spd);

		// Vision radius is set by the farthest-seeing group
		vrad = max(vrad, groups[i].species->vr);

		// If all species are invisible then the whole party is
		// invisible
		if (groups[i].species->visible)
			visible = true;

                // Check if any of the groups have a sleep sprite
                if (!sleep_sprite && groups[i].species->sleep_sprite)
                        sleep_sprite = groups[i].species->sleep_sprite;
	}

	if (!loader->matchToken('}'))
		return false;

	return true;
}

bool PartyType::isType(int classID)
{
        return (classID == PARTY_TYPE_ID);
}

int PartyType::getType()
{
        return PARTY_TYPE_ID;
}

int PartyType::getPmask()
{
        return pmask;
}

int PartyType::getVisionRadius()
{
        return vrad;
}

bool PartyType::isVisible()
{
        return visible;
}


// -----------------------------------------------------------------------------
//
// Party
//
// -----------------------------------------------------------------------------


Party::Party()
{
	act = WORKING;
	alignment = 0;
	fdx = 0;
	fdy = 0;
	size = 0;
	isWrapper = false;
	list_init(&members);
        n_members = 0;
	vehicle = 0;
	formation = NULL;
        wandering = false;
        memset(&pinfo, 0, sizeof(pinfo));
        ctrl = ctrl_party_ai;
}

Party::~Party()
{
        printf("Destroying %s\n", getName());
}

bool Party::isType(int classID)
{
        if (classID == PARTY_ID)
                return true;
        return Object::isType(classID);
}

int Party::getType()
{
        return PARTY_ID;
}

class PartyType *Party::getObjectType()
{
        return (class PartyType *) Object::getObjectType();
}

int Party::getVisionRadius()
{
        return getObjectType()->getVisionRadius();
}

void Party::init(int x, int y, struct place *place, class PartyType * type)
{
        Object::init(x, y, place, type);
}

int Party::getAlignment()
{
        return alignment;
}

void Party::setAlignment(int val)
{
        alignment = val;
}

bool Party::isHostile(int alignment)
{
        return (!(this->alignment & alignment));
}

void Party::setFleeVector(int x, int y)
{
        fdx = x;
        fdy = y;
}

void Party::getFleeVector(int *x, int *y)
{
        *x = fdx;
        *y = fdy;
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

bool Party::enter_town(class Portal * portal)
{
	int newx, newy, max_i, dx2, dy2, i;
	struct place *newplace;

	// Cannot enter town in vehicles (disembark?)
	if (vehicle)
		return false;

	newplace = portal->getToPlace();

	switch (vector_to_dir(dx, dy)) {
	case SOUTH:
		newx = place_w(newplace) / 2;
		newy = 0;
		max_i = place_w(newplace);
		dy2 = 0;
		dx2 = 1;
		break;
	case NORTH:
		newx = place_w(newplace) / 2;
		newy = place_h(newplace) - 1;
		max_i = place_w(newplace);
		dy2 = 0;
		dx2 = 1;
		break;
	case WEST:
		newx = newx = place_w(newplace) - 1;
		newy = place_h(newplace) / 2;
		max_i = place_h(newplace);
		dy2 = 1;
		dx2 = 0;
		break;
	case EAST:
		newx = 0;
		newy = place_h(newplace) / 2;
		max_i = place_h(newplace);
		dy2 = 1;
		dx2 = 0;
		break;
	default:
		assert(false);
	}

	for (i = 0; i < max_i; i++) {

		if (!place_is_occupied(newplace, newx, newy) &&
		    place_is_passable(newplace, newx, newy, getPmask(), 0)) {
			relocate(newplace, newx, newy);
			consolePrint("%s enters %s\n", getName(),
				     newplace->name);
			return true;
		}

		newx += i * dx2;
		newy += i * dy2;

		dx2 *= -1;
		dy2 *= -1;
	}

	return false;
}

MoveResult Party::move(int dx, int dy)
{
	struct place *newplace;
	int newx;
	int newy;
	struct place *oldplace;
	int oldx;
	int oldy;
	class Moongate *moongate;
	class Portal *portal;
	class Mech *mech;

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

                // -------------------------------------------------------------
                // Subtle: check if the player party is on the map. This
                // catches the case where the player has just engaged another
                // npc party in combat on this turn. I don't want this npc
                // party to move to that spot because then when the player
                // party exits combat they will be on top of this npc party.
                // -------------------------------------------------------------
                
                if (! player_party->isOnMap())
                        return WasOccupied;

		/* If this party is hostile to the player then begin combat */
		if (isHostile(player_party->alignment)) {

			struct move_info info;
			struct combat_info cinfo;

			memset(&info, 0, sizeof(info));
			info.place = getPlace();
			info.x = newx;
			info.y = newy;
			info.dx = dx;
			info.dy = dy;
			info.npc_party = this;

			memset(&cinfo, 0, sizeof(cinfo));
			cinfo.defend = true;
			cinfo.move = &info;

			combat_enter(&cinfo);
                        endTurn();
                        return EngagedEnemy;
		}

                if (player_party->isResting()) {

                        /* If the player is sleeping then kick him out of
                         * bed */
                        player_party->throw_out_of_bed();

                }

                /* Else abort the move */
                return WasOccupied;

	}

	/* Check if another entity is already there */
	if (place_is_occupied(oldplace, newx, newy)) {
		return WasOccupied;
	}

	/* Check for a vehicle. */
	class Vehicle *veh = place_get_vehicle(newplace, newx, newy);
	if (veh && (vehicle || veh->occupant)) {
		return WasOccupied;
	}

	/* Check for an automatic portal (and avoid it) */
	portal = place_get_portal(newplace, newx, newy);
	if (portal && portal->isAutomatic()) {
		return AvoidedPortal;
	}

	/* Check for a moongate (and avoid it) */
	moongate = place_get_moongate(newplace, newx, newy);
	if (moongate && moongate->isOpen()) {
		return AvoidedPortal;
	}

	/* Check passability */
	if (!place_is_passable(oldplace, newx, newy, getPmask(),
			       act == COMMUTING ? PFLAG_IGNOREMECHS : 0)) {
		return WasImpassable;
	}

        /* When wandering, don't wander over terrain hazards. When pathfinding,
         * assume that braving the hazard is the best course. */
	if (wandering && place_is_hazardous(newplace, newx, newy))
                return AvoidedHazard;


	// Check for a mech (not for passability, for sending the STEP
        // signal)
	mech = (class Mech *) place_get_object(getPlace(), newx, newy, mech_layer);
	if (mech)
		mech->activate(MECH_STEP);

	relocate(newplace, newx, newy);

	action_points -= place_get_movement_cost(getPlace(), getX(), getY());

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

	/* Look for a path. */
	memset(&as_info, 0, sizeof(as_info));
	as_info.x0 = getX();
	as_info.y0 = getY();
	as_info.x1 = mx;
	as_info.y1 = my;
	as_info.flags = PFLAG_IGNOREMECHS;
	path = place_find_path(Place, &as_info, getPmask(), NULL);

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


void Party::exec(struct exec_context *cntxt)
{
        assert(!isDestroyed());

        action_points += getActionPointsPerTurn();
        if (action_points <= 0)
                return;

        while (action_points > 0 && !isDestroyed()) {

                int initial_points = action_points;
                ctrl(this);
                /* If we didn't use any action points then we're idle and need
                 * to break to prevent an endless loop. */
                if (action_points == initial_points)
                        break;
	}

        /* Objects cannot save action points (but they can be in debt). */
        if (action_points > 0)
                action_points = 0;
}

void Party::init(class PartyType * type)
{
	Object::init(type);
}

void Party::init(class Character * ch)
{
	// This is a "wrapper" party around a single character.
	PartyType *wtype = new PartyType();
	wtype->init(ch);
	Object::init(wtype);

	isWrapper = true;
	ch->setOrder(0);
	ch->party = this;
	list_add(&members, &ch->plist);
	size = 1;
	alignment = ch->getAlignment();
}

bool Party::createMembers(void)
{
	int order = 0;
	class PartyType *type = getObjectType();
	char name[64];
	static int instance = 0;

	for (struct GroupInfo * ginfo = type->enumerateGroups(); ginfo;
	     ginfo = type->getNextGroup()) {

		int n;

		// For each group in the party, randomly generate the number of
		// NPCs from that group based on its max size (need at least
		// one). Then create that many NPC characters and add them to
		// the party.
#define RANDOM_NUMBER_OF_MEMBERS true
		if (RANDOM_NUMBER_OF_MEMBERS) {
			n = (random() % ginfo->n_max) + 1;
		} else {
			n = ginfo->n_max;
		}

		n = max(n, 1);

		while (n) {

			// Create and initialize a new "stock" character.
			class Character *c = new class Character();
			if (!c)
				break;
			snprintf(name, sizeof(name), "%s %s %d",
				 ginfo->species->name,
				 ginfo->occ ? ginfo->occ->name : "",
				 instance++);
			if (!(c->initStock(ginfo->species, ginfo->occ,
					   ginfo->sprite, name, order,
					   alignment)))
				return false;

			list_add(&members, &c->plist);
			c->party = this;

			order++;
			size++;
			n--;
		}
	}

	return true;
}

bool Party::load(class Loader * loader)
{
	bool hflag;
	class Character *c;
	class PartyType *type = getObjectType();
        char *conv_tag;

	assert(type);

	if (!Object::load(loader) ||
	    !loader->getBitmask(&alignment) || !loader->getBool(&hflag))
		return false;

        // ---------------------------------------------------------------------
        // Parse the conversation. This is to support parties of cardboard-type
        // npc's who all have a common conversation.
        // ---------------------------------------------------------------------

        if (! isWrapper) {
                if (!loader->getWord(&conv_tag)) {
                        return false;
                }
                if (strcmp(conv_tag, "null")) {
                        conv = (struct conv *) loader->lookupTag(conv_tag, CONVERSATION_TYPE_ID);
                        if (NULL == conv) {
                                loader->setError("Invalid CONV tag '%s'", conv_tag);
                                free(conv_tag);
                                return false;
                        }
                }
                free(conv_tag);
        }

        // ---------------------------------------------------------------------
	// In the wilderness check for a vehicle type for the party.
        // ---------------------------------------------------------------------

	if (getPlace()->type == wilderness_place) {
		char *vehicle_type_tag = 0;
		class VehicleType *vehicle_type;

		if (!loader->getWord(&vehicle_type_tag))
			return false;

		if (strcmp(vehicle_type_tag, "null")) {
			vehicle_type = (class VehicleType *) loader->
			    lookupTag(vehicle_type_tag, VEHICLE_TYPE_ID);
			if (!vehicle_type) {
				loader->setError("Invalid vehicle type tag "
						 "'%s'", vehicle_type_tag);
				free(vehicle_type_tag);
				return false;
			}

			vehicle = (class Vehicle *) vehicle_type->
			    createInstance();
			vehicle->occupant = this;
		}

		free(vehicle_type_tag);
	}

        // ---------------------------------------------------------------------
        // Create the party members. If this is a wrapper there is only the one
        // party member and all we need is the alignment.
        // ---------------------------------------------------------------------

	if (isWrapper) {
		assert(!list_empty(&members));
		c = outcast(members.next, class Character, plist);
		c->setAlignment(alignment);
		return true;
	}

	return createMembers();
}

void Party::forEachMember(bool(*fx) (class Character *, void *), void *data)
{
	struct list *elem, *tmp;

	elem = members.next;
	while (elem != &members) {
		class Character *c;

		tmp = elem->next;
		c = outcast(elem, class Character, plist);
		elem = tmp;

		if (fx(c, data))
			return;
	}
}

static bool myDestroyMember(class Character * c, void *data)
{
	class Party *party = (class Party *) data;

	party->removeMember(c);
	c->destroy();
	delete c;

	return false;
}

void Party::destroy()
{
	disembark();
	Object::destroy();	// removes it
	forEachMember(myDestroyMember, this);
	assert(list_empty(&members));
}

static bool myCleanupMember(class Character * c, void *data)
{
	class Party *party = (class Party *) data;

	c->setCombat(false);

	if (c->isOnMap())
		c->remove();

	if (c->isDead()) {
		party->removeMember(c);
		c->destroy();
		delete c;
	} else {
		c->rejuvenate();
	}

	return false;
}

void Party::cleanupAfterCombat()
{
	forEachMember(myCleanupMember, this);
}

void Party::removeMember(class Character * c)
{
	list_remove(&c->plist);
	c->party = 0;
	size--;
}

bool Party::addMember(class Character * c)
{
        list_add(&members, &c->plist);
        c->party = this;
        size++;
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

int Party::getPmask()
{
	if (vehicle)
		return vehicle->getPmask();
	return getObjectType()->getPmask();
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

        if (act == SLEEPING && getObjectType()->sleep_sprite)
                return getObjectType()->sleep_sprite;

	return type->getSprite();
}

void Party::disembark()
{
	if (vehicle) {
		assert(getPlace());
		vehicle->occupant = 0;
		vehicle->relocate(getPlace(), getX(), getY());
		vehicle = 0;
	}
}

int Party::getSpeed()
{
	if (vehicle)
		return vehicle->getSpeed();
	return getObjectType()->getSpeed();
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
		delete member;
		return false;
	}
	// otherwise at least one still alive
	dm_info->any_alive = true;

	return false;
}

void Party::damage(int damage)
{
	struct damage_member_info dm_info;

	// First apply damage to the vehicle. If the vehicle is destroyed then
	// destroy the party, too.
	if (vehicle) {
		vehicle->damage(damage);
		if (vehicle->isDestroyed()) {
			delete vehicle;
			vehicle = NULL;
			destroy();
		}
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
        // ---------------------------------------------------------------------
        // Emulate what I currently do for the player party.
        // ---------------------------------------------------------------------

        // ---------------------------------------------------------------------
        // The combat alg requires me to fill out a "position info" structure
        // based on the player party destination.
        // ---------------------------------------------------------------------
        
        combat_fill_position_info(&pinfo, getPlace(), getX(), getY(), dx, dy, false);

        // ---------------------------------------------------------------------
        // Set the party formation to a sane default.
        // ---------------------------------------------------------------------

        if (NULL == pinfo.formation)
                pinfo.formation = formation_get_default();

        // ---------------------------------------------------------------------
        // Party members must be placed such that they can pathfind back to the
        // party. This minimizes the chance of a party member getting stranded
        // (which in turn will strand the whole party in that place).
        // ---------------------------------------------------------------------

        pinfo.find_party = true;

        // ---------------------------------------------------------------------
        // Remove the party from the current place before distributing members.
        // ---------------------------------------------------------------------

        remove();
        mapSetDirty();

        // ---------------------------------------------------------------------
        // Use the combat algorithm to place each member. Currently this will
        // never fail, in the degenerate case all party members will end up
        // "stranded" on top of the destination tile.
        // ---------------------------------------------------------------------

        forEachMember(combat_place_character, &pinfo);

}

void Party::relocate(struct place *place, int x, int y)
{
	class Mech *mech;

        // ---------------------------------------------------------------------
        // Do the standard relocation first.
        // ---------------------------------------------------------------------

	Object::relocate(place, x, y);

        // ---------------------------------------------------------------------
        // NPC parties can trigger mechs with STEP events...
        // ---------------------------------------------------------------------        

	mech = (class Mech *) place_get_object(place, x, y, mech_layer);
	if (mech)
		mech->activate(MECH_STEP);

        // ---------------------------------------------------------------------
        // If the place requires parties to break up into individual members
        // then I have to switch from party to character mode here. Otherwise
        // I'm done.
        // ---------------------------------------------------------------------

        if (place_is_wilderness(place))
                return;

        distributeMembers();

        // ---------------------------------------------------------------------
        // Automatically end turn on mode switch or we might end up trying to
        // exec this object again when it has no place defined.
        // ---------------------------------------------------------------------

        endTurn();
}

struct formation *Party::get_formation()
{
	if (vehicle && vehicle->get_formation())
		return vehicle->get_formation();
	return getObjectType()->formation;
}

void Party::describe(int count)
{
        Object::describe(count);
        if (vehicle) {
                consolePrint(" in ");
                vehicle->describe(1);
        }
}

static bool get_member_movement_sound(class Character * member, void *data)
{
        char **sound = (char **)data;
	*sound = member->get_movement_sound();
        return data != 0;
}


char *Party::get_movement_sound()
{
        char *sound = 0;

	if (vehicle)
		return vehicle->get_movement_sound();
        forEachMember(get_member_movement_sound, &sound);
        return sound;
}

int Party::getActivity()
{
        return act;
}

static bool member_burn(class Character *member, void *data)
{
        member->burn();
        return false;
}

static bool member_poison(class Character *member, void *data)
{
        member->poison();
        return false;
}

static bool member_sleep(class Character *member, void *data)
{
        member->sleep();
        return false;
}

static bool member_apply_existing(class Character * pm, void *data)
{
	if (pm->isPoisoned()) {
                pm->damage(DAMAGE_POISON);
	}
        if (pm->isAsleep()) {
                if ((random() % 100) < PROB_AWAKEN) {
                        pm->awaken();
                }
        }
	return false;
}

void Party::applyExistingEffects()
{
        forEachMember(member_apply_existing, this);
        if (allDead())
                destroy();
}


void Party::burn()
{
        forEachMember(member_burn, NULL);
        if (allDead())
                destroy();
}

void Party::poison()
{
        forEachMember(member_poison, NULL);
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
        list_switch(&ch1->plist, &ch2->plist);
        tmp = ch1->getOrder();
        ch1->setOrder(ch2->getOrder());
        ch2->setOrder(tmp);
}
