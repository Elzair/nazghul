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
#include "NpcParty.h"
#include "place.h"
#include "portal.h"
#include "util.h"
#include "player.h"
#include "combat.h"
#include "Loader.h"
#include "conv.h"
#include "species.h"
#include "occ.h"
#include "sched.h"
#include "wind.h"
#include "Mech.h"
#include "vehicle.h"
#include "console.h"

#include <stdio.h>

#define DIRLOC(dir,place,coord) { \
        if ((dir) < 0) \
                (coord) = place_w((place)) - 1; \
        else if (! (dir)) \
                (coord) = place_w((place)) / 2; \
        else \
                (coord) = 0; \
}

/*****************************************************************************/

NpcPartyType::NpcPartyType():i_group(-1), n_groups(0), groups(NULL),
pmask(0), vrad(0), speed(0)
{
}

NpcPartyType::~NpcPartyType()
{
	if (groups != NULL)
		delete groups;
}

bool NpcPartyType::init(class Character * ch)
{
	char name[64];

	// Use the species and occupation as the name of the party.
	snprintf(name, sizeof(name), "%s %s", ch->species->name, ch->occ->name);

	if (!ObjectType::init("fake_tag", name, being_layer, ch->getSprite()))
		return false;

	pmask = ch->getPmask();
	speed = ch->getSpeed();
	vrad = ch->getVisionRadius();
	visible = ch->isVisible();
	return true;
}

struct GroupInfo *NpcPartyType::enumerateGroups(void)
{
	i_group = -1;
	return getNextGroup();
}

struct GroupInfo *NpcPartyType::getNextGroup(void)
{
	i_group++;
	if (i_group < n_groups)
		return &groups[i_group];
	return NULL;
}

class Object *NpcPartyType::createInstance()
{
	class NpcParty *obj = new NpcParty();
	if (obj)
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

bool NpcPartyType::load(class Loader * loader)
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
	}

	if (!loader->matchToken('}'))
		return false;

	return true;
}

NpcParty::NpcParty()
{
	sched = 0;
	act = WORKING;
	alignment = 0;
	fdx = 0;
	fdy = 0;
	home = 0;
	size = 0;
	conv = 0;
	isWrapper = false;
	list_init(&members);
	vehicle = 0;
	formation = 0;
        destroy_on_combat_exit = true;
}

NpcParty::~NpcParty()
{
        printf("Destroying %s\n", getName());
}

bool NpcParty::turn_vehicle(void)
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

	cost *= getPlace()->scale;
	turn_cost += cost;

	return true;
}

bool NpcParty::enter_town(class Portal * portal)
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

void NpcParty::move(int dx, int dy)
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
		return;

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
		return;
	}

	/* Check if the player is there. */
	if (newx == player_party->getX() && newy == player_party->getY()) {

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

                        this->destroy_on_combat_exit = false;

			player_party->move_to_combat(&cinfo);
		}

		/* Else abort the move */
		return;
	}

	/* Check if another entity is already there */
	if (place_is_occupied(oldplace, newx, newy)) {
		return;
	}

	/* Check for a vehicle. */
	class Vehicle *veh = place_get_vehicle(newplace, newx, newy);
	if (veh && (vehicle || veh->occupant)) {
		return;
	}

	/* Check for an automatic portal (and avoid it) */
	portal = place_get_portal(newplace, newx, newy);
	if (portal && portal->isAutomatic()) {
		return;
	}

	/* Check for a moongate (and avoid it) */
	moongate = place_get_moongate(newplace, newx, newy);
	if (moongate && moongate->isOpen()) {
		return;
	}

	/* Check passability */
	if (!place_is_passable(oldplace, newx, newy, getPmask(),
			       act == COMMUTING ? PFLAG_IGNOREMECHS : 0)) {
		return;
	}

	/* Check for a mech (not for passability, for sending the STEP signal) */
	mech = (class Mech *) place_get_object(Place, newx, newy, mech_layer);
	if (mech)
		mech->activate(MECH_STEP);

	relocate(newplace, newx, newy);

	turn_cost = place_get_movement_cost(getPlace(), getX(), getY());
	turn_cost *= getSpeed();

}

void NpcParty::wander()
{
	int dx = 0, dy = 0;

	loitering = true;

	/* Roll for direction */
	dx = random() % 3 - 1;
	if (!dx)
		dy = random() % 3 - 1;

	if (dx || dy) {

		// If this party is on a schedule then limit wandering to the
		// area specied in the current appt.
		if (sched) {
			int newx, newy;
			newx = getX() + dx;
			newy = getY() + dy;
			if (newx < sched->appts[appt].x ||
			    newx > (sched->appts[appt].x +
				    sched->appts[appt].w - 1) ||
			    newy < sched->appts[appt].y ||
			    newy > (sched->appts[appt].y +
				    sched->appts[appt].h) - 1)
				return;
		}

		move(dx, dy);
	}
}

bool NpcParty::gotoSpot(int mx, int my)
{
	// Common routine used by work() and commute().
	struct astar_node *path;
	struct astar_node *next;
	struct astar_search_info as_info;
	int dx;
	int dy;

	/* Look for a path. */
	memset(&as_info, 0, sizeof(as_info));
	as_info.x0 = getX();
	as_info.y0 = getY();
	as_info.x1 = mx;
	as_info.y1 = my;
	as_info.flags = PFLAG_IGNOREMECHS;
	path = place_find_path(Place, &as_info, getPmask());

	if (!path)
		return false;

	/* The first node in the path is the starting location. Get the next
	 * step. */
	next = path->next;
	if (next) {

		/* Get the movement vector */
		dx = next->x - getX();
		dy = next->y - getY();

		/* Attempt to move */
		move(dx, dy);
	}

	/* Cleanup */
	astar_path_destroy(path);

	return true;
}

bool NpcParty::attack_with_ordnance(int d)
{
	class OrdnanceType *ordnance;
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
        printf("%s -> player: [%d %d]\n", getName(), dx, dy);

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
			turn_cost += cost;
			return true;
		}

		ret = vehicle->fire_weapon(dx, dy);
		turn_cost += 
                        place_adjust_turn_cost(getPlace(),
                                               TURNS_TO_FIRE_VEHICLE_WEAPON);
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
		ret = vehicle->fire_weapon(dx, dy);
		turn_cost += 
                        place_adjust_turn_cost(getPlace(),
                                               TURNS_TO_FIRE_VEHICLE_WEAPON);
		assert(ret);	// to remind me if I change some assumptions
		return true;
	}
	// In range but no lined up on an axis. For now just return and let out
	// strategy be to close with the player's ship. In future I might want
	// to try and go to find and move toward the nearest axis point.
	printf("%s: not lined up to fire\n", getName());
	return false;
}

void NpcParty::work()
{
	int d;

	loitering = false;

	if (getAlignment() & player_party->alignment) {
		// This party is friendly to the player, so just wander for now
		// (later I'll add schedules).
		wander();
		return;
	}

	/* Check if the player is on this map */
	if (Place != getPlace()) {
		wander();
		// Workaround: this is really to handle the case where
		// wandering can't consume any turns because the party is
		// stranded somewhere surrounded by impassable terrain.
		return;
	}

        /* Check if the player is _on this spot_. Yes, this can happen under
         * current game rules. If a player enters a portal and an npc is on the
         * destination then... */
	if (getX() == player_party->getX() && getY() == player_party->getY()) {
                
                struct move_info info;
                struct combat_info cinfo;

                memset(&info, 0, sizeof(info));
                info.place = getPlace();
                info.x = getX();
                info.y = getY();
                info.dx = dx;
                info.dy = dy;
                info.npc_party = this;
                
                if (!dx && !dy)
                        info.dx = 1;
                else if (dx && dy)
                        info.dy = 0;

                memset(&cinfo, 0, sizeof(cinfo));
                cinfo.defend = true;
                cinfo.move = &info;
                
                this->destroy_on_combat_exit = false;

                player_party->move_to_combat(&cinfo);
		return;
	}

        

	/* Check if the player is in visible range */
	d = place_walking_distance(Place,
				   getX(), getY(),
				   player_party->getX(), player_party->getY());

	if (d > getVisionRadius()) {
		wander();
		return;
	}

	if (d > 1 && attack_with_ordnance(d)) {
		return;
	}

	if (!gotoSpot(player_party->getX(), player_party->getY())) {
		wander();
		return;
	}
}

void NpcParty::commute()
{
	int tx, ty;

	tx = sched->appts[appt].x + sched->appts[appt].w / 2;
	ty = sched->appts[appt].y + sched->appts[appt].h / 2;

	if (gotoSpot(tx, ty) && getX() == tx && getY() == ty) {
		// Arrived.
		printf("%s done COMMUTING\n", getName());
		act = sched->appts[appt].act;
	}
}

void NpcParty::synchronize(int turn)
{
	int hr, min;

	setTurn(turn);

	if (!sched || sched->n_appts == 0)
		return;

	for (appt = 0; appt < sched->n_appts; appt++) {
		hr = sched->appts[appt].hr;
		min = sched->appts[appt].min;

		if (hr > Clock.hour || (Clock.hour == hr && min > Clock.min)) {
			break;
		}
	}

	// The loader must ensure that the first appt in every schedule starts
	// at hour zero.
	assert(appt);

	// Back up to the previous appt.
	appt--;

	relocate(getPlace(),
		 sched->appts[appt].x + sched->appts[appt].w / 2,
		 sched->appts[appt].y + sched->appts[appt].h / 2);

	act = sched->appts[appt].act;
}

void NpcParty::advanceTurn(int turn)
{

	turn_cost = 0;

	// If this npc is on a schedule then check if it's time to commute to
	// the next appointment.
	if (sched) {
		int nextAppt = appt + 1;

		// Special case: the last appointment of the day is over when
		// the clock rolls over at midnight. We can detect clock
		// rollover by checking if the time is BEFORE the start of the
		// current appt.
		if (nextAppt == sched->n_appts) {
			if (Clock.hour < sched->appts[appt].hr) {
				act = COMMUTING;
				appt = 0;
			}
		}
		// Normal case: check if the clock time exceeds the start time
		// of our next appt.
		else if (Clock.hour >= sched->appts[nextAppt].hr &&
			 Clock.min >= sched->appts[nextAppt].min) {
			act = COMMUTING;
			appt = nextAppt;
			printf("%s COMMUTING to appt %d.\n", getName(), appt);
		}
	}

	switch (act) {
	case WORKING:
		work();
		if (loitering)
			setTurn(turn);
		break;
	case COMMUTING:
		commute();
		break;
	case SLEEPING:
	case EATING:
        default:
		// For now do nothing.
		setTurn(turn);
		break;
	}

	// printf("%s used %d turns\n", getName(), turn_cost);
	changeTurn(turn_cost);
}

void NpcParty::init(class NpcPartyType * type)
{
	Object::init(type);
}

void NpcParty::init(class Character * ch)
{
	// This is a "wrapper" party around a single character.
	NpcPartyType *wtype = new NpcPartyType();
	wtype->init(ch);
	Object::init(wtype);

	isWrapper = true;
	ch->setOrder(0);
	ch->party = this;
	list_add(&members, &ch->plist);
	size = 1;
	conv = ch->conv;
	sched = ch->sched;
	alignment = ch->getAlignment();
}

bool NpcParty::createMembers(void)
{
	int order = 0;
	class NpcPartyType *type = getObjectType();
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
			// c = (class Character*)ginfo->type->createInstance();
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

bool NpcParty::load(class Loader * loader)
{
	bool hflag;
	class Character *c;
	char *conv_tag = 0;
	class NpcPartyType *type = getObjectType();

	assert(place);
	assert(type);

	// *** Load Party Parameters ***

	if (!Object::load(loader) ||
	    !loader->getBitmask(&alignment) || !loader->getBool(&hflag))
		return false;

	if (hflag)
		home = getPlace();

	// Note: conv may already be definied if this is a wrapper party. In
	// this case there should NOT be a conv tag.
	if (!isWrapper) {
		if (!loader->getWord(&conv_tag))
			return false;

		if (strcmp(conv_tag, "null")) {
			conv = (struct conv *) loader->lookupTag(conv_tag,
								 CONVERSATION_TYPE_ID);
			if (!conv) {
				loader->setError("Invalid CONV tag '%s'",
						 conv_tag);
				free(conv_tag);
				return false;
			}
		}

		free(conv_tag);
	}
	// In the wilderness check for a vehicle type for the party.
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
	// *** Create Party Members ***

	if (isWrapper) {

		// Only one party member and all we need is alignment.

		assert(!list_empty(&members));
		c = outcast(members.next, class Character, plist);
		c->setAlignment(alignment);
		return true;
	}

	return createMembers();
}

void NpcParty::forEachMember(bool(*fx) (class Character *, void *), void *data)
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
	class NpcParty *party = (class NpcParty *) data;

	party->removeMember(c);
	c->destroy();
	delete c;

	return false;
}

void NpcParty::destroy()
{
	disembark();
	Object::destroy();	// removes it
	forEachMember(myDestroyMember, this);
	assert(list_empty(&members));
}

static bool myCleanupMember(class Character * c, void *data)
{
	class NpcParty *party = (class NpcParty *) data;

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

void NpcParty::cleanupAfterCombat()
{
	forEachMember(myCleanupMember, this);
}

void NpcParty::removeMember(class Character * c)
{
	list_remove(&c->plist);
	c->party = 0;
	size--;
}

static bool add_to_player_party(class Character * c, void *data)
{
	// Note: I'll leave the party set as-is. It does no harm and might do
	// some good later.  Note: The order will be forgotten (changed to
	// match player party order).

	if (!player_party->add_to_party(c))
		assert(false);
	return false;
}

bool NpcParty::joinPlayer(void)
{
	if (player_party->get_room_in_party() < getSize())
		return false;
	remove();
	forEachMember(add_to_player_party, 0);
	return true;
}

struct conv *NpcParty::getConversation()
{
	return conv;
}

int NpcParty::getPmask()
{
	if (vehicle)
		return vehicle->getPmask();
	return getObjectType()->getPmask();
}

void NpcParty::paint(int sx, int sy)
{
	if (vehicle)
		vehicle->paint(sx, sy);
	else
		Object::paint(sx, sy);
}

void NpcParty::disembark()
{
	if (vehicle) {
		assert(getPlace());
		vehicle->occupant = 0;
		vehicle->relocate(getPlace(), getX(), getY());
		vehicle = 0;
	}
}

int NpcParty::getSpeed()
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
	member->changeHp(dm_info->damage);

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

void NpcParty::hit_by_ordnance(class OrdnanceType * ordnance)
{
	struct damage_member_info dm_info;

	// First apply damage to the vehicle. If the vehicle is destroyed then
	// destroy the party, too.
	if (vehicle) {
		vehicle->damage(ordnance->get_damage());
		if (vehicle->isDestroyed()) {
			delete vehicle;
			vehicle = NULL;
			destroy();
		}
		return;
	}
	// Apply damage to all party members. If they all die then the party is
	// destroyed, too.
	dm_info.damage = 0 - ordnance->get_damage();
	dm_info.any_alive = false;
	forEachMember(damage_member, &dm_info);
	if (!dm_info.any_alive) {
		printf("%s destroyed by %s\n", getName(), ordnance->getName());
		destroy();
	}
}

void NpcParty::relocate(struct place *place, int x, int y)
{
	class Mech *mech;

	Object::relocate(place, x, y);

	mech = (class Mech *) place_get_object(place, x, y, mech_layer);
	if (mech)
		mech->activate(MECH_STEP);

}

struct formation *NpcParty::get_formation()
{
	if (vehicle && vehicle->get_formation())
		return vehicle->get_formation();
	return getObjectType()->formation;
}

void NpcParty::describe(int count)
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


char *NpcParty::get_movement_sound()
{
        char *sound = 0;

	if (vehicle)
		return vehicle->get_movement_sound();
        forEachMember(get_member_movement_sound, &sound);
        return sound;
}
