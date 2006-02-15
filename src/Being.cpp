#include "astar.h"
#include "factions.h"
#include "Being.h"
#include "map.h"
#include "place.h"
#include "session.h"

// USE_CACHED_PATH works but it can cause some strange-seeming behavior. If a
// new, better route opens than the cached path then the being won't find it,
// but will blindly follow the cached path. Unless pathfinding becomes a
// performance issue leave this off.
//
// Addendum: actually, we need it on, because otherwise the member will
// "thrash" when, for example, a portcullis remains closed: pathfinding around
// it, then pathfinding through it and trying to open it, then around it again,
// etc...
#ifndef USE_CACHED_PATH
# define USE_CACHED_PATH true
#endif

Being::Being()
{
        setDefaults();
}

Being::Being(class ObjectType *type)
        : Object(type)
{
        setDefaults();
}

void Being::setDefaults()
{
        name = NULL;
        cachedPath = NULL;
        cachedPathPlace = NULL;
        baseFaction = INVALID_FACTION;
        currentFaction = baseFaction;
}

Being::~Being()
{
        if (cachedPath)
                astar_path_destroy(cachedPath);
        if (name)
                free(name);
}

void Being::setBaseFaction(int faction)
{
        baseFaction = faction;
        setCurrentFaction(faction);
}

int Being::getBaseFaction()
{
        return baseFaction;
}

int Being::getCurrentFaction()
{
        return currentFaction;
}

enum layer Being::getLayer()
{
        return being_layer;
}

bool Being::pathfindTo(struct place *destplace, int destx, int desty)
{
        struct astar_search_info as_info;
        struct astar_node *pathPtr;

        //printf("Being::pathfindTo %s -> %s (%d, %d)\n", getName(), destplace->name, destx, desty);

        if (isStationary())
                return false;

        // For now, don't try to pathfind between places.
        if (destplace != getPlace()) {
                warn("%s in %s, can't pathfind to %s", getName(),
                     getPlace()->name, destplace->name);
                return false;
        }

        //dbg("%s pathfind from (%d %d) to (%d %d)\n", 
        //    getName(), getX(), getY(), destx, desty);

        // Check the cachedPath
        if (USE_CACHED_PATH && cachedPath) {

                //dbg("cachedPath: ");
                //dump_path(cachedPath);

                // If the cached path is for a different place then we can't
                // use it
                if (getPlace() != cachedPathPlace) {
                        //dbg("old place\n");
                        astar_path_destroy(cachedPath);
                        cachedPath = NULL;
                        cachedPathPlace = NULL;

                } else {

                        pathPtr = cachedPath;

                        // If the cached path does not start from the current
                        // coordinates then we can't use it.
                        if (pathPtr->x != getX() ||
                            pathPtr->y != getY()) {
                                //dbg("old start\n");
                                astar_path_destroy(cachedPath);
                                cachedPath = NULL;
                                cachedPathPlace = NULL;
                        } else {
                                //dbg("tracing\n");
                                // Trace down the path until it ends or hits
                                // the target
                                while (pathPtr && 
                                       (pathPtr->x != destx ||
                                        pathPtr->y != desty))
                                        pathPtr = pathPtr->next;
                                
                                // If this path is no good then destroy it,
                                // we'll have to get a new one.
                                if (! pathPtr) {
                                        //dbg("won't reach\n");
                                        astar_path_destroy(cachedPath);
                                        cachedPath = NULL;
                                        cachedPathPlace = NULL;
                                }
                        }
                }
        }

        // If we don't have a valid path then try to find one, first by
        // ignoring mechanisms.
        if (! USE_CACHED_PATH || ! cachedPath) {
                //dbg("searching\n");
                memset(&as_info, 0, sizeof (as_info));
                as_info.x0 = getX();
                as_info.y0 = getY();
                as_info.x1 = destx;
                as_info.y1 = desty;
                as_info.flags = PFLAG_IGNORECOMPANIONS|PFLAG_IGNOREMECHS;
                cachedPath = place_find_path(getPlace(), &as_info, this);
        }

        // If we still don't have a valid path then give up
        if (!cachedPath) {
                //dbg("none found\n");
                return false;
        }
        
        // If the path does not lead anywhere then we must be at our
        // destination, so we can destroy it and return.
        pathPtr = cachedPath->next;
        if (! pathPtr) {
                //dbg("already there\n");
                astar_path_destroy(cachedPath);
                cachedPath = NULL;
                cachedPathPlace = NULL;
                return true;
        }

        // Otherwise the path is good, so cache the place.
        cachedPathPlace = getPlace();
        //dbg("Found path: ");
        //dump_path(cachedPath);

        enum MoveResult result;
        result = move(pathPtr->x - getX(), 
                      pathPtr->y - getY());

        // If the move failed because something impassable is there then check
        // for a mech and try to handle it. This is good enough to get through
        // the usual implementation of a door.
        if (result == WasImpassable) {

                //dbg("impassable\n");
                class Object *mech;
                mech = place_get_object(getPlace(), 
                                        pathPtr->x, 
                                        pathPtr->y, 
                                        mech_layer);
                if (mech && mech->getObjectType()->canHandle()) {
                        //dbg("handling %s\n", mech->getName());
                        mech->getObjectType()->handle(mech, this);
                        mapSetDirty();
                        
                        // Now try and move again.
                        result = move(pathPtr->x - getX(), 
                                      pathPtr->y - getY());
                }

                if (WasImpassable == result) {

                        //dbg("still impassable\n");
                        // If the move was still impassable then try and find a
                        // path that avoids mechanisms. Destroy this path
                        // first.
                        astar_path_destroy(cachedPath);
                        cachedPath = NULL;
                        cachedPathPlace = NULL;
                        
                        // Redo the search
                        memset(&as_info, 0, sizeof (as_info));
                        as_info.x0 = getX();
                        as_info.y0 = getY();
                        as_info.x1 = destx;
                        as_info.y1 = desty;
                        as_info.flags = PFLAG_IGNORECOMPANIONS;
                        cachedPath = place_find_path(getPlace(), &as_info, 
                                                     this);
                        
                        // If we still don't have a valid path then give up
                        if (!cachedPath) {
                                //dbg("no path\n");
                                return false;
                        }
                        
                        //dbg("New path: ");
                        //dump_path(cachedPath);

                        // Otherwise the path is good, so cache the place.
                        cachedPathPlace = getPlace();
                        pathPtr = cachedPath->next;
                        
                        // Try to take the next step along the path.
                        result = move(pathPtr->x - getX(), 
                                      pathPtr->y - getY());
                        
                }
        }

        // If the move worked (as evidenced by the fact that our location
        // changed to the next node) then free the first node and make the next
        // node the head of the path so we can continue using it next turn.
        if (getX() == pathPtr->x &&
            getY() == pathPtr->y) {
                //dbg("ok\n");
                if (USE_CACHED_PATH) {
                        astar_node_destroy(cachedPath);
                        cachedPath = pathPtr;
                } else {
                        astar_path_destroy(cachedPath);
                        cachedPath = NULL;
                        cachedPathPlace = NULL;
                }
                return true;
        }

        return false;
}

char *Being::getName() 
{
        if (name)
                return name;
        return "<no name>";
}

void Being::setName(char *val) 
{
        if (val)
                name = strdup(val);
        else if (name) {
                free(name);
                name = NULL;
        }
}

void Being::setCurrentFaction(int faction)
{
        currentFaction = faction;
}
