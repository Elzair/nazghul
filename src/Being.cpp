#include "Being.h"
#include "session.h"

Being::Being()
{
        factions = hstack_new();
}

Being::Being(class ObjectType *type)
        : Object(type)
{
        factions = hstack_new();
}

Being::~Being()
{
        clearFactions();
        hstack_del(factions);
}

int Being::pushFaction(int faction)
{
        return hstack_push(factions, (void*)faction);
}

void Being::popFaction()
{
        hstack_pop(factions);
}

void Being::rmFaction(int handle)
{
        hstack_rm(factions, handle);
}

int Being::getFaction()
{
        if (! hasFaction())
                return -1;

        return (int)hstack_top(factions);
}

int Being::setFaction(int faction)
{
        clearFactions();
        return hstack_push(factions, (void*)faction);
}

bool Being::hasFaction()
{
        return ! hstack_empty(factions);
}

bool Being::bottomFaction()
{
        return hstack_depth(factions) == 1;
}

static void being_save_faction_data(struct save *save, void *data)
{
        save->append(save, "%d ", (int)data);
}

void Being::saveFactions(struct save *save)
{
        save->write(save, ";; factions\n");
        save->write(save, "");
        hstack_save(factions, save, being_save_faction_data);
        save->write(save, "\n");
}

void Being::clearFactions()
{
        while (! hstack_empty(factions))
                hstack_pop(factions);
}

void Being::restoreFaction(int handle, int faction)
{
        hstack_restore(factions, (void*)faction, handle);
}
