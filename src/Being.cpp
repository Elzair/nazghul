#include "Being.h"
#include "session.h"

Being::Being()
{
}

Being::Being(class ObjectType *type)
        : Object(type)
{
}

Being::~Being()
{
}

void Being::setBaseFaction(int faction)
{
        baseFaction = faction;
}

int Being::getBaseFaction()
{
        return baseFaction;
}

int Being::getCurrentFaction()
{
        return baseFaction;
}

enum layer Being::getLayer()
{
        return being_layer;
}

