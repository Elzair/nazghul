/* Copyright (c) 2002 Gordon McNutt */
#include "terrain.h"
#include "sprite.h"
#include "util.h"

#include <stdlib.h>

struct terrain *terrain_create(char *tag,
			       char *name,
			       unsigned int pmask,
			       struct sprite *sprite,
			       char glyph, int id, unsigned char alpha)
{
	struct terrain *terrain;

	CREATE(terrain, struct terrain, 0);

	terrain->tag = strdup(tag);
	terrain->name = strdup(name);
	terrain->pmask = pmask;
	terrain->sprite = sprite;
	terrain->glyph = glyph;
	terrain->id = id;
	terrain->alpha = alpha;

	return terrain;
}

void terrain_destroy(struct terrain *terrain)
{
	if (terrain->tag)
		free(terrain->tag);
	if (terrain->name)
		free(terrain->name);
	free(terrain);
}
