/* Copyright (c) 2002 Gordon McNutt */
#include "los.h"

#include <stdlib.h>
#include <string.h>

#define los_NUMALGS sizeof(algs)/sizeof(algs[0])

extern int FLOODFILL_Init(struct los *los);
extern int ANGBAND_Init(struct los *los);

struct alg_list_entry {
	char *name;
	int (*init) (struct los * los);
} algs[] = {
	{
	"floodfill", FLOODFILL_Init}, {
"angband", ANGBAND_Init},};

struct los *los_create(char *name, int w, int h, int r)
{
	struct los *los;
	unsigned int i;

	/* Check if alg is supported */
	for (i = 0; i < los_NUMALGS; i++) {
		if (!strcmp(algs[i].name, name))
			break;
	}

	/* Check if alg was not found */
	if (i == los_NUMALGS)
		return 0;

	/* Allocate the "base class" */
	los = (struct los *) malloc(sizeof(struct los));
	if (!los)
		return 0;

	memset(los, 0, sizeof(struct los));

	los->w = w;
	los->h = h;
	los->r = r;

	/* Allocate the visibility buffer */
	los->vmask = (unsigned char *) malloc(w * h);
	if (!los->vmask)
		goto fail;

	/* Allocate the alpha buffer */
	los->alpha = (unsigned char *) malloc(w * h);
	if (!los->alpha)
		goto fail;

	/* Initialize the specific algorithm */
	if (algs[i].init(los) < 0)
		goto fail;

	return los;

      fail:
	los_destroy(los);
	return 0;

}

void los_destroy(struct los *los)
{
	if (los->destroy)
		los->destroy(los);
	if (los->vmask)
		free(los->vmask);
	if (los->alpha)
		free(los->alpha);
	free(los);
}
