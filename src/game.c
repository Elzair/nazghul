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
#define DEBUG
#include "debug.h"

#include "game.h"
#include "sprite.h"
#include "terrain.h"
#include "place.h"
#include "lexer.h"
#include "util.h"
#include "images.h"
#include "portal.h"
#include "NpcParty.h"
#include "common.h"
#include "player.h"
#include "sky.h"
#include "moongate.h"
#include "ascii.h"
#include "map.h"
#include "cursor.h"
#include "Arms.h"
#include "Field.h"
#include "Spell.h"
#include "Item.h"
#include "Trap.h"
#include "Loader.h"
#include "conv.h"
#include "occ.h"
#include "species.h"
#include "sched.h"
#include "Reagent.h"
#include "Mech.h"
#include "screen.h"
#include "vehicle.h"
#include "formation.h"
#include "combat.h"
#include "Container.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>              // isspace()
#include <SDL/SDL_image.h>
#include <unistd.h>
#include <sys/mman.h>
#include <stdarg.h>
#include <SDL/SDL.h>            // for SDL_GetTicks()

// Prototypes of functions declared 'static':
void *lookupTag(char *tag, int tid);
static struct terrain_palette *LTP_wrapper(class Loader * loader);


#define PARSE_INT(tag,var)         \
  if ((! MATCH_WORD((tag))) || (! MATCH(lexer_INT))) { \
    ret = -1; \
    goto cleanup; \
   } \
  (var) = atoi(lexer_lexeme(Lexer));

#define PARSE_CHAR(tag,var)         \
  if (! MATCH_WORD((tag))) { \
    ret = -1; \
    goto cleanup; \
   } \
  (var) = lexer_lex(Lexer); \

#define PARSE_STRING(tag,var)         \
  if ((! MATCH_WORD((tag))) || (! MATCH(lexer_STRING))) { \
    ret = -1; \
    goto cleanup; \
   } \
  if (0 == ((var) = strdup(lexer_lexeme(Lexer)))) { \
    ret = -1; \
    goto cleanup; \
  } \

#define PARSE_WORD(tag,var)         \
  if ((! MATCH_WORD((tag))) || (! MATCH(lexer_WORD))) { \
    ret = -1; \
    goto cleanup; \
   } \
  if (0 == ((var) = strdup(lexer_lexeme(Lexer)))) { \
    ret = -1; \
    goto cleanup; \
  } \

#define PARSE_TAG(tag) \
  if (0 == ((tag) = strdup(lexer_lexeme(Lexer)))) { \
    ret = -1; \
    goto cleanup; \
  }

#define PARSE_START_OF_BLOCK() \
  if (! MATCH('{')) { \
    ret = -1; \
    goto cleanup; \
  }

#define PARSE_END_OF_BLOCK() \
  if (! MATCH('}')) { \
    ret = -1; \
    goto cleanup; \
  }

void PARSE_ASSERT(bool expr, char *format, ...)
{
        if (expr)
                return;

        static char str[256];
        va_list ap;
        va_start(ap, format);
        vsnprintf(str, sizeof (str) - 1, format, ap);
        va_end(ap);
        fprintf(stderr, str);
        assert(expr);
}

static struct lexer *Lexer;
static struct list Images;
static struct list Terrains;
static struct list Maps;
static struct list Places;
static struct list ObjectTypes;
static struct list Conv;
static struct list Characters;
static struct list NpcPartyTypes;
static struct list Occs;
static struct list Species;
static struct list Schedules;
static struct list Objects;
static struct list Formations;

static bool frame_loaded;
static bool cursor_loaded;

static int MATCH(int token)
{
        if (lexer_lex(Lexer) != token) {
                err("line %d: Expected token %c, got %c '%s'", 
                    Lexer->line, token, lexer_token(Lexer), lexer_lexeme(Lexer));
                exit(-1);
        }
        return 1;
}

static int MATCH_WORD(char *word)
{
        MATCH(lexer_WORD);
        if (strcmp(lexer_lexeme(Lexer), word)) {
                err("line %d: Expected word '%s', got '%s'", 
                    Lexer->line, word, lexer_lexeme(Lexer));
                exit(-1);
        }
        return 1;
}

static int parseBitmap(char *field)
{
        int bmap = 0;

        if (!MATCH_WORD(field))
                return -1;

        if (!MATCH('('))
                return -1;

        lexer_lex(Lexer);

        if (Lexer->token == ')')
                return bmap;

        for (;;) {

                PARSE_ASSERT(Lexer->token == lexer_INT,
                             "Expected INT, got '%s'\n", Lexer->lexeme);
                bmap |= atoi(Lexer->lexeme);
                lexer_lex(Lexer);
                if (Lexer->token == ')')
                        return bmap;
                PARSE_ASSERT(Lexer->token == '|',
                             "Expected '|' or ')', got '%s'\n", Lexer->lexeme);
                lexer_lex(Lexer);
        }
}

/* Internal ******************************************************************/

#if 0
static struct images *game_load_images()
{
        int ret = 0;
        char *tag = 0;
        int w;
        int h;
        int rows;
        int cols;
        int offx;
        int offy;
        char *filename = 0;
        struct images *images = 0;

        if (!MATCH_WORD("images"))
                goto cleanup;
        MATCH(lexer_WORD);
        PARSE_TAG(tag);
        PARSE_START_OF_BLOCK();
        PARSE_INT("image_width", w);
        PARSE_INT("image_height", h);
        PARSE_INT("file_rows", rows);
        PARSE_INT("file_cols", cols);
        PARSE_INT("file_offx", offx);
        PARSE_INT("file_offy", offy);
        PARSE_STRING("file", filename);
        PARSE_END_OF_BLOCK();

        images = images_create(tag, w, h, rows, cols, offx, offy, filename);

      cleanup:
        if (filename)
                free(filename);
        if (tag)
                free(tag);

        return images;
}

static enum SpriteStyle parseSpriteStyle(void)
{
        char *tag = 0;
        int ret = 0;
        enum SpriteStyle style = NormalSprite;

        PARSE_WORD("style", tag);

        if (!strcmp(tag, "normal"))
                return NormalSprite;

        if (!strcmp(tag, "wave"))
                return WaveSprite;

        if (!strcmp(tag, "rotate"))
                return RotatedSprite;

        err("line %d: ignoring unknown sprite style '%s'", Lexer->line,
            Lexer->lexeme);
      cleanup:
        return style;
}

static struct sprite *game_load_sprite(struct images *images)
{
        struct sprite *sprite = 0;
        char *tag = 0;
        int n_frames;
        int index, n_facings, facings, i;
        enum SpriteStyle style;
        int ret = 0;

        PARSE_TAG(tag);
        MATCH('{');
        PARSE_INT("frames", n_frames);
        PARSE_INT("index", index);
        style = parseSpriteStyle();
        PARSE_INT("facings", facings);
        MATCH('}');

        // Calculate the number of facing sequences as the number of bits set
        // in the bitmap
        n_facings = 0;
        for (i = 0; i < NUM_PLANAR_DIRECTIONS; i++) {
                if (facings & (1 << i))
                        n_facings++;
        }

        // The sprite may not explicitly support different facings. Even so, it
        // always has one default sequence.
        if (n_facings == 0)
                n_facings = 1;

        sprite = spriteCreate(tag, n_frames, index, style, images, n_facings,
                              facings);
        if (!sprite) {
                err("Error allocating sprite");
                goto cleanup;
        }

      cleanup:
        if (tag)
                free(tag);
        return sprite;
}

static int game_load_sprites(struct images *images)
{
        int ret = 0;

        if (!MATCH_WORD("sprites")) {
                ret = -1;
                goto cleanup;
        }

        if (!MATCH('{'))
                return -1;

        while (lexer_lex(Lexer) != '}') {
                struct sprite *sprite;

                sprite = game_load_sprite(images);
                if (!sprite) {
                        err("Error parsing sprite");
                        return -1;
                }

                spriteAdd(sprite);

        }

        if (lexer_token(Lexer) != '}')
                return -1;
      cleanup:
        return ret;
}
#endif // 0

static struct terrain *game_terrain_lookup(char *tag)
{
        struct list *elem;

        list_for_each(&Terrains, elem) {
                struct terrain *terrain =
                    list_entry(elem, struct terrain, list);
                if (!strcmp(terrain->tag, tag)) {
                        return terrain;
                }
        }
        return 0;
}

static class OrdnanceType *lookupOrdnanceType(char *tag)
{
        struct list *elem;
        list_for_each(&OrdnanceTypes, elem) {
                class OrdnanceType *at;
                at = list_entry(elem, class OrdnanceType, list);
                if (!strcmp(at->getTag(), tag))
                        return at;
        }
        return 0;
}

static struct terrain_map *game_terrain_map_lookup(char *tag)
{
        struct list *elem;

        list_for_each(&Maps, elem) {
                struct terrain_map *terrain_map =
                    list_entry(elem, struct terrain_map, list);
                if (!strcmp(terrain_map->tag, tag)) {
                        return terrain_map;
                }
        }

        return 0;
}

static struct place *game_place_lookup(char *tag)
{
        struct list *elem;

        list_for_each(&Places, elem) {
                struct place *place = list_entry(elem, struct place, list);
                if (!strcmp(place->tag, tag)) {
                        return place;
                }
        }

        return 0;
}

class ObjectType *game_object_type_lookup(char *tag)
{
        struct list *elem;

        list_for_each(&ObjectTypes, elem) {
                class ObjectType *p;
                p = list_entry(elem, class ObjectType, list);
                if (!strcmp(p->getTag(), tag)) {
                        return p;
                }
        }

        return 0;
}

static class ObjectType *parseObjectType(char *field, int typeId)
{
        class ObjectType *type = NULL;
        char *tag = 0;
        int ret;

        PARSE_WORD(field, tag);
        if (tag && strcmp("null", tag)) {
                type = game_object_type_lookup(tag);
                PARSE_ASSERT(type != NULL, "Invalid object type tag '%s'", tag);
                PARSE_ASSERT(type->isType(typeId), "Object is wrong type");
        }

      cleanup:
        if (tag)
                free(tag);
        return type;
}

static struct sprite *parseSprite(char *field)
{
        struct sprite *sprite = NULL;
        char *tag = 0;
        int ret;

        PARSE_WORD(field, tag);
        if (tag && strcmp("null", tag)) {
                sprite = spriteLookup(tag);
                PARSE_ASSERT(sprite != NULL, "Invalid sprite tag '%s'", tag);
        }

      cleanup:
        if (tag)
                free(tag);
        return sprite;
}

static struct terrain *game_load_terrain(void)
{
        struct terrain *terrain = 0;
        char *tag = 0;
        char *name = 0;
        unsigned char passable;
        char *sprite_tag = 0;
        int id;
        int alpha;
        struct sprite *sprite = 0;
        int ret = 0;

        PARSE_TAG(tag);
        PARSE_START_OF_BLOCK();
        PARSE_STRING("name", name);
        PARSE_INT("pmask", passable);
        PARSE_WORD("sprite", sprite_tag);
        PARSE_INT("id", id);
        PARSE_INT("alpha", alpha);

        if (!(sprite = spriteLookup(sprite_tag))) {
                err("line %d: invalid sprite tag %s", Lexer->line, sprite_tag);
                goto cleanup;
        }
        terrain = terrain_create(tag, name, passable, sprite, id, alpha);

        PARSE_INT("movement_cost", terrain->movement_cost);
        PARSE_INT("effects", terrain->effects);
        PARSE_INT("light", terrain->light);

        // terrain->color = 0xFFFFFFFF;

        lexer_lex(Lexer);
        if (Lexer->token == lexer_WORD) {
                if (!strcmp(Lexer->lexeme, "color")) {
                        Uint8 red, grn, blu;
                        Uint32 color;
                        lexer_lex(Lexer);
                        PARSE_ASSERT(Lexer->token == lexer_INT,
                                     "Expected INT value for color, "
                                     "got %s\n", Lexer->lexeme);
                        sscanf(Lexer->lexeme, "0x%x", &color);
                        red = (color >> 16) & 0xFF;
                        grn = (color >> 8) & 0xFF;
                        blu = (color) & 0xFF;
                        terrain->color = screenMapRGB(red, grn, blu);
                }
                else {
                        PARSE_ASSERT(false,
                                     "Expected keyword 'color' or '}', got %s\n",
                                     Lexer->lexeme);
                }
                lexer_lex(Lexer);
        }

        PARSE_ASSERT(Lexer->token == '}', "Expected '}', got '%s'\n",
                     Lexer->lexeme);

      cleanup:
        if (sprite_tag)
                free(sprite_tag);
        if (tag)
                free(tag);
        return terrain;
}

static int game_load_Terrains()
{
        if (!MATCH('{'))
                return -1;

        while (lexer_lex(Lexer) != '}') {
                struct terrain *terrain;

                terrain = game_load_terrain();
                if (!terrain)
                        return -1;

                list_add(&Terrains, &terrain->list);
        }

        if (lexer_token(Lexer) != '}')
                return -1;

        return 0;
}

#ifdef SUPPORT_BINARY_TERRAIN_MAPS
static struct terrain_map *game_load_binary_terrain_map(char *tag)
{
        struct terrain_map *terrain_map = 0;
        char *file = 0;
        int width;
        int height;
        struct terrain *terrain = 0;
        int i;
        int *start = 0;
        int len;
        int ret = 0;

        PARSE_STRING("file", file);

        start = (int *) mmap_file(file, &len);
        if (!start)
                return 0;

        width = start[1];
        height = start[0];

        if (!(terrain_map = terrain_map_create(tag, width, height)))
                return 0;

        for (i = 0; i < terrain_map->w * terrain_map->h; i++) {

                struct list *elem;
                int id = start[i + 2];

                /* try the last one first before doing a search */
                if (terrain) {
                        if (terrain->id == id) {
                                terrain_map->terrain[i] = terrain;
                                continue;
                        }
                }

                /* search for a hit */
                list_for_each(&Terrains, elem) {
                        terrain = list_entry(elem, struct terrain, list);
                        if (terrain->id == id) {
                                terrain_map->terrain[i] = terrain;
                                break;
                        }
                }

                PARSE_ASSERT(elem != &Terrains,
                             "Could not find terrain with id %d", id);
        }

        // SAM: This function needs to implement a means of 
        //      creating or setting a suitable terrain_palette, 
        //      or it will break other things which use 
        //      terrain_map->palette.
        // 
        //      On the other hand, this function is deprecated,
        //      and may be removed in the near future.
        printf
            ("game_load_binary_terrain_map() terrain_map needs a terrain_palette.\n");
        assert(terrain->palette);

      cleanup:
        if (file)
                free(file);
        if (start)
                munmap(start, len);
        return terrain_map;
}
#endif                          // SUPPORT_BINARY_TERRAIN_MAPS

static int game_load_palette()
{
  // The construct we are parsing looks like:
  // 
  // PALETTE pal_foobar {
  // "AB" t_foo;
  // "x"  t_bar;
  // }
  // 
  // Our caller already parsed the top-level keyword "PALETTE".

  class Loader loader;
  struct terrain_palette *palette;
  char * palette_tag = 0;
  int ret = 0;

  lexer_lex(Lexer);
  PARSE_TAG(palette_tag);
  PARSE_START_OF_BLOCK();

  loader.lexer     = Lexer;
  loader.lookupTag = lookupTag;
  loader.advance();
  
  if ((palette = LTP_wrapper(&loader)) == NULL) {
    err("%s", loader.error);
    return -1;
  }
  palette->tag = palette_tag;
  // palette_print(stdout, INITIAL_INDENTATION, palette);
  list_add(&Terrain_Palettes, &palette->list);
  // Note that LTP_wrapper() already parsed the closing '}'
  
 cleanup:  
  // Do any free() activity, etc needed in failure cases here:
  
  return ret;
} // game_load_palette()

static void *lookupConversation(char *tag)
{
        struct list *elem;

        list_for_each(&Conv, elem) {
                struct conv *conv;

                conv = outcast(elem, struct conv, list);
                if (!strcmp(tag, conv->tag))
                        return conv;
        }

        return 0;
}

static class Character *lookupCharacter(char *tag)
{
        struct list *elem;

        list_for_each(&Characters, elem) {
                class Character *ch;

                ch = outcast(elem, class Character, llist);
                if (!strcmp(tag, ch->tag))
                        return ch;
        }

        return 0;
}

static class NpcPartyType *lookupNpcPartyType(char *tag)
{
        struct list *elem;

        list_for_each(&NpcPartyTypes, elem) {
                class NpcPartyType *ch;

                ch = outcast(elem, class NpcPartyType, list);
                if (!strcmp(tag, ch->getTag()))
                        return ch;
        }

        return 0;
}

static struct occ *lookupOcc(char *tag)
{
        struct list *elem;

        list_for_each(&Occs, elem) {
                struct occ *ch;

                ch = outcast(elem, struct occ, list);
                if (!strcmp(tag, ch->tag))
                        return ch;
        }

        return 0;
}

#define list_lookup_tag(head, tag, type, field, val) \
{\
        struct list *elem;\
		list_for_each((head), elem) {\
		        (val) = outcast(elem, type, field);\
				if (!strcmp(tag, (val)->tag))\
				        break;\
                (val) = 0;\
        }\
}

void *lookupTag(char *tag, int tid)
{
        switch (tid) {
        case SPRITE_ID:
                return spriteLookup(tag);
                break;
        case CONVERSATION_TYPE_ID:
        case RESPONSE_TYPE_ID:
                return lookupConversation(tag);
        case CHARACTER_ID:
                return lookupCharacter(tag);
        case NPCPARTY_TYPE_ID:
                return lookupNpcPartyType(tag);
        case OCC_ID:
                return lookupOcc(tag);
        case SPECIES_ID:
                {
                        struct species *val = 0;
                        list_lookup_tag(&Species, tag, struct species, list,
                                        val);
                        return val;
                }
        case SCHEDULE_ID:
                {
                        struct sched *val = 0;
                        list_lookup_tag(&Schedules, tag, struct sched, list,
                                        val);
                        return val;
                }
        case PLACE_ID:
                {
                        struct place *val = 0;
                        list_lookup_tag(&Places, tag, struct place, list, val);
                        return val;
                }
        case MAP_ID:
                {
                        struct terrain_map *val = 0;
                        list_lookup_tag(&Maps, tag, struct terrain_map, list,
                                        val);
                        return val;
                }
        case TERRAIN_ID:
                {
                        struct terrain *val = 0;
                        list_lookup_tag(&Terrains, tag, struct terrain, list,
                                        val);
                        return val;
                }
        case TERRAIN_PALETTE_ID:
                {
                        struct terrain_palette *val = 0;
                        list_lookup_tag(&Terrain_Palettes, tag, struct terrain_palette, list,
                                        val);
                        return val;
                }
        case IMAGES_ID:
                {
                        struct images *val = 0;
                        list_lookup_tag(&Images, tag, struct images, list, val);
                        return val;
                }
        case VEHICLE_TYPE_ID:
                {
                        class ObjectType *type = game_object_type_lookup(tag);
                        if (!type || !type->isType(VEHICLE_TYPE_ID))
                                return 0;
                        return type;
                }
        case FORMATION_TYPE_ID:
                {
                        struct formation *val = 0;
                        list_lookup_tag(&Formations, tag, struct formation,
                                        list, val);
                        return val;
                }
        case MECH_ID:
                {
                        struct list *elem;
                        list_for_each(&Objects, elem) {
                                class Object *obj;

                                obj = outcast(elem, class Object, list);
                                if (!obj->isType(MECH_ID))
                                        continue;
                                if (!strcmp(obj->script_tag, tag))
                                        return (class Mech *) obj;
                        }
                        return 0;
                }
        default:
                return game_object_type_lookup(tag);
        }
}

// SAM: Moved 'struct terrain_palette_entry' to terrain.h

static struct terrain_palette_entry *load_terrain_palette_entry(class Loader *
                                                                loader,
                                                                int
                                                                *num_entries,
                                                                int *widest);

static struct terrain_palette *LTP_wrapper(class Loader * loader)
{
        struct terrain_palette *palette;
        int num_entries = 0;
        int widest = 0;

        palette = new_terrain_palette();
        palette->set =
            load_terrain_palette_entry(loader, &num_entries, &widest);
        palette->num_entries = num_entries;
        palette->widest_glyph = widest;

        return palette;
}                               // LTP_wrapper()


static struct terrain_palette_entry *load_terrain_palette_entry(class Loader *
                                                                loader, int *n,
                                                                int *widest)
{
        struct terrain_palette_entry *set;
        struct terrain_palette_entry entry;
        char *terrain_tag;
        int i;
        int len;

        // base case
        if (loader->matchToken('}')) {
                set = new struct terrain_palette_entry[*n];
                PARSE_ASSERT(!(set == NULL),
                             "Failed to allocate memory for terrain_palette_entry.");
                memset(set, 0, *n * sizeof (struct terrain_palette_entry));
                return set;
        }
        // recursive case
        memset(&entry, 0, sizeof (entry));

        if (!loader->getString(&entry.glyph) || !loader->getWord(&terrain_tag))
                return NULL;

        len = strlen(entry.glyph);

        PARSE_ASSERT(!(len == 0),
                     "Zero-length glyph (glyph '' terrain '%s') "
                     "found in terrain palette.\n", terrain_tag);

        PARSE_ASSERT(!(len > LONGEST_TERRAIN_GLYPH),
                     "Too-long glyph '%s' (length %d, max = %d) "
                     "found in terrain palette.\n",
                     entry.glyph, len, LONGEST_TERRAIN_GLYPH);

        for (i = 0; i < len; i++) {
                PARSE_ASSERT(!(isspace(entry.glyph[i])),
                             "Whitespace-containing glyph '%s' "
                             "found in terrain palette.\n", entry.glyph);
        }

        // SAM:
        // Add code here for terrain_palette_entry to accept a 'null' entry.
        // Hmmm...perhaps terrain should accept binding a null terrain?
        // That would mean that here, it is not a special case...
        entry.terrain = (struct terrain *) loader->lookupTag(terrain_tag,
                                                             TERRAIN_ID);
        PARSE_ASSERT(!(entry.terrain == NULL),
                     "Error loading terrain map palette: '%s' "
                     "is not a valid TERRAIN tag", terrain_tag);

        free(terrain_tag);

        if (len > *widest)
                *widest = len;  // This is the widest glyph thus far
        i = *n;
        (*n)++;

        // And recurse, for the next palette entry:
        set = load_terrain_palette_entry(loader, n, widest);
        PARSE_ASSERT(!(set == NULL),
                     "Got a null terrain_palette back somehow!");
        set[i] = entry;

        return set;
}

static struct terrain_map *game_load_ascii_terrain_map(char *tag)
{
        struct terrain_map *terrain_map = 0;
        struct terrain_palette *palette = 0;
        char *palette_tag;
        unsigned int width;
        unsigned int height;
        int x, y;
        int ret = 0;            // Wanted by certain PARSE_*() macros
        class Loader loader;
        bool one_char_per_tile = false;

        PARSE_INT("width", width);
        PARSE_INT("height", height);

        loader.lexer     = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();

        // SAM: We could almost determine this by palette->widest now,
        //      though compact and WS-padded 1-byte-wide palette maps 
        //      both exist...
        if (loader.matchWord("one_char_per_tile")) {
                // This field is optional
                if (!loader.getBool(&one_char_per_tile))
                        return NULL;
        }

        // Palette
        if ( !loader.matchWord("palette")  ||
             !loader.getWord(&palette_tag)  ) {
          err("%s", loader.error);
          return NULL;
        }
        palette = (struct terrain_palette *) lookupTag(palette_tag, TERRAIN_PALETTE_ID);
        PARSE_ASSERT(palette, "Map '%s' has invalid palette '%s'.\n", tag, palette_tag);
        // palette_print(stdout, INITIAL_INDENTATION, palette);

        // Terrain {} block
        if (!(terrain_map = terrain_map_create(tag, width, height)))
                return 0;
        terrain_map->palette = palette;

        if (one_char_per_tile) {
                // one_char_per_tile
                if (!loader.matchWord("terrain") || !loader.matchToken('{')) {
                        err("Error loading MAP %s: %s", tag, loader.error);
                        return NULL;
                }
                //printf("map tag '%s' width %d height %d\n", tag, width, height);

                // We grab the map terrain lines one at a time:
                for (y = 0; y < terrain_map->h; y++) {
                        char *map_line = 0;
                        bool got_line = loader.getString(&map_line);
                        PARSE_ASSERT(got_line,
                                     "Failed to getString() for map line %d of map '%s'.\n",
                                     y, tag);
                        //printf("line %2d = '%s'\n", y, map_line);

                        // Now we slice the map line into glyphs:
                        //   Note that, unlike multi-byte palette maps
                        //   we do not ignore or allow leading or trailing whitespace
                        //   in map_line before/after the glyph characters.
                        int len = strlen(map_line);
                        PARSE_ASSERT((len == terrain_map->w),
                                     "Error loading (single-byte palette) ASCII MAP '%s':\n"
                                     "  Found %d glyphs on map line %d, expected %d.\n",
                                     tag, len, y, terrain_map->w);

                        for (x = 0; x < terrain_map->w; x++) {
                                int i = (y * terrain_map->w) + x;
                                char two_bytes[2];
                                char *glyph = two_bytes;
                                glyph[0] = map_line[x];
                                glyph[1] = '\0';
                                // printf("x=%d glyph='%s'\n", x, glyph);

                                struct terrain *tt =
                                    palette_terrain_for_glyph(palette, glyph);
                                PARSE_ASSERT(tt,
                                             "Error loading (single-byte palette) ASCII MAP '%s':\n"
                                             "  Glyph '%s' at XY=(%d,%d) "
                                             "does not map to a TERRAIN type.\n"
                                             "  (Check the palette definition versus the terrain block.)\n",
                                             tag, glyph, x, y);
                                // printf("tt->tag='%s' name='%s'\n", tt->tag, tt->name);
                                terrain_map->terrain[i] = tt;
                        }       // for (x)

                        free(map_line);
                }               // for (y)

                // terrain {} block end was already grabbed the lexer, it seems
                PARSE_END_OF_BLOCK();   // MAP {} block
        }                       // one_char_per_tile

        else {
                // !one_char_per_tile
                if (!loader.matchWord("terrain") || !loader.matchToken('{')) {
                        err("Error loading MAP %s: %s", tag, loader.error);
                        return NULL;
                }
                //printf("map tag '%s' width %d height %d\n", tag, width, height);

                // We grab the map terrain lines one at a time:
                for (y = 0; y < terrain_map->h; y++) {
                        char *map_line;
                        char *p;
                        bool got_line = loader.getString(&map_line);
                        PARSE_ASSERT(got_line,
                                     "Failed to getString() for map line %d of map '%s'.\n",
                                     y, tag);
                        //printf("line %2d = '%s'\n", y, map_line);

                        // Now we slice the map line into glyphs:
                        p = strtok(map_line, " \t\r\n");        // Prime the pump
                        x = 0;
                        while (1) {
                                //printf("x=%d p='%s'\n", x, p);
                                int i = (y * terrain_map->w) + x;
                                struct terrain *tt =
                                    palette_terrain_for_glyph(palette, p);
                                PARSE_ASSERT(tt,
                                             "Error loading (multi-byte palette) ASCII MAP '%s':\n"
                                             "  Glyph '%s' at XY=(%d,%d) "
                                             "does not map to a TERRAIN type.\n"
                                             "  (Check the palette definition versus the terrain block.)\n",
                                             tag, p, x, y);
                                //printf("tt->tag='%s' name='%s'\n", tt->tag, tt->name);
                                terrain_map->terrain[i] = tt;
                                x++;
                                p = strtok(NULL, " \t\r\n");
                                if (p == NULL) {
                                        break;
                                }
                        }       // while(1)

                        PARSE_ASSERT((x == terrain_map->w),
                                     "Error loading (multi-byte palette) ASCII MAP '%s':\n"
                                     "  Found %d glyphs on map line %d, expected %d.\n",
                                     tag, x, y, terrain_map->w);
                        free(map_line);
                }               // for (y)
                PARSE_ASSERT((y == terrain_map->h),
                             "Error loading (multi-byte palette) ASCII MAP '%s':\n"
                             "  Found %d lines in the terrain {} block, expected %d.\n",
                             tag, y, terrain_map->h);

                // terrain {} block end was already grabbed the lexer, it seems
                PARSE_END_OF_BLOCK();   // MAP {} block
        }                       // !one_char_per_tile

      cleanup:

        // terrain_map_print(stdout, INITIAL_INDENTATION, terrain_map);

        return terrain_map;
}                               // game_load_ascii_terrain_map()


#ifdef OLD_MAP

static struct terrain_map *game_load_terrain_map()
{
        struct terrain_map *terrain_map;
        char *tag = 0;
        char *type = 0;
        int ret = 0;

        PARSE_TAG(tag);
        PARSE_START_OF_BLOCK();
        PARSE_WORD("type", type);

        if (!strcmp(type, "ascii")) {
                terrain_map = game_load_ascii_terrain_map(tag);
                // PARSE_END_OF_BLOCK();
        }

#ifdef SUPPORT_BINARY_TERRAIN_MAPS
        else if (!strcmp(type, "binary")) {
                terrain_map = game_load_binary_terrain_map(tag);
                PARSE_END_OF_BLOCK();
        }
#endif                          // SUPPORT_BINARY_TERRAIN_MAPS

        else {
                err("line %d: unknown terrain_map type %s", Lexer->line, type);
        }

      cleanup:
        if (type)
                free(type);
        if (tag)
                free(tag);
        return terrain_map;
}

static int game_load_Maps()
{
        if (!MATCH('{'))
                goto fail;

        while (lexer_lex(Lexer) != '}') {
                struct terrain_map *terrain_map;

                terrain_map = game_load_terrain_map();
                if (!terrain_map)
                        return -1;

                list_add(&Maps, &terrain_map->list);

        }

        if (lexer_token(Lexer) != '}')
                goto fail;
        return 0;

      fail:
        return -1;
}
#else                           // OLD_MAP

static int load_map(void)
{
        struct terrain_map *terrain_map = 0;
        char *tag = 0;
        char *type = 0;
        int ret = 0;

        lexer_lex(Lexer);
        PARSE_TAG(tag);
        PARSE_START_OF_BLOCK();
        PARSE_WORD("type", type);

        if (!strcmp(type, "ascii")) {
                terrain_map = game_load_ascii_terrain_map(tag);
                // PARSE_END_OF_BLOCK();
        }

#ifdef SUPPORT_BINARY_TERRAIN_MAPS
        else if (!strcmp(type, "binary")) {
                terrain_map = game_load_binary_terrain_map(tag);
                PARSE_END_OF_BLOCK();
        }
#endif                          // SUPPORT_BINARY_TERRAIN_MAPS

        else {
                err("line %d: unknown terrain_map type %s", Lexer->line, type);
        }

      cleanup:
        if (type)
                free(type);
        if (tag)
                free(tag);

        if (!terrain_map) {
                return -1;
        }

        list_add(&Maps, &terrain_map->list);
        return 0;
}

#endif                          // OLD_MAP

static Object *game_load_place_item(struct place *place)
{
        class ObjectType *type;
        class Object *obj = 0;
        class Loader loader;
        class Character *ch;

        PARSE_ASSERT((Lexer->token == lexer_WORD),
                     "Expected type tag, got '%s'\n", Lexer->lexeme);

        if ((type = game_object_type_lookup(Lexer->lexeme)) ||
            (type = lookupNpcPartyType(Lexer->lexeme))) {

                // gmcnutt: big glaring HACK. If this object type occupies the
                // container layer then allocate it specifically as a
                // Container.
                if (type->getLayer() == container_layer) {

                        obj = new Container();
                        PARSE_ASSERT(obj, "Failed to create instance of %s\n",
                                     type->getName());
                        obj->init(type);

                } else {

                        // This is an object type so create an instance.
                        obj = type->createInstance();
                        PARSE_ASSERT(obj, "Failed to create instance of %s\n",
                                     type->getName());

                }

        }
        else if ((ch = lookupCharacter(Lexer->lexeme))) {
                // This is an NPC character so create a wrapper party to hold
                // it.
                obj = new NpcParty();
                PARSE_ASSERT(obj, "Failed to create wrapper for %s\n",
                             ch->getName());
                ((class NpcParty *) obj)->init((class Character *) ch);

        }
        else {
                PARSE_ASSERT(type, "Invalid type tag '%s'\n", Lexer->lexeme);
        }

        // Must do this BEFORE calling load() because NPC parties will check
        // the 'home' flag as they load and assign the current place as their
        // home.  
        obj->setPlace(place);

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();
        if (!obj->load(&loader)) {
                PARSE_ASSERT(false, "Failed to load object named %s: %s\n",
                             obj->getName(), loader.error);
        }

        if (obj->script_tag) {
                list_add(&Objects, &obj->list);
        }

        return obj;
}

static bool loadConnect(class Loader * loader)
{
        char *left_tag = 0, *right_tag = 0;
        bool right_to_left = false, left_to_right = false;
        struct list *elem;
        class Mech *left = 0, *right = 0;

        if (!loader->getWord(&left_tag))
                return false;

        if (loader->matchToken('<')) {
                right_to_left = true;
        }

        if (!loader->matchToken('=')) {
                free(left_tag);
                return false;
        }

        if (loader->matchToken('>')) {
                left_to_right = true;
        }

        if (!left_to_right && !right_to_left) {
                loader->setError("Invalid CONNECT operator, should be =>, <= "
                                 "or <=>");
                return false;
        }
        // left off here -- parse right name && bind && connect
        if (!loader->getWord(&right_tag))
                return false;

        // bind the tag names to mechs
        list_for_each(&Objects, elem) {
                class Object *obj;

                obj = outcast(elem, class Object, list);
                if (!strcmp(obj->script_tag, left_tag))
                        left = (class Mech *) obj;
                if (!strcmp(obj->script_tag, right_tag))
                        right = (class Mech *) obj;
        }

        if (!left) {
                loader->setError("Invalid MECH tag %s", left_tag);
                free(left_tag);
                return false;
        }

        if (!right) {
                loader->setError("Invalid MECH tag %s", right_tag);
                free(right_tag);
                return false;
        }

        free(left_tag);
        free(right_tag);

        if (left_to_right) {
                left->port = right;
        }

        if (right_to_left) {
                right->port = left;
        }

        return true;
}

static int game_load_place_items(struct place *place)
{
        class Loader loader;
        int ret = 0;

        if (!MATCH_WORD("objects"))
                return -1;

        PARSE_START_OF_BLOCK();
        lexer_lex(Lexer);
        while (Lexer->token != '}') {
                Object *object;

                if (!strcmp(Lexer->lexeme, "CONNECT")) {
                        loader.lexer = Lexer;
                        loader.lookupTag = lookupTag;
                        loader.advance();
                        if (!loadConnect(&loader)) {
                                err("Failed to CONNECT: %s", loader.error);
                        }
                        continue;
                }

                object = game_load_place_item(place);
                if (!object)
                        return -1;

                if (place_add_object(place, object)) {
                        err("Failed to add object %s to place %s (hint: if "
                            "the object is vehicle then only one vehicle may "
                            "occupy a tile)", object->getName(), place->name);
                        delete object;
                }

        }

      cleanup:
        return ret;
}

static struct typ_npc_party_info *recursively_load_typical_npc_party_info(class
                                                                          Loader
                                                                          *
                                                                          loader,
                                                                          int
                                                                          *n)
{
        struct typ_npc_party_info *info, tmp;
        int i;

#if 1
        char *type_tag;
        if (loader->matchToken('}')) {
                info = new struct typ_npc_party_info[*n];
                return info;
        }

        if (!loader->getWord(&type_tag) ||
            !loader->getInt(&tmp.prob) || !loader->getBitmask(&tmp.align))
                return 0;

        tmp.type = (class NpcPartyType *) loader->lookupTag(type_tag,
                                                            NPCPARTY_TYPE_ID);
        if (!tmp.type) {
                loader->setError("Invalid NPCPARTY_TYPE tag %s", type_tag);
                free(type_tag);
                return 0;
        }
        free(type_tag);

        i = *n;
        (*n)++;
        info = recursively_load_typical_npc_party_info(loader, n);
        if (info)
                info[i] = tmp;

        return info;
#else
        lexer_lex(Lexer);

        if (Lexer->token == '}') {
                info = new struct typ_npc_party_info[*n];
                return info;
        }
        // type = game_object_type_lookup(Lexer->lexeme);
        type = (class ObjectType *) lookupTag(Lexer->lexeme, NPCPARTY_TYPE_ID);
        PARSE_ASSERT(type, "'%s' is an invalid type tag", Lexer->lexeme);
        PARSE_ASSERT(type->isType(NPCPARTY_TYPE_ID),
                     "Type '%s' is not an NPC party type", Lexer->lexeme);
        lexer_lex(Lexer);
        PARSE_ASSERT(Lexer->token == lexer_INT, "Expected integer, got '%s'",
                     Lexer->lexeme);
        prob = atoi(Lexer->lexeme);

        i = *n;
        (*n)++;
        info = recursively_load_typical_npc_party_info(n);
        if (info) {
                info[i].type = (class NpcPartyType *) type;
                info[i].prob = prob;
        }

        return info;
#endif
}

static int game_load_typical_npc_party_info(struct place *place)
{
        int n;
        struct typ_npc_party_info *info;
        class Loader loader;

        MATCH_WORD("typical_npc_parties");
        MATCH('{');

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();

        n = 0;
        info = recursively_load_typical_npc_party_info(&loader, &n);
        PARSE_ASSERT(info, loader.error);

        place->n_typ_npc_parties = n;
        place->typ_npc_parties = info;

        return 0;
}

static struct place *game_load_place(void)
{
        struct place *place = 0;
        struct place *parent = 0;
        char *tag = 0;
        char *type_name = 0;
        enum place_type type;
        char *parent_tag = 0;
        unsigned int x;
        unsigned int y;
        char *name = 0;
        int wraps;
        char *map_tag = 0;
        struct terrain_map *terrain_map;
        int ret = 0;
        class Portal *portal;
        char *object_type_tag = 0;
        class ObjectType *object_type;
        int underground;

        PARSE_TAG(tag);
        PARSE_START_OF_BLOCK();
        PARSE_WORD("type", type_name);
        PARSE_WORD("parent", parent_tag);
        PARSE_INT("x", x);
        PARSE_INT("y", y);
        PARSE_STRING("name", name);
        PARSE_INT("wraps", wraps);
        PARSE_WORD("map", map_tag);
        PARSE_INT("underground", underground);

        if (!strcmp(type_name, "wilderness")) {
                type = wilderness_place;
        }
        else if (!strcmp(type_name, "town")) {
                type = town_place;
        }
        else if (!strcmp(type_name, "dungeon")) {
                type = dungeon_place;
        }
        else {
                err("line %d: invalid type name %s", Lexer->line, type_name);
                ret = -1;
                goto cleanup;
        }

        /* Towns and only towns must have a parent. */
        if (strcmp(parent_tag, "null")) {
                if (type != town_place) {
                        warn("Parents ignored for non-town Places");
                }
                if (!(parent = game_place_lookup(parent_tag))) {
                        err("line %d: invalid place tag %s", Lexer->line,
                            parent_tag);
                        goto cleanup;
                }
                if (parent->type != wilderness_place) {
                        err("line %d: place %s not a wilderness, so it can't "
                            "be a parent", Lexer->line, parent_tag);
                        goto cleanup;
                }
#ifdef REQUIRE_WILDERNESS
        }
        else if (type == town_place) {
                err("line %d: town Places must have a parent wilderness place",
                    Lexer->line);
                goto cleanup;
#endif
        }

        if (!(terrain_map = game_terrain_map_lookup(map_tag))) {
                err("line %d: invalid terrain_map tag %s", Lexer->line,
                    map_tag);
                goto cleanup;
        }

        place = place_create(type, parent, x, y, tag, name, wraps, terrain_map);
        if (!place)
                goto cleanup;

        if (game_load_place_items(place) < 0) {
                goto abort;
        }

        /* If this is a wilderness then load parties, vehicles and typical NPC
         * party info */
        if (type == wilderness_place) {
                if (game_load_typical_npc_party_info(place) < 0)
                        goto abort;
                PARSE_ASSERT(Lexer->token == '}', "Expected '}', got %s\n",
                             Lexer->lexeme);
        }
        else if (type == town_place) {

                /* If this is a town then load the sprite for its portal on the
                 * wilderness map */
                PARSE_WORD("object_type", object_type_tag);
                PARSE_END_OF_BLOCK();
        }
        else {
                PARSE_END_OF_BLOCK();
        }

        /* If this was a town then automatically add a portal for it. */
        if (type == town_place &&
#ifndef REQUIRE_WILDERNESS
            strcmp(parent_tag, "null")
#endif
            ) {
                object_type = game_object_type_lookup(object_type_tag);
                if (!object_type) {
                        err("line %d: invalid object_type tag %s", Lexer->line,
                            object_type_tag);
                        goto abort;
                }
                portal = new Portal();
                PARSE_ASSERT(portal, "Failed to create portal for town");
                portal->init(x, y, parent, object_type, -1, -1, place, true);
                portal->edge_entrance = true;
                place_add_object(portal->getFromPlace(), portal);
        }

        place->underground = !!underground;

      cleanup:
        if (object_type_tag)
                free(object_type_tag);
        if (map_tag)
                free(map_tag);
        if (name)
                free(name);
        if (parent_tag)
                free(parent_tag);
        if (type_name)
                free(type_name);
        if (tag)
                free(tag);
        return place;

      abort:
        place_destroy(place);
        place = 0;
        goto cleanup;
}

static class Portal *game_load_portal(void)
{
        class Portal *portal = 0;
        char *from_place_tag = 0;
        struct place *from_place = 0;
        unsigned int from_x;
        unsigned int from_y;
        char *to_place_tag = 0;
        struct place *to_place = 0;
        unsigned int to_x;
        unsigned int to_y;
        char *object_type_tag = 0;
        class ObjectType *object_type = 0;
        int auto_enter;
        int ret = 0;

        portal = new Portal();
        PARSE_ASSERT(portal, "Failed to allocate new Portal");

        PARSE_WORD("object_type", object_type_tag);
        PARSE_WORD("from_place", from_place_tag);
        PARSE_INT("from_x", from_x);
        PARSE_INT("from_y", from_y);
        PARSE_WORD("to_place", to_place_tag);
        PARSE_INT("to_x", to_x);
        PARSE_INT("to_y", to_y);
        PARSE_INT("auto_enter", auto_enter);
        PARSE_END_OF_BLOCK();

        if (!(from_place = game_place_lookup(from_place_tag))) {
                err("line %d: invalid place tag %s", Lexer->line,
                    from_place_tag);
                goto cleanup;
        }

        if (!(to_place = game_place_lookup(to_place_tag))) {
                err("line %d: invalid place tag %s", Lexer->line, to_place_tag);
                goto cleanup;
        }

        if (!(object_type = game_object_type_lookup(object_type_tag))) {
                err("line %d: invalid object_type tag %s", Lexer->line,
                    object_type_tag);
                goto cleanup;
        }

        portal->init(from_x, from_y, from_place, object_type, to_x, to_y,
                     to_place, !!auto_enter ? true : false);

      cleanup:
        if (object_type_tag)
                free(object_type_tag);
        if (to_place_tag)
                free(to_place_tag);
        if (from_place_tag)
                free(from_place_tag);
        return portal;
}

static int game_load_portals()
{
        if (!MATCH('{'))
                goto fail;

        while (lexer_lex(Lexer) == '{') {
                class Portal *portal;

                portal = game_load_portal();
                if (!portal)
                        return -1;

                place_add_object(portal->getFromPlace(), portal);
        }

        if (lexer_token(Lexer) != '}')
                goto fail;
        return 0;

      fail:
        return -1;
}

static int loadPartyMember(void)
{
        class Character *c;

        c = (class Character *) lookupTag(Lexer->lexeme, CHARACTER_ID);
        PARSE_ASSERT(c, "Invalid CHAR tag '%s'\n", Lexer->lexeme);
        c->setRestCredits(MAX_USEFUL_REST_HOURS_PER_DAY);
        if (!player_party->add_to_party(c)) {
                PARSE_ASSERT(false, "Failed to add %s to party\n",
                             c->getName());
        }
        return 0;
}

static void loadPartyMembers()
{
        MATCH_WORD("party_members");
        MATCH('{');
        while (lexer_lex(Lexer) != '}') {
                loadPartyMember();
        }
        if (lexer_token(Lexer) != '}') {
                err("line %d: expected '}', got %s", Lexer->line,
                    Lexer->lexeme);
                exit(-1);
        }
}

static void loadInventoryItem(void)
{
        class ObjectType *type;
        int num;

        PARSE_ASSERT((lexer_token(Lexer) == lexer_INT),
                     "Expected token %c, got %c (%s)", lexer_INT,
                     lexer_token(Lexer), lexer_lexeme(Lexer));
        num = atoi(lexer_lexeme(Lexer));

        MATCH(lexer_WORD);

        type = game_object_type_lookup(lexer_lexeme(Lexer));

        PARSE_ASSERT(type, "'%s' is not a valid object type tag",
                     lexer_lexeme(Lexer));

        player_party->add_to_inventory(type, num);
}

static void loadInventory(void)
{
        MATCH('{');
        while (lexer_lex(Lexer) != '}') {
                loadInventoryItem();
        }
        if (lexer_token(Lexer) != '}') {
                err("line %d: expected '}', got %s", Lexer->line,
                    Lexer->lexeme);
                exit(-1);
        }
}

static int loadPlayer()
{
        char *place_tag = 0;
        char *sprite_tag = 0;
        char *formation_tag = 0;
        char *campsite_map_tag = 0;
        char *campsite_formation_tag = 0;
        int ret = 0, x, y;

        PARSE_START_OF_BLOCK();
        PARSE_WORD("place", place_tag);
        PARSE_INT("x", x);
        player_party->setX(x);
        PARSE_INT("y", y);
        player_party->setY(y);
        PARSE_WORD("sprite", sprite_tag);
        PARSE_INT("speed", player_party->speed);
        //PARSE_INT("pmask", player_party->pmask); obsolete
        PARSE_WORD("mv_desc", player_party->mv_desc);
        PARSE_STRING("mv_sound", player_party->mv_sound);
        PARSE_INT("food", player_party->food);
        PARSE_INT("gold", player_party->gold);
        player_party->alignment = parseBitmap("alignment");

        // Optional player party parms:
        lexer_lex(Lexer);
        while (strcmp(lexer_lexeme(Lexer), "inventory")) {
                printf("Got %s\n", Lexer->lexeme);
                if (!strcmp(Lexer->lexeme, "formation")) {
                        MATCH(lexer_WORD);
                        PARSE_ASSERT((formation_tag = strdup(Lexer->lexeme)),
                                     "Error allocating string\n");
                }
                else if (!strcmp(Lexer->lexeme, "campsite_map")) {
                        MATCH(lexer_WORD);
                        PARSE_ASSERT((campsite_map_tag = strdup(Lexer->lexeme)),
                                     "Error allocating string\n");
                }
                else if (!strcmp(Lexer->lexeme, "campsite_formation")) {
                        MATCH(lexer_WORD);
                        PARSE_ASSERT((campsite_formation_tag =
                                      strdup(Lexer->lexeme)),
                                     "Error allocating string\n");

                }
                else {
                        PARSE_ASSERT(false, "Error loading player party: %s "
                                     "is not a valid parameter\n",
                                     Lexer->lexeme);
                }
                lexer_lex(Lexer);
        }

        loadInventory();
        loadPartyMembers();
        PARSE_END_OF_BLOCK();

        Place = game_place_lookup(place_tag);
        PARSE_ASSERT(Place, "invalid place tag %s", place_tag);
        player_party->setPlace(Place);
        place_add_object(Place, player_party);

        player_party->sprite = spriteLookup(sprite_tag);
        PARSE_ASSERT(player_party->sprite, "invalid sprite tag %s", sprite_tag);

        if (formation_tag && strcmp(formation_tag, "null")) {
                player_party->formation =
                    (struct formation *) lookupTag(formation_tag,
                                                   FORMATION_TYPE_ID);
                PARSE_ASSERT(player_party->formation, "Error loading player "
                             "party: %s is not a valid formation tag\n",
                             formation_tag);
        }

        if (campsite_map_tag && strcmp(campsite_map_tag, "null")) {
                player_party->campsite_map =
                    (struct terrain_map *) lookupTag(campsite_map_tag, MAP_ID);
                PARSE_ASSERT(player_party->formation, "Error loading player "
                             "party campsite map: %s is not a valid map tag\n",
                             campsite_map_tag);
        }

        if (campsite_formation_tag && strcmp(campsite_formation_tag, "null")) {
                player_party->campsite_formation =
                    (struct formation *) lookupTag(campsite_formation_tag,
                                                   FORMATION_TYPE_ID);
                PARSE_ASSERT(player_party->campsite_formation, "Error loading "
                             "player party campsite formation: %s is not a "
                             "valid formation tag\n", campsite_formation_tag);
        }

        if (Place->type == wilderness_place)
                player_party->context = CONTEXT_WILDERNESS;
        else
                player_party->context = CONTEXT_TOWN;

      cleanup:
        if (place_tag)
                free(place_tag);
        if (sprite_tag)
                free(sprite_tag);
        if (formation_tag)
                free(formation_tag);
        if (campsite_map_tag)
                free(campsite_map_tag);
        if (campsite_formation_tag)
                free(campsite_formation_tag);
        return ret;

}

static int loadSpriteSet()
{
        class Loader loader;
        char *tag = 0;
        struct images *images;
        struct sprite *sprite;

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();

        if ((images = images_load(&loader)) == NULL) {
                err("Error loading SPRITE_SET '%s': %s\n", tag, loader.error);
                return -1;
        }
        list_add(&Images, &images->list);

        // The image loading routine already parsed the "sprites {" tokens.

        while (!loader.matchToken('}')) {
                if ((sprite = sprite_load(&loader, images)) == NULL) {
                        err("Error loading SPRITE: %s\n", loader.error);
                        return -1;
                }
                spriteAdd(sprite);
        }

        if (!loader.matchToken('}')) {
                err("Error loading SPRITE_SET '%s': %s (no closing brace)\n",
                    tag, loader.error);
                return -1;
        }

        return 0;
}

#if 0
static int game_load_sprite_sets()
{
        if (!MATCH('{'))
                return -1;

        while (lexer_lex(Lexer) != '}') {
                if (game_load_sprite_set() < 0)
                        return -1;
        }

        return 0;
}
#endif

#define PARSE_ERROR(exp) \
        err("line %d: expected keyword %s, got '%s'", \
            Lexer->line,exp,lexer_lexeme(Lexer)); \
        ret = -1;     \
        goto cleanup;

static class ArmsType *loadArmsSubType(char *tag, char *name,
                                       struct sprite *sprite)
{
        class ArmsType *type;
        int attackValue, defendValue, numHands, range, slotMask, thrown = 0, U =
            0, weight;
        int ret;
        char *missileTag = NULL;
        char *fieldTag = NULL;
        class ArmsType *missileType = NULL;
        class FieldType *fieldType = NULL;

        type = new ArmsType();
        if (!type)
                return 0;

        PARSE_INT("attackValue", attackValue);
        PARSE_INT("defendValue", defendValue);
        PARSE_INT("slotMask", slotMask);
        PARSE_INT("numHands", numHands);
        PARSE_INT("range", range);

        PARSE_WORD("missile", missileTag);
        if (strcmp(missileTag, "null")) {
                missileType =
                    (class ArmsType *) game_object_type_lookup(missileTag);
                PARSE_ASSERT(missileType, "Invalid object type '%s'",
                             missileTag);
        }

        PARSE_INT("thrown", thrown);
        PARSE_INT("ubiquitousAmmo", U);
        fieldType = (class FieldType *) parseObjectType("field", FIELD_TYPE_ID);
        PARSE_INT("weight", weight);

        if (!type->
            init(tag, name, sprite, slotMask, attackValue, defendValue,
                 numHands, range)) {
                delete type;
                type = 0;
        }

        if (missileType != NULL)
                type->setMissileType(missileType);
        if (thrown)
                type->setThrown(true);
        if (fieldType != NULL)
                type->setFieldType(fieldType);
        if (U)
                type->setUbiquitousAmmo(true);
        type->setWeight(weight);

      cleanup:
        if (missileTag != NULL)
                free(missileTag);
        if (fieldTag != NULL)
                free(fieldTag);
        return type;
}

#if 0
static class CharacterType *loadCharacterSubType(char *tag, char *name,
                                                 struct sprite *sprite)
{
        class CharacterType *type;
        int vision_radius, speed, pmask, maxhp, baseArmourClass, baseMana;
        int ret;
        char *naturalWeaponTag = NULL;
        class ArmsType *naturalWeapon;
        int n_slots;
        int *slot_info = NULL;
        int numTypicalItems;
        struct TypicalObjectInfo *typicalItems;
        char *typicalItemsTag = NULL;
        char *itemContainerTypeTag = NULL;
        class ItemType *itemContainerType;
        int hasItems;
        int numSpells;
        struct ready_spell *spells;
        char *spellTypeTag = 0;
        int trappedContainerProbability;
        int numTypicalTraps;
        struct TypicalObjectInfo *typicalTraps;
        int baseStrength;

        type = new class CharacterType();
        if (!type)
                return 0;

        PARSE_INT("vision_radius", vision_radius);
        PARSE_INT("speed", speed);
        PARSE_INT("pmask", pmask);
        PARSE_INT("maxhp", maxhp);
        PARSE_INT("baseArmourClass", baseArmourClass);
        PARSE_INT("baseMana", baseMana);
        PARSE_INT("baseStrength", baseStrength);

        // 
        // Natural weapons
        // 
        PARSE_WORD("naturalWeapon", naturalWeaponTag);
        naturalWeapon = (class ArmsType *)
            game_object_type_lookup(naturalWeaponTag);
        PARSE_ASSERT(naturalWeapon, "Invalid object type tag '%s'",
                     naturalWeaponTag);

        // 
        // Slots
        // 
        PARSE_INT("n_slots", n_slots);
        slot_info = new int[n_slots];
        PARSE_ASSERT(slot_info, "Memory allocation failed");
        MATCH('{');
        for (int i = 0; i < n_slots; i++) {
                PARSE_INT("mask", slot_info[i]);
        }
        MATCH('}');

        // 
        // Container info
        // 
        PARSE_INT("hasItems", hasItems);
        PARSE_WORD("itemContainerType", itemContainerTypeTag);
        itemContainerType = (class ItemType *)
            game_object_type_lookup(itemContainerTypeTag);
        PARSE_ASSERT(itemContainerType, "Invalid object type tag '%s'",
                     itemContainerTypeTag);
        PARSE_INT("trappedContainerProbability", trappedContainerProbability);
        PARSE_INT("numTypicalContainerTraps", numTypicalTraps);
        typicalTraps = new struct TypicalObjectInfo[numTypicalTraps];
        PARSE_ASSERT(typicalTraps, "Memory allocation failed");
        MATCH('{');
        for (int i = 0; i < numTypicalTraps; i++) {
                typicalTraps[i].type = parseObjectType("object_type",
                                                       TRAP_TYPE_ID);
                PARSE_INT("probability", typicalTraps[i].probability);
        }
        MATCH('}');

        // 
        // Typical Items
        // 
        PARSE_INT("numTypicalItems", numTypicalItems);
        typicalItems = new struct TypicalObjectInfo[numTypicalItems];
        PARSE_ASSERT(typicalItems, "Memory allocation failed");
        MATCH('{');
        for (int i = 0; i < numTypicalItems; i++) {
                PARSE_WORD("object_type", typicalItemsTag);
                typicalItems[i].type = (class ItemType *)
                    game_object_type_lookup(typicalItemsTag);
                PARSE_ASSERT(typicalItems[i].type,
                             "Invalid object type tag '%s'", typicalItemsTag);
                free(typicalItemsTag);
                typicalItemsTag = 0;
                PARSE_INT("probability", typicalItems[i].probability);
                PARSE_INT("n_max", typicalItems[i].n_max);
        }
        MATCH('}');

        // 
        // Natural Spells/Abilities
        // 
        PARSE_INT("numSpells", numSpells);
        spells = new struct ready_spell[numSpells];
        PARSE_ASSERT(spells, "Memory allocation failed");
        MATCH('{');
        for (int i = 0; i < numSpells; i++) {
                PARSE_WORD("spell_type", spellTypeTag);
                spells[i].spell = (class SpellType *)
                    game_object_type_lookup(spellTypeTag);
                PARSE_ASSERT(spells[i].spell, "Invalid spell type tag '%s'",
                             spellTypeTag);
                free(spellTypeTag);
                spellTypeTag = 0;
        }
        MATCH('}');

        if (!type->init(tag, name, sprite, vision_radius, speed, pmask, maxhp,
                        baseArmourClass, n_slots, slot_info,
                        numTypicalItems, typicalItems, itemContainerType,
                        hasItems)) {
                delete type;
                type = 0;
        }

        type->setNaturalWeapon(naturalWeapon);
        type->setSpells(numSpells, spells);
        type->setSleepSprite(parseSprite("sleepSprite"));
        type->setBaseMana(baseMana);
        type->setTrappedContainerInfo(trappedContainerProbability,
                                      numTypicalTraps, typicalTraps);
        type->setBaseStrength(baseStrength);

      cleanup:
        if (naturalWeaponTag)
                free(naturalWeaponTag);
        if (typicalItemsTag)
                free(typicalItems);
        if (spellTypeTag)
                free(spellTypeTag);
        return type;
}
#endif

static class ItemType *loadItemSubType(char *tag, char *name,
                                       struct sprite *sprite)
{
        class ItemType *type;
        int amount, duration, ret, effect, target, food, consumable;
        char *message = 0;

        type = new ItemType();
        if (!type)
                return 0;

        PARSE_INT("effect", effect);
        PARSE_INT("amount", amount);
        PARSE_INT("duration", duration);
        PARSE_INT("target", target);
        PARSE_INT("food", food);
        PARSE_INT("consumable", consumable);
        PARSE_STRING("message", message);

        if (!type->init(tag, name, sprite, effect, amount, duration)) {
                delete type;
                type = 0;
        }

        type->setTarget(target);
        type->setFood(food != 0);
        type->setConsumable(consumable != 0);
        if (message && strlen(message))
                type->setMessage(message);

      cleanup:
        if (message)
                free(message);
        return type;
}

static void loadSprite(struct sprite **sprite)
{
        char *tag = 0;
        int ret;
        PARSE_WORD("sprite", tag);
        *sprite = spriteLookup(tag);
        PARSE_ASSERT(*sprite, "Invalid sprite tag %s", tag);
      cleanup:
        if (tag)
                free(tag);
}

static class MoongateType *loadMoongateSubType(char *tag, char *name,
                                               struct sprite *sprite)
{
        class MoongateType *type;
        int ret, i, n_sprites, maxLight;
        char *enter_sound = 0;

        type = new MoongateType();
        if (!type)
                return 0;

        PARSE_STRING("enter_sound", enter_sound);
        PARSE_INT("maxLight", maxLight);
        PARSE_INT("sprites", n_sprites);
        if (!type->init(tag, name, sprite, n_sprites, enter_sound))
                goto cleanup;

        type->setMaxLight(maxLight);

        MATCH('{');
        for (i = 0; i < type->getNumPhases(); i++) {
                loadSprite(&sprite);
                type->setSprite(i, sprite);
        }
        MATCH('}');

        goto done;

      cleanup:
        if (type)
                delete type;
        type = 0;
      done:
        if (enter_sound)
                free(enter_sound);
        return type;
}

#if 0
static class NpcPartyType *loadNpcPartySubType(char *tag, char *name,
                                               struct sprite *sprite)
{
        class NpcPartyType *type = NULL;
        int n_groups, ret;
        struct GroupInfo *groups;
        char *groupTag = NULL;

        type = new NpcPartyType();
        PARSE_ASSERT(type, "Failed to allocate new NpcPartyType");

        PARSE_INT("n_groups", n_groups);
        groups = new struct GroupInfo[n_groups];
        PARSE_ASSERT(groups != NULL, "Failed to allocate GroupInfo");

        MATCH('{');
        for (int i = 0; i < n_groups; i++) {

                PARSE_WORD("object_type", groupTag);
                groups[i].type =
                    (class CharacterType *) game_object_type_lookup(groupTag);
                PARSE_ASSERT(groups[i].type, "Invalid object type tag '%s'",
                             groupTag);

                free(groupTag);
                groupTag = 0;

                PARSE_INT("n_max", groups[i].n_max);
        }
        MATCH('}');

        if (!type->init(tag, name, sprite, n_groups, groups))
                goto cleanup;

        goto done;

      cleanup:
        if (groupTag)
                free(groupTag);
        if (groups)
                delete groups;
        if (type) {
                delete type;
                type = NULL;
        }
      done:
        return type;
}
#endif

static class FieldType *loadFieldSubType(char *tag, char *name,
                                         struct sprite *sprite)
{
        class FieldType *type;
        int effects, ret, light, dur;

        type = new class FieldType();
        PARSE_ASSERT(type != NULL, "Failed to allocate FieldType");
        type->init(tag, name, field_layer, sprite);
        PARSE_INT("effects", effects);
        type->setEffects(effects);
        PARSE_INT("light", light);
        type->setLight(light);
        PARSE_INT("duration", dur);
        type->setDuration(dur);
        PARSE_INT("pmask", dur);
        type->setPmask(dur);
      cleanup:
        return type;
}

#if 0
static class SpellType *loadSpellSubType(char *tag, char *name,
                                         struct sprite *sprite)
{
        class SpellType *type;
        int effect, ret, range, manaCost, val;
        class ArmsType *missile;

        type = new class SpellType();
        PARSE_ASSERT(type != NULL, "Failed to allocate SpellType");
        type->init(tag, name, null_layer, sprite);
        PARSE_INT("manaCost", manaCost);
        type->setManaCost(manaCost);
        PARSE_INT("range", range);
        type->setRange(range);
        missile = (class ArmsType *) parseObjectType("missile", ARMS_TYPE_ID);
        type->setMissileType(missile);
        PARSE_INT("effect", effect);
        type->setEffect(effect);
        PARSE_INT("amount", val);
        type->setAmount(val);
      cleanup:
        return type;
}
#endif
static class TrapType *loadTrapSubType(char *tag, char *name,
                                       struct sprite *sprite)
{
        class TrapType *type;
        int effects, amount, ret;

        type = new class TrapType();
        type->init(tag, name, null_layer, sprite);
        PARSE_INT("effects", effects);
        type->setEffects(effects);
        PARSE_INT("amount", amount);
        type->setAmount(amount);

      cleanup:
        return type;
}

static class ObjectType *loadObjectType()
{
        class ObjectType *ot = 0;
        char *tag = 0;
        char *name = 0;
        char *sprite_tag = 0;
        char *subtype_tag = 0;
        struct sprite *sprite;
        int ret, layer;

        PARSE_TAG(tag);
        PARSE_START_OF_BLOCK();
        PARSE_STRING("name", name);
        sprite = parseSprite("sprite");
        PARSE_WORD("subtype", subtype_tag);

        if (strcmp(subtype_tag, "arms") == 0)
                ot = loadArmsSubType(tag, name, sprite);
        else if (strcmp(subtype_tag, "item") == 0)
                ot = loadItemSubType(tag, name, sprite);
        else if (strcmp(subtype_tag, "moongate") == 0)
                ot = loadMoongateSubType(tag, name, sprite);
        else if (strcmp(subtype_tag, "field") == 0)
                ot = loadFieldSubType(tag, name, sprite);
        else if (strcmp(subtype_tag, "trap") == 0)
                ot = loadTrapSubType(tag, name, sprite);
        else if (strcmp(subtype_tag, "ammo") == 0) {
                ot = new AmmoType();
                ot->init(tag, name, projectile_layer, sprite);
        }

        else if (strcmp(subtype_tag, "null") == 0) {
                PARSE_INT("layer", layer);
                ot = new ObjectType();
                ot->init(tag, name, (enum layer) layer, sprite);
        }

        else {
                PARSE_ASSERT(0, "unknown subtype '%s'", subtype_tag);
        }

        PARSE_END_OF_BLOCK();

        goto done;

      cleanup:
        if (ot)
                delete ot;
      done:
        if (tag)
                free(tag);
        if (name)
                free(name);
        if (sprite_tag)
                free(sprite_tag);
        if (subtype_tag)
                free(subtype_tag);
        return ot;
}

static int loadObjectTypes()
{
        if (!MATCH('{'))
                return -1;

        while (lexer_lex(Lexer) != '}') {
                class ObjectType *type;

                type = loadObjectType();
                if (!type)
                        return -1;

                list_add(&ObjectTypes, &type->list);
        }

        return 0;
}

static class VehicleType *game_load_vehicle_type()
{
        class VehicleType *vt = 0;
        class OrdnanceType *ordnance = 0;
        char *sprite_tag = 0;
        struct sprite *sprite;
        char *dir_tag = 0;
        char *ordnance_tag = 0, *tag = 0, *name = 0, *mv_desc, *mv_sound;
        int ret = 0;
        int flag, speed, pmask;
        bool must_turn;
        char *map_tag = 0;
        struct terrain_map *map = 0;

        vt = new class VehicleType();
        if (!vt)
                return 0;

        PARSE_TAG(tag);
        PARSE_START_OF_BLOCK();
        PARSE_STRING("name", name);

        PARSE_WORD("sprite", sprite_tag);
        sprite = spriteLookup(sprite_tag);
        PARSE_ASSERT(sprite, "invalid sprite tag %s", sprite_tag);

        PARSE_INT("speed", speed);
        PARSE_INT("pmask", pmask);
        PARSE_INT("must_turn", flag);
        must_turn = !!flag;
        PARSE_WORD("mv_desc", mv_desc);
        PARSE_STRING("mv_sound", mv_sound);
        PARSE_WORD("ordnance", ordnance_tag);
        if (strcmp(ordnance_tag, "null")) {
                ordnance = lookupOrdnanceType(ordnance_tag);
                PARSE_ASSERT(ordnance, "Invalid ordnance tag %s", ordnance_tag);
        }
        PARSE_INT("max_hp", vt->max_hp);
        PARSE_WORD("map", map_tag);
        if (strcmp(map_tag, "null")) {
                map = (struct terrain_map *) lookupTag(map_tag, MAP_ID);
                PARSE_ASSERT(map, "Invalid MAP tag %s", map_tag);
        }
        PARSE_END_OF_BLOCK();

        if (!vt->
            init(tag, name, sprite, speed, pmask, mv_desc, mv_sound, ordnance,
                 must_turn)) {
                delete vt;
                if (mv_desc)
                        free(mv_desc);
                if (mv_sound)
                        free(mv_sound);
                vt = 0;
        }
        vt->map = map;

      cleanup:
        if (ret < 0) {
                delete vt;
                vt = 0;
        }
        if (tag)
                free(tag);
        if (name)
                free(name);
        if (sprite_tag)
                free(sprite_tag);
        if (dir_tag)
                free(dir_tag);
        if (ordnance_tag)
                free(ordnance_tag);
        if (map_tag)
                free(map_tag);
        return vt;
}

static int game_load_vehicle_types()
{
        if (!MATCH('{'))
                return -1;

        while (lexer_lex(Lexer) != '}') {
                class VehicleType *type;

                type = game_load_vehicle_type();
                if (!type)
                        return -1;

                list_add(&ObjectTypes, &type->list);
        }

        return 0;
}

static int game_bind_combat_map()
{
        char *terrain_tag = 0;
        char *map_tag = 0;
        int ret = 0;
        struct terrain *terrain;
        struct terrain_map *terrain_map;

        PARSE_TAG(terrain_tag);

        terrain = game_terrain_lookup(terrain_tag);
        if (!terrain) {
                err("line %d: invalid tag %s", Lexer->line, terrain_tag);
                ret = -1;
                goto cleanup;
        }

        MATCH(lexer_WORD);
        PARSE_TAG(map_tag);

        terrain_map = game_terrain_map_lookup(map_tag);
        if (!terrain_map) {
                err("line %d: invalid tag %s", Lexer->line, map_tag);
                ret = -1;
                goto cleanup;
        }

        if (terrain_map->w < COMBAT_MAP_W || terrain_map->h < COMBAT_MAP_H) {
                err("line %d: MAP %s must be at least %d x %d to be used",
                    Lexer->line, map_tag, COMBAT_MAP_W, COMBAT_MAP_H);
                err(" as a combat map. It is only %d x %d", terrain_map->w,
                    terrain_map->h);
                ret = -1;
                goto cleanup;
        }

        terrain_combat_map(terrain) = terrain_map;

      cleanup:
        if (terrain_tag)
                free(terrain_tag);
        if (map_tag)
                free(map_tag);
        return ret;
}

static int game_bind_combat_maps()
{
        if (!MATCH('{'))
                return -1;

        while (lexer_lex(Lexer) != '}') {
                if (game_bind_combat_map() < 0)
                        return -1;
        }

        return 0;

}

static void parsePlace(struct place **place)
{
        char *tag = 0;
        int ret;
        PARSE_WORD("place", tag);
        *place = game_place_lookup(tag);
        PARSE_ASSERT(*place, "Invalid place tag %s", tag);
      cleanup:
        if (tag)
                free(tag);
}

static void loadMoon(struct moon *moon)
{
        int ret;
        MATCH('{');
        PARSE_INT("phase", moon->phase);
        PARSE_INT("days_per_cycle", moon->days_per_cycle);
        PARSE_INT("arc", moon->arc);
        MATCH('}');
      cleanup:
        return;
}

static void loadSun()
{
        int ret;
        char *tag = 0;
        MATCH_WORD("sun");
        MATCH('{');
        PARSE_INT("arc", Sun.arc);
        loadSprite(&Sun.sprite);
        MATCH('}');
        clockSet();
      cleanup:
        if (tag)
                free(tag);
}

static void loadMoons()
{
        MATCH_WORD("moons");
        MATCH('{');
        loadMoon(&Moons[0]);
        loadMoon(&Moons[1]);
        MATCH('}');
}

static int loadMoongate()
{
        class Moongate *moongate;
        struct place *place;
        int ret = 0, x, y, phase;
        char *type_tag = 0;
        class MoongateType *type;

        moongate = new Moongate();
        PARSE_ASSERT(moongate, "Failed to allocate Moongate");

        if (lexer_token(Lexer) != '{') {
                err("line %d: expected '{', got %s", Lexer->line,
                    Lexer->lexeme);
                exit(-1);
        }

        parsePlace(&place);
        PARSE_WORD("object_type", type_tag);
        PARSE_INT("x", x);
        PARSE_INT("y", y);
        PARSE_INT("phase", phase);
        MATCH('}');

        type = (class MoongateType *) game_object_type_lookup(type_tag);
        PARSE_ASSERT(type, "Invalid object type tag %s", type_tag);

        moongate->init(x, y, place, type, phase);

        Moongates[phase] = moongate;
        place_add_moongate(moongate->getPlace(), moongate);

      cleanup:
        if (type_tag)
                free(type_tag);
        return ret;
}

static void loadMoongates()
{
        MATCH_WORD("moongates");
        MATCH('{');
        while (lexer_lex(Lexer) != '}') {
                loadMoongate();
        }
}

static void loadPhase(int phase)
{
        loadSprite(&MoonInfo.sprite[phase]);
}

static void loadPhases()
{
        int i;
        int ret;

        PARSE_INT("phases", MoonInfo.phases);

        Moongates =
            (class Moongate **) malloc(sizeof (class Moongate *) *
                                       MoonInfo.phases);
        PARSE_ASSERT(Moongates, "Failed to allocate moongate table");
        memset(Moongates, 0, (sizeof (class Moongate *) * MoonInfo.phases));

        MoonInfo.sprite =
            (struct sprite **) malloc(sizeof (struct sprite *) *
                                      MoonInfo.phases);
        PARSE_ASSERT(MoonInfo.sprite, "Failed to allocate moon sprite array");

        MATCH('{');
        for (i = 0; i < MoonInfo.phases; i++) {
                loadPhase(i);
        }
        MATCH('}');
      cleanup:
        return;
}

static int loadMoonInfo()
{
        MATCH('{');
        loadPhases();
        loadMoons();
        loadSun();
        loadMoongates();
        MATCH('}');
        return 0;
}

/* Ordnance types ************************************************************/

static class OrdnanceType *loadOrdnanceType()
{
        class OrdnanceType *vt = 0;
        class ObjectType *ammo;
        char *ord_tag = 0, *tag = 0, *name = 0, *fire_sound = 0;
        int ret = 0, range, damage;

        vt = new class OrdnanceType();
        if (!vt)
                return 0;

        PARSE_TAG(tag);
        PARSE_START_OF_BLOCK();
        PARSE_STRING("name", name);
        PARSE_STRING("fire_sound", fire_sound);
        PARSE_INT("range", range);
        PARSE_INT("damage", damage);
        PARSE_WORD("ammo", ord_tag);
        ammo = game_object_type_lookup(ord_tag);
        PARSE_ASSERT(ammo, "invalid object type tag %s", ord_tag);
        PARSE_END_OF_BLOCK();

        if (!vt->init(tag, name, NULL, fire_sound, range, damage, ammo))
                ret = -1;

      cleanup:
        if (ret < 0) {
                delete vt;
                if (tag)
                        free(tag);
                if (fire_sound)
                        free(fire_sound);
                if (name)
                        free(name);
                vt = 0;
        }
        if (ord_tag)
                free(ord_tag);
        return vt;
}

static int loadOrdnanceTypes()
{
        list_init(&OrdnanceTypes);

        if (!MATCH('{'))
                return -1;

        while (lexer_lex(Lexer) != '}') {
                class OrdnanceType *type;
                type = loadOrdnanceType();
                if (!type)
                        return -1;
                list_add(&OrdnanceTypes, &type->list);
        }

        return 0;
}

static int loadAscii()
{
        char *tag = 0;
        struct images *images;
        int offset;
        int ret = 0;

        MATCH('{');
        PARSE_WORD("images", tag);
        images = (struct images *) lookupTag(tag, IMAGES_ID);
        PARSE_ASSERT(images, "Invalid image tag %s", tag);
        PARSE_INT("offset", offset);
        MATCH('}');

        asciiSetImages(images, offset);
      cleanup:
        if (tag)
                free(tag);
        return ret;
}

static int loadCrosshair()
{
        char *tag = 0;
        int ret = 0;
        class ObjectType *type;

        MATCH('{');
        PARSE_WORD("objectType", tag);
        MATCH('}');

        type = game_object_type_lookup(tag);
        PARSE_ASSERT(type, "Invalid object type tag '%s'", tag);

        Cursor = new class Cursor();
        Cursor->init(type);

      cleanup:
        if (tag)
                free(tag);
        return ret;
}

static int loadResponse()
{
        struct response *resp;
        class Loader loader;

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();
        resp = convLoadResponse(&loader);
        PARSE_ASSERT(resp, "Error loading RESP: %s", loader.error);
        list_add(&Conv, &resp->list);

        return 0;
}

static int loadConversation()
{
        struct conv *conv;
        class Loader loader;

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();
        conv = convLoadConversation(&loader);
        PARSE_ASSERT(conv, "Error loading CONV: %s\n", loader.error);
        list_add(&Conv, &conv->list);

        return 0;
}

static int loadCharacter()
{
        class Character *ch;
        class Loader loader;
        bool ret;

        ch = new class Character();
        PARSE_ASSERT(ch, "Failed to allocate Character\n");

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();
        ret = ch->load(&loader);
        PARSE_ASSERT(ret, "Error loading CHAR: %s\n", loader.error);

        list_add(&Characters, &ch->llist);

        return 0;
}

static int loadNpcPartyType(void)
{
        class NpcPartyType *type;
        class Loader loader;
        bool ret;

        type = new NpcPartyType();
        PARSE_ASSERT(type, "Failed to allocate NpcPartyType\n");

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();
        ret = type->load(&loader);
        PARSE_ASSERT(ret, "Error loading PARTY: %s\n", loader.error);

        list_add(&NpcPartyTypes, &type->list);

        return 0;
}

static int loadOcc(void)
{
        struct occ *occ;
        class Loader loader;

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();
        occ = occLoad(&loader);
        PARSE_ASSERT(occ, "Error loading OCC: %s\n", loader.error);

        list_add(&Occs, &occ->list);

        return 0;
}

static int loadSpecies(void)
{
        struct species *species;
        class Loader loader;

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();
        species = speciesLoad(&loader);
        PARSE_ASSERT(species, "Error loading SPECIES: %s\n", loader.error);

        list_add(&Species, &species->list);

        return 0;
}

static int loadSchedules(void)
{
        struct sched *sched;
        class Loader loader;

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();
        sched = schedLoad(&loader);
        PARSE_ASSERT(sched, "Error loading SCHED: %s\n", loader.error);

        list_add(&Schedules, &sched->list);

        return 0;
}

static int loadPlace()
{
        struct place *place;
        lexer_lex(Lexer);
        place = game_load_place();
        PARSE_ASSERT(place, "Error loading PLACE\n");
        list_add(&Places, &place->list);
        return 0;
}

static int loadSpell(void)
{
        class Spell *spell;
        class Loader loader;

        spell = new Spell();
        PARSE_ASSERT(spell, "Memory allocation error loading SPELL\n");

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();
        if (!spell->load(&loader))
                PARSE_ASSERT(false, "Error loading SPELL: %s\n", loader.error);

        list_add(&ObjectTypes, &spell->list);

        return 0;
}

static int loadReagent(void)
{
        class ReagentType *reagent;
        class Loader loader;

        reagent = new ReagentType();
        PARSE_ASSERT(reagent, "Memory allocation error loading REAGENT\n");

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();

        if (!reagent->load(&loader))
                PARSE_ASSERT(false, "Error loading REAGENT: %s\n",
                             loader.error);

        list_add(&ObjectTypes, &reagent->list);

        return 0;
}

static int loadMagicWords(void)
{
        class Loader loader;

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();

        if (!Spell_load_magic_words(&loader))
                PARSE_ASSERT(false, "Error loading MAGIC_WORDS: %s\n",
                             loader.error);

        return 0;
}

static int loadMechType(void)
{
        class MechType *mech;
        class Loader loader;

        mech = new MechType();
        PARSE_ASSERT(mech, "Memory allocation error loading MECH_TYPE\n");

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();

        if (!mech->load(&loader))
                PARSE_ASSERT(false, "Error loading MECH_TYPE: %s\n",
                             loader.error);

        list_add(&ObjectTypes, &mech->list);

        return 0;
}

static int stackPlaces(void)
{
        class Loader loader;
        char *tag1, *tag2;
        struct place *place1, *place2;

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();

        if (!loader.getWord(&tag1) || !loader.getWord(&tag2))
                PARSE_ASSERT(false, "Error in STACK_PLACES: %s\n",
                             loader.error);

        place1 = (struct place *) loader.lookupTag(tag1, PLACE_ID);
        PARSE_ASSERT(place1, "Error in STACK_PLACES: invalid PLACE tag '%s'\n",
                     tag1);

        place2 = (struct place *) loader.lookupTag(tag2, PLACE_ID);
        PARSE_ASSERT(place2, "Error in STACK_PLACES: invalid PLACE tag '%s'\n",
                     tag2);

        place1->below = place2;
        place2->above = place1;

        return 0;
}

static int loadFrame(void)
{
        class Loader loader;
        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();
        if (screenLoadFrame(&loader) == -1) {
                PARSE_ASSERT(false, "Error in FAME: %s\n", loader.error);
        }
        if (!loader.matchToken('}')) {
                PARSE_ASSERT(false, "Error in FAME: %s\n", loader.error);
        }
        frame_loaded = true;
        return 0;
}

static int loadCursor(void)
{
        class Loader loader;
        char *tag;

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();
        if (!loader.getWord(&tag)) {
                PARSE_ASSERT(false, "Error in CURSOR: %s\n", loader.error);
        }
        CursorSprite = (struct sprite *) lookupTag(tag, SPRITE_ID);
        PARSE_ASSERT(CursorSprite, "Error in CURSOR: Invalid SPRITE tag "
                     "'%s'\n", tag);
        free(tag);
        cursor_loaded = true;
        return 0;
}

static int loadCombat(void)
{
        class Loader loader;

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();
        PARSE_ASSERT(combatLoad(&loader) == 0, "Error in COMBAT: %s\n",
                     loader.error);
        return 0;
}

static int loadFormation(void)
{
        class Loader loader;
        char *tag;
        struct formation *formation;
        int width, height, i, x, y;

        loader.lexer = Lexer;
        loader.lookupTag = lookupTag;
        loader.advance();
        if (!loader.getWord(&tag)) {
                PARSE_ASSERT(false, "Error in FORMATION: %s\n", loader.error);
        }
        // allocate the formation struct
        formation = new struct formation;
        PARSE_ASSERT(formation, "Failed to allocate FORMATION\n");
        memset(formation, 0, sizeof (formation));
        list_init(&formation->list);
        formation->tag = tag;

        if (!loader.matchToken('{') ||
            !loader.getIntKeyValue("w", &width) ||
            !loader.getIntKeyValue("h", &height) ||
            !loader.getIntKeyValue("n", &formation->n)) {
                PARSE_ASSERT(false, "Error in FORMATION %s: %s\n", tag,
                             loader.error);
        }

        if (0 == (width % 2) || 0 == (height % 2)) {
                warn("Warning: FORMATION %s does not have odd dimensions "
                     "[%d x %d], so rotation may look a bit odd\n", tag, width,
                     height);
        }
        // allocate the formation entry array
        formation->entry = new struct formation_entry[formation->n];
        PARSE_ASSERT(formation->entry, "Failed to allocate FORMATION "
                     "entries\n");
        memset(formation->entry, 0, formation->n *
               sizeof (struct formation_entry));

        // parse the entries
        for (y = 0, i = 0; y < height; y++) {
                for (x = 0; x < width; x++, i++) {
                        int order;

                        if (loader.matchToken('.'))
                                continue;

                        if (!loader.getInt(&order)) {
                                PARSE_ASSERT(false,
                                             "Error parsing FORMATION %s "
                                             "entry %d: %s\n", tag, i,
                                             loader.error);
                        }

                        if (order < 0 || order >= formation->n) {
                                PARSE_ASSERT(false,
                                             "Error parsing FORMATION %s "
                                             "entry %d: %d is out of range "
                                             "[0 %d]\n", tag, i, order,
                                             formation->n - 1);
                        }

                        if (formation->entry[order].x != 0 ||
                            formation->entry[order].y != 0) {
                                warn("Warning: FORMATION %s entry %d appears "
                                     "to duplicate order %d\n", tag, i, order);
                        }
                        // offset from the origin
                        formation->entry[order].x = x - width / 2;
                        formation->entry[order].y = y - height / 2;
                }
        }

        if (!loader.matchToken('}')) {
                PARSE_ASSERT(false, "Error in FORMATION %s: %s\n", tag,
                             loader.error);
        }

        list_add(&Formations, &formation->list);

        return 0;
}

struct keyword {
        char *word;
        int (*fx) (void);
        bool advance;
};

static struct keyword keywords[] = {
        {"SPRITE_SET", loadSpriteSet, false},
        {"terrains", game_load_Terrains, true},
        {"PALETTE", game_load_palette, false},
#ifdef OLD_MAP
        {"maps", game_load_Maps, true},
#else
        {"MAP", load_map, true},
#endif
        {"portals", game_load_portals, true},
        {"player", loadPlayer, true},
        {"object_types", loadObjectTypes, true},
        {"combat_maps", game_bind_combat_maps, true},
        {"vehicle_types", game_load_vehicle_types, true},
        {"moon_info", loadMoonInfo, true},
        {"ordnance_types", loadOrdnanceTypes, true},
        {"ascii", loadAscii, true},
        {"CROSSHAIR", loadCrosshair, true},
        {"RESP", loadResponse, false},
        {"CONV", loadConversation, false},
        {"CHAR", loadCharacter, false},
        {"PARTY", loadNpcPartyType, false},
        {"OCC", loadOcc, false},
        {"SPECIES", loadSpecies, false},
        {"SCHED", loadSchedules, false},
        {"PLACE", loadPlace, true},
        {"SPELL", loadSpell, false},
        {"MAGIC_WORDS", loadMagicWords, false},
        {"REAGENT", loadReagent, false},
        {"STACK_PLACES", stackPlaces, false},
        {"MECH_TYPE", loadMechType, false},
        {"FRAME", loadFrame, false},
        {"CURSOR", loadCursor, false},
        {"FORMATION", loadFormation, false},
        {"COMBAT", loadCombat, false},
};

#define NUM_KEYWORDS (sizeof(keywords)/sizeof(keywords[0]))

static void initLoader(void)
{
        list_init(&Terrains);
        list_init(&Terrain_Palettes);
        list_init(&Maps);
        list_init(&Places);
        list_init(&Images);
        list_init(&ObjectTypes);
        list_init(&Conv);
        list_init(&Characters);
        list_init(&NpcPartyTypes);
        list_init(&Occs);
        list_init(&Species);
        list_init(&Schedules);
        list_init(&Objects);
        list_init(&Formations);
}

int loadGame(char *filename)
{
        char *start;
        int len = 0;
        int ret = 0;
        unsigned int i;
        class Loader loader;
        int t0, t1, t2, t3, t4, t5;

        frame_loaded = false;
        cursor_loaded = false;

        t0 = SDL_GetTicks();

        start = mmap_file(filename, &len);
        if (!start)
                return 0;

        Lexer = lexer_create(4096);
        if (!Lexer) {
                ret = -1;
                goto unplace;
        }

        lexer_init(Lexer, start, len);
        Lexer->ignore_semicolons = 1;

        initLoader();

        lexer_lex(Lexer);

        t1 = SDL_GetTicks();

        while (Lexer->token != 0) {
                for (i = 0; i < NUM_KEYWORDS; i++) {

                        if (strcmp(lexer_lexeme(Lexer), keywords[i].word))
                                continue;

                        t2 = SDL_GetTicks();
                        if (keywords[i].fx() < 0) {
                                err("%s line %d: error parsing %s", filename,
                                    Lexer->line, keywords[i].word);
                                ret = -1;
                                goto cleanup;
                        }
                        t3 = SDL_GetTicks();
                        // printf("%s: %d ms\n", keywords[i].word, t3 - t2);
                        break;
                }

                if (i == NUM_KEYWORDS) {
                        err("%s line %d: unknown keyword '%s'\n", filename,
                            Lexer->line, lexer_lexeme(Lexer));
                        ret = -1;
                        goto cleanup;
                }
                // hack: the conversation load routines already advanced the
                // lexer
                if (keywords[i].advance)
                        lexer_lex(Lexer);
        }

        t4 = SDL_GetTicks();

        // Make sure we got everything we need
        if (!frame_loaded) {
                err("No FRAME construct detected! Without this I can't paint "
                    "the frame.\n");
                ret = -1;
                goto cleanup;
        }
        if (!cursor_loaded) {
                err("No CURSOR construct detected! Without this I can't paint "
                    "the cursor.\n");
                ret = -1;
                goto cleanup;
        }
        // bind all the tags
        struct list *elem;
        loader.lookupTag = lookupTag;
        list_for_each(&ObjectTypes, elem) {
                class ObjectType *type = outcast(elem, class ObjectType, list);
                if (!type->bindTags(&loader)) {
                        err("Bind error: %s", loader.error);
                        ret = -1;
                        goto cleanup;
                }
        }

        t5 = SDL_GetTicks();

#ifdef PROFILE
        printf("Setup time: %d\n", t1 - t0);
        printf("Parse time: %d\n", t4 - t1);
        printf("Bind time: %d\n", t5 - t4);
#endif

      cleanup:
        lexer_destroy(Lexer);
      unplace:
        munmap(start, len);

        return ret;
}

int gameave(char *filename)
{
        return -1;
}
