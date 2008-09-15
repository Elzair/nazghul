;;----------------------------------------------------------------------------
;; Images
;;----------------------------------------------------------------------------
;; 'ss_sprite_set_name tile_pix_w tile_pix_h tiles_h tiles_w x_offset y_offset "path/filename"
;;
;;                  'ss_sprite_set_name 
;;                  |                    tile_pixels_wide tile_pixels_high
;;                  |                    |       sheet_tiles_high sheet_tiles_wide
;;                  |                    |       |       sheet_pixels_x_offset sheet_pixels_y_offset
;;                  |                    |       |       |     "path/filename"
;;                  |                    |       |       |     |
;;                  v                    v       v       v     v
;;----------------------------------------------------------------------------
(kern-mk-sprite-set 'ss_u4_shapes        32 32   16 16   0 0  "shapes.png")
(kern-mk-sprite-set 'ss_u4_charset       8  16    8 16   0 0  "charset.png")
(kern-mk-sprite-set 'ss_frame            16 16    4  4   0 0  "frame.png")
(kern-mk-sprite-set 'ss_rune             32 32    4  8   0 0  "rune.png")
(kern-mk-sprite-set 'ss_addon            32 32   16  8   0 0  "addons.png")
(kern-mk-sprite-set 'ss_moons            16 16    4  8   0 0  "moons.png")
(kern-mk-sprite-set 'ss_signs            32 32    1  8   0 0  "signs.png")
(kern-mk-sprite-set 'ss_runestones       32 32    4  8   0 0  "runestones.png")
(kern-mk-sprite-set 'ss_newmonst         32 32   16  8   0 0  "newmonst.png") 
(kern-mk-sprite-set 'ss_newfolks         32 32   16  8   0 0  "newfolks.png")
(kern-mk-sprite-set 'ss_buildings        32 32    1  2   0 0  "tower.png")
(kern-mk-sprite-set 'ss_overlays         32 32   13  8   0 0  "newterrain.png")
(kern-mk-sprite-set 'ss_effects          8  16    3  16  0 0  "effects.png")
(kern-mk-sprite-set 'ss_bigobjects       40 40    8  8   0 0  "bigobjects.png")
(kern-mk-sprite-set 'ss_humanoids        32 32   16  8   0 0  "humanoids.png")
(kern-mk-sprite-set 'ss_people           32 32   16  8   0 0  "hirespeople.png")
(kern-mk-sprite-set 'ss_monsters         32 32   16  8   0 0  "monsters.png")
(kern-mk-sprite-set 'ss_ship             32 32    8  8   0 0  "ship.png")
(kern-mk-sprite-set 'ss_sfx              32 32    8  8   0 0  "sfx.png")
(kern-mk-sprite-set 'ss_creatures        32 32   16  8   0 0  "creatures.png")
(kern-mk-sprite-set 'ss_tools            32 32    8  8   0 0  "tools.png")
(kern-mk-sprite-set 'ss_quests           32 32   16  8   0 0  "quests.png")


;; New paper-doll sprite sets
(kern-mk-sprite-set 'ss_bodies     32 32 4 4 0 0 "bodies.png")
(kern-mk-sprite-set 'ss_adornments 32 32 4 4 0 0 "adornments.png")
(kern-mk-sprite-set 'ss_clothes    32 32 5 4 0 0 "clothes.png")
