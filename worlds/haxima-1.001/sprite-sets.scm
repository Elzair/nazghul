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
(kern-mk-sprite-set 'ss_frame_orig       16 16    4  4   0 0   "images/gmcnutt/frame_pieces.16x16.plain_blue_bars.png")
(kern-mk-sprite-set 'ss_moons            16 16    8  1   0 0   "images/gmcnutt/moons-16x16.png")

(kern-mk-sprite-set 'ss_little_sprites   8  16    8 16   0 0   "images/jsteele/charset-8x16.png")
(kern-mk-sprite-set 'ss_rune_font_8x16   8  16    1 32   0 0   "images/sglasby/rune_font_8x16.png")
(kern-mk-sprite-set 'ss_rune_font_32x32  32 32    4  8   0 0   "images/sglasby/rune_font_32x32.png")

(kern-mk-sprite-set 'ss_u4               32 32   16 16   0 0   "images/jsteele/shapes-32x32.png")
(kern-mk-sprite-set 'ss_angband          32 32   63 32   0 0   "images/dgervais/angband-32x32.png")
(kern-mk-sprite-set 'ss_dg_wearable      32 32   20 12   0 0   "images/dgervais/dg_wearable_items-32x32.png")
(kern-mk-sprite-set 'ss_dg_missiles      32 32   16 16   0 0   "images/dgervais/dg_missiles.trans.png")

(kern-mk-sprite-set 'ss_mine             32 32   16  8   0 0   "images/mixed/mine-32x32.png")
(kern-mk-sprite-set 'ss_mechanisms       32 32    4  8   0 0   "images/sglasby/mechanisms.png")
(kern-mk-sprite-set 'ss_lost_dragon      32 32    8  8   0 0   "images/dbailey/lost_dragon.png")
(kern-mk-sprite-set 'ss_features_1       32 32    8  8   0 0   "images/gmcnutt/features.png")
(kern-mk-sprite-set 'ss_features_2       32 32    8  8   0 0   "images/gmcnutt/features2.png")

(kern-mk-sprite-set 'ss_kg_roads         32 32    8  4   0 0   "images/kgabbert/new_roads.png")
(kern-mk-sprite-set 'ss_kg_terrains      32 32   32  8   0 0   "images/kgabbert/new_terrains.png")
