;;----------------------------------------------------------------------------
;; Terrains
;;----------------------------------------------------------------------------

(define (terrain-effect-burn obj)
  (kern-obj-apply-damage obj "burning" 10))

(define (terrain-effect-poison obj)
  (if (> (kern-dice-roll "1d20") 10)
      (begin
        (kern-log-msg "Noxious fumes!")
        (kern-obj-add-effect obj ef_poison nil)        
        )))


;; ----------------------------------------------------------------------------
;; Terrain Table 
;; -------------
;; The elements are passed to kern-mk-terrain, which takes these args:
;; 
;;      tag: variable name to reference terrain elsewhere in the script
;;     name: name presented to UI
;;   pclass: determines passability / movement AP cost  (see game.scm)
;;   sprite: animated image
;;        t: transparency to line-of-sight (0 == opaque, 1 == transparent)
;;    light: radiated light strength
;;  step-on: nil, or a procedure to run when an object is placed on the
;;           terrain, which takes the object as it's first argument (2nd is
;;           success/failed movement?)
;; ----------------------------------------------------------------------------

;; TODO: 
;; 
;; Make LOS/!LOS variants
;; Make passable / non-passable variants
;; 
;; Add pclass:
;;   pclass-space
;;   pclass-peaks
;; 
;; Add sprites:
;;   s_astral_void
;;   s_astral_twinkle
;;   s_roiling_fog
;; 
;;   s_acid_lake
;;   s_boiling_mud
;; 
;;   s_tower


(define wilderness_scale_terrains
  (list
   ;;    tag                name            pclass           sprite            t light step-on
   ;;    =================  ==============  =============    ==============    = ===== =======

   ;; Liquid terrain:
   (list 'T_deep            "deep water"    pclass-deep      s_deep            1 0 nil)
   (list 'T_shallow         "shallow water" pclass-deep      s_shallow         1 0 nil)
   (list 'T_shoals          "shoals"        pclass-shoals    s_shoals          1 0 nil)
   ;; Note: We do not currently differentiate between fresh and salt water.

   ;; Other liquid terrain:
   (list 'T_lava            "lava"          pclass-grass     s_lava            1 128 'burn)
   ;(list 'T_acid_lake      "acid lake"     pclass-shoals    s_acid_lake       1   0 'burn)
   ;(list 'T_boiling_mud    "boiling mud"   pclass-shoals    s_boiling_mud     1   0 'burn)

   ;; Astral Void terrain:
;   (list 'T_astral_void     "astral void"   pclass-space     s_astral_void     1 0 'astral-void-stepon)
;   (list 'T_astral_twinkle  "astral void"   pclass-space     s_astral_twinkle  1 0 'astral-void-stepon)
;   (list 'T_astral_fog      "astral void"   pclass-space     s_roiling_fog     1 0 'astral-void-stepon)

   ;; Marshes, Swamps, and Bogs:
   ;; Some definitions:
   ;;   Bogs    - areas of wet land, which gather peat.  
   ;;             Bogs may be found at high altitudes, and in northern climes.
   ;;   Swamps  - areas of wet land, with woody vegetation (trees).
   ;;             Swamps are found in warm climates (mangroves, cypresses).
   ;;   Marshes - areas of wet land, with grassy vegetation
   ;; 
   ;; For our purposes, we are interested in whether they have pclass grass, or grass+shoals,
   ;; whether they block LOS, and whether they are "poisonous" (and to what intensity).
   (list 'T_bog             "bog"           pclass-forest    s_bog             1 0 'terrain-effect-poison)
   ;(list 'T_bog             "bog"           pclass-forest    s_bog             1 0 nil)
   ;(list 'T_bog             "bog"           pclass-forest    s_bog             1 0 nil)

   ;; Flat ground terrain:
   (list 'T_grass           "grass"         pclass-grass     s_grass           1 0 nil)
   (list 'T_trees           "trees"         pclass-trees     s_trees           1 0 nil)
   (list 'T_forest          "forest"        pclass-forest    s_forest          0 0 nil)

   ;; Elevated ground terrain:
   (list 'T_hills           "hills"         pclass-forest    s_hills           1 0 nil)
   (list 'T_mountains       "mountains"     pclass-mountains s_mountains       0 0 nil)
;  (list 'T_mountain_peaks  "high peaks"    pclass-peaks     s_mountain_peak   0 0 nil)

   ;; Portal/Subplace "placeholder" terrains:
   ;; TODO: These will be replaced later.
   ;;       The placeholder concept is useful, but they will be terrain features.
   (list 'Tph_hut           "a hut"         pclass-grass     s_hut             1  0 nil)
   (list 'Tph_village       "a village"     pclass-grass     s_hamlet          1 32 nil)  ;; s_village
   (list 'Tph_town          "a town"        pclass-grass     s_town            1 64 nil)
   (list 'Tph_keep          "a keep"        pclass-grass     s_keep            1 32 nil)
   (list 'Tph_castle        "a castle"      pclass-grass     s_castle          1 64 nil)
;   (list 'Tph_tower         "a tower"       pclass-grass     s_tower           1 32 nil)

   (list 'Tph_ruin          "a ruin"        pclass-grass     s_ruin            1  0 nil)
   (list 'Tph_cave          "a cave"        pclass-grass     s_cave_entrance   1  0 nil)
   (list 'Tph_mine          "a mineshaft"   pclass-grass     s_mine_entrance   1  0 nil)
   (list 'Tph_dungeon       "a dungeon"     pclass-grass     s_dungeon         1  0 nil)
   (list 'Tph_shrine        "a shrine"      pclass-grass     s_shrine          1 32 nil)

   )) ; wilderness_scale_terrains


(define wilderness_scale_terrain_features
  (list
   ;; Large-scale walls and other construction:
   ;; TODO: These will be terrain features.
   (list 'TF_leftwing       "castle wall"   pclass-wall      s_leftwing        1 0 nil)
   (list 'TF_rightwing      "castle wall"   pclass-wall      s_rightwing       1 0 nil)

   ;; Bridges:
   ;; The WE and NS bridge are single-tile-wide bridges spanning a section NS or WE
   ;; TODO: These should be terrain features.
   ;; TODO: Perhaps a chance-of-troll-encounter procedure for the step-on arg?
   (list 'TF_bridge_WE      "bridge (WE)"   pclass-bridge    s_ew_bridge       1 0 nil)
   (list 'TF_bridge_NS      "bridge (NS)"   pclass-bridge    s_ns_bridge       1 0 nil)

   ;; Trails:
   ;; These are arranged in a 4x4 grid like:
   ;;     0 1 2 3
   ;;     4 5 6 7
   ;;     8 9 a b
   ;;     c d e f
   ;; TODO: These will be terrain features.
   (list 'TF_trail_0        "trail"         pclass-grass     s_trail_0         1 0 nil)
   (list 'TF_trail_1        "trail"         pclass-grass     s_trail_1         1 0 nil)
   (list 'TF_trail_2        "trail"         pclass-grass     s_trail_2         1 0 nil)
   (list 'TF_trail_3        "trail"         pclass-grass     s_trail_3         1 0 nil)
   (list 'TF_trail_4        "trail"         pclass-grass     s_trail_4         1 0 nil)
   (list 'TF_trail_5        "trail"         pclass-grass     s_trail_5         1 0 nil)
   (list 'TF_trail_6        "trail"         pclass-grass     s_trail_6         1 0 nil)
   (list 'TF_trail_7        "trail"         pclass-grass     s_trail_7         1 0 nil)
   (list 'TF_trail_8        "trail"         pclass-grass     s_trail_8         1 0 nil)
   (list 'TF_trail_9        "trail"         pclass-grass     s_trail_9         1 0 nil)
   (list 'TF_trail_a        "trail"         pclass-grass     s_trail_a         1 0 nil)
   (list 'TF_trail_b        "trail"         pclass-grass     s_trail_b         1 0 nil)
   (list 'TF_trail_c        "trail"         pclass-grass     s_trail_c         1 0 nil)
   (list 'TF_trail_d        "trail"         pclass-grass     s_trail_d         1 0 nil)
   (list 'TF_trail_e        "trail"         pclass-grass     s_trail_e         1 0 nil)
   (list 'TF_trail_f        "trail"         pclass-grass     s_trail_f         1 0 nil)

   )) ; wilderness_scale_terrain_features


(define town_scale_terrains
  (list

;; Small-scale (Town) Terrain:
   (list 't_cobblestone     "cobblestone"   pclass-grass     s_cobblestone     1 0 nil)
   (list 't_flagstones      "flagstones"    pclass-grass     s_flagstone       1 0 nil)
   (list 't_fireplace       "fireplace"     pclass-grass     s_fireplace       1 2048 'burn)
   (list 't_secret_door     "secret door"   pclass-grass     s_secret_door     0 0 nil)

   (list 't_red_roses       "roses"         pclass-forest    s_trees_red       1 0 nil)

   (list 't_wall_rock       "rock wall"     pclass-wall      s_wall_rock       0 0 nil)
   (list 't_wall            "wall"          pclass-wall      s_wall_stone      0 0 nil)
   (list 't_wall_torch      "wall torch"    pclass-wall      s_wall_torch      0 512 'burn)
   (list 't_boulder         "boulder"       pclass-wall      s_boulder         1 0 nil)
   (list 't_ankh            "ankh"          pclass-wall      s_ankh            1 0 nil)
   (list 't_altar           "altar"         pclass-wall      s_altar           1 0 nil)
   (list 't_pillar          "pillar"        pclass-wall      s_pillar          1 0 nil)
   (list 't_arrow_slit      "arrow slit"    pclass-wall      s_arrow_slit      1 0 nil)
   (list 't_window_in_stone "window"        pclass-wall      s_window_in_stone 1 0 nil)

   (list 't_counter_middle  "counter"       pclass-wall      s_counter_2x1_c   1 0 nil)
   (list 't_counter_right   "counter"       pclass-wall      s_counter_2x1_e   1 0 nil)
   (list 't_counter_left    "counter"       pclass-wall      s_counter_2x1_w   1 0 nil)
   (list 't_counter_1x1     "counter"       pclass-wall      s_counter_1x1     1 0 nil)

   (list 't_deck            "deck"          pclass-grass     s_deck            1 0 nil)
   (list 't_ship_hull       "ship's hull"   pclass-wall      s_wall            1 0 nil)
   (list 't_sh_hull_NE      "ship's hull"   pclass-wall      s_wall_b          1 0 nil)
   (list 't_sh_hull_NW      "ship's hull"   pclass-wall      s_wall_a          1 0 nil)
   (list 't_sh_hull_SE      "ship's hull"   pclass-wall      s_wall_c          1 0 nil)
   (list 't_sh_hull_SW      "ship's hull"   pclass-wall      s_wall_d          1 0 nil)
   (list 't_mast            "mast"          pclass-wall      s_mast            1 0 nil)
   (list 't_ships_wheel     "ship's wheel"  pclass-wall      s_ships_wheel     1 0 nil)

   (list 't_sea_wall        "sea wall"      pclass-wall      s_wall            0 0 nil)
   (list 't_sea_wall_NE     "sea wall"      pclass-wall      s_wall_b          0 0 nil)
   (list 't_sea_wall_NW     "sea wall"      pclass-wall      s_wall_a          0 0 nil)
   (list 't_sea_wall_SE     "sea wall"      pclass-wall      s_wall_c          0 0 nil)
   (list 't_sea_wall_SW     "sea wall"      pclass-wall      s_wall_d          0 0 nil)

;; Bridges:
;; The WE and NS bridge are single-tile-wide bridges spanning a section NS or WE
;; The "wide" bridge sections are for wide (multi-tile) bridges on small-scale maps
;(kern-mk-terrain 't_bridge_WE             "bridge (WE)"  14 s_bridge_WE             1 2  0 nil)
;(kern-mk-terrain 't_bridge_NS             "bridge (NS)"  14 s_bridge_NS             1 2  0 nil)

;(kern-mk-terrain 't_bridge_NS_wide_left   "bridge (NS left)"    14 s_bridge_NS_wide_left   1 2  0 nil)
;(kern-mk-terrain 't_bridge_NS_wide_middle "bridge (NS middle)"  14 s_bridge_NS_wide_middle 1 2  0 nil)
;(kern-mk-terrain 't_bridge_NS_wide_right  "bridge (NS right)"   14 s_bridge_NS_wide_right  1 2  0 nil)

;(kern-mk-terrain 't_bridge_WE_wide_top    "bridge (WE top)"     14 s_bridge_WE_wide_top    1 2  0 nil)
;(kern-mk-terrain 't_bridge_WE_wide_middle "bridge (WE middle)"  14 s_bridge_WE_wide_middle 1 2  0 nil)
;(kern-mk-terrain 't_bridge_WE_wide_bottom "bridge (WE bottom)"  14 s_bridge_WE_wide_bottom 1 2  0 nil)

   (list 't_A               "an A"          pclass-wall      s_A               1 0 nil)
   (list 't_B               "a B"           pclass-wall      s_B               1 0 nil)
   (list 't_C               "a C"           pclass-wall      s_C               1 0 nil)
   (list 't_D               "a D"           pclass-wall      s_D               1 0 nil)
   (list 't_E               "an E"          pclass-wall      s_E               1 0 nil)
   (list 't_F               "an F"          pclass-wall      s_F               1 0 nil)
   (list 't_G               "a G"           pclass-wall      s_G               1 0 nil)
   (list 't_H               "an H"          pclass-wall      s_H               1 0 nil)
   (list 't_I               "an I"          pclass-wall      s_I               1 0 nil)
   (list 't_J               "a J"           pclass-wall      s_J               1 0 nil)
   (list 't_K               "a K"           pclass-wall      s_K               1 0 nil)
   (list 't_L               "an L"          pclass-wall      s_L               1 0 nil)
   (list 't_M               "an M"          pclass-wall      s_M               1 0 nil)
   (list 't_N               "an N"          pclass-wall      s_N               1 0 nil)
   (list 't_O               "an O"          pclass-wall      s_O               1 0 nil)
   (list 't_P               "a P"           pclass-wall      s_P               1 0 nil)
   (list 't_Q               "a Q"           pclass-wall      s_Q               1 0 nil)
   (list 't_R               "an R"          pclass-wall      s_R               1 0 nil)
   (list 't_S               "an S"          pclass-wall      s_S               1 0 nil)
   (list 't_T               "a T"           pclass-wall      s_T               1 0 nil)
   (list 't_U               "a U"           pclass-wall      s_U               1 0 nil)
   (list 't_V               "a V"           pclass-wall      s_V               1 0 nil)
   (list 't_W               "a W"           pclass-wall      s_W               1 0 nil)
   (list 't_X               "an X"          pclass-wall      s_X               1 0 nil)
   (list 't_Y               "a Y"           pclass-wall      s_Y               1 0 nil)
   (list 't_Z               "a Z"           pclass-wall      s_Z               1 0 nil)

   (list 't_rune_A          "a rune"        pclass-wall      s_rune_A          1 0 nil)
   (list 't_rune_B          "a rune"        pclass-wall      s_rune_B          1 0 nil)
   (list 't_rune_C          "a rune"        pclass-wall      s_rune_C          1 0 nil)
   (list 't_rune_D          "a rune"        pclass-wall      s_rune_D          1 0 nil)
   (list 't_rune_E          "a rune"        pclass-wall      s_rune_E          1 0 nil)
   (list 't_rune_F          "a rune"        pclass-wall      s_rune_F          1 0 nil)
   (list 't_rune_G          "a rune"        pclass-wall      s_rune_G          1 0 nil)
   (list 't_rune_H          "a rune"        pclass-wall      s_rune_H          1 0 nil)
   (list 't_rune_I          "a rune"        pclass-wall      s_rune_I          1 0 nil)
   (list 't_rune_J          "a rune"        pclass-wall      s_rune_J          1 0 nil)
   (list 't_rune_K          "a rune"        pclass-wall      s_rune_K          1 0 nil)
   (list 't_rune_L          "a rune"        pclass-wall      s_rune_L          1 0 nil)
   (list 't_rune_M          "a rune"        pclass-wall      s_rune_M          1 0 nil)
   (list 't_rune_N          "a rune"        pclass-wall      s_rune_N          1 0 nil)
   (list 't_rune_O          "a rune"        pclass-wall      s_rune_O          1 0 nil)
   (list 't_rune_P          "a rune"        pclass-wall      s_rune_P          1 0 nil)
   (list 't_rune_Q          "a rune"        pclass-wall      s_rune_Q          1 0 nil)
   (list 't_rune_R          "a rune"        pclass-wall      s_rune_R          1 0 nil)
   (list 't_rune_S          "a rune"        pclass-wall      s_rune_S          1 0 nil)
   (list 't_rune_T          "a rune"        pclass-wall      s_rune_T          1 0 nil)
   (list 't_rune_U          "a rune"        pclass-wall      s_rune_U          1 0 nil)
   (list 't_rune_V          "a rune"        pclass-wall      s_rune_V          1 0 nil)
   (list 't_rune_W          "a rune"        pclass-wall      s_rune_W          1 0 nil)
   (list 't_rune_X          "a rune"        pclass-wall      s_rune_X          1 0 nil)
   (list 't_rune_Y          "a rune"        pclass-wall      s_rune_Y          1 0 nil)
   (list 't_rune_Z          "a rune"        pclass-wall      s_rune_Z          1 0 nil)
   (list 't_rune_TH         "a rune"        pclass-wall      s_rune_TH         1 0 nil)
   (list 't_rune_EE         "a rune"        pclass-wall      s_rune_EE         1 0 nil)
   (list 't_rune_NG         "a rune"        pclass-wall      s_rune_NG         1 0 nil)
   (list 't_rune_EA         "a rune"        pclass-wall      s_rune_EA         1 0 nil)
   (list 't_rune_ST         "a rune"        pclass-wall      s_rune_ST         1 0 nil)
   (list 't_rune_DOT        "a rune"        pclass-wall      s_rune_DOTSEP     1 0 nil)


;; Hmmm...
   (list 't_fire_terrain    "fire"          pclass-grass     s_field_fire      1 512  'burn)
   (list 't_bridge          "bridge"        pclass-grass     s_ew_bridge       1 0 nil)
   (list 't_bridge_top      "bridge"        pclass-grass     s_bridge_top      1 0 nil)
   (list 't_bridge_bottom   "bridge"        pclass-grass     s_bridge_bottom   1 0 nil)
   (list 't_brazier         "brazier"       pclass-wall      s_brazier         1 512 'burn)

   )) ; town_scale_terrains

(define town_scale_terrain_features
  (list
   ;; A number of what used to be terrain will become terrain features...
   ))


(map (lambda (terrain) (apply kern-mk-terrain terrain)) wilderness_scale_terrains)
(map (lambda (terrain) (apply kern-mk-terrain terrain))       town_scale_terrains)
;; wilderness_scale_terrain_features
;; town_scale_terrain_features


;; ===========================================================================


;; old/existing terrains:
;;     to be replaced by the above, which are split into large/small scale terrains

;; ----------------------------------------------------------------------------
;; Terrain table
;;   t = transparency (to line-of-sight)
;;   The pclass definitions are in game.scm
;; ----------------------------------------------------------------------------

(define terrains
  (list
   ;;    tag                name            pclass           sprite            t light step-on
   ;;    =================  ==============  =============    ==============    = ===== =======
   (list 't_deep            "deep water"    pclass-deep      s_deep            1 0 nil)
   (list 't_shallow         "shallow water" pclass-deep      s_shallow         1 0 nil)
   (list 't_shoals          "shoals"        pclass-shoals    s_shoals          1 0 nil)

   (list 't_grass           "grass"         pclass-grass     s_grass           1 0 nil)

   (list 't_trees           "trees"         pclass-trees     s_trees           1 0 nil)
   (list 't_red_roses       "roses"         pclass-forest    s_trees_red       1 0 nil)
   (list 't_forest_v        "forest"        pclass-forest    s_forest          1 0 nil)
   (list 't_forest          "forest"        pclass-forest    s_forest          0 0 nil)

   (list 't_hills           "hills"         pclass-forest    s_hills           1 0 nil)
   (list 't_mountains_v     "mountains"     pclass-mountains s_mountains       1 0 nil)
   (list 't_mountains       "mountains"     pclass-mountains s_mountains       0 0 nil)

   (list 't_bog             "bog"           pclass-forest    s_bog             1 0 'terrain-effect-poison)

   (list 't_lava            "lava"          pclass-grass     s_lava            1  128 'burn)
   (list 't_fire_terrain    "fire"          pclass-grass     s_field_fire      1  512 'burn)
   (list 't_fireplace       "fireplace"     pclass-grass     s_fireplace       1 2048 'burn)

   (list 't_cobblestone     "cobblestone"   pclass-grass     s_cobblestone     1 0 nil)
   (list 't_flagstones      "flagstones"    pclass-grass     s_flagstone       1 0 nil)

   (list 't_leftwing        "castle wall"   pclass-wall      s_leftwing        1 0 nil)
   (list 't_rightwing       "castle wall"   pclass-wall      s_rightwing       1 0 nil)

   (list 't_ship_hull       "ship's hull"   pclass-wall      s_wall            1 0 nil)
   (list 't_sh_hull_NE      "ship's hull"   pclass-wall      s_wall_b          1 0 nil)
   (list 't_sh_hull_NW      "ship's hull"   pclass-wall      s_wall_a          1 0 nil)
   (list 't_sh_hull_SE      "ship's hull"   pclass-wall      s_wall_c          1 0 nil)
   (list 't_sh_hull_SW      "ship's hull"   pclass-wall      s_wall_d          1 0 nil)
   (list 't_mast            "mast"          pclass-wall      s_mast            1 0 nil)
   (list 't_ships_wheel     "ship's wheel"  pclass-wall      s_ships_wheel     1 0 nil)
   (list 't_deck            "deck"          pclass-grass     s_deck            1 0 nil)

   (list 't_boulder         "boulder"       pclass-boulder   s_boulder         1 0 'slip)

   (list 't_wall_rock_v     "rock wall"     pclass-wall      s_wall_rock       1 0 nil)
   (list 't_wall_rock       "rock wall"     pclass-wall      s_wall_rock       0 0 nil)

   (list 't_wall_v          "wall"          pclass-wall      s_wall_stone      1 0 nil)
   (list 't_wall            "wall"          pclass-wall      s_wall_stone      0 0 nil)

   (list 't_wall_torch      "wall torch"    pclass-wall      s_wall_torch      0 512 'burn)
   (list 't_arrow_slit      "arrow slit"    pclass-wall      s_arrow_slit      1 0 nil)
   (list 't_window_in_stone "window"        pclass-wall      s_window_in_stone 1 0 nil)

   (list 't_secret_door     "secret door"   pclass-grass     s_secret_door     0 0 nil)

   (list 't_sea_wall_v      "sea wall"      pclass-wall      s_wall            1 0 nil)
   (list 't_sea_wall        "sea wall"      pclass-wall      s_wall            0 0 nil)

   (list 't_sea_wall_NE     "sea wall"      pclass-wall      s_wall_b          0 0 nil)
   (list 't_sea_wall_NW     "sea wall"      pclass-wall      s_wall_a          0 0 nil)
   (list 't_sea_wall_SE     "sea wall"      pclass-wall      s_wall_c          0 0 nil)
   (list 't_sea_wall_SW     "sea wall"      pclass-wall      s_wall_d          0 0 nil)

   (list 't_ankh            "ankh"          pclass-wall      s_ankh            1 0 nil)
   (list 't_altar           "altar"         pclass-wall      s_altar           1 0 nil)
   (list 't_pillar          "pillar"        pclass-wall      s_pillar          1 0 nil)

   (list 't_counter_2x1_w   "counter"       pclass-wall      s_counter_2x1_w   1 0 nil)
   (list 't_counter_2x1_c   "counter"       pclass-wall      s_counter_2x1_c   1 0 nil)
   (list 't_counter_2x1_e   "counter"       pclass-wall      s_counter_2x1_e   1 0 nil)
   (list 't_counter_1x1     "counter"       pclass-wall      s_counter_1x1     1 0 nil)

   (list 't_bridge_WE       "bridge"       pclass-bridge    s_ew_bridge        1 0 nil)

   (list 't_trail_0         "trail"         pclass-grass     s_trail_0         1 0 nil)
   (list 't_trail_1         "trail"         pclass-grass     s_trail_1         1 0 nil)
   (list 't_trail_2         "trail"         pclass-grass     s_trail_2         1 0 nil)
   (list 't_trail_3         "trail"         pclass-grass     s_trail_3         1 0 nil)
   (list 't_trail_4         "trail"         pclass-grass     s_trail_4         1 0 nil)
   (list 't_trail_5         "trail"         pclass-grass     s_trail_5         1 0 nil)
   (list 't_trail_6         "trail"         pclass-grass     s_trail_6         1 0 nil)
   (list 't_trail_7         "trail"         pclass-grass     s_trail_7         1 0 nil)
   (list 't_trail_8         "trail"         pclass-grass     s_trail_8         1 0 nil)
   (list 't_trail_9         "trail"         pclass-grass     s_trail_9         1 0 nil)
   (list 't_trail_a         "trail"         pclass-grass     s_trail_a         1 0 nil)
   (list 't_trail_b         "trail"         pclass-grass     s_trail_b         1 0 nil)
   (list 't_trail_c         "trail"         pclass-grass     s_trail_c         1 0 nil)
   (list 't_trail_d         "trail"         pclass-grass     s_trail_d         1 0 nil)
   (list 't_trail_e         "trail"         pclass-grass     s_trail_e         1 0 nil)
   (list 't_trail_f         "trail"         pclass-grass     s_trail_f         1 0 nil)

   (list 't_A               "an A"          pclass-wall      s_A               1 0 nil)
   (list 't_B               "a B"           pclass-wall      s_B               1 0 nil)
   (list 't_C               "a C"           pclass-wall      s_C               1 0 nil)
   (list 't_D               "a D"           pclass-wall      s_D               1 0 nil)
   (list 't_E               "an E"          pclass-wall      s_E               1 0 nil)
   (list 't_F               "an F"          pclass-wall      s_F               1 0 nil)
   (list 't_G               "a G"           pclass-wall      s_G               1 0 nil)
   (list 't_H               "an H"          pclass-wall      s_H               1 0 nil)
   (list 't_I               "an I"          pclass-wall      s_I               1 0 nil)
   (list 't_J               "a J"           pclass-wall      s_J               1 0 nil)
   (list 't_K               "a K"           pclass-wall      s_K               1 0 nil)
   (list 't_L               "an L"          pclass-wall      s_L               1 0 nil)
   (list 't_M               "an M"          pclass-wall      s_M               1 0 nil)
   (list 't_N               "an N"          pclass-wall      s_N               1 0 nil)
   (list 't_O               "an O"          pclass-wall      s_O               1 0 nil)
   (list 't_P               "a P"           pclass-wall      s_P               1 0 nil)
   (list 't_Q               "a Q"           pclass-wall      s_Q               1 0 nil)
   (list 't_R               "an R"          pclass-wall      s_R               1 0 nil)
   (list 't_S               "an S"          pclass-wall      s_S               1 0 nil)
   (list 't_T               "a T"           pclass-wall      s_T               1 0 nil)
   (list 't_U               "a U"           pclass-wall      s_U               1 0 nil)
   (list 't_V               "a V"           pclass-wall      s_V               1 0 nil)
   (list 't_W               "a W"           pclass-wall      s_W               1 0 nil)
   (list 't_X               "an X"          pclass-wall      s_X               1 0 nil)
   (list 't_Y               "a Y"           pclass-wall      s_Y               1 0 nil)
   (list 't_Z               "a Z"           pclass-wall      s_Z               1 0 nil)

   (list 't_rune_A          "a rune"        pclass-wall      s_rune_A          1 0 nil)
   (list 't_rune_B          "a rune"        pclass-wall      s_rune_B          1 0 nil)
   (list 't_rune_C          "a rune"        pclass-wall      s_rune_C          1 0 nil)
   (list 't_rune_D          "a rune"        pclass-wall      s_rune_D          1 0 nil)
   (list 't_rune_E          "a rune"        pclass-wall      s_rune_E          1 0 nil)
   (list 't_rune_F          "a rune"        pclass-wall      s_rune_F          1 0 nil)
   (list 't_rune_G          "a rune"        pclass-wall      s_rune_G          1 0 nil)
   (list 't_rune_H          "a rune"        pclass-wall      s_rune_H          1 0 nil)
   (list 't_rune_I          "a rune"        pclass-wall      s_rune_I          1 0 nil)
   (list 't_rune_J          "a rune"        pclass-wall      s_rune_J          1 0 nil)
   (list 't_rune_K          "a rune"        pclass-wall      s_rune_K          1 0 nil)
   (list 't_rune_L          "a rune"        pclass-wall      s_rune_L          1 0 nil)
   (list 't_rune_M          "a rune"        pclass-wall      s_rune_M          1 0 nil)
   (list 't_rune_N          "a rune"        pclass-wall      s_rune_N          1 0 nil)
   (list 't_rune_O          "a rune"        pclass-wall      s_rune_O          1 0 nil)
   (list 't_rune_P          "a rune"        pclass-wall      s_rune_P          1 0 nil)
   (list 't_rune_Q          "a rune"        pclass-wall      s_rune_Q          1 0 nil)
   (list 't_rune_R          "a rune"        pclass-wall      s_rune_R          1 0 nil)
   (list 't_rune_S          "a rune"        pclass-wall      s_rune_S          1 0 nil)
   (list 't_rune_T          "a rune"        pclass-wall      s_rune_T          1 0 nil)
   (list 't_rune_U          "a rune"        pclass-wall      s_rune_U          1 0 nil)
   (list 't_rune_V          "a rune"        pclass-wall      s_rune_V          1 0 nil)
   (list 't_rune_W          "a rune"        pclass-wall      s_rune_W          1 0 nil)
   (list 't_rune_X          "a rune"        pclass-wall      s_rune_X          1 0 nil)
   (list 't_rune_Y          "a rune"        pclass-wall      s_rune_Y          1 0 nil)
   (list 't_rune_Z          "a rune"        pclass-wall      s_rune_Z          1 0 nil)
   (list 't_rune_TH         "a rune"        pclass-wall      s_rune_TH         1 0 nil)
   (list 't_rune_EE         "a rune"        pclass-wall      s_rune_EE         1 0 nil)
   (list 't_rune_NG         "a rune"        pclass-wall      s_rune_NG         1 0 nil)
   (list 't_rune_EA         "a rune"        pclass-wall      s_rune_EA         1 0 nil)
   (list 't_rune_ST         "a rune"        pclass-wall      s_rune_ST         1 0 nil)
   (list 't_rune_DOT        "a rune"        pclass-wall      s_rune_DOTSEP     1 0 nil)

   ))

(map (lambda (terrain) (apply kern-mk-terrain terrain)) terrains)
