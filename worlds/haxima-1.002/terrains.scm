;;----------------------------------------------------------------------------
;; Terrains
;;----------------------------------------------------------------------------

(define (terrain-effect-burn obj)
  (kern-obj-apply-damage obj "burning" 10))

(define (terrain-effect-poison obj)
  (if (and (> (kern-dice-roll "1d20") 10)
           (kern-obj-is-being? obj)
           (kern-obj-add-effect obj ef_poison nil))
      (kern-log-msg "Noxious fumes!")))

(define terrains
  (list
   ;;    tag                name            pclass           sprite            t light step-on
   ;;    =================  ==============  =============    ==============    = ===== =======
   (list 't_stars           "stars"         pclass-space     s_stars           1 0 nil)
   (list 't_deep            "deep water"    pclass-deep      s_deep            1 0 nil)
   (list 't_sunlit_deep     "deep water"    pclass-deep      s_deep            1 64 nil)
   (list 't_shallow         "water"          pclass-deep      s_shallow         1 0 nil)
   (list 't_blendable_shoals    "shallow water"  pclass-shoals    s_shoals          1 0 nil)
   (list 't_shoals          "shallow water"  pclass-shoals    s_shoals          1 0 nil)

   (list 't_grass           "grass"         pclass-grass     s_grass           1 0 nil)
   (list 't_sunlit_grass    "grass"         pclass-grass     s_grass           1 64 nil)

   (list 't_trees           "trees"         pclass-trees     s_trees           1 0 nil)
   (list 't_forest_v        "forest"        pclass-forest    s_forest          1 0 nil)
   (list 't_forest          "forest"        pclass-forest    s_forest          0 0 nil)

   (list 't_hills           "hills"         pclass-trees     s_hills           1 0 nil)
   (list 't_mountains_v     "mountains"     pclass-mountains s_mountains       1 0 nil)
   (list 't_mountains       "mountains"     pclass-mountains s_mountains       0 0 nil)
   (list 't_fake_mountains  "mountains"     pclass-grass     s_mountains       0 0 nil)

   (list 't_bog             "bog"           pclass-forest    s_bog             1 0 'terrain-effect-poison)

   (list 't_lava            "lava"          pclass-grass     s_lava            1  128 'burn)
   (list 't_fake_lava       "lava"          pclass-grass     s_lava            1  128 nil)
   (list 't_deep_lava       "deep lava"     pclass-deep      s_deep_lava       1  0  'great-burn)
   (list 't_fire_terrain    "fire"          pclass-grass     s_field_fire      1  512 'burn)
   (list 't_fireplace       "fireplace"     pclass-grass     s_fireplace       1 2048 'burn)

   (list 't_cobblestone     "cobblestone"   pclass-grass     s_cobblestone     1 0 nil)
   (list 't_flagstones      "flagstones"    pclass-grass     s_flagstone       1 0 nil)
   (list 't_inv_wall        "flagstones"    pclass-repel     s_flagstone       1 0 'burn)
   (list 't_doorway         "doorway"       pclass-grass     s_doorway 1 0 nil)
   (list 't_leftwing        "castle wall"   pclass-wall      s_leftwing        1 0 nil)
   (list 't_rightwing       "castle wall"   pclass-wall      s_rightwing       1 0 nil)

   (list 't_ship_hull       "ship's bulwark"   pclass-wall      s_wall         1 0 nil)
   (list 't_ship_hull2      "ship's hull"   pclass-wall      s_wall            0 0 nil)
   (list 't_sh_hull_NE      "ship's hull"   pclass-wall      s_wall_b          1 0 nil)
   (list 't_sh_hull_NW      "ship's hull"   pclass-wall      s_wall_a          1 0 nil)
   (list 't_sh_hull_SE      "ship's hull"   pclass-wall      s_wall_c          1 0 nil)
   (list 't_sh_hull_SW      "ship's hull"   pclass-wall      s_wall_d          1 0 nil)
   (list 't_mast            "mast"          pclass-wall      s_mast            1 0 nil)
   (list 't_ships_wheel     "ship's wheel"  pclass-wall      s_ships_wheel     1 0 nil)
   (list 't_deck            "deck"          pclass-grass     s_deck            1 0 nil)

   (list 't_boulder         "boulder"       pclass-boulder   s_boulder         1 0 nil)

   (list 't_wall_rock_v     "rock wall"     pclass-wall      s_wall_rock       1 0 nil)
   (list 't_wall_rock       "rock wall"     pclass-wall      s_wall_rock       0 0 nil)
   (list 't_fake_wall_rock  "rock wall"     pclass-forest    s_secret_rock       0 0 nil)

   (list 't_wall_v          "wall"          pclass-wall      s_wall_stone      1 0 nil)
   (list 't_wall            "wall"          pclass-wall      s_wall_stone      0 0 nil)
   (list 't_fake_wall       "wall"          pclass-forest    s_wall_stone      0 0 nil)

   (list 't_wall_torch      "wall torch"    pclass-wall      s_wall_torch      0 1024 'burn)
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
   (list 't_statue          "statue"        pclass-wall      s_statue          1 0 nil)
   (list 't_altar           "altar"         pclass-wall      s_altar           1 0 nil)
   (list 't_rune_altar      "rune altar"    pclass-wall      s_altar           1 64 nil)
   (list 't_active_altar    "activated rune altar" pclass-wall s_active_altar  1 512 nil)
   (list 't_pillar          "pillar"        pclass-wall      s_pillar          1 0 nil)
   (list 't_false_pillar    "pillar"        pclass-grass     s_pillar          1 0 nil)

   (list 't_counter_2x1_w   "counter"       pclass-boulder   s_counter_2x1_w   1 0 nil)
   (list 't_counter_2x1_c   "counter"       pclass-boulder   s_counter_2x1_c   1 0 nil)
   (list 't_counter_2x1_e   "counter"       pclass-boulder   s_counter_2x1_e   1 0 nil)
   (list 't_counter_1x1     "counter"       pclass-boulder   s_counter_1x1     1 0 nil)

   (list 't_bridge_WE       "bridge"        pclass-bridge    s_ew_bridge       1 0 nil)
   (list 't_bridge_NS       "bridge"        pclass-bridge    s_ns_bridge       1 0 nil)
   (list 't_lava_bridge_NS  "bridge"        pclass-bridge    s_ns_bridge       1 0 nil)
   (list 't_chasm           "chasm"         pclass-space     s_null            1 0 nil)
   (list 't_void            "empty space"          pclass-space     s_null            1 0 nil)

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
   (list 't_fake_O          "an O"          pclass-forest    s_O               1 0 nil)
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

   (list 't_rune_A          "a runic sign"        pclass-wall      s_rune_A          1 0 nil)
   (list 't_rune_B          "a runic sign"        pclass-wall      s_rune_B          1 0 nil)
   (list 't_rune_C          "a runic sign"        pclass-wall      s_rune_C          1 0 nil)
   (list 't_rune_D          "a runic sign"        pclass-wall      s_rune_D          1 0 nil)
   (list 't_rune_E          "a runic sign"        pclass-wall      s_rune_E          1 0 nil)
   (list 't_rune_F          "a runic sign"        pclass-wall      s_rune_F          1 0 nil)
   (list 't_rune_G          "a runic sign"        pclass-wall      s_rune_G          1 0 nil)
   (list 't_rune_H          "a runic sign"        pclass-wall      s_rune_H          1 0 nil)
   (list 't_rune_I          "a runic sign"        pclass-wall      s_rune_I          1 0 nil)
   (list 't_rune_J          "a runic sign"        pclass-wall      s_rune_J          1 0 nil)
   (list 't_rune_K          "a runic sign"        pclass-wall      s_rune_K          1 0 nil)
   (list 't_rune_L          "a runic sign"        pclass-wall      s_rune_L          1 0 nil)
   (list 't_rune_M          "a runic sign"        pclass-wall      s_rune_M          1 0 nil)
   (list 't_rune_N          "a runic sign"        pclass-wall      s_rune_N          1 0 nil)
   (list 't_rune_O          "a runic sign"        pclass-wall      s_rune_O          1 0 nil)
   (list 't_rune_P          "a runic sign"        pclass-wall      s_rune_P          1 0 nil)
   (list 't_rune_Q          "a runic sign"        pclass-wall      s_rune_Q          1 0 nil)
   (list 't_rune_R          "a runic sign"        pclass-wall      s_rune_R          1 0 nil)
   (list 't_rune_S          "a runic sign"        pclass-wall      s_rune_S          1 0 nil)
   (list 't_rune_T          "a runic sign"        pclass-wall      s_rune_T          1 0 nil)
   (list 't_rune_U          "a runic sign"        pclass-wall      s_rune_U          1 0 nil)
   (list 't_rune_V          "a runic sign"        pclass-wall      s_rune_V          1 0 nil)
   (list 't_rune_W          "a runic sign"        pclass-wall      s_rune_W          1 0 nil)
   (list 't_rune_X          "a runic sign"        pclass-wall      s_rune_X          1 0 nil)
   (list 't_rune_Y          "a runic sign"        pclass-wall      s_rune_Y          1 0 nil)
   (list 't_rune_Z          "a runic sign"        pclass-wall      s_rune_Z          1 0 nil)
   (list 't_rune_TH         "a runic sign"        pclass-wall      s_rune_TH         1 0 nil)
   (list 't_rune_EE         "a runic sign"        pclass-wall      s_rune_EE         1 0 nil)
   (list 't_rune_NG         "a runic sign"        pclass-wall      s_rune_NG         1 0 nil)
   (list 't_rune_EA         "a runic sign"        pclass-wall      s_rune_EA         1 0 nil)
   (list 't_rune_ST         "a runic sign"        pclass-wall      s_rune_ST         1 0 nil)
   (list 't_rune_DOT        "a runic sign"        pclass-wall      s_rune_DOTSEP     1 0 nil)
   (list 't_equip_sign    "an equipment shop sign" pclass-wall s_torch_sign  0 0 nil)
   (list 't_weapon_sign   "an arms shop sign" pclass-wall s_shield_sign 0 0 nil)
   (list 't_healer_sign   "a hospital sign" pclass-wall s_ankh_sign   0 0 nil)
   (list 't_tavern_sign   "a tavern sign" pclass-wall s_beer_sign   0 0 nil)
   (list 't_inn_sign      "an inn sign" pclass-wall s_bed_sign    0 0 nil) 
   (list 't_alchemy_sign      "an alchemy sign" pclass-wall s_potion_sign    0 0 nil) 
   (list 't_magic_sign      "a reagent shop sign" pclass-wall s_mushroom_sign    0 0 nil) 

   ))

(map (lambda (terrain) (apply kern-mk-terrain terrain)) terrains)

;;----------------------------------------------------------------------------
;; Make some blended shore terrain types

(define (mk-shore-terrain tag . sprites)
  (kern-mk-terrain tag "shallow water" pclass-shoals
                   (mk-composite-sprite (cons s_shoals sprites))
                   1 0 nil))

(mk-shore-terrain 't_shore_n  s_grass_n )
(mk-shore-terrain 't_shore_w  s_grass_w )
(mk-shore-terrain 't_shore_nw s_grass_nw)
(mk-shore-terrain 't_shore_e  s_grass_e )
(mk-shore-terrain 't_shore_ne s_grass_ne)
(mk-shore-terrain 't_shore_we s_grass_e s_grass_w)
(mk-shore-terrain 't_shore_nwe s_grass_ne s_grass_nw)
(mk-shore-terrain 't_shore_s  s_grass_s )
(mk-shore-terrain 't_shore_ns s_grass_s s_grass_n)
(mk-shore-terrain 't_shore_ws s_grass_sw)
(mk-shore-terrain 't_shore_nws s_grass_sw s_grass_nw)
(mk-shore-terrain 't_shore_es s_grass_se)
(mk-shore-terrain 't_shore_nes s_grass_se s_grass_ne)
(mk-shore-terrain 't_shore_wes s_grass_se s_grass_sw)
(mk-shore-terrain 't_shore_c s_grass_se s_grass_sw s_grass_ne s_grass_nw)

(define tset_shore
(list
   t_shoals    ;; 0: none
   t_shore_n   ;; 1: north
   t_shore_w   ;; 2: west
   t_shore_nw  ;; 3: north west
   t_shore_e   ;; 4: east
   t_shore_ne  ;; 5: east north
   t_shore_we  ;; 6: east west
   t_shore_nwe ;; 7: east west north
   t_shore_s   ;; 8: south
   t_shore_ns  ;; 9: south north
   t_shore_ws  ;; 10: south west
   t_shore_nws ;; 11: south west north
   t_shore_es  ;; 12: south east
   t_shore_nes ;; 13: south east north
   t_shore_wes ;; 14: south east west
   t_shore_c ;; 15: south east west north
   ))
   
(define tset_water
(append tset_shore
          (list t_shoals
                t_shallow
                t_deep
                t_sunlit_deep
                t_bridge_WE
                t_bridge_NS)))

;;----------------------------------------------------------------------------

(define bad-terrain-list
  (list t_bog
        t_lava
        t_deep_lava
        t_fire_terrain
        t_fireplace
        t_inv_wall
        t_wall_torch
        ))

(define inflammable-terrain-list
  (list t_bog
        t_deep
        t_shallow
        t_shoals
        t_sunlit_deep
        t_stars
		t_void
		t_chasm
        ))
		
(load "blendterrains.scm")

(define (is-bad-terrain? kter)
  (in-list? kter bad-terrain-list))

(define (is-inflammable-terrain? kter)
  (in-list? kter inflammable-terrain-list))

(define (is-deck? kter)
  (in-list? kter 
            (list t_deck
                  t_ship_hull
                  t_ship_hull2
                  t_mast
                  t_ships_wheel
                  )))

;;----------------------------------------------------------------------------
;; terrain map blenders (automatically run within kern-mk-map)
;;(kern-mk-blender t_blendable_shoals tset_water tset_shore)
;;(kern-mk-blender t_shallow tset_water tset_wshore)
;;(kern-mk-blender t_grass tset_grass tset_foothills) 
