;; TODO: We need 32x32 number (and roman numeral II, III, ...) sprites

(define large_chest
  (kern-mk-container
   t_large_wooden_chest ;; type
   nil ;; trap

   (list

    ;; Food
    (list 10 t_mushroom)

    )
   ))


(kern-mk-map 
 'm_sprite_gallery_1 19 128 pal_expanded
  ;                               1  1  1  1  1  1  1  1  1
  ; 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8
 (list
  "xx [ .S .P .R .I .T .E  @  @  .G .A .L .L .E .R .Y ]  xx ";  // 00
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 01
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 02
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 03
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 04
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 05
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 06
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 07
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 08
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 09
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 10
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 11
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 12
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 13
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 14
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 15
  "xx .. .. .M .I .S .S .I .L .E .S .. .. .. .. .. .. .. xx ";  // 16
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 17
  "xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx ";  // 18
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 19
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 20
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 21
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 22
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 23
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 24
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 25
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 26
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 27
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 28
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 29
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 30
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 31
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 32
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 33
  "xx .. .. .W .E .A .R .A .B .L .E .. .I .T .E .M .S .. xx ";  // 34
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 35
  "xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx ";  // 36
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 37
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 38
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 39
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 40
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 41
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 42
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 43
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 44
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 45
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 46
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 47
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 48
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 49
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 50
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 51
  "xx .. .. .W .E .A .P .O .N .S .. .. .. .. .. .. .. .. xx ";  // 52
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 53
  "xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx ";  // 54
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 55
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 56
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 57
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 58
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 59
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 60
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 61
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 62
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 63
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 64
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 65
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 66
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 67
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 68
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 69
  "xx .. .. .T .O .O .L .S .. .. .. .. .. .. .. .. .. .. xx ";  // 70
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 71
  "xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx ";  // 72
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 73
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 74
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 75
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 76
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 77
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 78
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 79
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 80
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 81
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 82
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 83
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 84
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 85
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 86
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 87
  "xx .. .. .R .E .A .D .A .B .L .E .. .. .I .T .E .M .S xx ";  // 88
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 89
  "xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx ";  // 90
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 91
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 92
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 93
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 94
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 95
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 96
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 97
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 98
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 99
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 100
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 101
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 102
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 103
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 104
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 105
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 106
  "xx .. .. .C .O .M .M .O .D .I .T .I .E .S .. .. .. .. xx ";  // 107
  "xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx ";  // 108
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 109
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 110
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 111
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 112
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 113
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 114
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 115
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 116
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 117
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 118
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 119
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 120
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 121
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 122
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 123
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  // 124
  "xx .. .. .P .O .T .I .O .N .S .. .. .. .. .. .. .. .. xx ";  // 125
 "xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx ";  // 126
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ";  // 127
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ";  // 128
  ))
  ;                               1  1  1  1  1  1  1  1  1
  ; 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8


(kern-mk-place 'p_sprite_gallery_1 ; tag
               "Sprite Gallery 1"  ; name
               s_moongate_full     ; sprite
               m_sprite_gallery_1  ; map
               #f                  ; wraps
               #f                  ; underground
               #f                  ; large-scale (wilderness)
               #f                  ; tmp combat place
               nil ; subplaces
               nil ; neighbors
               (list
                (list (mk-perm-gate 'p_terrain_test 21 2)  9  1)

                ;; Lightning bolts:
                (list (kern-mk-obj t_lightning_bolt_red       1)  1  2)
                (list (kern-mk-obj t_lightning_bolt_green     1)  1  3)
                (list (kern-mk-obj t_lightning_bolt_blue      1)  1  4)
                (list (kern-mk-obj t_lightning_bolt_cyan      1)  1  5)
                (list (kern-mk-obj t_lightning_bolt_purple    1)  1  6)
                (list (kern-mk-obj t_lightning_bolt_yellow    1)  1  7)
                (list (kern-mk-obj t_lightning_bolt_brown     1)  1  8)
                (list (kern-mk-obj t_lightning_bolt_orange    1)  1  9)
                (list (kern-mk-obj t_lightning_bolt_white     1)  1 10)
                (list (kern-mk-obj t_lightning_bolt_grey      1)  1 11)
                (list (kern-mk-obj t_lightning_bolt_dark_grey 1)  1 12)
                (list (kern-mk-obj t_lightning_bolt_chromatic 1)  1 13)

                ;; Magic ball (fireball core or similar):
                (list (kern-mk-obj t_fireball_red       1)  3  2)
                (list (kern-mk-obj t_fireball_green     1)  3  3)
                (list (kern-mk-obj t_fireball_blue      1)  3  4)
                (list (kern-mk-obj t_fireball_cyan      1)  3  5)
                (list (kern-mk-obj t_fireball_purple    1)  3  6)
                (list (kern-mk-obj t_fireball_yellow    1)  3  7)
                (list (kern-mk-obj t_fireball_brown     1)  3  8)
                (list (kern-mk-obj t_fireball_orange    1)  3  9)
                (list (kern-mk-obj t_fireball_white     1)  3 10)
                (list (kern-mk-obj t_fireball_grey      1)  3 11)
                (list (kern-mk-obj t_fireball_dark_grey 1)  3 12)
                (list (kern-mk-obj t_fireball_chromatic 1)  3 13)

                ;; Magic burst/bloom/aura/field (5 ball lightnings in one tile):
                (list (kern-mk-obj t_magic_field_red       1)  5  2)
                (list (kern-mk-obj t_magic_field_green     1)  5  3)
                (list (kern-mk-obj t_magic_field_blue      1)  5  4)
                (list (kern-mk-obj t_magic_field_cyan      1)  5  5)
                (list (kern-mk-obj t_magic_field_purple    1)  5  6)
                (list (kern-mk-obj t_magic_field_yellow    1)  5  7)
                (list (kern-mk-obj t_magic_field_brown     1)  5  8)
                (list (kern-mk-obj t_magic_field_orange    1)  5  9)
                (list (kern-mk-obj t_magic_field_white     1)  5 10)
                (list (kern-mk-obj t_magic_field_grey      1)  5 11)
                (list (kern-mk-obj t_magic_field_dark_grey 1)  5 12)
                (list (kern-mk-obj t_magic_field_blue_2    1)  5 13)

                (list (kern-mk-obj t_magic_field_red_pulsed       1)  7  2)
                (list (kern-mk-obj t_magic_field_green_pulsed     1)  7  3)
                (list (kern-mk-obj t_magic_field_blue_pulsed      1)  7  4)
                (list (kern-mk-obj t_magic_field_white_pulsed     1)  7  5)

                (list (kern-mk-obj t_magic_field_purple_pulsed    1)  7  7)
                (list (kern-mk-obj t_magic_field_brown_pulsed     1)  7  8)
                (list (kern-mk-obj t_magic_field_blue_2_pulsed    1)  7  9)
                (list (kern-mk-obj t_magic_field_chromatic_pulsed 1)  7 10)

                ;; Lightning discharges:
                (list (kern-mk-obj t_lightning_discharge_red   1)  9  2)
                (list (kern-mk-obj t_lightning_discharge_green 1)  9  3)
                (list (kern-mk-obj t_lightning_discharge_blue  1)  9  4)
                (list (kern-mk-obj t_lightning_discharge_black 1)  9  5)

                ;; Lightning discharges in cloud:
                (list (kern-mk-obj t_lightning_cloud_red    1)  9  7)
                (list (kern-mk-obj t_lightning_cloud_green  1)  9  8)
                (list (kern-mk-obj t_lightning_cloud_blue   1)  9  9)
                (list (kern-mk-obj t_lightning_cloud_purple 1)  9 10)
                (list (kern-mk-obj t_lightning_cloud_yellow 1)  9 11)
                (list (kern-mk-obj t_lightning_cloud_orange 1)  9 12)

                ;; Magical spheres:
                (list (kern-mk-obj t_magic_sphere_red    1) 11  2)
                (list (kern-mk-obj t_magic_sphere_green  1) 11  3)
                (list (kern-mk-obj t_magic_sphere_blue   1) 11  4)
                (list (kern-mk-obj t_magic_sphere_yellow 1) 11  5)
                (list (kern-mk-obj t_magic_sphere_purple 1) 11  6)
                (list (kern-mk-obj t_magic_sphere_cyan   1) 11  7)

                ;; Magical spheres with light rays:
                (list (kern-mk-obj t_magic_sphere_rays_red    1) 11  9)
                (list (kern-mk-obj t_magic_sphere_rays_green  1) 11 10)
                (list (kern-mk-obj t_magic_sphere_rays_blue   1) 11 11)
                (list (kern-mk-obj t_magic_sphere_rays_yellow 1) 11 12)
                (list (kern-mk-obj t_magic_sphere_rays_purple 1) 11 13)
                (list (kern-mk-obj t_magic_sphere_rays_cyan   1) 11 14)

                ;; Arrows and Bolts and such in flight:
                (list (kern-mk-obj t_arrow_wood_red    1) 13  2)
                (list (kern-mk-obj t_arrow_metal_red   1) 13  3)
                (list (kern-mk-obj t_arrow_wood_green  1) 13  4)
                (list (kern-mk-obj t_arrow_metal_green 1) 13  5)


                ;; Tiles from ss_dg_wearable:
                (list (kern-mk-obj t_helm_leather_1    1)  1  19)
                (list (kern-mk-obj t_helm_leather_2    1)  2  19)
                (list (kern-mk-obj t_helm_metal_1      1)  3  19)
                (list (kern-mk-obj t_helm_metal_2      1)  4  19)
                (list (kern-mk-obj t_helm_metal_3      1)  5  19)
                (list (kern-mk-obj t_helm_gold_horned  1)  6  19)
                (list (kern-mk-obj t_helm_metal_horned 1)  7  19)
                (list (kern-mk-obj t_hat_green_feather 1)  8  19)
                (list (kern-mk-obj t_crown_1           1)  9  19)
                (list (kern-mk-obj t_crown_2           1) 10  19)
                (list (kern-mk-obj t_crown_3           1) 11  19)
                (list (kern-mk-obj t_crown_4           1) 12  19)
                
                (list (kern-mk-obj t_amulet_1  1)  1  20)
                (list (kern-mk-obj t_amulet_2  1)  2  20)
                (list (kern-mk-obj t_amulet_3  1)  3  20)
                (list (kern-mk-obj t_amulet_4  1)  4  20)
                (list (kern-mk-obj t_amulet_5  1)  5  20)
                (list (kern-mk-obj t_amulet_6  1)  6  20)
                (list (kern-mk-obj t_amulet_7  1)  7  20)
                (list (kern-mk-obj t_amulet_8  1)  8  20)
                (list (kern-mk-obj t_amulet_9  1)  9  20)
                (list (kern-mk-obj t_amulet_10 1) 10  20)
                (list (kern-mk-obj t_amulet_11 1) 11  20)
                (list (kern-mk-obj t_amulet_12 1) 12  20)
                (list (kern-mk-obj t_amulet_13 1) 13  20)
                (list (kern-mk-obj t_amulet_14 1) 14  20)
                (list (kern-mk-obj t_amulet_15 1) 15  20)
                (list (kern-mk-obj t_amulet_16 1) 16  20)

                (list (kern-mk-obj t_ring_1  1)  1  21)
                (list (kern-mk-obj t_ring_2  1)  2  21)
                (list (kern-mk-obj t_ring_3  1)  3  21)
                (list (kern-mk-obj t_ring_4  1)  4  21)
                (list (kern-mk-obj t_ring_5  1)  5  21)
                (list (kern-mk-obj t_ring_6  1)  6  21)
                (list (kern-mk-obj t_ring_7  1)  7  21)
                (list (kern-mk-obj t_ring_8  1)  8  21)
                (list (kern-mk-obj t_ring_9  1)  9  21)
                (list (kern-mk-obj t_ring_10 1) 10  21)
                (list (kern-mk-obj t_ring_11 1) 11  21)
                (list (kern-mk-obj t_ring_12 1) 12  21)
                (list (kern-mk-obj t_ring_13 1) 13  21)
                (list (kern-mk-obj t_ring_14 1) 14  21)
                (list (kern-mk-obj t_ring_15 1) 15  21)
                (list (kern-mk-obj t_ring_16 1) 16  21)
                (list (kern-mk-obj t_ring_17 1) 17  21)

                (list (kern-mk-obj t_ring_18 1)  1  22)
                (list (kern-mk-obj t_ring_21 1)  2  22)
                (list (kern-mk-obj t_ring_20 1)  3  22)
                (list (kern-mk-obj t_ring_21 1)  4  22)
                (list (kern-mk-obj t_ring_22 1)  5  22)
                (list (kern-mk-obj t_ring_23 1)  6  22)
                (list (kern-mk-obj t_ring_24 1)  7  22)

                (list (kern-mk-obj t_cloak_1 1)  1  23)
                (list (kern-mk-obj t_cloak_2 1)  2  23)
                (list (kern-mk-obj t_cloak_3 1)  3  23)
                (list (kern-mk-obj t_cloak_4 1)  4  23)
                (list (kern-mk-obj t_cloak_5 1)  5  23)
                (list (kern-mk-obj t_cloak_6 1)  6  23)
                (list (kern-mk-obj t_cloak_7 1)  7  23)
                (list (kern-mk-obj t_cloak_8 1)  8  23)
                (list (kern-mk-obj t_cloak_9 1)  9  23)

                (list (kern-mk-obj t_robe_1    1) 11  23)
                (list (kern-mk-obj t_robe_2    1) 12  23)
                (list (kern-mk-obj t_robe_3    1) 13  23)
                (list (kern-mk-obj t_robe_4    1) 14  23)
                (list (kern-mk-obj t_robe_rags 1) 15  23)

                (list (kern-mk-obj t_gloves_green  1)  1  24)
                (list (kern-mk-obj t_gloves_tan    1)  2  24)
                (list (kern-mk-obj t_gloves_brown  1)  3  24)
                (list (kern-mk-obj t_gloves_grey   1)  4  24)
                (list (kern-mk-obj t_gloves_yellow 1)  5  24)
                (list (kern-mk-obj t_gloves_blue   1)  6  24)

                (list (kern-mk-obj t_boots_green   1)  8  24)
                (list (kern-mk-obj t_boots_tan     1)  9  24)
                (list (kern-mk-obj t_boots_brown   1) 10  24)
                (list (kern-mk-obj t_boots_metal   1) 11  24)
                (list (kern-mk-obj t_boots_yellow  1) 12  24)

                (list (kern-mk-obj t_shield_pattern_1 1)  1  25)
                (list (kern-mk-obj t_shield_pattern_2 1)  2  25)
                (list (kern-mk-obj t_shield_pattern_3 1)  3  25)
                (list (kern-mk-obj t_shield_pattern_4 1)  4  25)
                (list (kern-mk-obj t_shield_pattern_5 1)  5  25)
                (list (kern-mk-obj t_shield_pattern_6 1)  6  25)
                (list (kern-mk-obj t_shield_pattern_7 1)  7  25)
                (list (kern-mk-obj t_shield_pattern_blank_gold 1)  8  25)

                (list (kern-mk-obj t_shield_blank_wooden_1 1) 10  25)
                (list (kern-mk-obj t_shield_blank_wooden_2 1) 11  25)
                (list (kern-mk-obj t_shield_blank_round_1  1) 12  25)
                (list (kern-mk-obj t_shield_blank_round_2  1) 13  25)
                (list (kern-mk-obj t_shield_blank_round_3  1) 14  25)
                (list (kern-mk-obj t_shield_blank_heater_1 1) 15  25)
                (list (kern-mk-obj t_shield_blank_heater_2 1) 16  25)

                (list (kern-mk-obj t_breastplate_dragon_1  1)  1  26)
                (list (kern-mk-obj t_breastplate_dragon_2  1)  2  26)
                (list (kern-mk-obj t_breastplate_dragon_3  1)  3  26)
                (list (kern-mk-obj t_breastplate_dragon_4  1)  4  26)
                (list (kern-mk-obj t_breastplate_dragon_5  1)  5  26)
                (list (kern-mk-obj t_breastplate_dragon_6  1)  6  26)

                (list (kern-mk-obj t_shield_symbol_crown        1)  8  26)
                (list (kern-mk-obj t_shield_symbol_unicorn_1    1)  9  26)
                (list (kern-mk-obj t_shield_symbol_unicorn_2    1) 10  26)
                (list (kern-mk-obj t_shield_symbol_lion_rampant 1) 11  26)
                (list (kern-mk-obj t_shield_symbol_skull        1) 12  26)

                (list (kern-mk-obj t_armor_leather_1 1)  1  27)
                (list (kern-mk-obj t_armor_leather_2 1)  2  27)
                (list (kern-mk-obj t_armor_leather_3 1)  3  27)
                (list (kern-mk-obj t_armor_leather_4 1)  4  27)
                (list (kern-mk-obj t_armor_leather_5 1)  5  27)

                (list (kern-mk-obj t_armor_golden_1  1)  7  27)
                (list (kern-mk-obj t_armor_green_1   1)  8  27)

                (list (kern-mk-obj t_armor_chain_1   1) 10  27)
                (list (kern-mk-obj t_armor_chain_2   1) 11  27)
                (list (kern-mk-obj t_armor_chain_3   1) 12  27)
                (list (kern-mk-obj t_armor_chain_4   1) 13  27)
                (list (kern-mk-obj t_armor_chain_5   1) 14  27)
                (list (kern-mk-obj t_armor_chain_6   1) 15  27)
                (list (kern-mk-obj t_armor_chain_7   1) 16  27)
                (list (kern-mk-obj t_armor_chain_8   1) 17  27)

                (list (kern-mk-obj t_armor_plate_1   1)  1  28)
                (list (kern-mk-obj t_armor_plate_2   1)  2  28)
                (list (kern-mk-obj t_armor_plate_3   1)  3  28)
                (list (kern-mk-obj t_armor_plate_4   1)  4  28)
                (list (kern-mk-obj t_armor_plate_5   1)  5  28)

                (list (kern-mk-obj t_armor_dragon_1  1)  1  29)
                (list (kern-mk-obj t_armor_dragon_2  1)  2  29)
                (list (kern-mk-obj t_armor_dragon_3  1)  3  29)
                (list (kern-mk-obj t_armor_dragon_4  1)  4  29)
                (list (kern-mk-obj t_armor_dragon_5  1)  5  29)
                (list (kern-mk-obj t_armor_dragon_6  1)  6  29)
                (list (kern-mk-obj t_armor_dragon_7  1)  7  29)
                (list (kern-mk-obj t_armor_dragon_8  1)  8  29)

                (list (kern-mk-obj t_armor_dragon_9  1) 10  29)
                (list (kern-mk-obj t_armor_dragon_10 1) 11  29)
                (list (kern-mk-obj t_armor_dragon_11 1) 12  29)
                (list (kern-mk-obj t_armor_dragon_12 1) 13  29)
                (list (kern-mk-obj t_armor_dragon_13 1) 14  29)
                (list (kern-mk-obj t_armor_dragon_14 1) 15  29)
                (list (kern-mk-obj t_armor_dragon_15 1) 16  29)
                (list (kern-mk-obj t_armor_dragon_16 1) 17  29)


                ;; ss_dg_weapons
                (list (kern-mk-obj t_sword_broken_1   1)  1 37)
                (list (kern-mk-obj t_sword_broken_2   1)  2 37)
                (list (kern-mk-obj t_sword_broken_3   1)  3 37)
                (list (kern-mk-obj t_sword_broken_4   1)  4 37)

                (list (kern-mk-obj t_dagger_1         1)  6 37)
                (list (kern-mk-obj t_dagger_2         1)  7 37)
                (list (kern-mk-obj t_dagger_3         1)  8 37)

                (list (kern-mk-obj t_sword_short_1    1) 10 37)
                (list (kern-mk-obj t_sword_short_2    1) 11 37)
                (list (kern-mk-obj t_sword_short_3    1) 12 37)

                (list (kern-mk-obj t_rapier_1         1)  1 38)
                (list (kern-mk-obj t_rapier_2         1)  2 38)
                (list (kern-mk-obj t_saber_1          1)  3 38)
                (list (kern-mk-obj t_saber_2          1)  4 38)
                (list (kern-mk-obj t_cutlass_1        1)  5 38)
                (list (kern-mk-obj t_cutlass_2        1)  6 38)
                (list (kern-mk-obj t_falchion_1       1)  7 38)

                (list (kern-mk-obj t_sword_long_1     1)  9 38)
                (list (kern-mk-obj t_sword_long_2     1) 10 38)
                (list (kern-mk-obj t_sword_long_3     1) 11 38)

                (list (kern-mk-obj t_sword_bastard    1)  1 39)
                (list (kern-mk-obj t_sword_katana     1)  2 39)

                (list (kern-mk-obj t_sword_zweihander 1)  4 39)
                (list (kern-mk-obj t_sword_vorpal     1)  5 39)

                (list (kern-mk-obj t_sword_eldritch 1)  7 39)
                (list (kern-mk-obj t_sword_mystic   1)  8 39)
                (list (kern-mk-obj t_sword_flaming  1)  9 39)

                (list (kern-mk-obj t_mace_1         1)  1 40)
                (list (kern-mk-obj t_mace_2         1)  2 40)
                (list (kern-mk-obj t_mace_3         1)  3 40)
                (list (kern-mk-obj t_mace_4         1)  4 40)
                (list (kern-mk-obj t_mace_magical   1)  5 40)

                (list (kern-mk-obj t_hammer_golden       1)  7 40)
                (list (kern-mk-obj t_hammer_warhammer    1)  8 40)
                (list (kern-mk-obj t_hammer_sledgehammer 1)  9 40)
                (list (kern-mk-obj t_hammer_giant        1) 10 40)

                (list (kern-mk-obj t_flail_1    1)  1 41)
                (list (kern-mk-obj t_flail_2    1)  2 41)
                (list (kern-mk-obj t_flail_3    1)  3 41)
                (list (kern-mk-obj t_flail_4    1)  4 41)
                (list (kern-mk-obj t_flail_5    1)  5 41)
                (list (kern-mk-obj t_nunchaku_1 1)  6 41)
                (list (kern-mk-obj t_nunchaku_2 1)  7 41)

                (list (kern-mk-obj t_whip       1)  9 41)

                (list (kern-mk-obj t_axe_1      1) 11 41)
                (list (kern-mk-obj t_axe_2      1) 12 41)
                (list (kern-mk-obj t_axe_3      1) 13 41)
                (list (kern-mk-obj t_axe_4      1) 14 41)
                (list (kern-mk-obj t_axe_5      1) 15 41)

                (list (kern-mk-obj t_scythe_1   1)  1 42)
                (list (kern-mk-obj t_scythe_2   1)  2 42)

                (list (kern-mk-obj t_spear_1    1)  4 42)
                (list (kern-mk-obj t_spear_2    1)  5 42)
                (list (kern-mk-obj t_spear_3    1)  6 42)
                (list (kern-mk-obj t_trident_1  1)  7 42)
                (list (kern-mk-obj t_trident_2  1)  8 42)
                (list (kern-mk-obj t_trident_3  1)  9 42)
                (list (kern-mk-obj t_lance      1) 10 42)

                (list (kern-mk-obj t_quarterstaff_1 1)  1 43)
                (list (kern-mk-obj t_quarterstaff_2 1)  2 43)

                (list (kern-mk-obj t_halberd_1  1)  4 43)
                (list (kern-mk-obj t_halberd_2  1)  5 43)
                (list (kern-mk-obj t_halberd_3  1)  6 43)

                (list (kern-mk-obj t_shortbow   1)  1 44)
                (list (kern-mk-obj t_longbow_1  1)  2 44)
                (list (kern-mk-obj t_longbow_2  1)  3 44)
                (list (kern-mk-obj t_crossbow_1 1)  4 44)
                (list (kern-mk-obj t_crossbow_2 1)  5 44)
                (list (kern-mk-obj t_crossbow_3 1)  6 44)

                (list (kern-mk-obj t_sling                1)  8 44)
                (list (kern-mk-obj t_sling_bullet         1)  9 44)
                (list (kern-mk-obj t_sling_stones         1) 10 44)

                (list (kern-mk-obj t_dart_red             1) 12 44)
                (list (kern-mk-obj t_dart_green_dripping  1) 13 44)
                (list (kern-mk-obj t_dart_blue_dripping   1) 14 44)
                (list (kern-mk-obj t_shuriken             1) 15 44)
                (list (kern-mk-obj t_shuriken_spinning    1) 16 44)

                (list (kern-mk-obj t_arrow_wooden         1)  1 45)
                (list (kern-mk-obj t_arrow_metal          1)  2 45)
                (list (kern-mk-obj t_arrow_white          1)  3 45)
                (list (kern-mk-obj t_arrow_gold           1)  4 45)

                (list (kern-mk-obj t_arrow_wooden_flaming 1)  6 45)
                (list (kern-mk-obj t_arrow_metal_flaming  1)  7 45)
                (list (kern-mk-obj t_arrow_wooden_acid    1)  8 45)
                (list (kern-mk-obj t_arrow_metal_acid     1)  9 45)
                (list (kern-mk-obj t_arrow_wooden_frost   1) 10 45)
                (list (kern-mk-obj t_arrow_metal_frost    1) 11 45)

                (list (kern-mk-obj t_arrow_glowing_red    1) 13 45)
                (list (kern-mk-obj t_arrow_glowing_green  1) 14 45)  ;; TODO: fix sprite
                (list (kern-mk-obj t_arrow_glowing_blue   1) 15 45)
                (list (kern-mk-obj t_arrow_glowing_purple 1) 16 45)

                (list (kern-mk-obj t_bolt_wooden          1)  1 46)
                (list (kern-mk-obj t_bolt_metal           1)  2 46)
                (list (kern-mk-obj t_bolt_white           1)  3 46)
                (list (kern-mk-obj t_bolt_gold            1)  4 46)

                (list (kern-mk-obj t_bolt_wooden_flaming  1)  6 46)
                (list (kern-mk-obj t_bolt_metal_flaming   1)  7 46)
                (list (kern-mk-obj t_bolt_wooden_acid     1)  8 46)
                (list (kern-mk-obj t_bolt_metal_acid      1)  9 46)
                (list (kern-mk-obj t_bolt_wooden_frost    1) 10 46)
                (list (kern-mk-obj t_bolt_metal_frost     1) 11 46)

                (list (kern-mk-obj t_bolt_glowing_red     1) 13 46)
                (list (kern-mk-obj t_bolt_glowing_green   1) 14 46)  ;; TODO: fix sprite (moving 1 px L/R)
                (list (kern-mk-obj t_bolt_glowing_blue    1) 15 46)  ;; TODO: fix sprite (color)
                (list (kern-mk-obj t_bolt_glowing_purple  1) 16 46)  ;; TODO: fix sprite (color, moving)


                ;; ss_dg_tools
                (list (kern-mk-obj t_chest_small_wooden_closed  1) 1 55)
                (list (kern-mk-obj t_chest_large_wooden_closed  1) 2 55)
                (list (kern-mk-obj t_chest_small_metal_1_closed 1) 3 55)
                (list (kern-mk-obj t_chest_large_metal_1_closed 1) 4 55)
                (list (kern-mk-obj t_chest_small_metal_2_closed 1) 5 55)
                (list (kern-mk-obj t_chest_small_metal_2_closed 1) 6 55)
                (list (kern-mk-obj t_chest_wooden_broken        1) 7 55)

                (list (kern-mk-obj t_metal_spike                1) 9 55)

                (list (kern-mk-obj t_pickaxe_1    1)  1 56)
                (list (kern-mk-obj t_pickaxe_2    1)  2 56)
                (list (kern-mk-obj t_pickaxe_3    1)  3 56)
                (list (kern-mk-obj t_pickaxe_4    1)  4 56)

                (list (kern-mk-obj t_shovel_1     1)  6 56)
                (list (kern-mk-obj t_shovel_2     1)  7 56)
                (list (kern-mk-obj t_shovel_3     1)  8 56)
                (list (kern-mk-obj t_shovel_4     1)  9 56)

                (list (kern-mk-obj t_lamp_1_unlit 1) 11 56)
                (list (kern-mk-obj t_lamp_1_lit   1) 12 56)
                (list (kern-mk-obj t_lamp_2_unlit 1) 13 56)
                (list (kern-mk-obj t_lamp_2_lit   1) 14 56)
                (list (kern-mk-obj t_torch_unlit  1) 15 56)
                (list (kern-mk-obj t_torch_lit    1) 16 56)

                (list (kern-mk-obj t_rod_1   1) 1 57)
                (list (kern-mk-obj t_rod_2   1) 2 57)
                (list (kern-mk-obj t_rod_3   1) 3 57)
                (list (kern-mk-obj t_rod_4   1) 4 57)
                (list (kern-mk-obj t_rod_5   1) 5 57)
                (list (kern-mk-obj t_rod_6   1) 6 57)
                (list (kern-mk-obj t_rod_7   1) 7 57)
                (list (kern-mk-obj t_rod_8   1) 8 57)
                (list (kern-mk-obj t_rod_9   1) 9 57)

                (list (kern-mk-obj t_wand_1  1) 1 58)
                (list (kern-mk-obj t_wand_2  1) 2 58)
                (list (kern-mk-obj t_wand_3  1) 3 58)
                (list (kern-mk-obj t_wand_4  1) 4 58)
                (list (kern-mk-obj t_wand_5  1) 5 58)
                (list (kern-mk-obj t_wand_6  1) 6 58)
                (list (kern-mk-obj t_wand_7  1) 7 58)
                (list (kern-mk-obj t_wand_8  1) 8 58)
                (list (kern-mk-obj t_wand_9  1) 9 58)

                (list (kern-mk-obj t_staff_1 1) 1 59)
                (list (kern-mk-obj t_staff_2 1) 2 59)
                (list (kern-mk-obj t_staff_3 1) 3 59)
                (list (kern-mk-obj t_staff_4 1) 4 59)
                (list (kern-mk-obj t_staff_5 1) 5 59)
                (list (kern-mk-obj t_staff_6 1) 6 59)
                (list (kern-mk-obj t_staff_7 1) 7 59)


                ;; ss_dg_readable
                (list (kern-mk-obj t_scroll_magic_1  1)  1  73)
                (list (kern-mk-obj t_scroll_magic_2  1)  2  73)
                (list (kern-mk-obj t_scroll_magic_3  1)  3  73)
                (list (kern-mk-obj t_scroll_magic_4  1)  4  73)
                (list (kern-mk-obj t_scroll_magic_5  1)  5  73)
                (list (kern-mk-obj t_scroll_magic_6  1)  6  73)
                (list (kern-mk-obj t_scroll_magic_7  1)  7  73)
                (list (kern-mk-obj t_scroll_magic_8  1)  8  73)

                (list (kern-mk-obj t_scroll_spell_1  1)  1  74)
                (list (kern-mk-obj t_scroll_spell_2  1)  2  74)
                (list (kern-mk-obj t_scroll_spell_3  1)  3  74)
                (list (kern-mk-obj t_scroll_spell_4  1)  4  74)
                (list (kern-mk-obj t_scroll_spell_5  1)  5  74)
                (list (kern-mk-obj t_scroll_spell_6  1)  6  74)
                (list (kern-mk-obj t_scroll_spell_7  1)  7  74)
                (list (kern-mk-obj t_scroll_spell_8  1)  8  74)
                (list (kern-mk-obj t_scroll_spell_9  1)  9  74)
                (list (kern-mk-obj t_scroll_spell_10 1) 10  74)
                (list (kern-mk-obj t_scroll_spell_11 1) 11  74)
                (list (kern-mk-obj t_scroll_spell_12 1) 12  74)
                (list (kern-mk-obj t_scroll_spell_13 1) 13  74)
                (list (kern-mk-obj t_scroll_spell_14 1) 14  74)
                (list (kern-mk-obj t_scroll_spell_15 1) 15  74)
                (list (kern-mk-obj t_scroll_spell_16 1) 16  74)

                (list (kern-mk-obj t_scroll_spell_17 1)  1  75)
                (list (kern-mk-obj t_scroll_spell_18 1)  2  75)
                (list (kern-mk-obj t_scroll_spell_19 1)  3  75)
                (list (kern-mk-obj t_scroll_spell_20 1)  4  75)
                (list (kern-mk-obj t_scroll_spell_21 1)  5  75)
                (list (kern-mk-obj t_scroll_spell_22 1)  6  75)
                (list (kern-mk-obj t_scroll_spell_23 1)  7  75)
                (list (kern-mk-obj t_scroll_spell_24 1)  8  75)
                (list (kern-mk-obj t_scroll_spell_25 1)  9  75)
                (list (kern-mk-obj t_scroll_spell_26 1) 10  75)
                (list (kern-mk-obj t_scroll_spell_27 1) 11  75)
                (list (kern-mk-obj t_scroll_spell_28 1) 12  75)
                (list (kern-mk-obj t_scroll_spell_29 1) 13  75)
                (list (kern-mk-obj t_scroll_spell_30 1) 14  75)
                (list (kern-mk-obj t_scroll_spell_31 1) 15  75)
                (list (kern-mk-obj t_scroll_spell_32 1) 16  75)

                (list (kern-mk-obj t_scroll_spell_33 1)  1  76)
                (list (kern-mk-obj t_scroll_spell_34 1)  2  76)
                (list (kern-mk-obj t_scroll_spell_35 1)  3  76)
                (list (kern-mk-obj t_scroll_spell_36 1)  4  76)
                (list (kern-mk-obj t_scroll_spell_37 1)  5  76)
                (list (kern-mk-obj t_scroll_spell_38 1)  6  76)
                (list (kern-mk-obj t_scroll_spell_39 1)  7  76)
                (list (kern-mk-obj t_scroll_spell_40 1)  8  76)
                (list (kern-mk-obj t_scroll_spell_41 1)  9  76)
                (list (kern-mk-obj t_scroll_spell_42 1) 10  76)

                (list (kern-mk-obj t_book_red_1 1)  1  77)
                (list (kern-mk-obj t_book_red_2 1)  2  77)
                (list (kern-mk-obj t_book_red_3 1)  3  77)
                (list (kern-mk-obj t_book_red_4 1)  4  77)

                (list (kern-mk-obj t_book_red_glowing_1 1)  6  77)
                (list (kern-mk-obj t_book_red_glowing_2 1)  7  77)
                (list (kern-mk-obj t_book_red_glowing_3 1)  8  77)
                (list (kern-mk-obj t_book_red_glowing_4 1)  9  77)
                (list (kern-mk-obj t_book_red_glowing_5 1) 10  77)

                (list (kern-mk-obj t_book_grey_1 1)  1  78)
                (list (kern-mk-obj t_book_grey_2 1)  2  78)
                (list (kern-mk-obj t_book_grey_3 1)  3  78)
                (list (kern-mk-obj t_book_grey_4 1)  4  78)

                (list (kern-mk-obj t_book_grey_glowing_1 1)  6  78)
                (list (kern-mk-obj t_book_grey_glowing_2 1)  7  78)
                (list (kern-mk-obj t_book_grey_glowing_3 1)  8  78)
                (list (kern-mk-obj t_book_grey_glowing_4 1)  9  78)
                (list (kern-mk-obj t_book_grey_glowing_5 1) 10  78)

                (list (kern-mk-obj t_book_blue_1 1)  1  79)
                (list (kern-mk-obj t_book_blue_2 1)  2  79)
                (list (kern-mk-obj t_book_blue_3 1)  3  79)
                (list (kern-mk-obj t_book_blue_4 1)  4  79)

                (list (kern-mk-obj t_book_blue_glowing_1 1)  6  79)
                (list (kern-mk-obj t_book_blue_glowing_2 1)  7  79)
                (list (kern-mk-obj t_book_blue_glowing_3 1)  8  79)
                (list (kern-mk-obj t_book_blue_glowing_4 1)  9  79)
                (list (kern-mk-obj t_book_blue_glowing_5 1) 10  79)

                (list (kern-mk-obj t_book_green_1 1)  1  80)
                (list (kern-mk-obj t_book_green_2 1)  2  80)
                (list (kern-mk-obj t_book_green_3 1)  3  80)
                (list (kern-mk-obj t_book_green_4 1)  4  80)

                (list (kern-mk-obj t_book_green_glowing_1 1)  6  80)
                (list (kern-mk-obj t_book_green_glowing_2 1)  7  80)
                (list (kern-mk-obj t_book_green_glowing_3 1)  8  80)
                (list (kern-mk-obj t_book_green_glowing_4 1)  9  80)
                (list (kern-mk-obj t_book_green_glowing_5 1) 10  80)

                (list (kern-mk-obj t_book_orange_1 1)  1  81)
                (list (kern-mk-obj t_book_orange_2 1)  2  81)
                (list (kern-mk-obj t_book_orange_3 1)  3  81)
                (list (kern-mk-obj t_book_orange_4 1)  4  81)

                (list (kern-mk-obj t_book_orange_glowing_1 1)  6  81)
                (list (kern-mk-obj t_book_orange_glowing_2 1)  7  81)
                (list (kern-mk-obj t_book_orange_glowing_3 1)  8  81)
                (list (kern-mk-obj t_book_orange_glowing_4 1)  9  81)
                (list (kern-mk-obj t_book_orange_glowing_5 1) 10  81)

                (list (kern-mk-obj t_book_lite_blue_1 1)  1  82)
                (list (kern-mk-obj t_book_lite_blue_2 1)  2  82)
                (list (kern-mk-obj t_book_lite_blue_3 1)  3  82)
                (list (kern-mk-obj t_book_lite_blue_4 1)  4  82)

                (list (kern-mk-obj t_book_lite_blue_glowing_1 1)  6  82)
                (list (kern-mk-obj t_book_lite_blue_glowing_2 1)  7  82)
                (list (kern-mk-obj t_book_lite_blue_glowing_3 1)  8  82)
                (list (kern-mk-obj t_book_lite_blue_glowing_4 1)  9  82)
                (list (kern-mk-obj t_book_lite_blue_glowing_5 1) 10  82)

                (list (kern-mk-obj t_book_pink_1 1)  1  83)
                (list (kern-mk-obj t_book_pink_2 1)  2  83)
                (list (kern-mk-obj t_book_pink_3 1)  3  83)
                (list (kern-mk-obj t_book_pink_4 1)  4  83)

                (list (kern-mk-obj t_book_pink_glowing_1 1)  6  83)
                (list (kern-mk-obj t_book_pink_glowing_2 1)  7  83)
                (list (kern-mk-obj t_book_pink_glowing_3 1)  8  83)
                (list (kern-mk-obj t_book_pink_glowing_4 1)  9  83)
                (list (kern-mk-obj t_book_pink_glowing_5 1) 10  83)


                ;; ss_dg_commodities
                (list (kern-mk-obj t_dg_biscuit    1) 1 91)
                (list (kern-mk-obj t_dg_bread      1) 2 91)
                (list (kern-mk-obj t_dg_jerky      1) 3 91)
                (list (kern-mk-obj t_dg_ham        1) 4 91)
                (list (kern-mk-obj t_dg_chicken    1) 5 91)
                (list (kern-mk-obj t_dg_moldy_meat 1) 6 91)

                (list (kern-mk-obj t_dg_booze_jug  1) 1 92)
                (list (kern-mk-obj t_dg_beer_mug   1) 2 92)
                (list (kern-mk-obj t_dg_wine_flask 1) 3 92)

                (list (kern-mk-obj t_dg_mushroom_black      1)  1 93)
                (list (kern-mk-obj t_dg_mushroom_white      1)  2 93)
                (list (kern-mk-obj t_dg_mushroom_grey       1)  3 93)
                (list (kern-mk-obj t_dg_mushroom_orange     1)  4 93)
                (list (kern-mk-obj t_dg_mushroom_red        1)  5 93)
                (list (kern-mk-obj t_dg_mushroom_green      1)  6 93)
                (list (kern-mk-obj t_dg_mushroom_blue       1)  7 93)
                (list (kern-mk-obj t_dg_mushroom_brown      1)  8 93)
                (list (kern-mk-obj t_dg_mushroom_grey_2     1)  9 93)
                (list (kern-mk-obj t_dg_mushroom_white_2    1) 10 93)
                (list (kern-mk-obj t_dg_mushroom_purple     1) 11 93)
                (list (kern-mk-obj t_dg_mushroom_yellow_2   1) 12 93)
                (list (kern-mk-obj t_dg_mushroom_red_2      1) 13 93)
                (list (kern-mk-obj t_dg_mushroom_green_2    1) 14 93)
                (list (kern-mk-obj t_dg_mushroom_light_blue 1) 15 93)
                (list (kern-mk-obj t_dg_mushroom_tan        1) 16 93)

                (list (kern-mk-obj t_dg_mushroom_bunch_grey          1) 1 94)
                (list (kern-mk-obj t_dg_mushroom_bunch_green         1) 2 94)
                (list (kern-mk-obj t_dg_mushroom_bunch_tan           1) 3 94)
                (list (kern-mk-obj t_dg_mushroom_bunch_pink          1) 4 94)
                (list (kern-mk-obj t_dg_mushroom_bunch_purple        1) 5 94)
                (list (kern-mk-obj t_dg_mushroom_bunch_black         1) 6 94)
                (list (kern-mk-obj t_dg_mushroom_bunch_glowing_green 1) 7 94)

                (list (kern-mk-obj t_coins_copper    1)  1 95)
                (list (kern-mk-obj t_coins_silver    1)  2 95)
                (list (kern-mk-obj t_coins_gold      1)  3 95)
                (list (kern-mk-obj t_coins_blue_1    1)  4 95)
                (list (kern-mk-obj t_coins_blue_2    1)  5 95)
                (list (kern-mk-obj t_crystals_green  1)  6 95)
                (list (kern-mk-obj t_crystals_blue   1)  7 95)
                (list (kern-mk-obj t_crystals_purple 1)  8 95)
                (list (kern-mk-obj t_gems_red        1)  9 95)
                (list (kern-mk-obj t_gems_blue       1) 10 95)
                (list (kern-mk-obj t_gems_assorted   1) 11 95)


                ;; ss_kg_potions_1 (empty flasks, bubbling potions)
                (list (kern-mk-obj t_kg_potion_empty_1 1) 01 109)
                (list (kern-mk-obj t_kg_potion_empty_2 1) 02 109)
                (list (kern-mk-obj t_kg_potion_empty_3 1) 03 109)
                (list (kern-mk-obj t_kg_potion_empty_4 1) 04 109)
                (list (kern-mk-obj t_kg_potion_empty_5 1) 05 109)
                (list (kern-mk-obj t_kg_potion_empty_6 1) 06 109)

                (list (kern-mk-obj t_kg_potion_bubbling_pink   1) 01 110)
                (list (kern-mk-obj t_kg_potion_bubbling_red    1) 02 110)
                (list (kern-mk-obj t_kg_potion_bubbling_purple 1) 03 110)
                (list (kern-mk-obj t_kg_potion_bubbling_yellow 1) 04 110)
                (list (kern-mk-obj t_kg_potion_bubbling_green  1) 05 110)
                (list (kern-mk-obj t_kg_potion_bubbling_blue   1) 06 110)


                ;; ss_kg_potions_2 (6 shapes, 7 colors, fullness(3/3, 2/3, 1/3), plus dull colors)
                                        ; pink
                (list (kern-mk-obj t_kg_potion_pink_f33_1 1) 01 111)
                (list (kern-mk-obj t_kg_potion_pink_f23_1 1) 02 111)
                (list (kern-mk-obj t_kg_potion_pink_f13_1 1) 03 111)

                (list (kern-mk-obj t_kg_potion_pink_f33_2 1) 04 111)
                (list (kern-mk-obj t_kg_potion_pink_f23_2 1) 05 111)
                (list (kern-mk-obj t_kg_potion_pink_f13_2 1) 06 111)

                (list (kern-mk-obj t_kg_potion_pink_f33_3 1) 07 111)
                (list (kern-mk-obj t_kg_potion_pink_f23_3 1) 08 111)
                (list (kern-mk-obj t_kg_potion_pink_f13_3 1) 09 111)

                (list (kern-mk-obj t_kg_potion_pink_f33_4 1) 10 111)
                (list (kern-mk-obj t_kg_potion_pink_f23_4 1) 11 111)
                (list (kern-mk-obj t_kg_potion_pink_f13_4 1) 12 111)

                (list (kern-mk-obj t_kg_potion_pink_f33_5 1) 13 111)
                (list (kern-mk-obj t_kg_potion_pink_f23_5 1) 14 111)
                (list (kern-mk-obj t_kg_potion_pink_f13_5 1) 15 111)

                (list (kern-mk-obj t_kg_potion_pink_f33_6 1) 16 111)
                (list (kern-mk-obj t_kg_potion_pink_f23_6 1) 17 111)
                (list (kern-mk-obj t_kg_potion_pink_f13_6 1) 18 111)

                                        ; red
                (list (kern-mk-obj t_kg_potion_red_f33_1 1) 01 112)
                (list (kern-mk-obj t_kg_potion_red_f23_1 1) 02 112)
                (list (kern-mk-obj t_kg_potion_red_f13_1 1) 03 112)

                (list (kern-mk-obj t_kg_potion_red_f33_2 1) 04 112)
                (list (kern-mk-obj t_kg_potion_red_f23_2 1) 05 112)
                (list (kern-mk-obj t_kg_potion_red_f13_2 1) 06 112)

                (list (kern-mk-obj t_kg_potion_red_f33_3 1) 07 112)
                (list (kern-mk-obj t_kg_potion_red_f23_3 1) 08 112)
                (list (kern-mk-obj t_kg_potion_red_f13_3 1) 09 112)

                (list (kern-mk-obj t_kg_potion_red_f33_4 1) 10 112)
                (list (kern-mk-obj t_kg_potion_red_f23_4 1) 11 112)
                (list (kern-mk-obj t_kg_potion_red_f13_4 1) 12 112)

                (list (kern-mk-obj t_kg_potion_red_f33_5 1) 13 112)
                (list (kern-mk-obj t_kg_potion_red_f23_5 1) 14 112)
                (list (kern-mk-obj t_kg_potion_red_f13_5 1) 15 112)

                (list (kern-mk-obj t_kg_potion_red_f33_6 1) 16 112)
                (list (kern-mk-obj t_kg_potion_red_f23_6 1) 17 112)
                (list (kern-mk-obj t_kg_potion_red_f13_6 1) 18 112)

                                        ; green
                (list (kern-mk-obj t_kg_potion_green_f33_1 1) 01 113)
                (list (kern-mk-obj t_kg_potion_green_f23_1 1) 02 113)
                (list (kern-mk-obj t_kg_potion_green_f13_1 1) 03 113)

                (list (kern-mk-obj t_kg_potion_green_f33_2 1) 04 113)
                (list (kern-mk-obj t_kg_potion_green_f23_2 1) 05 113)
                (list (kern-mk-obj t_kg_potion_green_f13_2 1) 06 113)

                (list (kern-mk-obj t_kg_potion_green_f33_3 1) 07 113)
                (list (kern-mk-obj t_kg_potion_green_f23_3 1) 08 113)
                (list (kern-mk-obj t_kg_potion_green_f13_3 1) 09 113)

                (list (kern-mk-obj t_kg_potion_green_f33_4 1) 10 113)
                (list (kern-mk-obj t_kg_potion_green_f23_4 1) 11 113)
                (list (kern-mk-obj t_kg_potion_green_f13_4 1) 12 113)

                (list (kern-mk-obj t_kg_potion_green_f33_5 1) 13 113)
                (list (kern-mk-obj t_kg_potion_green_f23_5 1) 14 113)
                (list (kern-mk-obj t_kg_potion_green_f13_5 1) 15 113)

                (list (kern-mk-obj t_kg_potion_green_f33_6 1) 16 113)
                (list (kern-mk-obj t_kg_potion_green_f23_6 1) 17 113)
                (list (kern-mk-obj t_kg_potion_green_f13_6 1) 18 113)

                                        ; deep_blue
                (list (kern-mk-obj t_kg_potion_deep_blue_f33_1 1) 01 114)
                (list (kern-mk-obj t_kg_potion_deep_blue_f23_1 1) 02 114)
                (list (kern-mk-obj t_kg_potion_deep_blue_f13_1 1) 03 114)

                (list (kern-mk-obj t_kg_potion_deep_blue_f33_2 1) 04 114)
                (list (kern-mk-obj t_kg_potion_deep_blue_f23_2 1) 05 114)
                (list (kern-mk-obj t_kg_potion_deep_blue_f13_2 1) 06 114)

                (list (kern-mk-obj t_kg_potion_deep_blue_f33_3 1) 07 114)
                (list (kern-mk-obj t_kg_potion_deep_blue_f23_3 1) 08 114)
                (list (kern-mk-obj t_kg_potion_deep_blue_f13_3 1) 09 114)

                (list (kern-mk-obj t_kg_potion_deep_blue_f33_4 1) 10 114)
                (list (kern-mk-obj t_kg_potion_deep_blue_f23_4 1) 11 114)
                (list (kern-mk-obj t_kg_potion_deep_blue_f13_4 1) 12 114)

                (list (kern-mk-obj t_kg_potion_deep_blue_f33_5 1) 13 114)
                (list (kern-mk-obj t_kg_potion_deep_blue_f23_5 1) 14 114)
                (list (kern-mk-obj t_kg_potion_deep_blue_f13_5 1) 15 114)

                (list (kern-mk-obj t_kg_potion_deep_blue_f33_6 1) 16 114)
                (list (kern-mk-obj t_kg_potion_deep_blue_f23_6 1) 17 114)
                (list (kern-mk-obj t_kg_potion_deep_blue_f13_6 1) 18 114)

                                        ; light_blue
                (list (kern-mk-obj t_kg_potion_light_blue_f33_1 1) 01 115)
                (list (kern-mk-obj t_kg_potion_light_blue_f23_1 1) 02 115)
                (list (kern-mk-obj t_kg_potion_light_blue_f13_1 1) 03 115)

                (list (kern-mk-obj t_kg_potion_light_blue_f33_2 1) 04 115)
                (list (kern-mk-obj t_kg_potion_light_blue_f23_2 1) 05 115)
                (list (kern-mk-obj t_kg_potion_light_blue_f13_2 1) 06 115)

                (list (kern-mk-obj t_kg_potion_light_blue_f33_3 1) 07 115)
                (list (kern-mk-obj t_kg_potion_light_blue_f23_3 1) 08 115)
                (list (kern-mk-obj t_kg_potion_light_blue_f13_3 1) 09 115)

                (list (kern-mk-obj t_kg_potion_light_blue_f33_4 1) 10 115)
                (list (kern-mk-obj t_kg_potion_light_blue_f23_4 1) 11 115)
                (list (kern-mk-obj t_kg_potion_light_blue_f13_4 1) 12 115)

                (list (kern-mk-obj t_kg_potion_light_blue_f33_5 1) 13 115)
                (list (kern-mk-obj t_kg_potion_light_blue_f23_5 1) 14 115)
                (list (kern-mk-obj t_kg_potion_light_blue_f13_5 1) 15 115)

                (list (kern-mk-obj t_kg_potion_light_blue_f33_6 1) 16 115)
                (list (kern-mk-obj t_kg_potion_light_blue_f23_6 1) 17 115)
                (list (kern-mk-obj t_kg_potion_light_blue_f13_6 1) 18 115)

                                        ; yellow
                (list (kern-mk-obj t_kg_potion_yellow_f33_1 1) 01 116)
                (list (kern-mk-obj t_kg_potion_yellow_f23_1 1) 02 116)
                (list (kern-mk-obj t_kg_potion_yellow_f13_1 1) 03 116)

                (list (kern-mk-obj t_kg_potion_yellow_f33_2 1) 04 116)
                (list (kern-mk-obj t_kg_potion_yellow_f23_2 1) 05 116)
                (list (kern-mk-obj t_kg_potion_yellow_f13_2 1) 06 116)

                (list (kern-mk-obj t_kg_potion_yellow_f33_3 1) 07 116)
                (list (kern-mk-obj t_kg_potion_yellow_f23_3 1) 08 116)
                (list (kern-mk-obj t_kg_potion_yellow_f13_3 1) 09 116)

                (list (kern-mk-obj t_kg_potion_yellow_f33_4 1) 10 116)
                (list (kern-mk-obj t_kg_potion_yellow_f23_4 1) 11 116)
                (list (kern-mk-obj t_kg_potion_yellow_f13_4 1) 12 116)

                (list (kern-mk-obj t_kg_potion_yellow_f33_5 1) 13 116)
                (list (kern-mk-obj t_kg_potion_yellow_f23_5 1) 14 116)
                (list (kern-mk-obj t_kg_potion_yellow_f13_5 1) 15 116)

                (list (kern-mk-obj t_kg_potion_yellow_f33_6 1) 16 116)
                (list (kern-mk-obj t_kg_potion_yellow_f23_6 1) 17 116)
                (list (kern-mk-obj t_kg_potion_yellow_f13_6 1) 18 116)

                                        ; blue
                (list (kern-mk-obj t_kg_potion_blue_f33_1 1) 01 117)
                (list (kern-mk-obj t_kg_potion_blue_f23_1 1) 02 117)
                (list (kern-mk-obj t_kg_potion_blue_f13_1 1) 03 117)

                (list (kern-mk-obj t_kg_potion_blue_f33_2 1) 04 117)
                (list (kern-mk-obj t_kg_potion_blue_f23_2 1) 05 117)
                (list (kern-mk-obj t_kg_potion_blue_f13_2 1) 06 117)

                (list (kern-mk-obj t_kg_potion_blue_f33_3 1) 07 117)
                (list (kern-mk-obj t_kg_potion_blue_f23_3 1) 08 117)
                (list (kern-mk-obj t_kg_potion_blue_f13_3 1) 09 117)

                (list (kern-mk-obj t_kg_potion_blue_f33_4 1) 10 117)
                (list (kern-mk-obj t_kg_potion_blue_f23_4 1) 11 117)
                (list (kern-mk-obj t_kg_potion_blue_f13_4 1) 12 117)

                (list (kern-mk-obj t_kg_potion_blue_f33_5 1) 13 117)
                (list (kern-mk-obj t_kg_potion_blue_f23_5 1) 14 117)
                (list (kern-mk-obj t_kg_potion_blue_f13_5 1) 15 117)

                (list (kern-mk-obj t_kg_potion_blue_f33_6 1) 16 117)
                (list (kern-mk-obj t_kg_potion_blue_f23_6 1) 17 117)
                (list (kern-mk-obj t_kg_potion_blue_f13_6 1) 18 117)

                                        ; dull purple
                (list (kern-mk-obj t_kg_potion_dull_purple_1 1) 01 118)
                (list (kern-mk-obj t_kg_potion_dull_purple_2 1) 02 118)
                (list (kern-mk-obj t_kg_potion_dull_purple_3 1) 03 118)
                (list (kern-mk-obj t_kg_potion_dull_purple_4 1) 04 118)
                (list (kern-mk-obj t_kg_potion_dull_purple_5 1) 05 118)
                (list (kern-mk-obj t_kg_potion_dull_purple_6 1) 06 118)
                                        ; brown
                (list (kern-mk-obj t_kg_potion_brown_1 1) 01 119)
                (list (kern-mk-obj t_kg_potion_brown_2 1) 02 119)
                (list (kern-mk-obj t_kg_potion_brown_3 1) 03 119)
                (list (kern-mk-obj t_kg_potion_brown_4 1) 04 119)
                (list (kern-mk-obj t_kg_potion_brown_5 1) 05 119)
                (list (kern-mk-obj t_kg_potion_brown_6 1) 06 119)
                                        ; dull green
                (list (kern-mk-obj t_kg_potion_dull_green_1 1) 01 120)
                (list (kern-mk-obj t_kg_potion_dull_green_2 1) 02 120)
                (list (kern-mk-obj t_kg_potion_dull_green_3 1) 03 120)
                (list (kern-mk-obj t_kg_potion_dull_green_4 1) 04 120)
                (list (kern-mk-obj t_kg_potion_dull_green_5 1) 05 120)
                (list (kern-mk-obj t_kg_potion_dull_green_6 1) 06 120)
                                        ; dull dark_blue
                (list (kern-mk-obj t_kg_potion_dull_dark_blue_1 1) 01 121)
                (list (kern-mk-obj t_kg_potion_dull_dark_blue_2 1) 02 121)
                (list (kern-mk-obj t_kg_potion_dull_dark_blue_3 1) 03 121)
                (list (kern-mk-obj t_kg_potion_dull_dark_blue_4 1) 04 121)
                (list (kern-mk-obj t_kg_potion_dull_dark_blue_5 1) 05 121)
                (list (kern-mk-obj t_kg_potion_dull_dark_blue_6 1) 06 121)
                                        ; dull light_blue
                (list (kern-mk-obj t_kg_potion_dull_light_blue_1 1) 01 122)
                (list (kern-mk-obj t_kg_potion_dull_light_blue_2 1) 02 122)
                (list (kern-mk-obj t_kg_potion_dull_light_blue_3 1) 03 122)
                (list (kern-mk-obj t_kg_potion_dull_light_blue_4 1) 04 122)
                (list (kern-mk-obj t_kg_potion_dull_light_blue_5 1) 05 122)
                (list (kern-mk-obj t_kg_potion_dull_light_blue_6 1) 06 122)
                                        ; dull yellow
                (list (kern-mk-obj t_kg_potion_dull_yellow_1 1) 01 123)
                (list (kern-mk-obj t_kg_potion_dull_yellow_2 1) 02 123)
                (list (kern-mk-obj t_kg_potion_dull_yellow_3 1) 03 123)
                (list (kern-mk-obj t_kg_potion_dull_yellow_4 1) 04 123)
                (list (kern-mk-obj t_kg_potion_dull_yellow_5 1) 05 123)
                (list (kern-mk-obj t_kg_potion_dull_yellow_6 1) 06 123)
                                        ; dull blue
                (list (kern-mk-obj t_kg_potion_dull_blue_1 1) 01 124)
                (list (kern-mk-obj t_kg_potion_dull_blue_2 1) 02 124)
                (list (kern-mk-obj t_kg_potion_dull_blue_3 1) 03 124)
                (list (kern-mk-obj t_kg_potion_dull_blue_4 1) 04 124)
                (list (kern-mk-obj t_kg_potion_dull_blue_5 1) 05 124)
                (list (kern-mk-obj t_kg_potion_dull_blue_6 1) 06 124)




                
                ) ; objects
               nil ; hooks
               nil ; edge entrances
               )


(kern-mk-map 
 'm_terrain_test 24 32 pal_expanded
  ;                               1  1  1  1  1  1  1  1  1  1  2  2  2  2 
  ; 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8  9  0  1  2  3 
 (list
  "xx xx xx .. .. xx xx xx .. .. xx .. .. .. xx .. .. xx xx xx .. .. xx xx";  // 00
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx";  // 01
  "xx .. .A .B .C .D .E .F .G .H .. .I .J .K .L .M .N .O .P .. .. .. .. ..";  // 02
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..";  // 03
  ".. .. ,A ,B ,C ,D ,E ,F ,G ,H .. ,I ,J ,K ,L ,M ,N ,O ,P .. .. .. .. ..";  // 04
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx";  // 05
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx";  // 06
  "xx .. .Q .R .S .T .U .V .W .X .. .Y .Z  [  @  @  @  @ ]  .. .. .. .. xx";  // 07
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..";  // 08
  ".. .. ,Q ,R ,S ,T ,U ,V ,W ,X .. ,Y ,Z ;T ;E ;N ;A ;S ;D .. .. .. .. ..";  // 09
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx";  // 10
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx";  // 11
  "xx .. __ __ __ .. %% %% %% .. {{ {{ {{ .. .. .. .. .. .. .. .. .. .. xx";  // 12
  ".. .. __ __ .. .. %% %% .. .. {{ {{ .. .. .. .. .. .. .. .. .. .. .. ..";  // 13
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..";  // 14
  ".. .. -- -- -- .. tt tt tt .. ^^ ^^ ^^ .. .. .. .. .. .. .. .. .. .. ..";  // 15
  ".. .. -- -- .. .. tt tt .. .. ^^ ^^ .. .. .. .. .. .. .. .. .. .. .. ..";  // 16
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..";  // 17
  ".. .. ~~ ~~ ~~ .. || || || .. !  !  !  .. .. .. .. .. .. .. .. .. .. ..";  // 18
  ".. .. ~~ ~~ .. .. || || .. .. !  !  .. .. .. .. .. .. .. .. .. .. .. ..";  // 19
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx";  // 20
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx";  // 21
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx";  // 22
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..";  // 23
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..";  // 24
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx";  // 25
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx";  // 26
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx";  // 27
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..";  // 28
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..";  // 29
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx";  // 30
  "xx xx xx .. .. xx xx xx .. .. xx .. .. .. xx .. .. xx xx xx .. .. xx xx";  // 31
  ))
  ;                               1  1  1  1  1  1  1  1  1  1  2  2  2  2 
  ; 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8  9  0  1  2  3 


(kern-mk-place 'p_terrain_test    ; tag
               "Terrain Test"     ; name
               s_dungeon          ; sprite
               m_terrain_test     ; map
               #f                 ; wraps
               #f                 ; underground
               #f                 ; large-scale (wilderness)
               #f                 ; tmp combat place
               nil ; subplaces

;; BUG: A subplace of this small-scale place puts an object at the x,y
;;      but stepping on, or (E)nter, does not enter that place.
;               (list
;                (list p_sprite_gallery_1  21  2)  ; TODO: entrance should be a portal, not subplace
;                ) ; subplaces

               nil ; neighbors
               (list
                (list (mk-perm-gate 'p_sprite_gallery_1  9  1) 21  2)


                (list (kern-mk-obj t_arrow          1) 01 01)
                (list (kern-mk-obj t_warhead        1) 01 02)
                (list (kern-mk-obj t_cannonball     1) 01 03)
                (list (kern-mk-obj t_poison_bolt    1) 01 04)  ;; crash and lock upon pickup
                (list (kern-mk-obj t_fireball       1) 01 05)  ;; immediate crash and exit upon pickup
                (list (kern-mk-obj t_bow            1) 01 06)
                (list (kern-mk-obj t_rpg            1) 01 07)
                (list (kern-mk-obj t_oil            1) 01 08)
                (list (kern-mk-obj t_spear          1) 01 09)
                (list (kern-mk-obj t_thrown_boulder 1) 01 10)  ;; odd -- character can wield and throw
                (list (kern-mk-obj t_dagger         1) 01 11)
                (list (kern-mk-obj t_iron_helm      1) 01 12)
                (list (kern-mk-obj t_sm_shield      1) 01 13)

                (list (kern-mk-obj deathball        1) 01 15)  ;; crash and lock upon pickup
                (list (kern-mk-obj short-sword      1) 01 16)
                (list (kern-mk-obj wooden-buckler   1) 01 17)


                (list large_chest 1 30)

                ) ; objects
               nil ; hooks
               nil ; edge entrances
               )















;; eof
