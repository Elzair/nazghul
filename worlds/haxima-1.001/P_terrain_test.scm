;; TODO: entrance to Sprite Gallery 1 should be a portal, not a subplace
;; TODO: We need 32x32 number (and roman numeral II, III, ...) sprites


(kern-mk-map 
 'm_sprite_gallery_1 19 40 pal_expanded
  ;                               1  1  1  1  1  1  1  1  1
  ; 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8
 (list
  "xx .S .P .R .I .T .E @  @  .G .A .L .L .E .R .Y @  .I xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  ))
  ;                               1  1  1  1  1  1  1  1  1
  ; 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8



;; mk-scenery-type is rather like mk-reagent-type, 
;; but on the terrain features layer, and without an interface.
;; 
;; In future, it will be needful to have some kind of interface --
;; it is an explicit goal in Haxima to enable the player to interact with 
;; the world (and its' objects) whenever reasonable.
(define (mk-scenery-type tag name sprite)
  (mk-obj-type tag name sprite layer-tfeat nil))

;; Lightning bolts:
(mk-scenery-type 't_lightning_bolt_red        "a lightning bolt" s_lightning_bolt_red)
(mk-scenery-type 't_lightning_bolt_green      "a lightning bolt" s_lightning_bolt_green)
(mk-scenery-type 't_lightning_bolt_blue       "a lightning bolt" s_lightning_bolt_blue)
(mk-scenery-type 't_lightning_bolt_cyan       "a lightning bolt" s_lightning_bolt_cyan)
(mk-scenery-type 't_lightning_bolt_purple     "a lightning bolt" s_lightning_bolt_purple)
(mk-scenery-type 't_lightning_bolt_yellow     "a lightning bolt" s_lightning_bolt_yellow)
(mk-scenery-type 't_lightning_bolt_brown      "a lightning bolt" s_lightning_bolt_brown)
(mk-scenery-type 't_lightning_bolt_orange     "a lightning bolt" s_lightning_bolt_orange)
(mk-scenery-type 't_lightning_bolt_white      "a lightning bolt" s_lightning_bolt_white)
(mk-scenery-type 't_lightning_bolt_grey       "a lightning bolt" s_lightning_bolt_grey)
(mk-scenery-type 't_lightning_bolt_dark_grey  "a lightning bolt" s_lightning_bolt_dark_grey)
(mk-scenery-type 't_lightning_bolt_chromatic  "a lightning bolt" s_lightning_bolt_chromatic)
;; Magic ball (fireball core or similar):
(mk-scenery-type 't_fireball_red        "a fireball" s_magic_ball_core_red)
(mk-scenery-type 't_fireball_green      "a fireball" s_magic_ball_core_green)
(mk-scenery-type 't_fireball_blue       "a fireball" s_magic_ball_core_blue)
(mk-scenery-type 't_fireball_cyan       "a fireball" s_magic_ball_core_cyan)
(mk-scenery-type 't_fireball_purple     "a fireball" s_magic_ball_core_purple)
(mk-scenery-type 't_fireball_yellow     "a fireball" s_magic_ball_core_yellow)
(mk-scenery-type 't_fireball_brown      "a fireball" s_magic_ball_core_brown)
(mk-scenery-type 't_fireball_orange     "a fireball" s_magic_ball_core_orange)
(mk-scenery-type 't_fireball_white      "a fireball" s_magic_ball_core_white)
(mk-scenery-type 't_fireball_grey       "a fireball" s_magic_ball_core_grey)
(mk-scenery-type 't_fireball_dark_grey  "a fireball" s_magic_ball_core_dark_grey)
(mk-scenery-type 't_fireball_chromatic  "a fireball" s_magic_ball_core_chromatic)
;; Magic burst/bloom/aura/field (5 ball lightnings in one tile):
(mk-scenery-type 't_magic_field_red           "a magic field" s_magic_field_red)
(mk-scenery-type 't_magic_field_green         "a magic field" s_magic_field_green)
(mk-scenery-type 't_magic_field_blue          "a magic field" s_magic_field_blue)
(mk-scenery-type 't_magic_field_cyan          "a magic field" s_magic_field_cyan)
(mk-scenery-type 't_magic_field_purple        "a magic field" s_magic_field_purple)
(mk-scenery-type 't_magic_field_yellow        "a magic field" s_magic_field_yellow)
(mk-scenery-type 't_magic_field_brown         "a magic field" s_magic_field_brown)
(mk-scenery-type 't_magic_field_orange        "a magic field" s_magic_field_orange)
(mk-scenery-type 't_magic_field_white         "a magic field" s_magic_field_white)
(mk-scenery-type 't_magic_field_grey          "a magic field" s_magic_field_grey)
(mk-scenery-type 't_magic_field_dark_grey     "a magic field" s_magic_field_dark_grey)
(mk-scenery-type 't_magic_field_blue_2        "a magic field" s_magic_field_blue_2)
(mk-scenery-type 't_magic_field_red_pulsed    "a pulsing magic field" s_magic_field_red_pulsed)
(mk-scenery-type 't_magic_field_green_pulsed  "a pulsing magic field" s_magic_field_green_pulsed)
(mk-scenery-type 't_magic_field_blue_pulsed   "a pulsing magic field" s_magic_field_blue_pulsed)
(mk-scenery-type 't_magic_field_white_pulsed  "a pulsing magic field" s_magic_field_white_pulsed)
(mk-scenery-type 't_magic_field_purple_pulsed "a pulsing magic field" s_magic_field_purple_pulsed)
(mk-scenery-type 't_magic_field_brown_pulsed  "a pulsing magic field" s_magic_field_brown_pulsed)
(mk-scenery-type 't_magic_field_blue_2_pulsed "a pulsing magic field" s_magic_field_blue_2_pulsed)
;; Chromatic ball lightning (8 frames):
(mk-scenery-type 't_magic_field_chromatic_pulsed "a pulsing magic field" s_magic_field_chromatic_pulsed)
;; Lightning discharges:
(mk-scenery-type 't_lightning_discharge_red   "lightning" s_lightning_discharge_red)
(mk-scenery-type 't_lightning_discharge_green "lightning" s_lightning_discharge_green)
(mk-scenery-type 't_lightning_discharge_blue  "lightning" s_lightning_discharge_blue)
(mk-scenery-type 't_lightning_discharge_black "lightning" s_lightning_discharge_black)
;; Lightning discharges in cloud:
(mk-scenery-type 't_lightning_cloud_red    "a lightning cloud" s_lightning_cloud_blue_red)
(mk-scenery-type 't_lightning_cloud_green  "a lightning cloud" s_lightning_cloud_blue_green)
(mk-scenery-type 't_lightning_cloud_blue   "a lightning cloud" s_lightning_cloud_blue_blue)
(mk-scenery-type 't_lightning_cloud_purple "a lightning cloud" s_lightning_cloud_blue_purple)
(mk-scenery-type 't_lightning_cloud_yellow "a lightning cloud" s_lightning_cloud_blue_yellow)
(mk-scenery-type 't_lightning_cloud_orange "a lightning cloud" s_lightning_cloud_blue_orange)
;; Magical spheres:
(mk-scenery-type 't_magic_sphere_red    "a magical sphere" s_magic_sphere_red)
(mk-scenery-type 't_magic_sphere_green  "a magical sphere" s_magic_sphere_green)
(mk-scenery-type 't_magic_sphere_blue   "a magical sphere" s_magic_sphere_blue)
(mk-scenery-type 't_magic_sphere_yellow "a magical sphere" s_magic_sphere_yellow)
(mk-scenery-type 't_magic_sphere_purple "a magical sphere" s_magic_sphere_purple)
(mk-scenery-type 't_magic_sphere_cyan   "a magical sphere" s_magic_sphere_cyan)
;; Magical spheres with light rays:
(mk-scenery-type 't_magic_sphere_rays_red    "a magical sphere" s_magic_sphere_rays_red)
(mk-scenery-type 't_magic_sphere_rays_green  "a magical sphere" s_magic_sphere_rays_green)
(mk-scenery-type 't_magic_sphere_rays_blue   "a magical sphere" s_magic_sphere_rays_blue)
(mk-scenery-type 't_magic_sphere_rays_yellow "a magical sphere" s_magic_sphere_rays_yellow)
(mk-scenery-type 't_magic_sphere_rays_purple "a magical sphere" s_magic_sphere_rays_purple)
(mk-scenery-type 't_magic_sphere_rays_cyan   "a magical sphere" s_magic_sphere_rays_cyan)
;; Arrows and Bolts and such in flight:
(mk-scenery-type 't_arrow_wood_red    "an arrow" s_arrow_wood_red)
(mk-scenery-type 't_arrow_metal_red   "an arrow" s_arrow_metal_red)
(mk-scenery-type 't_arrow_wood_green  "an arrow" s_arrow_wood_green)
(mk-scenery-type 't_arrow_metal_green "an arrow" s_arrow_metal_green)

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
                (list (mk-perm-gate 'p_terrain_test 21 2)  9 38)

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

                (list (kern-mk-obj t_magic_field_red_pulsed       1)  5 15)
                (list (kern-mk-obj t_magic_field_green_pulsed     1)  5 16)
                (list (kern-mk-obj t_magic_field_blue_pulsed      1)  5 17)
                (list (kern-mk-obj t_magic_field_white_pulsed     1)  5 18)

                (list (kern-mk-obj t_magic_field_purple_pulsed    1)  7 15)
                (list (kern-mk-obj t_magic_field_brown_pulsed     1)  7 16)
                (list (kern-mk-obj t_magic_field_blue_2_pulsed    1)  7 17)
                (list (kern-mk-obj t_magic_field_chromatic_pulsed 1)  7 18)

                ;; Lightning discharges:
                (list (kern-mk-obj t_lightning_discharge_red   1)  7  2)
                (list (kern-mk-obj t_lightning_discharge_green 1)  7  3)
                (list (kern-mk-obj t_lightning_discharge_blue  1)  7  4)
                (list (kern-mk-obj t_lightning_discharge_black 1)  7  5)

                ;; Lightning discharges in cloud:
                (list (kern-mk-obj t_lightning_cloud_red    1)  7  7)
                (list (kern-mk-obj t_lightning_cloud_green  1)  7  8)
                (list (kern-mk-obj t_lightning_cloud_blue   1)  7  9)
                (list (kern-mk-obj t_lightning_cloud_purple 1)  7 10)
                (list (kern-mk-obj t_lightning_cloud_yellow 1)  7 11)
                (list (kern-mk-obj t_lightning_cloud_orange 1)  7 12)

                ;; Magical spheres:
                (list (kern-mk-obj t_magic_sphere_red    1)  9  2)
                (list (kern-mk-obj t_magic_sphere_green  1)  9  3)
                (list (kern-mk-obj t_magic_sphere_blue   1)  9  4)
                (list (kern-mk-obj t_magic_sphere_yellow 1)  9  5)
                (list (kern-mk-obj t_magic_sphere_purple 1)  9  6)
                (list (kern-mk-obj t_magic_sphere_cyan   1)  9  7)

                ;; Magical spheres with light rays:
                (list (kern-mk-obj t_magic_sphere_rays_red    1)  9  9)
                (list (kern-mk-obj t_magic_sphere_rays_green  1)  9 10)
                (list (kern-mk-obj t_magic_sphere_rays_blue   1)  9 11)
                (list (kern-mk-obj t_magic_sphere_rays_yellow 1)  9 12)
                (list (kern-mk-obj t_magic_sphere_rays_purple 1)  9 13)
                (list (kern-mk-obj t_magic_sphere_rays_cyan   1)  9 14)

                ;; Arrows and Bolts and such in flight:
                (list (kern-mk-obj t_arrow_wood_red    1)  9 16)
                (list (kern-mk-obj t_arrow_metal_red   1)  9 17)
                (list (kern-mk-obj t_arrow_wood_green  1) 10 16)
                (list (kern-mk-obj t_arrow_metal_green 1) 10 17)
                
                ) ; objects
               nil ; hooks
               )


(kern-mk-map 
 'm_terrain_test 24 32 pal_expanded
  ;                               1  1  1  1  1  1  1  1  1  1  2  2  2  2 
  ; 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8  9  0  1  2  3 
 (list
  "xx xx xx .. .. xx xx xx .. .. xx .. .. .. xx .. .. xx xx xx .. .. xx xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .A .B .C .D .E .F .G .H .. .I .J .K .L .M .N .O .P .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. ,A ,B ,C ,D ,E ,F ,G ,H .. ,I ,J ,K ,L ,M ,N ,O ,P .. .. .. .. .."
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .Q .R .S .T .U .V .W .X .. .Y .Z  [  @  @  @  @ ]  .. .. .. .. xx"
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. ,Q ,R ,S ,T ,U ,V ,W ,X .. ,Y ,Z ;T ;E ;N ;A ;S ;D .. .. .. .. .."
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. __ __ __ .. %% %% %% .. {{ {{ {{ .. .. .. .. .. .. .. .. .. .. xx"
  ".. .. __ __ .. .. %% %% .. .. {{ {{ .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. -- -- -- .. tt tt tt .. ^^ ^^ ^^ .. .. .. .. .. .. .. .. .. .. .."
  ".. .. -- -- .. .. tt tt .. .. ^^ ^^ .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. ~~ ~~ ~~ .. || || || .. !  !  !  .. .. .. .. .. .. .. .. .. .. .."
  ".. .. ~~ ~~ .. .. || || .. .. !  !  .. .. .. .. .. .. .. .. .. .. .. .."
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx xx xx .. .. xx xx xx .. .. xx .. .. .. xx .. .. xx xx xx .. .. xx xx"
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
                (list (mk-perm-gate 'p_sprite_gallery_1  9 38) 21  2)

                ) ; objects
               nil ; hooks
               )















;; eof
