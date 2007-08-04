;; --------------- regular water ------------------------


(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "water" pclass-deep
                   (mk-composite-sprite (cons s_shallow sprites))
                   trn 0 nil))

(mk-blend-terrain 't_wshore_n  s_grass_n )
(mk-blend-terrain 't_wshore_w  s_grass_w )
(mk-blend-terrain 't_wshore_nw s_grass_nw)
(mk-blend-terrain 't_wshore_e  s_grass_e )
(mk-blend-terrain 't_wshore_ne s_grass_ne)
(mk-blend-terrain 't_wshore_we s_grass_e s_grass_w)
(mk-blend-terrain 't_wshore_nwe s_grass_ne s_grass_nw)
(mk-blend-terrain 't_wshore_s  s_grass_s )
(mk-blend-terrain 't_wshore_ns s_grass_s s_grass_n)
(mk-blend-terrain 't_wshore_ws s_grass_sw)
(mk-blend-terrain 't_wshore_nws s_grass_sw s_grass_nw)
(mk-blend-terrain 't_wshore_es s_grass_se)
(mk-blend-terrain 't_wshore_nes s_grass_se s_grass_ne)
(mk-blend-terrain 't_wshore_wes s_grass_se s_grass_sw)
(mk-blend-terrain 't_wshore_c s_grass_se s_grass_sw s_grass_ne s_grass_nw)

(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "deep water" pclass-deep
                   (mk-composite-sprite (cons s_deep sprites))
                   trn 0 nil))

(mk-blend-terrain 't_dshore_n  s_grass_n )
(mk-blend-terrain 't_dshore_w  s_grass_w )
(mk-blend-terrain 't_dshore_nw s_grass_nw)
(mk-blend-terrain 't_dshore_e  s_grass_e )
(mk-blend-terrain 't_dshore_ne s_grass_ne)
(mk-blend-terrain 't_dshore_we s_grass_e s_grass_w)
(mk-blend-terrain 't_dshore_nwe s_grass_ne s_grass_nw)
(mk-blend-terrain 't_dshore_s  s_grass_s )
(mk-blend-terrain 't_dshore_ns s_grass_s s_grass_n)
(mk-blend-terrain 't_dshore_ws s_grass_sw)
(mk-blend-terrain 't_dshore_nws s_grass_sw s_grass_nw)
(mk-blend-terrain 't_dshore_es s_grass_se)
(mk-blend-terrain 't_dshore_nes s_grass_se s_grass_ne)
(mk-blend-terrain 't_dshore_wes s_grass_se s_grass_sw)
(mk-blend-terrain 't_dshore_c s_grass_se s_grass_sw s_grass_ne s_grass_nw)

(kern-mk-terrain 't_water_rocks "boulder" pclass-waterboulder
                   (mk-composite-sprite (list s_shoals s_boulder_over))
                   lgt 0 nil)

;;----------------------------
;; Water edges

(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "grass" pclass-grass
                   (mk-composite-sprite (cons s_shoals (cons s_grassi_c sprites)))
                   trn 0 nil))

(mk-blend-terrain 't_grassw_ne s_grasso_se s_grasso_sw s_grasso_nw)
(mk-blend-terrain 't_grassw_nw s_grasso_se s_grasso_sw s_grasso_ne)
(mk-blend-terrain 't_grassw_es s_grasso_sw s_grasso_ne s_grasso_nw)
(mk-blend-terrain 't_grassw_ws s_grasso_se s_grasso_ne s_grasso_nw)
(mk-blend-terrain 't_grassw_nwe s_grasso_se s_grasso_sw)
(mk-blend-terrain 't_grassw_nes s_grasso_sw s_grasso_nw)
(mk-blend-terrain 't_grassw_nws s_grasso_se s_grasso_ne)
(mk-blend-terrain 't_grassw_wes s_grasso_ne s_grasso_nw)

(kern-mk-terrain 't_grassw_c "grass" pclass-shoals
                   (mk-composite-sprite (list s_shoals s_grassi_c))
                   trn 0 nil)
				   
(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "trees" pclass-trees
                   (mk-composite-sprite (cons s_shoals (cons s_trees_c sprites)))
                   lgt 0 nil))

(mk-blend-terrain 't_treew_ne s_trees_se s_trees_sw s_trees_nw)
(mk-blend-terrain 't_treew_nw s_trees_se s_trees_sw s_trees_ne)
(mk-blend-terrain 't_treew_es s_trees_sw s_trees_ne s_trees_nw)
(mk-blend-terrain 't_treew_ws s_trees_se s_trees_ne s_trees_nw)
(mk-blend-terrain 't_treew_nwe s_trees_se s_trees_sw)
(mk-blend-terrain 't_treew_nes s_trees_sw s_trees_nw)
(mk-blend-terrain 't_treew_nws s_trees_se s_trees_ne)
(mk-blend-terrain 't_treew_wes s_trees_ne s_trees_nw)

(kern-mk-terrain 't_treew_c "trees" pclass-trees
                   (mk-composite-sprite (list s_shoals s_trees_c))
                   lgt 0 nil)
				   
(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "hills" pclass-hills
                   (mk-composite-sprite (cons s_shoals (cons s_hills_c sprites)))
                   dns 0 nil))

(mk-blend-terrain 't_hillw_ne s_hills_se s_hills_sw s_hills_nw)
(mk-blend-terrain 't_hillw_nw s_hills_se s_hills_sw s_hills_ne)
(mk-blend-terrain 't_hillw_es s_hills_sw s_hills_ne s_hills_nw)
(mk-blend-terrain 't_hillw_ws s_hills_se s_hills_ne s_hills_nw)
(mk-blend-terrain 't_hillw_nwe s_hills_se s_hills_sw)
(mk-blend-terrain 't_hillw_nes s_hills_sw s_hills_nw)
(mk-blend-terrain 't_hillw_nws s_hills_se s_hills_ne)
(mk-blend-terrain 't_hillw_wes s_hills_ne s_hills_nw)

(kern-mk-terrain 't_hillw_c "hills" pclass-trees
                   (mk-composite-sprite (list s_shoals s_hills_c))
                   dns 0 nil)
				   
(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "mountains" pclass-mountains
                   (mk-composite-sprite (cons s_shoals (cons s_mount_c sprites)))
                   opq 0 nil))

(mk-blend-terrain 't_mountw_ne s_mount_se s_mount_sw s_mount_nw)
(mk-blend-terrain 't_mountw_nw s_mount_se s_mount_sw s_mount_ne)
(mk-blend-terrain 't_mountw_es s_mount_sw s_mount_ne s_mount_nw)
(mk-blend-terrain 't_mountw_ws s_mount_se s_mount_ne s_mount_nw)
(mk-blend-terrain 't_mountw_nwe s_mount_se s_mount_sw)
(mk-blend-terrain 't_mountw_nes s_mount_sw s_mount_nw)
(mk-blend-terrain 't_mountw_nws s_mount_se s_mount_ne)
(mk-blend-terrain 't_mountw_wes s_mount_ne s_mount_nw)

(kern-mk-terrain 't_mountw_c "mountains" pclass-mountains
                   (mk-composite-sprite (list s_shoals s_mount_c))
                   opq 0 nil)

;;----------------------------------------------------------------------------
;; Some blended hill terrain types

(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "grass" pclass-grass
                   (mk-composite-sprite (cons s_grass sprites))
                   trn 0 nil))

(mk-blend-terrain 't_hilledge_n  s_hill_n )
(mk-blend-terrain 't_hilledge_w  s_hill_w )
(mk-blend-terrain 't_hilledge_nw s_hill_n s_hill_w)
(mk-blend-terrain 't_hilledge_e  s_hill_e )
(mk-blend-terrain 't_hilledge_ne s_hill_n s_hill_e)
(mk-blend-terrain 't_hilledge_we s_hill_e s_hill_w)
(mk-blend-terrain 't_hilledge_nwe s_hill_n s_hill_e s_hill_w)
(mk-blend-terrain 't_hilledge_s  s_hill_s )
(mk-blend-terrain 't_hilledge_ns s_hill_s s_hill_n)
(mk-blend-terrain 't_hilledge_ws s_hill_s s_hill_w)
(mk-blend-terrain 't_hilledge_nws s_hill_s s_hill_w s_hill_n)
(mk-blend-terrain 't_hilledge_es s_hill_s s_hill_e)
(mk-blend-terrain 't_hilledge_nes s_hill_s s_hill_e s_hill_n)
(mk-blend-terrain 't_hilledge_wes s_hill_s s_hill_e s_hill_w)
(mk-blend-terrain 't_hilledge_c s_hill_s s_hill_e s_hill_w s_hill_n)

;; inner hill corners

(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "hills" pclass-hills
                   (mk-composite-sprite (cons s_grass (cons s_hills_c sprites)))
                   dns 0 nil))

(mk-blend-terrain 't_hilli_ne s_hills_se s_hills_sw s_hills_nw)
(mk-blend-terrain 't_hilli_nw s_hills_se s_hills_sw s_hills_ne)
(mk-blend-terrain 't_hilli_es s_hills_sw s_hills_ne s_hills_nw)
(mk-blend-terrain 't_hilli_ws s_hills_se s_hills_ne s_hills_nw)
(mk-blend-terrain 't_hilli_nwe s_hills_se s_hills_sw)
(mk-blend-terrain 't_hilli_nes s_hills_sw s_hills_nw)
(mk-blend-terrain 't_hilli_nws s_hills_se s_hills_ne)
(mk-blend-terrain 't_hilli_wes s_hills_ne s_hills_nw)

(kern-mk-terrain 't_hilli_c "hills" pclass-trees
                   (mk-composite-sprite (list s_grass s_hills_c))
                   1 0 nil)

;;---------------------------------------------------------------------------
;; swamp corners

(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "bog" pclass-hills
                   (mk-composite-sprite (cons s_bog sprites))
                   trn 0 'terrain-effect-swamp))

(mk-blend-terrain 't_bog_nw s_grass_nw)
(mk-blend-terrain 't_bog_ne s_grass_ne)
(mk-blend-terrain 't_bog_nwe s_grass_ne s_grass_nw)
(mk-blend-terrain 't_bog_ws s_grass_sw)
(mk-blend-terrain 't_bog_nws s_grass_sw s_grass_nw)
(mk-blend-terrain 't_bog_es s_grass_se)
(mk-blend-terrain 't_bog_nes s_grass_se s_grass_ne)
(mk-blend-terrain 't_bog_wes s_grass_se s_grass_sw)
(mk-blend-terrain 't_bog_c s_grass_se s_grass_sw s_grass_ne s_grass_nw)

;;----------------------------------------------------------------
;; mountain corners

(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "mountains" pclass-mountains
                   (mk-composite-sprite (cons s_grass (cons s_mount_c sprites)))
                   opq 0 nil))

(mk-blend-terrain 't_mountg_ne s_mount_se s_mount_sw s_mount_nw)
(mk-blend-terrain 't_mountg_nw s_mount_se s_mount_sw s_mount_ne)
(mk-blend-terrain 't_mountg_es s_mount_sw s_mount_ne s_mount_nw)
(mk-blend-terrain 't_mountg_ws s_mount_se s_mount_ne s_mount_nw)
(mk-blend-terrain 't_mountg_nwe s_mount_se s_mount_sw)
(mk-blend-terrain 't_mountg_nes s_mount_sw s_mount_nw)
(mk-blend-terrain 't_mountg_nws s_mount_se s_mount_ne)
(mk-blend-terrain 't_mountg_wes s_mount_ne s_mount_nw)

(kern-mk-terrain 't_mountg_c "mountains" pclass-mountains
                   (mk-composite-sprite (list s_grass s_mount_c))
                   0 0 nil)
				 
;;----------------------------------------------------------------
;; forest corners
  
				   
(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "forest" pclass-forest
                   (mk-composite-sprite (cons s_grass (cons s_forest_c sprites)))
                   hvy 0 nil))

(mk-blend-terrain 't_forestg_ne s_forest_se s_forest_sw s_forest_nw)
(mk-blend-terrain 't_forestg_nw s_forest_se s_forest_sw s_forest_ne)
(mk-blend-terrain 't_forestg_es s_forest_sw s_forest_ne s_forest_nw)
(mk-blend-terrain 't_forestg_ws s_forest_se s_forest_ne s_forest_nw)
(mk-blend-terrain 't_forestg_nwe s_forest_se s_forest_sw)
(mk-blend-terrain 't_forestg_nes s_forest_sw s_forest_nw)
(mk-blend-terrain 't_forestg_nws s_forest_se s_forest_ne)
(mk-blend-terrain 't_forestg_wes s_forest_ne s_forest_nw)
			
				   
				   
;;-------------------------------------------------------
;; void *

(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "cliff edge" pclass-space
                   (mk-composite-sprite (cons s_null sprites))
                   trn 0 nil))

(mk-blend-terrain 't_voids_n  s_grass_n )
(mk-blend-terrain 't_voids_w  s_grass_w )
(mk-blend-terrain 't_voids_nw s_grass_nw)
(mk-blend-terrain 't_voids_e  s_grass_e )
(mk-blend-terrain 't_voids_ne s_grass_ne)
(mk-blend-terrain 't_voids_we s_grass_e s_grass_w)
(mk-blend-terrain 't_voids_nwe s_grass_ne s_grass_nw)
(mk-blend-terrain 't_voids_s  s_grass_s )
(mk-blend-terrain 't_voids_ns s_grass_s s_grass_n)
(mk-blend-terrain 't_voids_ws s_grass_sw)
(mk-blend-terrain 't_voids_nws s_grass_sw s_grass_nw)
(mk-blend-terrain 't_voids_es s_grass_se)
(mk-blend-terrain 't_voids_nes s_grass_se s_grass_ne)
(mk-blend-terrain 't_voids_wes s_grass_se s_grass_sw)
(mk-blend-terrain 't_voids_c s_grass_se s_grass_sw s_grass_ne s_grass_nw)

(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "grass" pclass-grass
                   (mk-composite-sprite (cons s_null (cons s_grassi_c sprites)))
                   trn 0 nil))

(mk-blend-terrain 't_grassv_ne s_grasso_se s_grasso_sw s_grasso_nw)
(mk-blend-terrain 't_grassv_nw s_grasso_se s_grasso_sw s_grasso_ne)
(mk-blend-terrain 't_grassv_es s_grasso_sw s_grasso_ne s_grasso_nw)
(mk-blend-terrain 't_grassv_ws s_grasso_se s_grasso_ne s_grasso_nw)
(mk-blend-terrain 't_grassv_nwe s_grasso_se s_grasso_sw)
(mk-blend-terrain 't_grassv_nes s_grasso_sw s_grasso_nw)
(mk-blend-terrain 't_grassv_nws s_grasso_se s_grasso_ne)
(mk-blend-terrain 't_grassv_wes s_grasso_ne s_grasso_nw)

(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "hills" pclass-hills
                   (mk-composite-sprite (cons s_null (cons s_hills_c sprites)))
                   dns 0 nil))

(mk-blend-terrain 't_hillv_ne s_hills_se s_hills_sw s_hills_nw)
(mk-blend-terrain 't_hillv_nw s_hills_se s_hills_sw s_hills_ne)
(mk-blend-terrain 't_hillv_es s_hills_sw s_hills_ne s_hills_nw)
(mk-blend-terrain 't_hillv_ws s_hills_se s_hills_ne s_hills_nw)
(mk-blend-terrain 't_hillv_nwe s_hills_se s_hills_sw)
(mk-blend-terrain 't_hillv_nes s_hills_sw s_hills_nw)
(mk-blend-terrain 't_hillv_nws s_hills_se s_hills_ne)
(mk-blend-terrain 't_hillv_wes s_hills_ne s_hills_nw)

(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "mountains" pclass-mountains
                   (mk-composite-sprite (cons s_null (cons s_mount_c sprites)))
                   opq 0 nil))

(mk-blend-terrain 't_mountv_ne s_mount_se s_mount_sw s_mount_nw)
(mk-blend-terrain 't_mountv_nw s_mount_se s_mount_sw s_mount_ne)
(mk-blend-terrain 't_mountv_es s_mount_sw s_mount_ne s_mount_nw)
(mk-blend-terrain 't_mountv_ws s_mount_se s_mount_ne s_mount_nw)
(mk-blend-terrain 't_mountv_nwe s_mount_se s_mount_sw)
(mk-blend-terrain 't_mountv_nes s_mount_sw s_mount_nw)
(mk-blend-terrain 't_mountv_nws s_mount_se s_mount_ne)
(mk-blend-terrain 't_mountv_wes s_mount_ne s_mount_nw)

;;---------------------------------------------------------------------------
;; tree corners

(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "trees" pclass-trees
                   (mk-composite-sprite (cons s_trees sprites))
                   lgt 0 nil))

(mk-blend-terrain 't_trees_nw s_treesi_nw s_grasso_nw)
(mk-blend-terrain 't_trees_ne s_treesi_ne s_grasso_ne)
(mk-blend-terrain 't_trees_nwe s_treesi_ne s_grasso_ne s_treesi_nw s_grasso_nw)
(mk-blend-terrain 't_trees_ws s_treesi_sw s_grasso_sw)
(mk-blend-terrain 't_trees_nws s_treesi_sw s_grasso_sw s_treesi_nw s_grasso_nw)
(mk-blend-terrain 't_trees_es s_treesi_se s_grasso_se)
(mk-blend-terrain 't_trees_nes s_treesi_se s_grasso_se s_treesi_ne s_grasso_ne)
(mk-blend-terrain 't_trees_wes s_treesi_se s_grasso_se s_treesi_sw s_grasso_sw)
(mk-blend-terrain 't_trees_c s_treesi_se s_grasso_se s_treesi_sw s_grasso_sw s_treesi_ne s_grasso_ne s_treesi_nw s_grasso_nw)

;; grass with tree corners

(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "grass" pclass-grass
                   (mk-composite-sprite (cons s_grass sprites))
                   trn 0 nil))

(mk-blend-terrain 't_grasst_nw s_treeso_nw)
(mk-blend-terrain 't_grasst_ne s_treeso_ne)
(mk-blend-terrain 't_grasst_nwe s_treeso_ne s_treeso_nw)
(mk-blend-terrain 't_grasst_ws s_treeso_sw)
(mk-blend-terrain 't_grasst_nws s_treeso_sw s_treeso_nw)
(mk-blend-terrain 't_grasst_es s_treeso_se)
(mk-blend-terrain 't_grasst_nes s_treeso_se s_treeso_ne)
(mk-blend-terrain 't_grasst_wes s_treeso_se s_treeso_sw)
(mk-blend-terrain 't_grasst_c s_treeso_se s_treeso_sw s_treeso_ne s_treeso_nw)


;;--------------------------------------------------------------------------
;; lava corners

(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "lava" pclass-hills
                   (mk-composite-sprite (cons s_lava sprites))
                   trn 0 'terrain-effect-lava))

(mk-blend-terrain 't_lava_n  s_grass_n )
(mk-blend-terrain 't_lava_w  s_grass_w )
(mk-blend-terrain 't_lava_nw s_grass_nw)
(mk-blend-terrain 't_lava_e  s_grass_e )
(mk-blend-terrain 't_lava_ne s_grass_ne)
(mk-blend-terrain 't_lava_we s_grass_e s_grass_w)
(mk-blend-terrain 't_lava_nwe s_grass_ne s_grass_nw)
(mk-blend-terrain 't_lava_s  s_grass_s )
(mk-blend-terrain 't_lava_ns s_grass_s s_grass_n)
(mk-blend-terrain 't_lava_ws s_grass_sw)
(mk-blend-terrain 't_lava_nws s_grass_sw s_grass_nw)
(mk-blend-terrain 't_lava_es s_grass_se)
(mk-blend-terrain 't_lava_nes s_grass_se s_grass_ne)
(mk-blend-terrain 't_lava_wes s_grass_se s_grass_sw)
(mk-blend-terrain 't_lava_c s_grass_se s_grass_sw s_grass_ne s_grass_nw)

;;----------------------------------------------------------------
;; natural stone edges

(define (mk-blend-terrain tag . sprites)
  (kern-mk-terrain tag "natural stone wall" pclass-wall
                   (mk-composite-sprite (cons s_nat_rock sprites))
                   opq 0 nil))

(mk-blend-terrain 't_nat_rock_n s_nat_rock_n)
(mk-blend-terrain 't_nat_rock_s s_nat_rock_s)
(mk-blend-terrain 't_nat_rock_w s_nat_rock_w)
(mk-blend-terrain 't_nat_rock_e s_nat_rock_e)
(mk-blend-terrain 't_nat_rock_nw s_nat_rock_n s_nat_rock_w)
(mk-blend-terrain 't_nat_rock_ne s_nat_rock_n s_nat_rock_e)
(mk-blend-terrain 't_nat_rock_ns s_nat_rock_n s_nat_rock_s)
(mk-blend-terrain 't_nat_rock_we s_nat_rock_w s_nat_rock_e)
(mk-blend-terrain 't_nat_rock_ws s_nat_rock_w s_nat_rock_s)
(mk-blend-terrain 't_nat_rock_es s_nat_rock_e s_nat_rock_s)
(mk-blend-terrain 't_nat_rock_nwe s_nat_rock_n s_nat_rock_w s_nat_rock_e)
(mk-blend-terrain 't_nat_rock_nws s_nat_rock_n s_nat_rock_w s_nat_rock_s)
(mk-blend-terrain 't_nat_rock_nes s_nat_rock_n s_nat_rock_e s_nat_rock_s)
(mk-blend-terrain 't_nat_rock_wes s_nat_rock_w s_nat_rock_e s_nat_rock_s)
(mk-blend-terrain 't_nat_rock_nwes s_nat_rock_n s_nat_rock_w s_nat_rock_e s_nat_rock_s)

;;--------------------------------------------------------------------
;; fix terrain types

(define bad-terrain-list
  (list t_bog
        t_lava
        t_deep_lava
        t_fire_terrain
        t_fireplace
        t_inv_wall
        t_wall_torch
		t_lava_n t_lava_w t_lava_nw t_lava_e 
		t_lava_ne t_lava_we t_lava_nwe t_lava_s 
		t_lava_ns t_lava_ws t_lava_nws t_lava_es 
		t_lava_nes t_lava_wes t_lava_c
		t_lava
		t_bog_nw t_bog_ne t_bog_nwe t_bog_ws 
		t_bog_nws t_bog_es t_bog_nes t_bog_wes 
		t_bog_c 
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
		t_bog_nw t_bog_ne t_bog_nwe t_bog_ws 
		t_bog_nws t_bog_es t_bog_nes t_bog_wes 
		t_bog_c 
		t_shore_n t_shore_w t_shore_nw t_shore_e
		t_shore_ne t_shore_we t_shore_nwe t_shore_s
		t_shore_ns t_shore_ws t_shore_nws t_shore_es
		t_shore_nes t_shore_wes t_shore_c
		t_wshore_n t_wshore_w t_wshore_nw t_wshore_e
		t_wshore_ne t_wshore_we t_wshore_nwe t_wshore_s
		t_wshore_ns t_wshore_ws t_wshore_nws t_wshore_es
		t_wshore_nes t_wshore_wes t_wshore_c
		t_dshore_n t_dshore_w t_dshore_nw t_dshore_e
		t_dshore_ne t_dshore_we t_dshore_nwe t_dshore_s
		t_dshore_ns t_dshore_ws t_dshore_nws t_dshore_es
		t_dshore_nes t_dshore_wes t_dshore_c
		t_voids_n t_voids_w t_voids_nw t_voids_e 
		t_voids_ne t_voids_we t_voids_nwe t_voids_s 
		t_voids_ns t_voids_ws t_voids_nws t_voids_es 
		t_voids_nes t_voids_wes t_voids_c 
        ))
		