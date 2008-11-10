(kern-load "gamestart-mech.scm")
(kern-load "gamestart-statues.scm")

(kern-mk-place 'p_char_setup "the Path"
  s_shrine ;; sprite
  (kern-mk-map 'm_char_setup 19 19 pal_expanded
	(list
	  "xx xx xx xx xx xx x! xx xx xx xx xx x! xx xx xx xx xx xx "
	  "x! @@ @@ .C .H .O .O .S .E @@ @@ @@ .Y .O .U .R @@ @@ x! "
	  "xx @@ @@ @@ @@ @@ @@ .P .A .T .H @@ @@ @@ @@ @@ @@ @@ xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ++ ,, ,, ,, ,, ,, ,, ,, ++ ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, cx cx cx ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, cx cx cx ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, cx cx cx ,, ,, cx cx cx ,, ,, cx cx cx ,, ,, xx "
	  "xx ,, ,, cx cx cx ,, ,, ,, ,, ,, ,, ,, cx cx cx ,, ,, xx "
	  "xx ,, ,, cx cx cx ,, ,, ,, ,, ,, ,, ,, cx cx cx ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "x! ,, ,, +s ,, ,, ,, ,, pp cc pp ,, ,, ,, ,, +s ,, ,, x! "
	  "xx ,, ,, ,, ,, ,, ,, pp ,, cc ,, pp ,, ,, ,, ,, ,, ,, xx "
	  "xx xx xx xx xx xx x! xx xx xx xx xx x! xx xx xx xx xx xx "
	)
	)

  #f #t #f #f
  ;; subplaces
  nil
  ;; neighbors
  nil
  
 ;; *** contents of the place ***
  (list
   (put (mk-step-trig 'get-player-name nil) 9 16)
   (put (kern-tag 'start-gate (mk-start-portal 'start-actual-game)) 9 4)
   (put (kern-mk-obj F_illum_perm 1) 3 1)
   (put (kern-mk-obj F_illum_perm 1) 15 1)
   (put (kern-mk-obj F_illum_perm 1) 9 1)
   (put (kern-mk-obj F_illum_perm 1) 10 4)
   (put (kern-mk-obj F_illum_perm 1) 8 4)
   )

  nil ;; hooks
  nil

)
 
(define (obj-line objfactory yloc xloc xmax)
	(kern-obj-put-at (objfactory xloc) (list p_char_setup xloc yloc))
	(if (< xloc xmax)
		(obj-line objfactory yloc (+ xloc 1) xmax)
	))  
 
;; Note: start-gate must be a tag to survive saving/reloading.
(set-roomdata p_char_setup (list 6 6 6 'start-gate))

(obj-line (lambda (unused)
	(mk-step-trig 'one-off-message "A portal beckons on the far side of the room" "intromes"))
	15 8 10)
	
(obj-line (lambda (unused)
	(mk-step-trig 'gamestart-light-lamps nil "lamps"))
	14 7 11)
	
(kern-obj-put-at (mk-step-trig 'gamestart-light-lamps nil "lamps") (list p_char_setup 7 15))
(kern-obj-put-at (mk-step-trig 'gamestart-light-lamps nil "lamps") (list p_char_setup 11 15))
(kern-obj-put-at (mk-step-trig 'gamestart-light-lamps nil "lamps") (list p_char_setup 7 16))
(kern-obj-put-at (mk-step-trig 'gamestart-light-lamps nil "lamps") (list p_char_setup 11 16))

(define (mk-start-statue tag name sprite conv)
  (let ((kchar (bind 
                (kern-mk-char 
                 tag            ; tag
                 name             ; name
                 sp_statue         ; species
                 nil              ; occ
                 sprite     ; sprite
                 faction-men      ; starting alignment
                 0 0 0            ; str/int/dex
                 999 0              ; hp mod/mult
                 0 0              ; mp mod/mult
                 max-health ; hp
                 0                   ; xp
                 max-health ; mp
                 0
                 9
                 #f               ; dead
                 conv         ; conv
                 nil           ; sched
                 'ankh-ai              ; special ai
                 nil              ; container
                 nil              ; readied
                 )
                nil)))
    (kern-char-set-known kchar #t)
    ))

(kern-obj-put-at (mk-start-statue 'str_statue "Statue of Might" s_str_statue 'gs-str-conv) (list p_char_setup 4 10))
(kern-obj-put-at (mk-start-statue 'dex_statue "Statue of Agility" s_dex_statue 'gs-dex-conv) (list p_char_setup 9 8))
(kern-obj-put-at (mk-start-statue 'int_statue "Statue of Wisdom" s_int_statue 'gs-int-conv) (list p_char_setup 14 10))

(obj-line (lambda (unused)
	(mk-step-trig 'gamestart-statue-speak 'str_statue "statspeak"))
	10 1 6)
(obj-line (lambda (unused)
	(mk-step-trig 'gamestart-statue-speak 'dex_statue "statspeak"))
	9 6 12)
(obj-line (lambda (unused)
	(mk-step-trig 'gamestart-statue-speak 'int_statue "statspeak"))
	10 12 17)

	
;;(gamestart-field-circle F_fire_perm p_char_setup 4 10 4)
;;(gamestart-field-circle F_acid_perm p_char_setup 9 8 4)
;;(gamestart-field-circle F_energy_perm p_char_setup 14 10 4)

;;quickstart stuff for playtesting

(if #f
    (begin
      (kern-obj-put-at 
       (mk-chest
        nil ;; trap
        
        '(
         
         ;; Food
         ( 1000 t_food)
         
         ;; Gold
         ( 1000 t_gold_coins)
         
         ;; Reagents
         ( 55 sulphorous_ash)
         ( 55 ginseng)
         ( 55 garlic)
         ( 55 spider_silk)
         ( 53 blood_moss)
         ( 53 black_pearl)
         ( 51 nightshade)
         ( 51 mandrake)
         
         ;; Items
         ( 9 t_vas_mani_scroll)
         ( 9 t_in_ex_por_scroll)
         ( 9 t_sanct_lor_scroll)
         ( 9 t_wis_quas_scroll)
         ( 9 t_xen_corp_scroll)
         ( 9 t_an_xen_ex_scroll)
         ( 9 t_vas_rel_por_scroll)
         ( 59 t_gem)
         ( 9 t_cure_potion)
         ( 9 t_mana_potion)
         ( 99 t_xp_potion)
         ( 99 t_str_potion)
         ( 99 t_dex_potion)
         ( 99 t_int_potion)
         ( 99 t_info_potion)
         ( 99 t_torch)
         ( 99 t_picklock)
         ( 9 t_pick)
         ( 2 t_dragons_blood)
         ( 2 t_hydras_blood)
         ( 2 t_shovel)
         ( 1 t_rune_k)
         
         
         ;; Arms
	 ( 1 t_dagger)
	 ( 1 t_mace)
	 ( 1 t_axe)
	 ( 1 t_sword)
	 ( 1 t_2H_axe)
	 ( 1 t_2H_sword)
	 ( 1 t_morning_star)
	 ( 1 t_halberd)
	 ( 1 t_staff)

	 ( 1 t_dagger_4)
         ( 1 t_sword_4)
         ( 1 t_morning_star_2)
	 ( 1 t_stun_wand)
	 ( 1 t_doom_staff)
	 ( 1 t_eldritch_blade)
	 ( 1 t_mystic_sword)
	 ( 1 t_flaming_sword)

         ( 1 t_shield)
	 ( 1 t_scratched_shield)

	 ( 1 t_leather_helm)
	 ( 1 t_chain_coif)
	 ( 1 t_iron_helm)

	 ( 1 t_armor_leather)
	 ( 1 t_armor_chain)
	 ( 1 t_armor_plate)

         ( 1 t_iron_helm_4)
	 ( 1 t_armor_leather_4)
	 ( 1 t_armor_chain_4)
         ( 1 t_armor_plate_4)

	 ( 1 t_chrono)
	 ( 1 t_spiked_shield)

	 ( 1 t_sling)
	 ( 3 t_spear)

         ( 1 t_sling_4)
	 ( 1 t_magic_axe)

	 (20 t_oil)
	 (20 t_slime_vial)

	 (  1 t_self_bow)
	 (  1 t_bow)
	 (  1 t_long_bow)
	 (  1 t_great_bow)
	 (500 t_arrow)

	 (  1 t_lt_crossbow)
	 (  1 t_crossbow)
	 (  1 t_hvy_crossbow)
	 (  1 t_trpl_crossbow)
	 (500 t_bolt)

         ))
       (list p_char_setup 16 17))
      (kern-obj-put-at
       (mk-ladder-down 'p_moongate_clearing 18 18) 
       (list p_char_setup 17 17)))
    )
