(kern-load "gamestart-mech.scm")

(kern-mk-place 'p_char_setup "the Path"
  s_shrine ;; sprite
  (kern-mk-map 'm_char_setup 19 19 pal_expanded
	(list
	  "xx xx xx xx xx xx x! xx xx xx xx xx x! xx xx xx xx xx xx "
	  "xx @@ @@ .C .H .O .O .S .E @@ @@ @@ .Y .O .U .R @@ @@ xx "
	  "xx @@ @@ @@ @@ @@ @@ @@ .P .A .T .H @@ @@ @@ @@ @@ @@ xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ++ ,, ,, ,, ,, ,, ,, ,, ++ ,, ,, ,, ,, xx "
	  "x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! "
	  "xx ,, sD cc cc cc cc cc cc cc cc cc cc cc cc cc sW ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, !! ,, ,, ,, ,, ,, ,, ,, !! ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, sS cc cc cc cc cc cc cc cc cc cc cc cc cc sW ,, xx "
	  "x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! "
	  "xx ,, ,, ,, ,, !! ,, ,, ,, ,, ,, ,, ,, !! ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, sS cc cc cc cc cc cc cc cc cc cc cc cc cc sD ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, +s ,, ,, pp cc pp ,, ,, +s ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, pp cc pp ,, ,, ,, ,, ,, ,, ,, xx "
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
	(mk-step-trig 'one-off-message "Would ye be strong, or swift?" "sdmes"))
	15 1 17)

(obj-line (lambda (unused)
	(mk-step-trig 'one-off-message "Would ye be strong, or wise?" "swmes"))
	11 1 17)

(obj-line (lambda (unused)
	(mk-step-trig 'one-off-message "Would ye be swift, or wise?" "dwmes"))
	7 1 17)
	
(obj-line (lambda (xloc)
	(mk-step-trig 'set-stat-info 0 (- xloc 3)))
	14 3 15)

(obj-line (lambda (xloc)
	(mk-step-trig 'set-stat-info 1 (- xloc 3)))
	10 3 15)
	
(obj-line (lambda (xloc)
	(mk-step-trig 'set-stat-info 2 (- xloc 3)))
	6 3 15)

;;quickstart stuff for playtesting

(if #f
    (begin
      (kern-obj-put-at 
       (mk-chest
        nil ;; trap
        
        (list
         
         ;; Food
         (list 1000 t_food)
         
         ;; Gold
         (list 1000 t_gold_coins)
         
         ;; Reagents
         (list 55 sulphorous_ash)
         (list 55 ginseng)
         (list 55 garlic)
         (list 55 spider_silk)
         (list 53 blood_moss)
         (list 53 black_pearl)
         (list 51 nightshade)
         (list 51 mandrake)
         
         ;; Items
         (list 9 t_vas_mani_scroll)
         (list 9 t_in_ex_por_scroll)
         (list 9 t_sanct_lor_scroll)
         (list 9 t_wis_quas_scroll)
         (list 9 t_vas_rel_por_scroll)
         (list 9 t_gem)
         (list 9 t_cure_potion)
         (list 9 t_mana_potion)
         (list 99 t_xp_potion)
         (list 99 t_str_potion)
         (list 99 t_dex_potion)
         (list 99 t_int_potion)
         (list 99 t_info_potion)
         (list 99 t_torch)
         (list 99 t_picklock)
         (list 9 t_pick)
         (list 2 t_dragons_blood)
         (list 2 t_hydras_blood)
         (list 2 t_shovel)
         (list 1 t_rune_k)
         
         
         ;; Arms
         (list 1 t_sword_4)
         (list 1 t_morning_star_2)
         (list 1 t_sling_4)
         (list 1 t_shield)
         (list 1 t_iron_helm_4)
         (list 1 t_armor_plate_4)
         ))
       (list p_char_setup 16 17))
      (kern-obj-put-at
       (mk-ladder-down 'p_moongate_clearing 18 18) 
       (list p_char_setup 17 17)))
    )
