(kern-load "joel.scm")
(kern-load "r2a_mech.scm")

(mk-dungeon-room
 'p_road_to_absalot_1 "Passage to Absalot"
	(list
	  "rr rr rr rr !! rr rr rr ,, ,, ,, rr rr rr rr !! rr rr rr "
	  "rr rr rr rr !! rr rr {{ ,, .. ,, {{ {{ {{ rr !! rr rr rr "
	  "rr rr rr {{ !! {{ {{ {{ ,, ,, ,, {{ !! !! !! !! {{ rr rr "
	  "rr {{ {{ {{ !! !! !! !! ,, ,, ,, {{ !! bb {{ !! {{ rr rr "
	  "rr {{ ~! !! !! {{ bb !! ,, ,, .. {{ !! {{ bb !! {{ rr rr "
	  "rr {{ ~! bb !! {{ {{ !! ,, ,, ,, {{ !! {{ {{ !! {{ rr rr "
	  "rr {{ ~! {{ !! {{ {{ !! .. ,, ,, {{ {{ {{ {{ !! {{ {{ ~r "
	  "rr {{ {{ {{ !! {{ {{ pp ,, ,, ,, pp {{ {{ {{ !! {{ {{ rr "
	  "rr rr {{ {{ !! {{ {{ ,, ,, ,, ,, ,, {{ !! !! !! !! {{ rr "
	  "rr rr {{ !! !! !! !! ,, ,, ,, ,, ,, {{ !! bb bb !! {{ rr "
	  "rr rr {{ !! bb bb !! ,, ,, ,, ,, ,, {{ !! bb {{ !! {{ rr "
	  "rr rr {{ !! {{ bb !! pp .. ,, ,, bb {{ !! {{ {{ {{ {{ rr "
	  "rr {{ {{ !! {{ {{ !! {{ {{ {{ {{ {{ {{ !! {{ {{ {{ {{ rr "
	  "rr {{ {{ !! {{ {{ !! {{ {{ rr {{ {{ {{ !! {{ {{ {{ {{ rr "
	  "rr {{ {{ !! {{ {{ {{ {{ rr rr {{ {{ {{ !! {{ {{ {{ {{ rr "
	  "rr rr {{ {{ {{ rr {{ {{ rr rr rr {{ {{ !! {{ {{ rr bb rr "
	  "rr rr rr {{ rr rr {{ rr rr rr rr rr {{ {{ {{ rr rr rr rr "
	  "rr rr rr {{ rr rr rr rr rr rr rr rr rr {{ rr rr rr rr rr "
	  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
	)
  (put (mk-ladder-up 'p_gate_to_absalot 9 1) 9 9)
  (put (mk-trap-door 'p_absalot_passage 1 38) 18 6)
 (put (spawn-pt 'troll) 4 12)
 (put (spawn-pt 'troll) 13 4)
 (put (spawn-pt 'headless) 9 4)
 (put (spawn-pt 'headless) 5 4)
 (put (spawn-pt 'gazer) 3 17)
 )

(mk-dungeon-room
 'p_fire_bridge "Fire Bridge"
	(list
	  "xx xx xx rr rr xx xx xx ,, ,, ,, xx xx xx xx xx xx xx xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr "
	  "xx ,, .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb rr "
	  "rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr "
	  "rr ,, ,, ,, ,, xx rr xx ,, w+ ,, xx xx xx ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, xx !_ !_ ,, ,, !! !_ !_ xx ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, w+ !! !_ !_ !_ !_ !_ !! w+ ,, ,, ,, ,, xx "
	  "rr ,, ,, ,, ,, xx !! !! !_ !_ !_ !! !! rr ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, xx xx !! !! !! !! !! xx xx ,, .. ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, w+ !! ,, ,, !! !! w+ ,, ,, ,, ,, ,, rr "
	  "xx ,, ,, ,, ,, ,, w+ !! ,, ,, bb !! w+ ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, w+ !! ,, ,, ,, !! w+ ,, ,, ,, ,, ,, xx "
	  "xx xx ,, ,, ,, xx rr !! !! ,, ,, !! xx xx ,, ,, ,, xx xx "
	  "!! xx bb w+ w+ xx !! !! ,, ,, ,, !! !! xx w+ w+ w+ xx !! "
	  "!! !! ,, !! !! !! !! !! ,, ,, !! !! !! !! !! !! !! !! !! "
	  "!! !! ,, !! !! !! !! {{ ,, ,, .. {{ !! !! !! !! !! !! !! "
	  "rr {{ {{ !! !! !! {{ {{ ,, ,, ,, {{ {{ !! !! !! {{ {{ rr "
	  "rr {{ {{ {{ {{ {{ {{ {{ .. ,, ,, {{ {{ {{ {{ {{ {{ rr rr "
	  "rr rr rr rr rr rr rr rr ,, ,, ,, rr rr rr rr rr rr rr rr "
	)
  (put (kern-tag 'fb-p2 (mk-portcullis)) 8 4)
  (put (kern-tag 'fb-p1
                 (mk-connected-portcullis 'fb-p2)) 10 4)
  (put (kern-tag 'fb-b1 (mk-tblitter 'p_fire_bridge
                                     8 6 3 3
                                     'm_deck_section)) 1 1)
  (put (mk-lever 'fb-p1) 3 10)
  (put (mk-lever 'fb-b1) 15 10)
  ;(put (mk-magic-locked-door) 9 0)
  (put (guard-pt 'craven-archer) 5 10)
  (put (guard-pt 'ghast) 3 11)
  (put (guard-pt 'skeletal-spear-thrower) 5 9)

  (put (guard-pt 'craven-archer) 13 11)
  (put (guard-pt 'ghast) 14 9)
  (put (guard-pt 'craven-archer) 13 9)

  (put (guard-pt 'death-knight) 8 3)
  (put (guard-pt 'death-knight) 10 3)
  (put (spawn-pt 'demon) 16 2)
 )


(mk-dungeon-room
 'p_road_to_absalot_3 "Passage to Absalot"
	(list
	  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
	  "rr rr rr {{ !! !! !! {{ {{ {{ {{ {{ !! !! !! {{ rr rr rr "
	  "rr rr {{ !! !! !! {{ {{ {{ {{ {{ {{ {{ {{ !! !! {{ rr rr "
	  "rr {{ !! !! !! {{ {{ {{ bb xx xx {{ {{ {{ !! !! !! {{ rr "
	  "rr !! !! !! {{ {{ {{ xx rr rr xx xx {{ {{ {{ !! !! !! rr "
	  "rr !! !! {{ {{ {{ xx xx xx xx xx xx xx {{ {{ {{ !! !! rr "
	  "rr !! !! {{ {{ xx xx xx ,, ,, ,, bb rr xx {{ {{ !! !! rr "
	  "rr !! !! {{ {{ rr xx xx ,, ,, ,, rr xx xx {{ {{ !! !! rr "
	  "rr !! !! {{ {{ bb rr xx ,, ,, ,, xx xx xx {{ {{ !! !! rr "
	  "rr !! !! {{ {{ {{ xx xx xx ,, xx xx xx {{ {{ {{ !! !! rr "
	  "rr !! !! !! {{ {{ {{ xx xx ,, xx xx {{ {{ {{ !! !! !! rr "
	  "rr !! !! !! !! {{ {{ {{ xx ,, xx {{ {{ {{ !! !! !! !! rr "
	  "rr {{ !! !! !! !! {{ {{ ,, ,, ,, {{ !! !! !! !! !! {{ ~r "
	  "rr {{ !! !! !! !! !! {{ .. ,, ,, {{ !! !! !! !! !! {{ rr "
	  "rr {{ {{ !! !! !! !! {{ ,, ,, ,, {{ !! !! !! !! {{ {{ rr "
	  "rr {{ {{ {{ !! !! xx rr ,, ,, .. xx xx !! !! {{ {{ {{ rr "
	  "rr rr {{ {{ {{ !! xx ,, ,, ,, ,, ,, xx !! {{ {{ {{ {{ rr "
	  "rr rr rr {{ {{ {{ rr bb ,, ,, ,, ,, ?? {{ {{ {{ {{ rr rr "
	  "rr rr rr rr rr xx xx xx ,, ,, ,, xx xx xx rr rr rr rr rr "
	)
 (put (mk-magic-locked-door) 9 10)
 (put (mk-ladder-up 'p_tower_of_absalot 9 9) 9 7)
 (put (mk-trap-door 'p_absalot_passage 1 2) 18 12)
 (put (guard-pt 'ghast) 10 8)
 (put (guard-pt 'death-knight) 8 8)
 (put (spawn-pt 'craven-archer) 10 6)
 (put (spawn-pt 'zorn) 8 6)
 )

(define (mk-r2a-statue)
  (bind 
   (kern-mk-char 
    'ch_r2a_statue           ; tag
    "Statue"             ; name
    sp_statue         ; species
    nil              ; occ
    s_statue     ; sprite
    faction-men      ; starting alignment
    0 0 0            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    (max-hp sp_statue nil 9 0 0) ; hp
    0                   ; xp
    (max-mp sp_statue nil 9 0 0) ; mp
    9
    #f               ; dead
    'r2a-statue-conv         ; conv
    nil           ; sched
    'ankh-ai              ; special ai
    nil              ; container
    nil              ; readied
    )
   nil))

(kern-mk-place 
 'p_absalot_passage     ; tag
 "Secret Passage"      ; name
  nil          ; sprite
	(kern-mk-map nil 19 40 pal_expanded
		(list
		  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		  "rr rr rr .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		  ".. .. .. .. .. {{ rr rr rr rr rr rr rr rr rr rr rr rr rr "
		  "rr {{ .. rr {{ .. rr rr rr rr rr rr rr rr rr rr rr rr rr "
		  "rr rr rr rr rr .. bb rr rr rr rr rr rr rr rr rr rr rr rr "
		  "rr rr rr rr rr .. .. {{ rr rr rr rr rr rr rr rr vv rr rr "
		  "rr rr rr rr rr rr {{ .. rr rr rr vv rr rr rr vv vv vv rr "
		  "rr rr rr rr rr rr rr .. rr rr rr vv vv rr rr vv vv vv rr "
		  "rr rr rr rr rr rr rr .. rr rr vv vv vv vv vv vv vv vv vv "
		  "rr rr rr rr rr rr {{ .. rr rr vv vv vv vv vv vv vv vv vv "
		  "rr rr rr rr rr rr .. bb rr rr vv vv vv vv vv vv vv vv vv "
		  "rr rr rr rr rr rr .. rr rr vv vv vv vv vv vv vv rr vv vv "
		  "rr rr rr rr rr {{ .. rr vv vv vv vv vv vv vv rr rr vv vv "
		  "rr rr rr rr rr .. vv vv vv vv vv vv vv vv rr rr rr vv vv "
		  "rr !! !! !! rr .. vv vv vv vv vv vv vv vv rr rr bb vv vv "
		  "!! !! !! !! !! !! ** vv vv vv vv vv vv vv vv rr vv vv vv "
		  "!! !_ !_ !_ !_ !_ ** vv vv vv vv vv vv vv vv vv vv vv vv "
		  "!_ !_ !_ !_ +s !! ** vv vv vv vv vv vv vv vv vv vv vv vv "
		  "!_ !_ !! !_ !_ !_ ** vv vv vv vv vv vv vv vv vv vv vv vv "
		  "!! !! !! !! !! !! ** vv vv vv vv vv vv vv vv vv vv vv vv "
		  "!! !! rr rr rr .. {{ vv vv vv vv vv vv vv vv vv vv vv vv "
		  "rr rr rr rr rr {{ .. vv vv vv vv vv vv vv vv vv vv vv vv "
		  "rr rr rr rr rr rr .. vv vv vv vv vv vv vv rr vv vv vv vv "
		  "rr rr rr rr rr {{ .. vv vv vv vv vv vv bb rr vv vv vv vv "
		  "rr rr rr rr rr .. vv vv vv vv vv vv vv rr rr rr vv vv vv "
		  "rr rr rr rr rr .. vv vv vv vv vv vv vv vv rr vv vv vv vv "
		  "rr rr rr rr rr {{ vv vv vv vv vv vv vv vv vv vv vv vv vv "
		  "rr rr rr rr {{ .. vv vv vv vv vv vv vv vv vv vv vv vv vv "
		  "rr rr rr rr .. .. rr rr vv vv vv vv vv vv vv vv vv vv vv "
		  "rr rr rr {{ .. {{ rr rr rr vv vv vv vv vv vv vv vv vv vv "
		  "rr rr rr {{ .. rr rr rr rr rr vv vv vv vv vv vv vv rr rr "
		  "rr rr rr .. .. rr rr rr rr rr vv vv vv rr vv vv rr rr rr "
		  "rr rr rr .. {{ rr rr rr rr rr vv rr rr rr vv rr rr rr rr "
		  "rr rr rr bb .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		  "rr rr rr rr .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		  "rr rr rr {{ .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		  "rr rr rr {{ .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		  "rr .. {{ .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		  ".. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		)	
	)

 #f              ; wraps
 #t              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors
 
 (list (put (mk-monman) 0 0) ; objects
 (put (mk-trap-door 'p_road_to_absalot_3 17 12) 0 2)
 (put (mk-trap-door 'p_road_to_absalot_1 17 6) 0 38)
 (put (make-invisible (mk-r2a-statue)) 4 17)
 (put (spawn-pt 'bat) 15 22)
 (put (spawn-pt 'bat) 10 10)
 (put (spawn-pt 'bat) 10 30)
 (put (spawn-pt 'bat) 16 30)
 (put (spawn-pt 'bat) 16 15)
 )
 (list
	'fix-lava
	'on-entry-to-dungeon-room
	) ;; hooks
 	nil
 )
 
(mk-dungeon-level 
 (list p_road_to_absalot_3)
 (list p_fire_bridge)
 (list p_road_to_absalot_1)
 )

(mk-tower
 'p_gate_to_absalot "Gate To Absalot"
 (list
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ xx xx xx xx xx ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ xx xx xx xx xx ,, ,, ,, xx xx xx xx rr ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, xx .. ,, ,, xx ,, ,, ,, bb ^^ ^^ ^^ "
      "^^ ^^ ^^ xx bb ,, ,, ,, ,, ,, ,, ,, ,, .. ,, rr ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, .. ,, xx ,, ,, ,, xx ,, ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, xx xx bb xx rr ,, ,, bb xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, bb xx bb bb bb xx ,, ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, w+ .. bb .. w+ ,, ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, xx .. .. .. xx bb ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx xx w+ xx rr .. .. .. xx xx w+ xx xx ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. {{ ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. {{ {{ ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ {{ .. .. .. .. && .. .. .. .. {{ {{ ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. {{ {{ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. {{ {{ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ {{ {{ .. .. .. .. .. {{ {{ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ {{ {{ .. .. .. .. .. {{ {{ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. {{ {{ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. {{ {{ ^^ ^^ ^^ "
  )
 (put (mk-ladder-down 'p_road_to_absalot_1 9 9) 9 1)
 (put (mk-joel) 9 10)
 (put (mk-npc 'bull 4) 12 12)
 (put (mk-npc 'bull 4) 10 15)
 )

(mk-tower
 'p_tower_of_absalot "Tower of Absalot"
 (list
      "^^ tt tt tt %% tt bb %% .. .. tt tt bb tt tt %% %% %% ^^ "
      "^^ tt tt %% ~~ %% %% %% %% =| %% bb %% %% %% %% ~~ ^^ ^^ "
      "^^ ^^ tt tt %% tt bb %% .. .. .. %% bb tt %% ~~ ^^ ^^ ^^ "
      "^^ ^^ tt tt tt tt tt bb .. .. .. bb tt tt tt %% ~~ ^^ ^^ "
      "^^ ^^ tt tt tt xx w+ xx rr .. xx xx bb xx tt tt tt tt ^^ "
      "^^ ^^ tt tt xx xx ,, ,, w+ .. w+ ,, ,, xx xx tt tt tt ^^ "
      "^^ ^^ tt tt w+ ,, ,, ,, rr d, xx ,, ,, ,, w+ tt tt tt ^^ "
      "^^ ^^ tt tt xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx tt tt tt ^^ "
      "^^ ^^ ^^ tt xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx tt tt ^^ ^^ "
      "^^ ^^ ^^ tt bb .. ,, ,, ,, /c ,, ,, ,, ,, w+ tt tt ^^ ^^ "
      "^^ ^^ ^^ tt tt .. ,, ,, ,, ,, ,, ,, ,, ,, rr tt tt ^^ ^^ "
      "^^ ^^ ^^ tt tt .. .. ,, ,, ,, ,, ,, ,, ,, rr tt tt ^^ ^^ "
      "^^ ^^ tt tt bb .. ,, ,, ,, ,, ,, ,, ,, ,, w+ tt tt ^^ ^^ "
      "^^ ^^ tt tt rr rr ,, ,, ,, ,, ,, ,, ,, xx xx tt tt ^^ ^^ "
      "^^ ^^ ^^ tt tt rr w+ xx xx w+ xx xx w+ xx tt tt ^^ ^^ ^^ "
      "^^ ^^ ^^ tt tt tt tt tt tt tt tt tt tt tt tt tt ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ tt tt tt tt tt tt tt tt tt tt ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
  )
 (put (mk-ladder-down 'p_road_to_absalot_3 9 7) 9 9)
 (put (spawn-pt 'death-knight) 6 7)
 (put (spawn-pt 'ghast) 12 11)
 )
 