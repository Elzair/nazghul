(kern-load "joel.scm")
(kern-load "r2a_mech.scm")

(mk-dungeon-room
 'p_road_to_absalot_1 "Passage to Absalot"
	(list
		"rr rr rr rr !! rr rr rr ,, ,, ,, rr rr rr rr !! rr rr rr "
		"rr rr rr rr !! rr rr {{ ,, .. ,, {{ {{ {{ rr !! rr rr rr "
		"rr rr rr {{ !! {A {{ {C ,, ,, ,, {{ !3 !! !! !! {{ rr rr "
		"rr {{ {{ {C !! !! !! !5 ,, ,, ,, {{ !! bb {F !! {{ rr rr "
		"rr {{ !3 !! !! {& bb !! ,, ,, {4 {{ !! {# bb !! {{ rr rr "
		"rr {{ !! bb !! {{ {% !! ,, ,, ,, {{ !e {{ {% !! {{ rr rr "
		"rr {{ !e {& !! {{ {{ !e .. ,, ,, {A {{ {{ {{ !! {{ {{ ~r "
		"rr {{ {{ {{ !! {{ {{ pp ,, ,, ,, pp {{ {{ {C !! {A {{ rr "
		"rr rr {{ {C !! {A {C ,, ,, ,, ,, ,, {{ !3 !! !! !5 {{ rr "
		"rr rr {{ !3 !! !! !5 ,, ,, ,, ,, ,, {{ !! bb bb !! {{ rr "
		"rr rr {{ !! bb bb !! ,, ,, ,, ,, ,, {{ !! bb {& !e {{ rr "
		"rr rr {{ !! {& bb !! pp {8 ,, ,, bb {{ !! {# {{ {{ {{ rr "
		"rr {{ {{ !! {{ {% !! {# {{ {{ {{ {{ {{ !! {{ {{ {{ {{ rr "
		"rr {{ {{ !! {{ {{ !e {{ {{ rr {{ {{ {{ !! {{ {{ {{ {{ rr "
		"rr {{ {{ !e {{ {{ {{ {{ rr rr {{ {{ {{ !! {{ {{ {{ {{ rr "
		"rr rr {{ {{ {{ rr {{ {{ rr rr rr {{ {{ !e {{ {{ rr bb rr "
		"rr rr rr {{ rr rr {{ rr rr rr rr rr {{ {{ {{ rr rr rr rr "
		"rr rr rr {{ rr rr rr rr rr rr rr rr rr {{ rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
	)
  (put (mk-ladder-up 'p_gate_to_absalot 9 1) 9 9)
  (put (mk-trap-door 'p_absalot_passage 1 38) 18 6)
 (put (spawn-pt 'troll-m) 4 12)
 (put (spawn-pt 'troll-m) 13 4)
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
		"!_ xx bb w+ w+ xx !! !! ,, ,, ,, !! !! xx w+ w+ w+ xx !_ "
		"!_ !! !! !! !! !! !! !! ,, ,, !! !! !! !! !! !! !! !! !_ "
		"!_ !! !! !! !! !! !c {& ,, ,, {4 {% !a !! !! !! !! !! !_ "
		"rr {{ {% !a !! !c {# {{ ,, ,, ,, {{ {% !a !! !c {# {{ rr "
		"rr {{ {{ {{ {{ {{ {{ {{ {2 ,, ,, {{ {{ {{ {{ {{ {{ rr rr "
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
		"rr rr rr {C !! !! !c {{ {{ {{ {{ {{ !a !! !! {A rr rr rr "
		"rr rr {C !3 !! !c {# {{ {{ {{ {{ {{ {{ {% !! !5 {A rr rr "
		"rr {C !3 !! !c {# {{ {{ bb xx xx {{ {{ {{ !a !! !5 {A rr "
		"rr !! !! !c {# {{ {{ xx rr rr xx xx {{ {{ {% !a !! !! rr "
		"rr !! !! {# {{ {{ xx xx xx xx xx xx xx {{ {{ {% !! !! rr "
		"rr !! !! {{ {{ xx xx xx ,, ,, ,, bb rr xx {{ {{ !! !! rr "
		"rr !! !! {{ {{ rr xx xx ,, ,, ,, rr xx xx {{ {{ !! !! rr "
		"rr !! !! {{ {{ bb rr xx ,, ,, ,, xx xx xx {{ {{ !! !! rr "
		"rr !! !! {A {{ {{ xx xx xx ,, xx xx xx {{ {{ {C !! !! rr "
		"rr !! !! !5 {A {{ {{ xx xx ,, xx xx {{ {{ {C !3 !! !! rr "
		"rr !! !! !! !5 {A {{ {{ xx ,, xx {{ {{ {C !3 !! !! !! rr "
		"rr {% !! !! !! !5 {A {{ ,, ,, ,, {{ !3 !! !! !! !! {# ~r "
		"rr {{ !a !! !! !! !5 {{ {2 ,, ,, {{ !! !! !! !! !c {{ rr "
		"rr {{ {% !a !! !! !! {{ ,, ,, ,, {{ !! !! !! !c {# {{ rr "
		"rr {{ {{ {% !a !! xx rr ,, ,, .. xx xx !! !c {# {{ {{ rr "
		"rr rr {{ {{ {% !a xx ,, ,, ,, ,, ,, xx !c {# {{ {{ {{ rr "
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
 (put (spawn-pt 'zorn) 7 6)
 (put (spawn-pt 'zorn) 9 6)
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
    999 0              ; hp mod/mult
    0 0              ; mp mod/mult
    max-health ; hp
    0                   ; xp
    max-health ; mp
    speed-human-hvy-armor
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
			".. {8 .. .. {c {{ rr rr rr rr rr rr rr rr rr rr rr rr rr "
			"rr {{ {2 rr {{ {3 rr rr rr rr rr rr rr rr rr rr rr rr rr "
			"rr rr rr rr rr .. bb rr rr rr rr rr rr rr rr rr rr rr rr "
			"rr rr rr rr rr .. {c {{ rr rr rr rr rr rr rr rr *7 rr rr "
			"rr rr rr rr rr rr {{ {3 rr rr rr *7 rr rr rr *3 vv *5 rr "
			"rr rr rr rr rr rr rr .. rr rr rr vv *5 rr rr vv vv vv rr "
			"rr rr rr rr rr rr rr .. rr rr *3 vv vv vv vv vv vv vv vv "
			"rr rr rr rr rr rr {{ {2 rr rr *2 vv vv vv vv vv vv vv vv "
			"rr rr rr rr rr rr {1 bb rr rr vv vv vv vv vv vv vv vv vv "
			"rr rr rr rr rr rr .. rr rr *3 vv vv vv vv vv *c rr vv vv "
			"rr rr rr rr rr {{ .. rr *3 vv vv vv vv vv *c rr rr *2 vv "
			"rr rr rr rr rr .. *3 vv vv vv vv vv vv vv rr rr rr *2 vv "
			"rr !! !! !! rr .. *2 vv vv vv vv vv vv vv rr rr bb vv vv "
			"!! !! !! !! !! !! ** vv vv vv vv vv vv vv *5 rr *3 vv vv "
			"!! !_ !_ !_ !_ !_ *. vv vv vv vv vv vv vv vv vv vv vv vv "
			"!_ !_ !_ !_ +s !! ** vv vv vv vv vv vv vv vv vv vv vv vv "
			"!_ !_ !! !_ !_ !_ ** vv vv vv vv vv vv vv vv vv vv vv vv "
			"!! !! !! !! !! !! *. vv vv vv vv vv vv vv vv vv vv vv vv "
			"!! !! rr rr rr {c {h vv vv vv vv vv vv vv vv vv vv vv vv "
			"rr rr rr rr rr {{ .. *2 vv vv vv vv vv vv vv vv vv vv vv "
			"rr rr rr rr rr rr .. *2 vv vv vv vv vv *c rr vv vv vv vv "
			"rr rr rr rr rr {{ .l vv vv vv vv vv vv bb rr *a vv vv vv "
			"rr rr rr rr rr .. *3 vv vv vv vv vv vv rr rr rr vv vv vv "
			"rr rr rr rr rr .. *2 vv vv vv vv vv vv *5 rr *3 vv vv vv "
			"rr rr rr rr rr {{ *2 vv vv vv vv vv vv vv vv vv vv vv vv "
			"rr rr rr rr {{ .. *a vv vv vv vv vv vv vv vv vv vv vv vv "
			"rr rr rr rr {1 {8 rr rr *a vv vv vv vv vv vv vv vv vv vv "
			"rr rr rr {{ {6 {{ rr rr rr *a vv vv vv vv vv vv vv vv vv "
			"rr rr rr {{ {2 rr rr rr rr rr vv vv vv vv vv vv *c rr rr "
			"rr rr rr {1 {8 rr rr rr rr rr *2 vv *c rr vv *c rr rr rr "
			"rr rr rr {4 {{ rr rr rr rr rr *e rr rr rr *e rr rr rr rr "
			"rr rr rr bb {1 rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
			"rr rr rr rr .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
			"rr rr rr {{ {2 rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
			"rr rr rr {{ {2 rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
			"rr {4 {{ {3 rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
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
		"^^ ^^ ^^ {{ {a .. .. .. .. .. .. .. .. .. {c {{ ^^ ^^ ^^ "
		"^^ ^^ ^^ {{ {{ {2 .. .. .. .. .. .. .. {4 {{ {{ ^^ ^^ ^^ "
		"^^ ^^ ^^ {{ {{ {2 .. .. .. && .. .. .. {4 {{ {{ ^^ ^^ ^^ "
		"^^ ^^ ^^ {{ {{ {a .. .. .. .. .. .. .. {c {{ {{ ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ {{ {{ {a .. .. .. .. .. {c {{ {{ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ {{ {{ {2 .. .. .. {4 {{ {{ ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ {{ {{ {2 .. .. .. {4 {{ {{ ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ {{ {{ {3 .. .. .. .. .. {5 {{ {{ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^^ {{ {{ {3 .. .. .. .. .. .. .. {5 {{ {{ ^^ ^^ ^^ "
	)
 (put (mk-ladder-down 'p_road_to_absalot_1 9 9) 9 1)
 (put (mk-joel) 9 10)
 (put (mk-npc 'bull 4) 12 12)
 (put (mk-npc 'bull 4) 10 15)
 )

(mk-tower
 'p_tower_of_absalot "Tower of Absalot"
	(list
		"^^ tt tt tc %% te bb %% .. .. ta tc bb ta tc %% %% %c ^^ "
		"^^ ta tt %b ~f %% %% %% %% =| %d bb %b %% %% %% ~f ^3 ^^ "
		"^^ ^5 tt t5 %e t7 bb %e .. .. .. %f bb t7 %a ~f ^b ^^ ^^ "
		"^^ ^^ tt tt tt tt td bb .. .. .. bb tb tt t5 %a ~f ^a ^^ "
		"^^ ^^ tt tt tc xx w+ xx rr .. xx xx bb xx ta tt tt t5 ^^ "
		"^^ ^^ tt tt xx xx ,, ,, w+ .. w+ ,, ,, xx xx tt tt tt ^^ "
		"^^ ^^ tt tt w+ ,, ,, ,, rr d, xx ,, ,, ,, w+ tt tt tt ^^ "
		"^^ ^^ ta tt xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx tt tt tc ^^ "
		"^^ ^^ ^5 tt xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx tt tt ^3 ^^ "
		"^^ ^^ ^^ tt bb .. ,, ,, ,, /c ,, ,, ,, ,, w+ tt tt ^^ ^^ "
		"^^ ^^ ^^ tt t5 .. ,, ,, ,, ,, ,, ,, ,, ,, rr tt tt ^^ ^^ "
		"^^ ^^ ^c tt tc .. .. ,, ,, ,, ,, ,, ,, ,, rr tt tt ^^ ^^ "
		"^^ ^^ t3 tt bb .. ,, ,, ,, ,, ,, ,, ,, ,, w+ tt tt ^^ ^^ "
		"^^ ^^ ta tt rr rr ,, ,, ,, ,, ,, ,, ,, xx xx tt tc ^^ ^^ "
		"^^ ^^ ^5 tt t5 rr w+ xx xx w+ xx xx w+ xx t3 tt ^3 ^^ ^^ "
		"^^ ^^ ^^ ta tt tt tt tt tt tt tt tt tt tt tt tc ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ ^5 ta tt tt tt tt tt tt tt tt tc ^3 ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
	)
 (put (mk-ladder-down 'p_road_to_absalot_3 9 7) 9 9)
 (put (spawn-pt 'death-knight) 6 7)
 (put (spawn-pt 'ghast) 12 11)
 )
 