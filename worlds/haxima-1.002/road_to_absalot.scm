(kern-load "joel.scm")
(kern-load "r2a_mech.scm")

(mk-dungeon-room
 'p_road_to_absalot_1 "Passage to Absalot"
	(list
		"rn rn rn r4 !! r2 rn rc ,, ,, ,, ra r8 r8 r4 !! r2 rn rn "
		"rn rn rn rc !! ra rc {{ ,, .. ,, {{ {{ {{ re !! ra rn rn "
		"rn r8 rc {{ !! {A {{ {C ,, ,, ,, {{ !3 !! !! !! {{ r2 rn "
		"r4 {{ {{ {C !! !! !! !5 ,, ,, ,, {{ !! bb {F !! {{ r2 rn "
		"r4 {{ !3 !! !! {& bb !! ,, ,, {4 {{ !! {# bb !! {{ r2 rn "
		"r4 {{ !! bb !! {{ {% !! ,, ,, ,, {{ !e {{ {% !! {{ ra rn "
		"r4 {{ !e {& !! {{ {{ !e .. ,, ,, {A {{ {{ {{ !! {{ {{ r~ "
		"r4 {{ {{ {{ !! {{ {{ pp ,, ,, ,, pp {{ {{ {C !! {A {{ r2 "
		"rn r5 {{ {C !! {A {C ,, ,, ,, ,, ,, {{ !3 !! !! !5 {{ r2 "
		"rn r4 {{ !3 !! !! !5 ,, ,, ,, ,, ,, {{ !! bb bb !! {{ r2 "
		"rn r4 {{ !! bb bb !! ,, ,, ,, ,, ,, {{ !! bb {& !e {{ r2 "
		"rn rc {{ !! {& bb !! pp {8 ,, ,, bb {{ !! {# {{ {{ {{ r2 "
		"r4 {{ {{ !! {{ {% !! {# {{ {{ {{ {{ {{ !! {{ {{ {{ {{ r2 "
		"r4 {{ {{ !! {{ {{ !e {{ {{ r7 {{ {{ {{ !! {{ {{ {{ {{ r2 "
		"r4 {{ {{ !e {{ {{ {{ {{ r3 r4 {{ {{ {{ !! {{ {{ {{ {{ r2 "
		"rn r5 {{ {{ {{ r7 {{ {{ r2 rn r5 {{ {{ !e {{ {{ r7 bb r2 "
		"rn rn r5 {{ r3 r4 {{ r3 rn rn rn r5 {{ {{ {{ r3 rn r1 rn "
		"rn rn r4 {{ r2 rn r1 rn rn rn rn rn r5 {{ r3 rn rn rn rn "
		"rn rn rn r1 rn rn rn rn rn rn rn rn rn r1 rn rn rn rn rn "
	)
  (put (mk-ladder-up 'p_gate_to_absalot 9 1) 9 9)
  (put (mk-bump-door 'p_absalot_passage 1 38) 18 6)
 (put (spawn-pt 'troll-m) 4 12)
 (put (spawn-pt 'troll-m) 13 4)
 (put (spawn-pt 'headless) 9 4)
 (put (spawn-pt 'headless) 5 4)
 (put (spawn-pt 'gazer) 3 17)
 )
 
(mk-place-music p_road_to_absalot_1 'ml-dungeon-adventure)

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
		"r5 {{ {% !a !! !c {# {{ ,, ,, ,, {{ {% !a !! !c {# {{ r3 "
		"r4 {{ {{ {{ {{ {{ {{ {{ {2 ,, ,, {{ {{ {{ {{ {{ {{ r3 rn "
		"rn r1 r1 r1 r1 r1 r1 r5 ,, ,, ,, r3 r1 r1 r1 r1 r1 rn rn "
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

  (put (kern-mk-obj t_spell_book_force_magick_matter 1) 17 9)
 )

(mk-place-music p_fire_bridge 'ml-dungeon-adventure)

(mk-dungeon-room
 'p_road_to_absalot_3 "Passage to Absalot"
	(list
		"rn rn rn r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 rn rn rn "
		"rn rn rc {C !! !! !c {{ {{ {{ {{ {{ !a !! !! {A ra rn rn "
		"rn rc {C !3 !! !c {# {{ {{ {{ {{ {{ {{ {% !! !5 {A ra rn "
		"r4 {C !3 !! !c {# {{ {{ bb xx xx {{ {{ {{ !a !! !5 {A r2 "
		"r4 !! !! !c {# {{ {{ xx rr rr xx xx {{ {{ {% !a !! !! r2 "
		"r4 !! !! {# {{ {{ xx xx xx xx xx xx xx {{ {{ {% !! !! r2 "
		"r4 !! !! {{ {{ xx xx xx ,, ,, ,, bb rr xx {{ {{ !! !! r2 "
		"r4 !! !! {{ {{ rr xx xx ,, ,, ,, rr xx xx {{ {{ !! !! r2 "
		"r4 !! !! {{ {{ bb rr xx ,, ,, ,, xx xx xx {{ {{ !! !! r2 "
		"r4 !! !! {A {{ {{ xx xx xx ,, xx xx xx {{ {{ {C !! !! r2 "
		"r4 !! !! !5 {A {{ {{ xx xx ,, xx xx {{ {{ {C !3 !! !! r2 "
		"r4 !! !! !! !5 {A {{ {{ xx ,, xx {{ {{ {C !3 !! !! !! r2 "
		"r4 {% !! !! !! !5 {A {{ ,, ,, ,, {{ !3 !! !! !! !! {# r~ "
		"r4 {{ !a !! !! !! !5 {{ {2 ,, ,, {{ !! !! !! !! !c {{ r2 "
		"r4 {{ {% !a !! !! !! {{ ,, ,, ,, {{ !! !! !! !c {# {{ r2 "
		"r4 {{ {{ {% !a !! xx rr ,, ,, .. xx xx !! !c {# {{ {{ r2 "
		"rn r5 {{ {{ {% !a xx ,, ,, ,, ,, ,, xx !c {# {{ {{ {{ r2 "
		"rn rn r5 {{ {{ {{ rr bb ,, ,, ,, ,, ?? {{ {{ {{ {{ r3 rn "
		"rn rn rn r1 r1 xx xx xx ,, ,, ,, xx xx xx r1 r1 r1 rn rn "
	)
 (put (mk-magic-locked-door) 9 10)
 (put (mk-ladder-up 'p_tower_of_absalot 9 9) 9 7)
 (put (mk-bump-door 'p_absalot_passage 1 2) 18 12)
 (put (guard-pt 'ghast) 10 8)
 (put (guard-pt 'death-knight) 8 8)
 (put (spawn-pt 'craven-archer) 10 6)
 (put (spawn-pt 'zorn) 8 6)
 (put (spawn-pt 'zorn) 7 6)
 (put (spawn-pt 'zorn) 9 6)
 )

(mk-place-music p_road_to_absalot_3 'ml-dungeon-adventure)

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
    0
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
		"rn rn rn r8 rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"r8 r8 rc .. ra r8 rn rn rn rn rn rn rn rn rn rn rn rn rn "
		".. {8 .. .. {c {{ r2 rn rn rn rn rn rn rn rn rn rn rn rn "
		"r5 {{ {2 r7 {{ {3 ra rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn r1 r1 rn r5 .. bb ra rn rn rn rn rn rn rn rn r8 rn rn "
		"rn rn rn rn r4 .. {c {{ r2 rn rn r8 rn rn rn rc *7 ra rn "
		"rn rn rn rn rn r5 {{ {3 r2 rn r4 *7 ra rn r4 *3 vv *5 r2 "
		"rn rn rn rn rn rn r5 .. r2 rn rc vv *5 ra rc vv vv vv ra "
		"rn rn rn rn rn rn rc .. r2 r4 *3 vv vv vv vv vv vv vv vv "
		"rn rn rn rn rn r4 {{ {2 r2 r4 *2 vv vv vv vv vv vv vv vv "
		"rn rn rn rn rn r4 {1 bb r2 rc vv vv vv vv vv vv vv vv vv "
		"rn rn rn rn rn rc .. r3 rc *3 vv vv vv vv vv *c r7 vv vv "
		"rn rn rn rn r4 {{ .. re *3 vv vv vv vv vv *c r3 r4 *2 vv "
		"rn r8 r8 r8 r4 .. *3 vv vv vv vv vv vv vv r3 rn rc *2 vv "
		"rc !! !! !! re .. *2 vv vv vv vv vv vv vv ra r4 bb vv vv "
		"!! !! !! !! !! !! *2 vv vv vv vv vv vv vv *5 re *3 vv vv "
		"!! !_ !_ !_ !_ !_ *2 vv vv vv vv vv vv vv vv vv vv vv vv "
		"!_ !_ !_ !_ +s !! *2 vv vv vv vv vv vv vv vv vv vv vv vv "
		"!_ !_ !! !_ !_ !_ *2 vv vv vv vv vv vv vv vv vv vv vv vv "
		"!! !! !! !! !! !! *a vv vv vv vv vv vv vv vv vv vv vv vv "
		"!! !! r3 r1 r5 {c {h vv vv vv vv vv vv vv vv vv vv vv vv "
		"r1 r1 rn rn r4 {{ .. *2 vv vv vv vv vv vv vv vv vv vv vv "
		"rn rn rn rn rn rd .. *2 vv vv vv vv vv *c r7 vv vv vv vv "
		"rn rn rn rn r4 {{ .l vv vv vv vv vv vv bb r6 *a vv vv vv "
		"rn rn rn rn r4 .. *3 vv vv vv vv vv vv rb rn rd vv vv vv "
		"rn rn rn rn r4 .. *2 vv vv vv vv vv vv *5 re *3 vv vv vv "
		"rn rn rn rn rc {{ *2 vv vv vv vv vv vv vv vv vv vv vv vv "
		"rn rn rn r4 {{ .. *a vv vv vv vv vv vv vv vv vv vv vv vv "
		"rn rn rn rc {1 {8 r3 r5 *a vv vv vv vv vv vv vv vv vv vv "
		"rn rn r4 {{ {6 {{ r2 rn r5 *a vv vv vv vv vv vv vv vv vv "
		"rn rn r4 {{ {2 r3 rn rn rn r5 vv vv vv vv vv vv *c r3 r1 "
		"rn rn r4 {1 {8 r2 rn rn rn r4 *2 vv *c r7 vv *c r3 rn rn "
		"rn rn r4 {4 {{ r2 rn rn rn r4 *e r3 r1 r4 *e r3 rn rn rn "
		"rn rn r4 bb {1 r2 rn rn rn rn r1 rn rn rn r1 rn rn rn rn "
		"rn rn rn rd .. r2 rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn r4 {{ {2 r2 rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn r8 rc {{ {2 r2 rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rc {4 {{ {3 r3 rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		".. .. r3 r1 rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"r1 r1 rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		)
	)

 #f              ; wraps
 #t              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors
 
 (list (put (mk-monman) 0 0) ; objects
 (put (mk-bump-door 'p_road_to_absalot_3 17 12) 0 2)
 (put (mk-bump-door 'p_road_to_absalot_1 17 6) 0 38)
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

(mk-place-music p_absalot_passage 'ml-dungeon-adventure)
 
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
	(list
		(list northwest 14 18)
		(list northeast 4 18)
	)
 (put (mk-ladder-down 'p_road_to_absalot_1 9 9) 9 1)
 (put (mk-joel) 9 10)
 (put (mk-npc 'bull 4) 12 12)
 (put (mk-npc 'bull 4) 10 15)
 )

(mk-place-music p_gate_to_absalot 'ml-small-town)

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
	(list
		(list southwest 11 0)
		(list southeast 2 0)
	)
 (put (mk-ladder-down 'p_road_to_absalot_3 9 7) 9 9)
 (put (spawn-pt 'death-knight) 6 7)
 (put (spawn-pt 'ghast) 12 11)
 )

(mk-place-music p_tower_of_absalot 'ml-outdoor-adventure)
 