(mk-dungeon-room
 'p_fire_sea "Fire Sea"
	(list
		"rn r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 rn rn rn "
		"r4 !_ !_ !! {A {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ ra rn rn "
		"r4 !_ !_ !_ !5 {A {{ {{ {3 {9 {9 {9 {9 {5 {{ {C ~! ra rn "
		"r4 !! !_ !! !! !5 {{ {3 {c {{ {{ {{ {{ {6 {{ ~! !_ ~! r2 "
		"r4 {% !e {# {{ !! {{ {6 {{ {C !3 !5 {{ {6 {{ {% ~! {# r2 "
		"r4 {{ {{ {3 {9 == {9 {c {C !3 !_ !! {{ {6 {{ !7 {{ {{ r2 "
		"r4 {{ {3 {c {{ !! {{ {{ !_ !_ !_ !_ {{ {a {9 == {9 {1 r2 "
		"r4 {{ {6 {{ {{ !_ !_ !_ r3 r9 r9 r5 !5 {{ {{ !! {{ {2 r2 "
		"r4 {{ {6 {{ {{ {% !a !_ r6 .. .. ra r5 !! !! !c {{ {2 r2 "
		"r4 {{ {a {9 {9 {5 {{ r3 r4 .. .. .. re {{ !! {{ {{ {2 r2 "
		"r4 {{ {{ {{ {{ {6 {{ ra r4 .. bb .. .. {9 == {9 {9 {8 r2 "
		"r4 {C !3 !! !! =| !! !_ r6 .. .. .. r7 {{ !! {{ {{ {{ r2 "
		"r4 !! !_ !c {{ {6 {{ !_ ra r9 r5 .. r2 r5 !_ !! !5 {A r2 "
		"r4 {% !e {# {{ {6 {{ !! !_ !_ ra r9 r8 rc !_ !_ !_ !! r2 "
		"rn r5 {{ {{ {{ {6 {{ !a !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ r2 "
		"rn rn r5 {{ {{ {6 {{ {% !a !! !! !_ !_ !_ !_ !_ !_ !_ r2 "
		"rn rn r4 bb {{ {a {5 {{ {{ {{ {% !a !! !! !_ !_ !_ !_ r2 "
		"rn rn rn r5 bb {{ {2 {1 {1 {5 {{ {{ {{ {% !a !_ !_ !_ r2 "
		"rn rn rn rn r1 r1 r1 r5 .. .. {5 {{ {{ {3 bb !! !! ~! r2 "
	)

 (put (kern-mk-obj t_rune_w 1) 11 12)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 8)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 10 8)
 (put (kern-mk-obj t_iron_helm_4 1) 10 8)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 9)
 (put (kern-mk-obj t_spell_book_enchantment_miscellanea 1) 9 10)
 (put (kern-mk-obj t_spell_book_illusion_2 1) 9 11)
 (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 10 9)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 9)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 10)
 (put (kern-mk-obj t_sword_4 1) 9 10)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 10)
 (put (kern-mk-obj t_armor_plate_4 1) 11 11)
 (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 9 11)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 10 11)
 (put (kern-mk-obj t_shield_4 1) 10 11)
 (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 11 11)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 12)

 (put (spawn-pt 'dragon) 11 10)
 )

(mk-place-music p_fire_sea 'ml-dungeon-adventure)

(mk-dungeon-room
 'p_smoldering_cave "Smoldering Cave"
	(list
		"rn rn rn rn rn rn rn r4 .. .. .. r2 rn rn rn rn rn rn rn "
		"rn rn rn rn rn r8 r8 r4 {8 .. {8 r2 r8 r8 rn rn rn rn rn "
		"rn rn rn rn rc !_ !_ re {{ {6 {{ re !_ !_ ra rn rn rn rn "
		"rn rn rn rc !_ !_ !_ !! {{ {6 {{ !! !_ !_ !_ ra rn rn rn "
		"rn rn r4 !_ !_ !_ !_ !_ !! =| !! !_ !_ !_ !_ !_ r2 rn rn "
		"rn rn rc !_ !_ !_ !_ !c {{ {6 {{ !a !_ !_ !_ !_ ra rn rn "
		"rn r4 !_ !_ !_ !_ !c bb {{ {6 {{ bb !a !_ !_ !_ !_ r2 rn "
		"rn r4 !_ !_ !_ !! bb {# {{ {6 {{ {% bb !! !_ !_ !_ r2 rn "
		"rn r4 !_ !_ !_ !! {# {{ {3 .. {5 {{ {% !! !_ !_ !_ r2 rn "
		"rn r4 !_ !_ !_ !! {{ {{ {2 .. {4 {{ {{ !! !_ !_ !_ r2 rn "
		"rn r4 !_ !_ !_ !! {A {{ {a {8 {c {{ {C !! !_ !_ !_ r2 rn "
		"rn r4 !_ !_ !_ !! bb {A {{ {{ {{ {C bb !! !_ !_ !_ r2 rn "
		"rn r4 !_ !_ !_ !_ !5 bb {A {{ {C bb !3 !_ !_ !_ !_ r2 rn "
		"rn rn r5 !_ !_ !_ !_ !! !! !! !! !! !_ !_ !_ !_ r3 rn rn "
		"rn rn r4 !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ r2 rn rn "
		"rn rn rn r5 !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ r3 rn rn rn "
		"rn rn rn rn r1 r5 !_ !_ !_ !_ !_ !_ !_ r3 r1 rn rn rn rn "
		"rn rn rn rn rn rn r1 r1 r1 r1 r1 r1 r1 rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
	)
 (put (mk-ladder-up 'p_shard 118 46) 9 9)
 (put (spawn-pt 'fire-slime) 9 1)
 (put (spawn-pt 'fire-slime) 7 3)
 (put (spawn-pt 'fire-slime) 10 4)

 ;; Asbestos spellbook, floating in the lava...
 (put (kern-mk-obj t_spell_book_force_magick_winds 1) 2 11)
 )

(mk-dungeon-level 
 (list p_fire_sea)
 (list p_smoldering_cave)
 )

(mk-place-music p_smoldering_cave 'ml-dungeon-adventure)
