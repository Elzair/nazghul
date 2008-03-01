;; Aka, reagent cave -- generate rare reagents here
(mk-dungeon-room
 'p_dank_cave "Dank Cave"
	(list
		"rn rn rn rn rn rn rn rn r8 r8 r8 rn rn rn rn rn rn rn rn "
		"rn rn rn rn r8 rn rn r4 .. .. .. r2 rn rn r8 rn rn rn rn "
		"rn rn rn rc {{ ra rn rc {8 .. {8 ra rn rc {{ ra rn rn rn "
		"rn rn rc {{ {{ {{ re {{ {{ {e {{ {{ re {{ {{ {{ ra rn rn "
		"rn rc {{ {C %f {A {{ {{ {{ {{ {{ {{ {{ {C .! {A {{ ra rn "
		"r4 {{ {{ %7 .! %3 %d {{ {{ r7 {{ {{ %b %% %% %5 {{ {{ r2 "
		"r4 {{ {{ %a %% %c {# {{ rb rn rd {{ {% %a %% %c {{ {{ r2 "
		"rn r5 {{ {% %e {# {{ {{ {{ re {{ {{ {{ {% %e {# {{ r3 rn "
		"rn rn r5 {{ {{ {{ r7 {{ {{ {{ {{ {{ r7 {{ {{ {{ r3 rn rn "
		"rn rn rn rd {{ rb rn rd {{ {{ {{ rb rn rd {{ rb rn rn rn "
		"rn rn rc {{ {{ {{ re {{ {{ {{ {{ {{ re {{ {{ {{ ra rn rn "
		"rn rc {{ {C %7 {A {{ {{ {{ r7 {{ {{ {{ {C %7 {A {{ ra rn "
		"r4 {{ {{ %3 %% %d {A {{ rb rn rd {{ {C %3 %% %5 {{ {{ r2 "
		"r4 {{ {{ %a %% .! %7 {A {{ re {{ {C %3 %% %% %c {{ {{ r2 "
		"rn r5 {{ {% %a %% %% %d {{ {{ {{ %b %% .! %e {# {{ r3 rn "
		"rn rn r5 {{ {% %a %c {# {{ r7 {{ {% %a %d {# {{ r3 rn rn "
		"rn rn rn r5 {{ {{ {{ {{ r3 rn r5 {{ {{ {{ {{ r3 rn rn rn "
		"rn rn rn rn r5 {{ {{ r3 rn rn rn r5 {{ {{ r3 rn rn rn rn "
		"rn rn rn rn rn r1 r1 rn rn rn rn rn r1 r1 rn rn rn rn rn "
	)

 ;; reagents
 (put (custom-pt 'grow-trig 'mandrake "1d5") 14 14)
 (put (custom-pt 'grow-trig 'nightshade "1d5") 6 14)
 (put (custom-pt 'grow-trig 'ginseng "1d5") 3 12)
 (put (custom-pt 'grow-trig 'ginseng "1d5") 3 5)
 (put (custom-pt 'grow-trig 'garlic "1d5") 15 5)
 (put (custom-pt 'grow-trig 'garlic "1d5") 3 13)
 (put (mk-ladder-up 'p_shamans_grove 14 11) 9 1)

 ;; monsters
 (put (spawn-pt 'gazer) 9 9)
 (put (spawn-pt 'headless) 8 7)
 (put (spawn-pt 'bat) 5 17)
 (put (spawn-pt 'rat) 4 2)
 (put (spawn-pt 'snake) 17 12)
 )

(mk-place-music p_dank_cave 'ml-dungeon-adventure)
