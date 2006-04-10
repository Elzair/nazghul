;; Aka, reagent cave -- generate rare reagents here
(mk-dungeon-room
 'p_dank_cave "Dank Cave"
	(list
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr "
		"rr rr rr rr {{ rr rr rr {8 .. {8 rr rr rr {{ rr rr rr rr "
		"rr rr rr {{ {{ {{ rr {{ {{ {e {{ {{ rr {{ {{ {{ rr rr rr "
		"rr rr {{ {C %f {A {{ {{ {{ {{ {{ {{ {{ {C .! {A {{ rr rr "
		"rr {{ {{ %7 .! %3 %d {{ {{ rr {{ {{ %b %% %% %5 {{ {{ rr "
		"rr {{ {{ %a %% %c {# {{ rr rr rr {{ {% %a %% %c {{ {{ rr "
		"rr rr {{ {% %e {# {{ {{ {{ rr {{ {{ {{ {% %e {# {{ rr rr "
		"rr rr rr {{ {{ {{ rr {{ {{ {{ {{ {{ rr {{ {{ {{ rr rr rr "
		"rr rr rr rr {{ rr rr rr {{ {{ {{ rr rr rr {{ rr rr rr rr "
		"rr rr rr {{ {{ {{ rr {{ {{ {{ {{ {{ rr {{ {{ {{ rr rr rr "
		"rr rr {{ {C %7 {A {{ {{ {{ rr {{ {{ {{ {C %7 {A {{ rr rr "
		"rr {{ {{ %3 %% %d {A {{ rr rr rr {{ {C %3 %% %5 {{ {{ rr "
		"rr {{ {{ %a %% .! %7 {A {{ rr {{ {C %3 %% %% %c {{ {{ rr "
		"rr rr {{ {% %a %% %% %d {{ {{ {{ %b %% .! %e {# {{ rr rr "
		"rr rr rr {{ {% %a %c {# {{ rr {{ {% %a %d {# {{ rr rr rr "
		"rr rr rr rr {{ {{ {{ {{ rr rr rr {{ {{ {{ {{ rr rr rr rr "
		"rr rr rr rr rr {{ {{ rr rr rr rr rr {{ {{ rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
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
