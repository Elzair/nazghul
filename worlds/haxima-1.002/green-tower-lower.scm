(kern-mk-map 
 'm_green_tower_lower 22 38 pal_expanded
 (list
		"rn r8 r8 rn r8 r8 r8 rn r8 r8 rn rn rn rn rn rr rr rr rr rr rn rn "
		"r4 cc cc rr cc cc cc rr cc cc r2 rn rn rn rr rr .. .. .. rr rr rn "
		"r4 cc cc cc cc cc cc cc cc cc r2 rn rn rr rr .. .. .. .. .. rr rr "
		"r4 cc cc rr cc cc cc rr cc cc r2 rn rn rr .. .. .. rr .. .. .. rr "
		"rn rr rr rr cc cc cc rr rr rr rn rn rn rr rr .. rr rr rr .. rr rr "
		"r4 cc cc rr cc cc cc rr cc cc r2 rn rn rr .. .. .. rr .. .. .. rr "
		"r4 cc cc cc cc cc cc cc cc cc r2 rn rn rr rr .. rr rr rr .. rr rr "
		"r4 cc cc rr cc cc cc rr cc cc r2 rn rn rr .. .. .. rr .. .. .. rr "
		"rn rr rr rr cc cc cc rr rr rr rn rn rn rr rr .. rr rr rr .. rr rr "
		"r4 cc cc rr cc cc cc rr cc cc r2 rn rn rr .. .. .. rr .. .. .. rr "
		"r4 cc cc cc cc cc cc cc cc cc r2 rn rn rr rr .. rr rr rr .. rr rr "
		"r4 cc cc rr cc cc cc rr cc cc r2 rn rn rr .. .. .. rr .. .. .. rr "
		"rn rr rr rr cc cc cc rr rr rr rr rr rn rr rr .. rr rr rr .. rr rr "
		"rn rr rr rr cc cc cc cc cc cc cc rr rr rr .. .. .. rr .. .. .. rr "
		"rr rr cc cc cc cc cc cc cc cc cc cc rr rr rr .. .. .. .. .. rr rr "
		"rr cc cc cc rr cc cc cc cc cc cc cc rr .. .. .. .. .. .. .. .. rr "
		"rr cc && cc rr rr rr rr xx x! cc x! xx .. rr ,C ,R ,Y ,P ,T rr rr "
		"xx xx xx xx xx xx xx xx xx cc cc cc xx ?? xx xx xx xx xx xx xx rr "
		"xx xx ,T ,A ,L ,O ,S xx xx cc cc cc xx cc cc _! x! _! cc cc xx rr "
		"xx x! cc cc cc cc cc x! xx cc cc cc xx cc cc -- _! -- cc cc xx rr "
		"xx cc cc cc cc cc cc cc xx cc cc cc cc cc cc -- -- -- cc cc xx rr "
		"xx cc cc cc aa cc cc cc xx cc cc cc xx cc cc cc cc cc cc cc xx rr "
		"xx cc cc cc cc cc cc cc xx cc cc cc xx cc cc cc cc cc cc cc xx rr "
		"xx x! cc cc cc cc cc x! xx cc cc cc xx ,C ,I ,S ,T ,E ,R ,N xx rr "
		"xx xx xx xx cc xx xx xx x! cc cc cc x! xx xx xx xx xx xx xx xx xx "
		"xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx "
		"xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx "
		"xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx "
		"xx xx xx xx cc xx xx xx x! cc cc cc x! xx xx xx cc xx xx xx xx xx "
		"xx cc cc cc cc cc cc cc xx cc cc cc xx cc cc cc cc cc cc cc xx rn "
		"xx cc cc cc cc cc cc cc xx cc cc cc xx cc cc cc cc cc cc cc ?? rn "
		"xx xx cc cc cc x! cc cc xx cc cc cc xx cc cc x! cc cc cc xx xx rn "
		"rn xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx rn rn "
		"rn xx xx xx cc cc cc cc xx cc cc cc xx cc cc cc cc xx xx xx rn rn "
		"rn rn rn xx xx xx cc cc xx cc cc cc xx cc cc xx xx xx rn rn rn rn "
		"rn rn rn rn rn xx xx xx xx cc cc cc xx xx xx xx rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn xx xx xx xx xx rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
  ))

(kern-mk-place 
 'p_green_tower_lower "Beneath Green Tower" nil 
 m_green_tower_lower 
 #f ;; is-wrapping?
 #t ;; is-underground?
 #f ;; is-wilderness? 
 #f  ;; is-tmp-combat-place?

 nil ;; subplaces

 ;; neighbors
 (list (list p_green_tower up))

 ;; objects
 (list

  (put (mk-monman) 0 0)
  (put (mk-ladder-up 'p_green_tower 32 32) 10 26)

  ;; cell doors
  (put (mk-locked-windowed-door) 3 2)
  (put (mk-locked-windowed-door) 3 6)
  (put (mk-locked-windowed-door) 3 10)
  (put (kern-tag 'kama-jail-door (mk-locked-windowed-door)) 7 2)
  (put (mk-locked-windowed-door) 7 6)
  (put (mk-locked-windowed-door) 7 10)

  ;; prisoners
  (put (mk-kama 'kama-jail-door) 8 2)

  ;; crypt
  (put (mk-ladder-down 'p_crypt 9 3) 17 1)
  (put (mk-corpse-with-loot) 14 3)
  (put (mk-corpse-with-loot) 14 5)
  (put (mk-corpse-with-loot) 14 11)
  (put (mk-corpse-with-loot) 14 13)
  (put (mk-corpse-with-loot) 16 5)
  (put (mk-corpse-with-loot) 16 9)
  (put (mk-corpse-with-loot) 16 11)
  (put (mk-corpse-with-loot) 16 13)
  (put (mk-corpse-with-loot) 18 5)
  (put (mk-corpse-with-loot) 18 7)
  (put (mk-corpse-with-loot) 18 9)
  (put (mk-corpse-with-loot) 18 13)
  (put (mk-corpse-with-loot) 20 3)
  (put (mk-corpse-with-loot) 20 5)
  (put (mk-corpse-with-loot) 20 9)
  (put (mk-corpse-with-loot) 20 11)

  (put (kern-tag 'gtl-portcullis-1 (mk-portcullis)) 10 16)

  ;; storage room doors
  (put (mk-windowed-door) 12 20)
  (put (mk-windowed-door) 4 24)
  (put (mk-windowed-door) 4 28)
  (put (mk-windowed-door) 8 32)  
  (put (mk-windowed-door) 12 32)
  (put (mk-windowed-door) 16 28)

  ;; monsters
  (put (spawn-pt 'rat) 15 2)
  (put (spawn-pt 'rat) 7 29)
  (put (spawn-pt 'rat) 2 32)
  (put (spawn-pt 'giant-spider) 13 29)
  (put (spawn-pt 'giant-spider) 19 29)

  ;; Jailer
  (put (mk-edward) 5 10)
  (put (mk-bed) 1 14)

  )


 ;; hooks
 (list 'on-entry-to-dungeon-room)
 nil ;; edge entrances
 )

(mk-place-music p_green_tower_lower 'ml-dungeon-town)
