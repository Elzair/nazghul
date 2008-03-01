(mk-dungeon-room
 'p_forsaken_prison "Forsaken Prison"
 (list
	  "rn rn rn rn xx xx xx xx xx xx xx xx xx xx xx rn rn rn rn "
	  "rn rn rn rn xx ,, ,, xx ,, ,, ,, xx ,, ,, xx rn rn rn rn "
	  "rn rn rn rn xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn rn rn rn "
	  "rn rn rn rn xx ,, ,, xx ,, ,, ,, xx ,, ,, xx rn rn rn rn "
	  "rn rn rn xx xx xx xx xx ,, ,, ,, xx xx xx xx xx rn rn rn "
	  "rn rn rn xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx rn rn rn "
	  "rn rn rn xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx rn rn rn "
	  "xx xx xx xx xx ,, xx xx ,, ,, ,, xx xx ,, xx xx xx xx xx "
	  "xx ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, xx "
	  "xx xx xx xx xx ,, xx xx ,, ,, ,, xx xx ,, xx xx xx xx xx "
	  "rn xx xx xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx rn rn rn "
	  "rn xx ?? ?? ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx rn rn rn "
	  "xx xx ?? xx xx xx xx xx ,, ,, ,, xx xx xx xx xx rn rn rn "
	  "xx ,, ,, ,, xx ,, ,, xx ,, ,, ,, xx ,, ,, xx rn rn rn rn "
	  "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn rn rn rn "
	  "xx ,, ,, ,, xx ,, ,, xx ,, ,, ,, xx ,, ,, xx rn rn rn rn "
	  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx rn rn rn rn "
  )
 (put (mk-ladder-up 'p_paladins_hold 8 17) 9 9)
 (put (mk-ladder-up 'p_mans_hideout 9 3) 2 16)
 (put (mk-locked-door) 7 2)
 (put (mk-locked-door) 11 2)
 (put (mk-locked-door) 13 7)
 (put (mk-locked-door) 15 9)
 (put (mk-locked-door) 13 11)
 (put (mk-locked-door) 11 16)
 (put (mk-locked-door) 7 16)
 (put (mk-locked-door) 5 11)
 (put (mk-locked-door) 3 9)
 (put (mk-locked-door) 5 7)

 ;; prisoners
 (put (mk-npc 'bandit 4) 5 2)
 (put (mk-npc 'cave-goblin-berserker 4) 6 15)
 (put (mk-npc 'troll 6) 2 9)
 (put (mk-npc 'warlock 8) 13 16)
 (put (mk-npc 'corrupt-halberdier 3) 17 9)
 (put (mk-npc 'skeletal-warrior 5) 13 1)

 ;; alas, expired prisoners
 (put (mk-corpse-with-loot) 14 5)
 (put (mk-corpse-with-loot) 12 12)
 (put (mk-corpse-with-loot) 14 5)
 (put (mk-corpse-with-loot) 5 6)

 )

(mk-place-music p_forsaken_prison 'ml-dungeon-adventure)
