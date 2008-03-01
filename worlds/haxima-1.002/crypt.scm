(kern-load "keep_crypt_mech.scm")

(mk-dungeon-room
 'p_crypt "Crypt"
 (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx ,, ,, ,, xx x! ,, ,, ,, x! xx ,, ,, ,, xx ,, xx "
      "xx xx xx ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, xx ,, xx "
      "xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, xx "
      "xx xx xx xx xx xx xx ,, ,, ,, ,, ,, xx xx xx xx xx ?? xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx xx ,, xx xx xx .C .R .Y .P .T xx xx xx ,, xx xx xx "
      "xx .. .. ,, .. .. xx xx xx x! xx xx xx .. .. ,, .. .. xx "
      "xx .. .. ,, .. .. xx xx xx ,, xx xx xx .. .. ,, .. .. xx "
      "xx .. .. ,, .. .. xx xx ,, ,, ,, xx xx .. .. ,, .. .. xx "
      "xx .. .. ,, .. .. xx xx ,, ,, ,, xx xx .. .. ,, .. .. xx "
      "xx .. .. ,, .. .. x! xx xx ,, xx xx x! .. .. ,, .. .. xx "
      "xx .. .. ,, .. .. xx ,, ,, ,, ,, ,, xx .. .. ,, .. .. xx "
      "xx .. .. ,, .. .. xx ,, ,, ,, ,, ,, xx .. .. ,, .. .. xx "
      "xx .. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, .. .. xx "
      "xx .. .. .. .. .. xx ,, ,, ,, ,, ,, xx .. .. .. .. .. xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  )
 ;; special mechanisms for turning corpses into skeletal warriors
 (put (kern-tag 'kc_kcma (mk-kcm (mk-rect 1 9 5 9))) 0 0)
 (put (kern-tag 'kc_kcmb (mk-kcm (mk-rect 13 9 5 9))) 0 0)

 (put (mk-ladder-up 'p_green_tower_lower 17 1) 9 3)
 (put (mk-ladder-down 'p_lich_tomb 9 16) 9 10)
 
 ;; mundane mechs
 (put (mk-locked-door) 6 3)
 (put (mk-magic-locked-door) 12 3)
 (put (mk-door) 6 7)
 (put (mk-door) 12 7)
 (put (mk-windowed-door) 15 8)
 (put (mk-windowed-door) 3 8)
 (put (kern-tag 'kc_pa (mk-connected-portcullis 'kc_kcma)) 6 16)
 (put (kern-tag 'kc_pb (mk-connected-portcullis 'kc_kcmb)) 12 16)
 (put (kern-tag 'kc_pc (mk-portcullis)) 9 13)
 (put (mk-lever 'kc_pb) 4 2)
 (put (mk-lever 'kc_pa) 14 2)
 (put (mk-lever 'kc_pc) 17 1)
 
 ;; corpses
 (put (mk-corpse) 1  9)
 (put (mk-corpse) 2  9)
 (put (mk-corpse) 4  9)
 (put (mk-corpse) 5  9)
 (put (mk-corpse) 13 9)
 (put (mk-corpse) 14 9)
 (put (mk-corpse) 16 9)
 (put (mk-corpse) 17 9)
 (put (mk-corpse) 1  10)
 (put (mk-corpse) 2  10)
 (put (mk-corpse) 4  10)
 (put (mk-corpse) 5  10)
 (put (mk-corpse) 13 10)
 (put (mk-corpse) 14 10)
 (put (mk-corpse) 16 10)
 (put (mk-corpse) 17 10)
 (put (mk-corpse) 1  11)
 (put (mk-corpse) 2  11)
 (put (mk-corpse) 4  11)
 (put (mk-corpse) 5  11)
 (put (mk-corpse) 13 11)
 (put (mk-corpse) 14 11)
 (put (mk-corpse) 16 11)
 (put (mk-corpse) 17 11)
 (put (mk-corpse) 1  12)
 (put (mk-corpse) 2  12)
 (put (mk-corpse) 4  12)
 (put (mk-corpse) 5  12)
 (put (mk-corpse) 13 12)
 (put (mk-corpse) 14 12)
 (put (mk-corpse) 16 12)
 (put (mk-corpse) 17 12)
 (put (mk-corpse) 1  13)
 (put (mk-corpse) 2  13)
 (put (mk-corpse) 4  13)
 (put (mk-corpse) 5  13)
 (put (mk-corpse) 13 13)
 (put (mk-corpse) 14 13)
 (put (mk-corpse) 16 13)
 (put (mk-corpse) 17 13)
 (put (mk-corpse) 1  14)
 (put (mk-corpse) 2  14)
 (put (mk-corpse) 4  14)
 (put (mk-corpse) 5  14)
 (put (mk-corpse) 13 14)
 (put (mk-corpse) 14 14)
 (put (mk-corpse) 16 14)
 (put (mk-corpse) 17 14)
 (put (mk-corpse) 1  15)
 (put (mk-corpse) 2  15)
 (put (mk-corpse) 4  15)
 (put (mk-corpse) 5  15)
 (put (mk-corpse) 13 15)
 (put (mk-corpse) 14 15)
 (put (mk-corpse) 16 15)
 (put (mk-corpse) 17 15)
 (put (mk-corpse) 1  16)
 (put (mk-corpse) 2  16)
 (put (mk-corpse) 16 16)
 (put (mk-corpse) 17 16)
 (put (mk-corpse) 1  17)
 (put (mk-corpse) 2  17)
 (put (mk-corpse) 3  17)
 (put (mk-corpse) 4  17)
 (put (mk-corpse) 5  17)
 (put (mk-corpse) 13 17)
 (put (mk-corpse) 14 17)
 (put (mk-corpse) 15 17)
 (put (mk-corpse) 16 17)
 (put (mk-corpse) 17 17)
 )

(mk-place-music p_crypt 'ml-creepy-area)
