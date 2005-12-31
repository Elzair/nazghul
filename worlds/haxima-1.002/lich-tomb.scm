(mk-dungeon-room
 'p_lich_tomb "Lich Tomb"
 (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx %% %% %% %% pp ,, ,, ,, cc ,, ,, ,, pp %% %% %% %% xx "
      "xx %% %% %% %% ,, ,, cc cc cc cc cc ,, ,, %% %% %% %% xx "
      "xx %% %% %% %% ,, cc cc cc cc cc cc cc ,, %% %% %% %% xx "
      "xx %% %% %% %% ,, ,, cc cc cc cc cc ,, ,, %% %% %% %% xx "
      "xx %% %% %% %% pp ,, ,, cc cc cc ,, ,, pp %% %% %% %% xx "
      "xx %% %% %% %% %% %% ,, cc cc cc ,, %% %% %% %% %% %% xx "
      "xx .. %% %% %% %% %% pp cc cc cc pp %% %% %% %% %% .. xx "
      "xx .. .. %% %% %% %% ,, cc cc cc ,, %% %% %% %% .. .. xx "
      "xx .. .. .. %% %% %% pp cc cc cc pp %% %% %% .. .. .. xx "
      "xx .. .. .. .. .. .. ,, cc cc cc ,, .. .. .. .. .. .. xx "
      "xx .. .. .. .. .. .. pp cc cc cc pp .. .. .. .. .. .. xx "
      "xx .. .. .. .. .. .. ,, cc cc cc ,, .. .. .. .. .. .. xx "
      "xx .. .. .. .. .. .. pp cc cc cc pp .. .. .. .. .. .. xx "
      "xx .. .. .. .. .. .. ,, cc cc cc ,, .. .. .. .. .. .. xx "
      "xx .. .. .. .. .. .. pp cc cc cc pp .. .. .. .. .. .. xx "
      "xx .. .. .. .. .. .. ,, cc cc cc ,, .. .. .. .. .. .. xx "
      "xx .. .. .. .. .. .. pp cc cc cc pp .. .. .. .. .. .. xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  )
 (put (mk-ladder-up 'p_crypt 9 10) 9 16)
 (put (kern-mk-obj F_sleep_perm 1) 8 1)
 (put (kern-mk-obj F_sleep_perm 1) 10 1)
 (put (mk-lich-king) 9 2)

 )
