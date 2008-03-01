(mk-dungeon-room
 'p_lich_tomb "Lich Tomb"
	(list
		"xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
		"xx %3 %% %% %5 pp ,, ,, ,, cc ,, ,, ,, pp %3 %% %% %5 xx "
		"xx %% %% %% %% ,, ,, cc cc cc cc cc ,, ,, %% %% %% %% xx "
		"xx %% %% %% %% ,, cc cc cc cc cc cc cc ,, %% %% %% %% xx "
		"xx %% %% %% %% ,, ,, cc cc cc cc cc ,, ,, %% %% %% %% xx "
		"xx %% %% %% %% pp ,, ,, cc cc cc ,, ,, pp %% %% %% %% xx "
		"xx %a %% %% %% %% %% ,, cc cc cc ,, %% %% %% %% %% %c xx "
		"xx .. %a %% %% %% %% pp cc cc cc pp %% %% %% %% %c .. xx "
		"xx .. .. %a %% %% %% ,, cc cc cc ,, %% %% %% %c .. .. xx "
		"xx .. .. .. %a %% %c pp cc cc cc pp %a %% %c .. .. .. xx "
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

(mk-place-music p_lich_tomb 'ml-creepy-area)
