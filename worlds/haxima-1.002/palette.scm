(kern-mk-palette 'pal_expanded
  (list
    ;; There are 110 entries in this palette
    ;; The widest glyph is 2 characters
    ;; 
    ;; Note: 
    ;; It is best if all glyphs in a palette are of the same width,
    ;; so all glyphs in this palette should be 2 characters wide, 
    ;; like "xx" rather than "x".
    ;; 
    ;; This constraint makes it easier to edit maps via search & replace,
    ;; and makes it easier to change the palette.
    ;; 
    (list  "xx"   t_wall)               ;; "wall"
    (list  "__"   t_deep)               ;; "deep water"
    (list  "_!"   t_sunlit_deep)               ;; "deep water"
    (list  "~*"   t_blendable_shoals)            ;; "shallow water"
	
    (list  "%%"   t_bog)                ;; "bog"
    (list  ".."   t_grass)              ;; "grass"
    (list  ".!"   t_sunlit_grass)              ;; "grass"
    (list  "tt"   t_trees)              ;; "trees"

    (list  "||"   t_forest)             ;; "forest"
    (list  "|."   t_forest_v)           ;; "forest"  (non-LOS-blocking)

    (list  "{{"   t_hills)              ;; "hills"

    (list  "^^"   t_mountains)          ;; "mountains"
    (list  "^."   t_mountains_v)        ;; "mountains" (non-LOS-blocking)
    (list  "^~"   t_fake_mountains)

    (list  ",,"   t_flagstones)         ;; "flagstones"
    (list  "~,"   t_inv_wall)
    (list  "d,"   t_doorway)
    (list  "cc"   t_cobblestone)        ;; "cobblestone"
    (list  "ee"   t_deck)               ;; "deck"
    (list  "oo"   t_mast)               ;; "mast"
    (list  "ff"   t_fire_terrain)       ;; "fire"
    (list  "!!"   t_lava)               ;; "lava"
    (list  "~!"   t_fake_lava)
    (list  "!_"   t_deep_lava)
    (list  "&&"   t_fireplace)          ;; "fireplace"

    (list  "x."   t_wall_v)             ;; "wall"  (non-LOS-blocking)
    (list  "~x"   t_fake_wall)

    (list  "**"   t_stars)              ;; "stars"
	(list  "*."   t_void)
    (list  "??"   t_secret_door)        ;; "secret door"
    (list  "pp"   t_pillar)             ;; "pillar"
    (list  "~p"   t_false_pillar)
    (list  "bb"   t_boulder)            ;; "boulder"

    (list  "rr"   t_wall_rock)          ;; "rock wall"
    (list  "r."   t_wall_rock_v)        ;; "rock wall"  (non-LOS-blocking)
    (list  "~r"   t_fake_wall_rock)     ;; "rock wall"  (fake)

    (list  "WW"   t_ships_wheel)        ;; "ship's wheel"
    (list  "x!"   t_wall_torch)         ;; "wall torch"
    (list  "##"   t_ship_hull)          ;; "ship's hull"
    (list  "#>"   t_ship_hull2)          ;; "ship's hull (LOS-blocking)"

    (list  ".A"   t_a)                  ;; "an A"
    (list  ".B"   t_b)                  ;; "a B"
    (list  ".C"   t_c)                  ;; "a C"
    (list  ".D"   t_d)                  ;; "a D"
    (list  ".E"   t_e)                  ;; "an E"
    (list  ".F"   t_f)                  ;; "an F"
    (list  ".G"   t_g)                  ;; "a G"
    (list  ".H"   t_h)                  ;; "an H"
    (list  ".I"   t_i)                  ;; "an I"
    (list  ".J"   t_j)                  ;; "a J"
    (list  ".K"   t_k)                  ;; "a K"
    (list  ".L"   t_l)                  ;; "an L"
    (list  ".M"   t_m)                  ;; "an M"
    (list  ".N"   t_n)                  ;; "an N"
    (list  ".O"   t_o)                  ;; "an O"
    (list  "~O"   t_fake_o)
    (list  ".P"   t_p)                  ;; "a P"
    (list  ".Q"   t_q)                  ;; "a Q"
    (list  ".R"   t_r)                  ;; "an R"
    (list  ".S"   t_s)                  ;; "an S"
    (list  ".T"   t_t)                  ;; "a T"
    (list  ".U"   t_u)                  ;; "a U"
    (list  ".V"   t_v)                  ;; "a V"
    (list  ".W"   t_w)                  ;; "a W"
    (list  ".X"   t_x)                  ;; "an X"
    (list  ".Y"   t_y)                  ;; "a Y"
    (list  ".Z"   t_z)                  ;; "a Z"

    (list  ",A"   t_rune_a)             ;; "a rune"
    (list  ",B"   t_rune_b)             ;; "a rune"
    (list  ",C"   t_rune_c)             ;; "a rune"
    (list  ",D"   t_rune_d)             ;; "a rune"
    (list  ",E"   t_rune_e)             ;; "a rune"
    (list  ",F"   t_rune_f)             ;; "a rune"
    (list  ",G"   t_rune_g)             ;; "a rune"
    (list  ",H"   t_rune_h)             ;; "a rune"
    (list  ",I"   t_rune_i)             ;; "a rune"
    (list  ",J"   t_rune_j)             ;; "a rune"
    (list  ",K"   t_rune_k)             ;; "a rune"
    (list  ",L"   t_rune_l)             ;; "a rune"
    (list  ",M"   t_rune_m)             ;; "a rune"
    (list  ",N"   t_rune_n)             ;; "a rune"
    (list  ",O"   t_rune_o)             ;; "a rune"
    (list  ",P"   t_rune_p)             ;; "a rune"
    (list  ",Q"   t_rune_q)             ;; "a rune"
    (list  ",R"   t_rune_r)             ;; "a rune"
    (list  ",S"   t_rune_s)             ;; "a rune"
    (list  ",T"   t_rune_t)             ;; "a rune"
    (list  ",U"   t_rune_u)             ;; "a rune"
    (list  ",V"   t_rune_v)             ;; "a rune"
    (list  ",W"   t_rune_w)             ;; "a rune"
    (list  ",X"   t_rune_x)             ;; "a rune"
    (list  ",Y"   t_rune_y)             ;; "a rune"
    (list  ",Z"   t_rune_z)             ;; "a rune"
    (list  ";T"   t_rune_th)            ;; "a rune"
    (list  ";E"   t_rune_ee)            ;; "a rune"
    (list  ";N"   t_rune_ng)            ;; "a rune"
    (list  ";A"   t_rune_ea)            ;; "a rune"
    (list  ";S"   t_rune_st)            ;; "a rune"
    (list  ";D"   t_rune_dot)           ;; "a rune"

    (list  "@@"   t_counter_2x1_c)      ;; "counter"
    (list  "[["   t_counter_2x1_w)      ;; "counter"
    (list  "]]"   t_counter_2x1_e)      ;; "counter"
    (list  "00"   t_counter_1x1)        ;; "counter"

    (list  "++"   t_ankh)               ;; "ankh"
    (list  "+s"   t_statue)               ;; "ankh"
    (list  "aa"   t_altar)              ;; "altar"
    (list  "ar"   t_rune_altar)              ;; "altar"
    (list  "a!"   t_active_altar)              ;; "altar"
    (list  "<<"   t_leftwing)           ;; "castle wall"
    (list  ">>"   t_rightwing)          ;; "castle wall"
    (list  "w+"   t_arrow_slit)         ;; "arrow slit"
    (list  "ws"   t_window_in_stone)    ;; "window"

    (list  "=="   t_bridge_WE)          ;; "east-west bridge"
    (list  "=|"   t_bridge_NS)          ;; "east-west bridge"
    (list  "=!"   t_lava_bridge_NS)
    (list  "vv"   t_chasm)              ;; "chasm"

    (list "sE" t_equip_sign)
    (list "sA" t_weapon_sign)
    (list "sH" t_healer_sign)
    (list "sT" t_tavern_sign)
    (list "sI" t_inn_sign)
    (list "sP" t_alchemy_sign)
    (list "sR" t_magic_sign)
	
	;; blended terrains (mostly terrain + corner of something else)
	
	(list  "/0"   t_trail_0)            ;; "trail"
    (list  "/1"   t_trail_1)            ;; "trail"
    (list  "/2"   t_trail_2)            ;; "trail"
    (list  "/3"   t_trail_3)            ;; "trail"
    (list  "/4"   t_trail_4)            ;; "trail"
    (list  "/5"   t_trail_5)            ;; "trail"
    (list  "/6"   t_trail_6)            ;; "trail"
    (list  "/7"   t_trail_7)            ;; "trail"
    (list  "/8"   t_trail_8)            ;; "trail"
    (list  "/9"   t_trail_9)            ;; "trail"
    (list  "/a"   t_trail_a)            ;; "trail"
    (list  "/b"   t_trail_b)            ;; "trail"
    (list  "/c"   t_trail_c)            ;; "trail"
    (list  "/d"   t_trail_d)            ;; "trail"
    (list  "/e"   t_trail_e)            ;; "trail"
    (list  "/f"   t_trail_f)            ;; "trail"
	
	(list  "~~" t_shoals)     ;; shallow + land
    (list  "~1" t_shore_n)
    (list  "~2" t_shore_w)
    (list  "~3" t_shore_nw)
    (list  "~4" t_shore_e)
    (list  "~5" t_shore_ne)
    (list  "~6" t_shore_we)
    (list  "~7" t_shore_nwe)
    (list  "~8" t_shore_s)
    (list  "~9" t_shore_ns)
    (list  "~a" t_shore_ws)
    (list  "~b" t_shore_nws)
    (list  "~c" t_shore_es)
    (list  "~d" t_shore_nes)
    (list  "~e" t_shore_wes)
    (list  "~f" t_shore_c)
	
    (list  "--" t_shallow)            ;; water + land
    (list  "-1" t_wshore_n)
    (list  "-2" t_wshore_w)
    (list  "-3" t_wshore_nw)
    (list  "-4" t_wshore_e)
    (list  "-5" t_wshore_ne)
    (list  "-6" t_wshore_we)
    (list  "-7" t_wshore_nwe)
    (list  "-8" t_wshore_s)
    (list  "-9" t_wshore_ns)
    (list  "-a" t_wshore_ws)
    (list  "-b" t_wshore_nws)
    (list  "-c" t_wshore_es)
    (list  "-d" t_wshore_nes)
    (list  "-e" t_wshore_wes)
    (list  "-f" t_wshore_c)
	
	(list  "_1" t_dshore_n)        ;; deep water + land
    (list  "_2" t_dshore_w)
    (list  "_3" t_dshore_nw)
    (list  "_4" t_dshore_e)
    (list  "_5" t_dshore_ne)
    (list  "_6" t_dshore_we)
    (list  "_7" t_dshore_nwe)
    (list  "_8" t_dshore_s)
    (list  "_9" t_dshore_ns)
    (list  "_a" t_dshore_ws)
    (list  "_b" t_dshore_nws)
    (list  "_c" t_dshore_es)
    (list  "_d" t_dshore_nes)
    (list  "_e" t_dshore_wes)
    (list  "_f" t_dshore_c)
	
	(list  "*1" t_voids_n)             ;; void + land
    (list  "*2" t_voids_w)
    (list  "*3" t_voids_nw)
    (list  "*4" t_voids_e)
    (list  "*5" t_voids_ne)
    (list  "*6" t_voids_we)
    (list  "*7" t_voids_nwe)
    (list  "*8" t_voids_s)
    (list  "*9" t_voids_ns)
    (list  "*a" t_voids_ws)
    (list  "*b" t_voids_nws)
    (list  "*c" t_voids_es)
    (list  "*d" t_voids_nes)
    (list  "*e" t_voids_wes)
    (list  "*f" t_voids_c)
	
	(list  "{1" t_hilledge_n)          ;; grass + hills
    (list  "{2" t_hilledge_w)
    (list  "{3" t_hilledge_nw)
    (list  "{4" t_hilledge_e)
    (list  "{5" t_hilledge_ne)
    (list  "{6" t_hilledge_we)
    (list  "{7" t_hilledge_nwe)
    (list  "{8" t_hilledge_s)
    (list  "{9" t_hilledge_ns)
    (list  "{a" t_hilledge_ws)
    (list  "{b" t_hilledge_nws)
    (list  "{c" t_hilledge_es)
    (list  "{d" t_hilledge_nes)
    (list  "{e" t_hilledge_wes)
    (list  "{f" t_hilledge_c)
	
    (list  "%3" t_bog_nw)              ;; bog + land
    (list  "%5" t_bog_ne)
    (list  "%7" t_bog_nwe)
    (list  "%a" t_bog_ws)
    (list  "%b" t_bog_nws)
    (list  "%c" t_bog_es)
    (list  "%d" t_bog_nes)
    (list  "%e" t_bog_wes)
    (list  "%f" t_bog_c)

    (list  "t3" t_trees_nw)               ;; trees + grass
    (list  "t5" t_trees_ne)
    (list  "t7" t_trees_nwe)
    (list  "ta" t_trees_ws)
    (list  "tb" t_trees_nws)
    (list  "tc" t_trees_es)
    (list  "td" t_trees_nes)
    (list  "te" t_trees_wes)
    (list  "tf" t_trees_c)

	(list  "t#" t_grasst_nw)             ;; grass + trees
    (list  "t%" t_grasst_ne)
    (list  "t&" t_grasst_nwe)
    (list  "tA" t_grasst_ws)
    (list  "tB" t_grasst_nws)
    (list  "tC" t_grasst_es)
    (list  "tD" t_grasst_nes)
    (list  "tE" t_grasst_wes)
    (list  "tF" t_grasst_c)
	
	(list  "~#" t_grassw_nw)           ;; grass + water
    (list  "~%" t_grassw_ne)
    (list  "~&" t_grassw_nwe)
    (list  "~A" t_grassw_ws)
    (list  "~B" t_grassw_nws)
    (list  "~C" t_grassw_es)
    (list  "~D" t_grassw_nes)
    (list  "~E" t_grassw_wes)
    (list  "~F" t_grassw_c)
	
	(list  "{#" t_hilli_nw)             ;; hills + grass
    (list  "{%" t_hilli_ne)
    (list  "{&" t_hilli_nwe)
    (list  "{A" t_hilli_ws)
    (list  "{B" t_hilli_nws)
    (list  "{C" t_hilli_es)
    (list  "{D" t_hilli_nes)
    (list  "{E" t_hilli_wes)
    (list  "{F" t_hilli_c)
	
	(list  "|#" t_forestg_nw)          ;; forest + grass
    (list  "|%" t_forestg_ne)
    (list  "|&" t_forestg_nwe)
    (list  "|A" t_forestg_ws)
    (list  "|B" t_forestg_nws)
    (list  "|C" t_forestg_es)
    (list  "|D" t_forestg_nes)
    (list  "|E" t_forestg_wes)

	(list  "tG" t_treew_nw)            ;; trees + water
    (list  "tH" t_treew_ne)
    (list  "tI" t_treew_nwe)
    (list  "tJ" t_treew_ws)
    (list  "tK" t_treew_nws)
    (list  "tL" t_treew_es)
    (list  "tM" t_treew_nes)
    (list  "tN" t_treew_wes)
    (list  "tO" t_treew_c)
	
	(list  "{G" t_hillw_nw)            ;; hills + water
    (list  "{H" t_hillw_ne)
    (list  "{I" t_hillw_nwe)
    (list  "{J" t_hillw_ws)
    (list  "{K" t_hillw_nws)
    (list  "{L" t_hillw_es)
    (list  "{M" t_hillw_nes)
    (list  "{N" t_hillw_wes)
    (list  "{O" t_hillw_c)	
	
	(list  "{g" t_hillv_nw)           ;; hills + void
    (list  "{h" t_hillv_ne)
    (list  "{i" t_hillv_nwe)
    (list  "{j" t_hillv_ws)
    (list  "{k" t_hillv_nws)
    (list  "{l" t_hillv_es)
    (list  "{m" t_hillv_nes)
    (list  "{n" t_hillv_wes)	
	
	(list  ".g" t_grassv_nw)          ;; grass + void
    (list  ".h" t_grassv_ne)
    (list  ".i" t_grassv_nwe)
    (list  ".j" t_grassv_ws)
    (list  ".k" t_grassv_nws)
    (list  ".l" t_grassv_es)
    (list  ".m" t_grassv_nes)
    (list  ".n" t_grassv_wes)
	
	(list  "^g" t_mountv_nw)        ;; mounts + void
    (list  "^h" t_mountv_ne)
    (list  "^i" t_mountv_nwe)
    (list  "^j" t_mountv_ws)
    (list  "^k" t_mountv_nws)
    (list  "^l" t_mountv_es)
    (list  "^m" t_mountv_nes)
    (list  "^n" t_mountv_wes)	
	
	(list  "^3" t_mountg_nw)     ;; mounts + grass
    (list  "^5" t_mountg_ne)
    (list  "^7" t_mountg_nwe)
    (list  "^a" t_mountg_ws)
    (list  "^b" t_mountg_nws)
    (list  "^c" t_mountg_es)
    (list  "^d" t_mountg_nes)
    (list  "^e" t_mountg_wes)
    (list  "^f" t_mountg_c)	
	
	(list  "^G" t_mountw_nw)        ;; mounts + water
    (list  "^H" t_mountw_ne) 
    (list  "^I" t_mountw_nwe)
    (list  "^J" t_mountw_ws)
    (list  "^K" t_mountw_nws)
    (list  "^L" t_mountw_es)
    (list  "^M" t_mountw_nes)
    (list  "^N" t_mountw_wes)
    (list  "^O" t_mountw_c)	
	
    (list  "!3" t_lava_nw)        ;; lava + land
    (list  "!5" t_lava_ne)
    (list  "!6" t_lava_we)
    (list  "!7" t_lava_nwe)
    (list  "!a" t_lava_ws)
    (list  "!b" t_lava_nws)
    (list  "!c" t_lava_es)
    (list  "!d" t_lava_nes)
    (list  "!e" t_lava_wes)
    (list  "!f" t_lava_c)
    
  )
) ;; palette pal_expanded

