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
    (list  "__"   t_deep)               ;; "deep water"
    (list  "--"   t_shallow)            ;; "shallow water"
    (list  "~~"   t_shoals)             ;; "shoals"
    (list  "%%"   t_bog)                ;; "bog"
    (list  ".."   t_grass)              ;; "grass"
    (list  "tt"   t_trees)              ;; "trees"

    (list  "||"   t_forest)             ;; "forest"
    (list  "|."   t_forest_v)           ;; "forest"  (non-LOS-blocking)

    (list  "{{"   t_hills)              ;; "hills"

    (list  "^^"   t_mountains)          ;; "mountains"
    (list  "^."   t_mountains_v)        ;; "mountains" (non-LOS-blocking)

    (list  ",,"   t_flagstones)         ;; "flagstones"
    (list  "cc"   t_cobblestone)        ;; "cobblestone"
    (list  "ee"   t_deck)               ;; "deck"
    (list  "oo"   t_mast)               ;; "mast"
    (list  "ff"   t_fire_terrain)       ;; "fire"
    (list  "!!"   t_lava)               ;; "lava"
    (list  "&&"   t_fireplace)          ;; "fireplace"

    (list  "xx"   t_wall)               ;; "wall"
    (list  "x."   t_wall_v)             ;; "wall"  (non-LOS-blocking)

    (list  "??"   t_secret_door)        ;; "secret door"
    (list  "pp"   t_pillar)             ;; "pillar"
    (list  "bb"   t_boulder)            ;; "boulder"

    (list  "rr"   t_wall_rock)          ;; "rock wall"
    (list  "r."   t_wall_rock_v)        ;; "rock wall"  (non-LOS-blocking)

    (list  "WW"   t_ships_wheel)        ;; "ship's wheel"
    (list  "x!"   t_wall_torch)         ;; "wall torch"
    (list  "##"   t_ship_hull)          ;; "ship's hull"

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
    (list  "aa"   t_altar)              ;; "altar"
    (list  "<<"   t_leftwing)           ;; "castle wall"
    (list  ">>"   t_rightwing)          ;; "castle wall"
    (list  "dd"   t_red_roses)          ;; "roses"
    (list  "w+"   t_arrow_slit)         ;; "arrow slit"
    (list  "ws"   t_window_in_stone)    ;; "window"

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
  )
) ;; palette pal_expanded

