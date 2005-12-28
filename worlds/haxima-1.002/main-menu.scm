(kern-mk-sprite-set 'ss_u4_shapes        32 32   16 16   0 0   "shapes.png")
(kern-mk-sprite-set 'ss_frame            16 16    4  4   0 0   "frame.png")
(kern-mk-sprite-set 'ss_u4_charset       8  16    8 16   0 0   "charset.png")

(kern-mk-sprite 's_null           ss_u4_shapes 1 126 #f 0 )
(kern-mk-sprite 'ls_whirlpool     ss_u4_charset 4 28 #f 0 )

(kern-mk-sprite 's_frame_ulc   ss_frame 1  0 #f 0 )
(kern-mk-sprite 's_frame_td    ss_frame 1  1 #f 0 )
(kern-mk-sprite 's_frame_urc   ss_frame 1  2 #f 0 )
(kern-mk-sprite 's_frame_endu  ss_frame 1  3 #f 0 )  ; top of vertical bar, currently unused
(kern-mk-sprite 's_frame_tr    ss_frame 1  4 #f 0 )
(kern-mk-sprite 's_frame_plus  ss_frame 1  5 #f 0 )  ; center crosspiece, currently unused
(kern-mk-sprite 's_frame_tl    ss_frame 1  6 #f 0 )
(kern-mk-sprite 's_frame_vert  ss_frame 1  7 #f 0 )
(kern-mk-sprite 's_frame_llc   ss_frame 1  8 #f 0 )
(kern-mk-sprite 's_frame_tu    ss_frame 1  9 #f 0 )
(kern-mk-sprite 's_frame_lrc   ss_frame 1 10 #f 0 )
(kern-mk-sprite 's_frame_endb  ss_frame 1 11 #f 0 )  ; bottom of vertical bar, currently unused
(kern-mk-sprite 's_frame_endl  ss_frame 1 12 #f 0 )
(kern-mk-sprite 's_frame_horz  ss_frame 1 13 #f 0 )
(kern-mk-sprite 's_frame_endr  ss_frame 1 14 #f 0 )
(kern-mk-sprite 's_frame_dot   ss_frame 1 15 #f 0 )  ; disconnected disk, currently unused

(kern-set-frame s_frame_ulc
                s_frame_urc
                s_frame_llc
                s_frame_lrc
                s_frame_td
                s_frame_tu
                s_frame_tl
                s_frame_tr
                s_null
                s_frame_horz
                s_frame_vert
                s_frame_endl
                s_frame_endr)
(kern-set-ascii ss_u4_charset 32)
(kern-set-cursor ls_whirlpool)
