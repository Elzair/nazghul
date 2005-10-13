(kern-mk-map 
 'm_ship 9 17 pal_expanded
 (list
  "-- -- -- -- -- -- -- -- --";
  "-- -- -- -- ## -- -- -- --";
  "-- -- -- ## ## ## -- -- --";
  "-- -- ## ## ee ## ## -- --";
  "-- ## ## ee ee ee ## ## --";
  "-- ## ee ee oo ee ee ## --";
  "ee ee ee ee ee ee ee ee ee";
  "-- ## ee ee ee ee ee ## --";
  "-- ## ee ee ee ee ee ## --";
  "-- ## ee ee ee ee ee ## --";
  "ee ee ee ee ee ee ee ee ee";
  "-- ## ee ee oo ee ee ## --";
  "-- ## ee ee WW ee ee ## --";
  "-- ## ee ee ee ee ee ## --";
  "-- ## ## ee ee ee ## ## --";
  "-- -- ## ## ## ## ## -- --";
  "-- -- -- -- -- -- -- -- --";
  ))

(kern-mk-vehicle-type 't_ship   ; tag
                      "ship"    ; name
                      s_ship    ; sprite
                      m_ship    ; map
                      t_cannon  ; ordnance
                      #t        ; vulnerable
                      #t        ; occupants die when destroyed
                      #t        ; must turn
                      "sail"    ; move description
                      sound-ship-move ; move sound
                      2           ; tailwind penalty
                      4           ; headwind penalty
                      1           ; crosswind penalty
                      100         ; max hp
                      speed-ship  ; speed
                      mmode-ship  ; pmask
                      )


(define (mk-ship)
  (kern-mk-vehicle t_ship north 100))

;;----------------------------------------------------------------------------
;; voidship
;;----------------------------------------------------------------------------
(kern-mk-map 
 'm_voidship 9 17 pal_expanded
 (list
  "** ** ** ** ** ** ** ** **";
  "** ** ** ** ## ** ** ** **";
  "** ** ** ## ## ## ** ** **";
  "** ** ## ## ee ## ## ** **";
  "** ## ## ee ee ee ## ## **";
  "** ## ee ee oo ee ee ## **";
  "ee ee ee ee ee ee ee ee ee";
  "** ## ee ee ee ee ee ## **";
  "** ## ee ee ee ee ee ## **";
  "** ## ee ee ee ee ee ## **";
  "ee ee ee ee ee ee ee ee ee";
  "** ## ee ee oo ee ee ## **";
  "** ## ee ee WW ee ee ## **";
  "** ## ee ee ee ee ee ## **";
  "** ## ## ee ee ee ## ## **";
  "** ** ## ## ## ## ## ** **";
  "** ** ** ** ** ** ** ** **";
  ))
(kern-mk-vehicle-type 't_voidship   ; tag
                      "voidship"    ; name
                      s_ship    ; sprite
                      m_voidship    ; map
                      t_cannon  ; ordnance
                      #t        ; vulnerable
                      #t        ; occupants die when destroyed
                      #t        ; must turn
                      "sail"    ; move description
                      sound-ship-move ; move sound
                      2           ; tailwind penalty
                      4           ; headwind penalty
                      1           ; crosswind penalty
                      100         ; max hp
                      speed-ship  ; speed
                      mmode-voidship  ; pmask
                      )


(define (mk-voidship)
  (kern-mk-vehicle t_voidship north 100))

