(kern-mk-map 
 'm_ship 9 17 pal_expanded
 (list
  "-- -- -- -- -- -- -- -- --";
  "-- -- -- -- ## -- -- -- --";
  "-- -- -- ## ## ## -- -- --";
  "-- -- ## ## ee ## ## -- --";
  "-- ## ## ee ee ee ## ## --";
  "-- ## ee ee  o ee ee ## --";
  "ee ee ee ee ee ee ee ee ee";
  "-- ## ee ee ee ee ee ## --";
  "-- ## ee ee ee ee ee ## --";
  "-- ## ee ee ee ee ee ## --";
  "ee ee ee ee ee ee ee ee ee";
  "-- ## ee ee  o ee ee ## --";
  "-- ## ee ee  W ee ee ## --";
  "-- ## ee ee ee ee ee ## --";
  "-- ## ## ee ee ee ## ## --";
  "-- -- ## ## ## ## ## -- --";
  "-- -- -- -- -- -- -- -- --";
  ))

(display t_cannon)(newline)

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
                      pmask-water ; pmask
                      2           ; tailwind penalty
                      4           ; headwind penalty
                      1           ; crosswind penalty
                      100         ; max hp
                      speed-ship  ; speed
                      )


(define (mk-ship)
  (kern-mk-vehicle t_ship north 100))
