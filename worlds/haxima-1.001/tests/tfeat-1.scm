;; save.scm -- a nazghul session file
;; Load the standard definitions file
(load "naz.scm")

(kern-load "game.scm")

(kern-mk-map 'm_ship 9 17 pal_expanded
  (list
    "-- -- -- -- -- -- -- -- -- "
    "-- -- -- -- ## -- -- -- -- "
    "-- -- -- ## ## ## -- -- -- "
    "-- -- ## ## ee ## ## -- -- "
    "-- ## ## ee ee ee ## ## -- "
    "-- ## ee ee  o ee ee ## -- "
    "ee ee ee ee ee ee ee ee ee "
    "-- ## ee ee ee ee ee ## -- "
    "-- ## ee ee ee ee ee ## -- "
    "-- ## ee ee ee ee ee ## -- "
    "ee ee ee ee ee ee ee ee ee "
    "-- ## ee ee  o ee ee ## -- "
    "-- ## ee ee  W ee ee ## -- "
    "-- ## ee ee ee ee ee ## -- "
    "-- ## ## ee ee ee ## ## -- "
    "-- -- ## ## ## ## ## -- -- "
    "-- -- -- -- -- -- -- -- -- "
  )
)
(kern-mk-map 'm_campsite 7 7 pal_expanded
  (list
    " b  b .. .. ..  b  b "
    " b .. .. .. .. ..  b "
    ".. .. .. .. .. .. .. "
    ".. .. ..  & .. .. .. "
    ".. .. .. .. .. .. .. "
    " b .. .. .. .. ..  b "
    " b  b .. .. ..  b  b "
  )
)
(kern-mk-place 'p_dark_passage "A Dark Passage"
  s_dungeon ;; sprite
  (kern-mk-map 'm_dark_passage 32 32 pal_expanded
    (list
      " x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x "
      " x ,, ,, ,, ,, ,, ,,  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x ~~ ~~  x  x  x  x  x "
      " x ,, ,, ,, ,, ,, ,,  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x ~~  x  x  x  x  x  x "
      " x ,, ,, ,, ,, ,, ,,  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x ~~ ~~ ~~  x  x  x  x  x  x "
      " x ,, ,, ,, ,, ,, ,,  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x ~~  x  x  x  x  x  x  x  x "
      " x ,, ,, ,, ,, ,, ,,  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x ~~  x  x  x  x  x  x  x  x "
      " x  x  x  x ,, ,, ,,  x  x  x  x  x  x  x  x  x  x  x  x  x ~~ ~~ ~~ ~~  x  x  x  x  x  x  x  x "
      " x  x  x  x ,, ,, ,,  x  x  x  x  x  x  x  x  x  x  x  x  x ~~  x  x  x  x  x  x  x  x  x  x  x "
      " x  x  x  x ,, ,, ,,  x  x  x  x  x  x  x  x  x  x  x  x  x ~~  x  x  x  x  x  x  x  x  x  x  x "
      " x  x  x  x ,, ,, ,,  x  x  x  x  x ,, ,, ,, ,, ,, ~~ ~~ ~~ ~~  x  x  x  x  x  x  x  x  x  x  x "
      " x  x  x  x ,, ,, ,,  x  x  x  x ,, ,, ,, ,, ,, ,, ~~ ~~ ~~ ~~ ~~ ~~  x  x  x  x  x  x  x  x  x "
      " x  x  x  x ,, ,, ,,  x  x ,, ,, ,, ,, ,, ,, ,, ~~ ~~ ~~ ~~ ,, ,, ,,  x  x  x  x  x  x  x  x  x "
      " x  x  x  x  x ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ~~ ,, ,, ,, ,, ,, ,,  x  x  x  x  x  x  x  x  x "
      " x  x  x  x  x  x ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ~~ ,, ,, ,, ,, ,, ,,  x  x  x  x  x  x  x  x  x "
      " x  x  x  x  x  x  x  x  x ,, ,, ,, ,, ,, ,, ,, ~~ ,, ,, ,, ,, ,, ,,  x  x  x  x  x  x  x  x  x "
      " x  x  x  x  x  x  x  x  x ,, ,, ,, ,, ,, ,, ,, ~~ ,, ,, ,, ,, ,, ,,  x  x  x  x  x  x  x  x  x "
      " x  x  x  x  x  x  x  x  x  x ,, ,, ,, ,, ,, ,, ~~ ,, ,, ,, ,, ,, ,, ,,  x  x  x  x  x  x  x  x "
      " x  x  x  x  x  x  x  x  x  x ,, ,, ,, ,, ,, ~~ ~~ ,, ,, ,, ,, ,, ,, ,,  x  x  x  x  x  x  x  x "
      " x  x  x  x  x  x  x  x  x  x ,, ,, ,, ,, ~~ ~~ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,,  x  x  x  x "
      " x  x  x  x  x  x  x  x  x  x ,, ,, ~~ ~~ ~~ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,,  x  x  x  x "
      " x  x  x  x  x  x  x  x  x  x ,, ~~ ~~ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,,  x  x ,, ,, ,,  x  x  x "
      " x  x  x  x  x  x  x  x  x  x ~~ ~~ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,,  x  x  x  x ,, ,, ,,  x  x  x "
      " x  x  x  x  x  x  x  x  x  x ~~  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x ,, ,, ,,  x  x  x "
      " x  x  x  x  x  x  x  x  x ~~ ~~  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x ,, ,, ,,  x  x  x "
      " x  x  x  x  x  x  x  x  x ~~  x  x  x  x .. .. .. .. .. .. ..  x  x  x  x  x ,, ,, ,,  x  x  x "
      " x  x  x  x  x  x  x  x ~~ ~~  x  x  x .. .. .. .. .. .. .. .. ..  x  x  x  x ,, ,, ,,  x  x  x "
      " x  x  x  x  x  x  x  x ~~  x  x  x  x .. .. .. .. .. .. .. .. ..  x  x  x  x ,, ,, ,, ,, ,,  x "
      " x  x  x  x  x  x  x ~~ ~~  x  x  x  x .. .. .. .. .. .. .. .. ..  x  x  x  x ,, ,, ,, ,, ,,  x "
      " x  x  x  x  x  x  x ~~ ~~  x  x  x  x ~~ .. .. .. .. .. .. .. ..  x  x  x  x ,, ,, ,, ,, ,,  x "
      " x  x  x  x  x  x  x  x ~~ ~~ ~~ ~~ ~~ ~~ ~~ .. .. .. .. .. .. ..  x  x  x  x ,, ,, ,, ,, ,,  x "
      " x  x  x  x  x  x  x  x  x  x  x  x  x ~~ .. .. .. .. .. .. ..  x  x  x  x  x ,, ,, ,, ,, ,,  x "
      " x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  ;; neighbors
  nil
  ;; contents
  (list
  ) ;; end of objects
  (list
  )
) ;; end of place p_dark_passage

(kern-mk-place 'p_pit_of_death "Pit of Death"
  s_dungeon ;; sprite
  (kern-mk-map 'm_pit_of_death 8 8 pal_expanded
    (list
      " x  x  x  x  x  x  x  x "
      " x  !  !  !  !  !  !  x "
      " x  !  !  !  !  !  !  x "
      " x  !  !  !  !  !  !  x "
      " x  !  !  !  !  !  !  x "
      " x  !  !  !  !  !  !  x "
      " x  !  !  !  !  !  !  x "
      " x  x  x  x  x  x  x  x "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  ;; neighbors
  nil
  ;; contents
  (list
  ) ;; end of objects
  (list
    'pit-of-death-pre-entry-hook
  )
) ;; end of place p_pit_of_death

(kern-mk-place 'p_green_tower_lower "Beneath GreenTower"
  nil ;; sprite
  (kern-mk-map 'm_green_tower_lower 64 64 pal_expanded
    (list
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r ~~ ~~ || || || ||  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || || || || || || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r ~~ || || || || ||  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || || || || || || || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r ~~ || || || ||  r  r  r  r  r  r  r  x  x  x  x x!  x  x x!  x  x  x  x  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  x tt  x tt tt || || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r ~~ || || || ||  r  r  r  r  r  r  r  x cc cc  x cc cc cc cc  x cc cc  x  r  r  r  r  r  r  r  r  r  r  r  r  r  r .. .. ..  x tt tt tt || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r ~~ ~~ || || ||  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc cc cc  x  r  r  r  r  r  r  r  r  r  r  r  r  r .. .. .. .. tt tt tt tt || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r || || ~~ || || ||  r  r  r  r  r  r  r  x cc cc  x cc cc cc cc  x cc cc  x  r  r  r  r  r  r  r  r  r  r  r  r  r  @ tt  @ tt  x tt tt tt || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r || || ~~ || || ||  r  r  r  r  r  r  r  x  x  x x! cc cc cc cc x!  x  x  x  r  r  r  r  r  r  r  r  r  r  r  r .. .. .. .. ..  x tt tt || || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r || || ~~ || || ||  r  r  r  r  r  r  r  x cc cc  x cc cc cc cc  x cc cc  x  r  r  r  r  r  r  r  r  r  r  r tt tt tt tt tt tt  x || || || || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r || || ~~ ~~ || ||  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc cc cc  x  r  r  r  r  r  r  r  r  r  r  r  x  x  x  x tt  x  x || || || || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r || || || ~~ || ||  r  r  r  r  r  r  r  x cc cc  x cc cc cc cc  x cc cc  x  r  r  r  r  r  r  r  r  r  r .. .. ..  x .. .. ..  x || || || || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r || || || ~~ || ||  r  r  r  r  r  r  r  x  x  x x! cc cc cc cc x!  x  x  x  r  r  r  r  r  r  r tt tt .. .. .. ..  ? .. .. ..  ? tt tt tt tt tt tt tt "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r .. .. ~~ ~~ ~~  r  r  r  r  r  r  r  x cc cc  x cc cc cc cc  x cc cc  x  r  r  r  r  r  r || || tt  x .. .. ..  x .. .. ..  x || || || || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || || || || ~~  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc cc cc  ? ||  r  r  r  r  r  x  x tt  x  x tt  x  x  x ..  x  x || || || || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || || || || ~~  r  r  r  r  r  r  r  x cc cc  x cc cc cc cc  x cc cc  x tt  r ~~ ~~ ~~ ~~ ~~ || tt || || || || || || || || || || || || || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || || || ~~ ~~  r  r  r  r  r  r  r  x  x  x  x x! cc cc x!  x  x  x  x  x  x ~~  x  x -- ~~ ~~ .. || || || || || || || || || || || || || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || || ~~ || .. || tt ~~ ~~ ~~ .. .. .. ..  x cc cc cc cc  x ~~ ~~ ~~ -- __ __ __  x -- -- ~~ ~~ || || || || || || || || || || || || || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || ~~ ~~ || .. || tt ~~ tt ~~ .. .. ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ -- __ __ __  x __ -- ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ || || || ~~ ~~ ~~ ~~ || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || ~~ tt  b ..  b .. ~~ tt ~~ .. .. ~~ tt  x cc cc cc cc  x cc cc ~~ -- __ __ __  x __ -- ~~ ~~ || || || || || || ~~ || || || ~~ || || ~~ ~~ || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r ~~ ~~ .. .. .. .. ~~ .. ~~ ~~ ~~ ~~ tt  x x! cc cc x!  x cc cc ~~ -- -- -- --  x -- ~~ ~~ tt tt || || || || || || || || || ~~ || || tt ~~ ~~ "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || ~~ ~~ ~~ ~~ ~~ ~~ .. .. .. .. .. ..  x cc cc cc cc  x cc cc ~~ ~~ ~~ ~~ ~~  x ~~ ~~ .. || tt || || || || || ~~ ~~ ~~ ~~ ~~ || tt tt tt || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r .. .. .. .. .. .. .. ..  r  r  r  x cc cc cc cc cc cc cc cc cc cc cc cc  x .. .. .. tt tt || || || || || || || || || || || || tt || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc  x cc cc cc cc cc cc cc x! .. .. ..  b tt || || || || || || tt || || || || || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  x x! cc cc x!  x ,C ,I ,S ,T ,E ,R ,N  x .. .. .. tt tt || tt || || || tt tt tt || || || || || || || "
      " r  r  r  r  r  r  r  r  r  r  r  x  x  x  x  x  x  x  x  x  x  r  r  r  r  r  r  x  x  x  x cc cc cc cc  x  x  x  x  x  x  x  x  x .. .. ..  b tt tt tt tt || || || tt || || ||  r ||  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  x ,B ,A ,R ,R ,A ,C ,K ,S  x  r  r  r  r  x  x  x cc cc  x cc cc cc cc  x cc cc  x  x  x cc cc  ? .. .. .. tt tt || tt || || || || || || || ||  r tt tt cc  r "
      " r  r  r  r  r  r  r  r  r  r  r x! cc cc cc cc cc cc cc cc x!  r  r  x  x  x cc cc cc cc  x cc cc cc cc  x cc cc cc cc  x  x  x  x .. .. ..  b tt || || tt  r  r  r  r  r  r  r  r cc cc cc  r "
      " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  r  x cc cc cc cc cc cc  x cc cc cc cc  x cc cc cc cc cc cc  x  x .. .. .. tt tt || || tt  r cc cc cc cc cc cc cc cc cc cc  r "
      " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  x  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x  x .. .. .. tt tt || || tt  r cc  r  r  r  r  r  r cc cc cc  r "
      " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  x cc cc cc cc cc cc cc  x cc cc cc cc  x cc cc cc cc cc cc cc  x  x  x x!  x  x  x x!  x  x cc  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  x cc cc cc cc cc cc cc  x cc cc cc cc  x cc cc cc cc cc cc cc  x  x ,C ,O ,U ,N ,C ,I ,L  x cc  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  x  x  x  x cc  x  x  x x! cc cc cc cc x!  x  x  x cc  x  x  x  x  x cc cc cc cc cc cc cc  x cc  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  x  x x!  x cc cc  x x!  x  x  x x! cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc x! cc cc cc cc cc cc cc x! cc  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  0  0  0 cc cc  x cc  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  0  0  0 cc cc  x cc  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  x  x x!  x cc cc  x x!  x  x  x x! cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc x! cc cc cc cc cc cc cc x! cc  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  x  x  x  x cc  x  x  x x! cc cc cc cc x!  x  x  x cc  x  x  x  x  x cc cc cc cc cc cc  &  x cc  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r x! cc cc cc cc cc cc cc cc x!  r  x cc cc cc cc cc cc cc  x cc cc cc cc  x cc cc cc cc cc cc cc  x  x  x x!  x  x  x x!  x  x cc  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  x cc cc cc cc cc cc cc  x cc cc cc cc  x cc cc cc cc cc cc cc  ? cc cc cc cc cc cc cc cc cc cc  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r x! cc cc cc cc cc cc cc cc x!  r  x  x cc cc cc cc cc cc  x cc cc cc cc  x cc cc cc cc cc cc  x  x  r  r  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  r  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x  r  r  r  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  x ,T ,R ,A ,I ,N ,I ,N ,G  x  r  r  x  x  x cc cc cc cc  x cc cc cc cc  x cc cc cc cc  x  x  x  r  r  r  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  x  x  x  x  x  x  x  x  x  x  r  r  r  r  x  x  x cc cc  x cc cc cc cc  x cc cc  x  x  x  r  r  r  r  r  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  x  x  x  x cc cc cc cc  x  x  x  x  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  x  x  x  x x! cc cc x!  x  x  x  x  x  x  x  x  x  x  r  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc cc cc cc  x  [  @  @  ]  x  r  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
      " r .. .. .. .. .. ..  r .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r x! cc cc cc cc cc cc cc cc cc cc cc x! cc cc cc cc  x  x  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
      " r .. .. .. .. .. ..  r .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r  x cc cc  0 cc cc  0 cc cc  0 cc cc cc cc cc cc cc  &  x  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
      " r .. .. .. .. .. ..  r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r  r  x cc cc  0 cc cc  0 cc cc  0 cc cc cc cc cc cc cc  &  x  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
      " r ..  r  r  r  r  r  r .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r  x cc cc  0 cc cc  0 cc cc  0 cc cc cc cc cc cc cc  &  x  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
      " r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r x! cc cc cc cc cc cc cc cc cc cc cc x! cc cc cc cc  &  x  r  r  r  r  r  r cc  r ,C ,R ,Y ,P ,T  r  r  r  r  r "
      " r ..  p ..  p ..  p ..  p ..  p ..  p ..  p ..  p .. ..  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc cc cc cc  x cc cc cc cc  x  x  r  r  r  r  r cc cc cc cc cc cc cc cc cc  r  r  r  r "
      " r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r  r  r  r  r  r  r  r  x ,D ,I ,N ,I ,N ,G  @ ,H ,A ,L ,L  x  [  @  @  ]  x  r  r  r  r  r  r cc  r  r cc cc cc cc cc cc cc  r  r  r "
      " r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r  r  r  r  r  r  r  r  x  x x!  x  x  x x!  x  x  x x!  x  x  x  x  x  x  x  r  r  r  r  r  r cc  r cc cc cc cc cc cc cc  r  r  r  r "
      " r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r  r cc  r  r  r cc  r  r  r  r  r "
      " r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r cc cc cc  r cc cc cc  r  r  r  r "
      " r ..  p ..  p ..  p ..  p ..  p ..  p ..  p ..  p .. ..  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r  r cc  r  r  r cc  r  r  r  r  r "
      " r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r cc cc cc  r cc cc cc  r  r  r  r "
      " r ..  r  r  r  r  r  r .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r  r cc  r  r  r cc  r  r  r  r  r "
      " r .. .. .. .. .. ..  r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  r cc cc cc  r cc cc cc  r  r  r  r "
      " r .. .. .. .. .. ..  r .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r  r  r cc  r  r  r  r  r "
      " r .. .. .. .. .. ..  r .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc cc cc  r cc cc cc  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r  r  r cc  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r "
      " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  ;; neighbors
  (list
    (list ;; begin above neighbor
      (kern-mk-place 'p_green_tower "Town of GreenTower"
        s_keep ;; sprite
        (kern-mk-map 'm_green_tower 64 64 pal_expanded
          (list
            "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || || tt  b .. /7 ..  b tt || || || || || || || || || || || || || || || || || || || || || || || || || || || || "
            "|| || || || || || || || || || || ||  r  r  r  r  r  r  r || || || || || || || || || || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || || || || || || || || || || || || || || "
            "|| ||  r  r  r  r  r || || ||  r  r  r  & cc cc cc cc  r || || || || || || || || || || tt  b .. /7 ..  b tt || || || || || || || || || || || ||  x  x  x  x  x  x  x  x  x tt tt || || || || || "
            "|| ||  r cc cc cc  r || || ||  r cc  r cc cc cc cc cc  r || || || || || || || || || tt tt tt .. /7 .. tt tt || || || || || || || || || || || ||  x .. .. .. .. .. .. ..  x tt tt tt || || || || "
            "|| ||  r cc cc cc  r || || ||  r cc cc cc cc cc cc cc  r || || || || || || || || || tt tt  b .. /4 /d /d /d /d /d /d /d /d /d /d /d /d /d /1 /d .. .. .. .. .. .. .. .. ws tt tt tt || || || || "
            "|| ||  r cc cc cc  r || || ||  r cc  r cc cc cc  r  r  r || || || || || || || || tt tt tt tt .. /7 ..  b tt || || || || || || || || || || /7 tt  x  @  @  @  @  @  @  @  x tt tt tt || || || || "
            "|| ||  r  r cc  r  r || || ||  r cc  r cc cc cc cc cc  r || || || || || || || || tt tt tt  b .. /7 .. tt tt || || || || || || || || || || /7 .. .. .. .. .. .. .. .. ..  x tt tt || || || || || "
            "|| || || || /7 || || || || || || /7  r  r  r  r  r  r  r || || || || || || || tt tt tt tt tt .. /7 .. tt tt || || || || || || || || || || /7 tt  x .S .H .R .O .O .M .S  x || || || || || || || "
            "|| || || || /8 /d /d /d /1 /d /d /a || || || || || || || || || || || || || || tt tt tt tt  b .. /7 ..  b tt || || || || || || || || || tt /7 tt  x  x  x  x  x  x  x  x  x || || || || || || || "
            "|| || || || || || || || /7 || || || || || || || || || || || || || || || || tt tt tt tt tt tt .. /7 .. tt tt || || || || || || || || tt tt /7 tt  x .. .. ..  x .. .. ..  x || || || || || || || "
            "|| || ||  r  r  r || || /7 || || || || || || || || || || || || || || || || tt tt tt tt tt  b .. /7 ..  b tt || || || || || || || || tt tt /8 /d .. .. .. ..  ? .. .. ..  ? tt tt tt tt tt tt tt "
            "||  r  r  r cc  r  r || /4 /d /d /d /d /d /d /d /d /d /d /2 || || || || tt tt tt tt tt tt tt .. /7 .. tt tt || || || || tt tt tt tt tt tt tt tt  x .. .. tt  x .. .. ..  x || || || || || || || "
            "||  r cc cc cc cc  r  r /7 || || || || || || || || || || /7 || || || || tt tt tt tt tt tt  b .. /7 ..  b tt || || || || tt .. tt tt tt || || ||  x  x ws  x  x  x ..  x  x || || || || || || || "
            "||  r cc cc cc cc cc cc /a || || || || || || || || tt || /7 || || || tt tt tt tt tt tt tt tt .. /7 .. tt tt || tt tt tt tt .. .. tt || || || || || || || || || || || || || || || || || || || || "
            "||  r  r cc cc cc  r  r || || || || || tt || || tt tt tt /7 || || || tt tt tt tt tt tt tt  b .. /7 ..  b tt || tt || || tt tt tt tt || || || || || || || || || || || || || || || || || || || || "
            "|| ||  r  r  &  r  r || tt || || || tt tt  b || || tt || /7 || tt tt tt tt tt tt tt tt tt tt .. /7 .. tt tt || tt || || || || || || || || || || || || || || || || || || || || || || || || || || "
            "|| || ||  r  r  r || tt tt tt || tt tt .. tt || || tt  b /7  b ..  b tt  b tt  b tt  b tt .. .. /7 .. .. tt  b tt  b tt tt  b tt  b tt  b tt tt || || || || || || || || || || || || || || || || "
            "|| || || tt || || || || tt || tt tt .. .. .. tt || tt .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt || || || || || || || || || || || || || || || "
            "|| || tt tt tt || || || || || || tt tt .. tt || ||  b .. /4 /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /2 .. || tt || || || || || || || || || || || || || || || "
            "|| || || tt || || || || || tt tt tt tt tt tt || || tt .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. tt tt || || || || || || || || || || || || || || || "
            "|| || || || || || || tt tt tt tt tt tt tt || || ||  b .. /7 .. .. ..  b  d  b  d  b  d .. .. .. /7 .. .. ..  b tt tt  b tt  b tt .. .. /7 ..  b tt || || || || || || || || || || || || || || || "
            "|| || || || || || tt tt tt .. tt tt tt || || || || tt .. /7 .. ..  d  d  d  d  d  d  d  b .. .. /7 .. ..  b tt tt tt tt tt tt  b tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
            "|| || || || ||  b tt tt .. .. .. tt tt tt || || ||  b .. /7 ..  b  d || || || || ||  d  x w+  x cc  x w+  x tt || || || || || tt tt .. /7 ..  b tt || || || || || || || || || || || || || || || "
            "|| || || || || tt tt .. .. .. .. .. tt tt || || || || .. /7 ..  d  d || || || ||  x w+  x cc cc cc cc cc  x  x  x || || || || tt  b .. /7 .. tt tt || || || || || || || || || || || || || || || "
            "|| || || || || tt tt tt .. .. .. tt tt tt || || ||  b .. /7 ..  b  d || ||  x  x  x cc cc cc cc cc cc cc cc cc  x  x  x || || tt tt .. /7 ..  b tt || || || || || || || || || || || || || || || "
            "|| || || || || tt tt tt tt .. tt tt tt || || || || || .. /7 ..  d  d ||  x  x cc cc cc cc cc cc cc cc cc cc cc cc cc  x  x || tt  b .. /7 .. tt tt || || || || || || || || || || || || || || || "
            "|| || tt tt tt tt || tt tt tt tt  b || || tt tt ||  b .. /7 ..  b  d ||  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
            "|| || tt || || || || || tt tt tt || || || || || || tt .. /7 ..  d  d  x  x cc cc cc  x x!  x  x cc  x  x x!  x cc cc cc  x  x tt  b .. /7 ..  b tt || || || || || || || || || || || || || || tt "
            "tt tt tt || || || || || || || || || || || || || tt  b .. /7 ..  b  d  x cc cc cc  x  x || tt .. cc .. tt ||  x  x cc cc cc  x tt tt .. /7 .. tt tt || || || || || || || || || || || || || || tt "
            " b tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. /7 ..  d  b  x cc cc cc x! || tt .. .. cc .. .. tt || x! cc cc cc  x  b .. .. /7 ..  b tt || || || || || || || || || || || || || || tt "
            ".. ..  b tt  b tt  b tt  b tt  b tt  b tt  b  b .. .. .. /7 .. .. .. w+ cc cc cc  x tt ..  b .. cc ..  b .. tt  x cc cc cc w+ .. .. .. /7 .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
            ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. ..  x cc cc cc  x .. .. .. .. cc .. .. .. ..  x cc cc cc  x .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
            "/d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d "
            ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. ..  x cc cc cc  x .. .. .. .. cc .. .. .. ..  x cc cc cc  x .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
            ".. ..  b tt  b tt  b tt  b tt  b tt  b tt tt tt  b .. .. /7 .. .. .. w+ cc cc cc  x tt ..  b .. cc ..  b .. tt  x cc cc cc w+ .. .. .. /7 .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
            " b tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. /7 .. ..  b  x cc cc cc x! || tt .. .. cc .. .. tt || x! cc cc cc  x  b .. .. /7 ..  b tt || || || || || || || || || || || tt tt tt tt "
            "tt tt || || || || tt tt tt tt tt tt tt tt || || tt tt .. /7 .. .. tt  x cc cc cc  x  x || tt .. cc .. tt ||  x  x cc cc cc  x tt .. .. /7 .. tt tt || || || || || || || || || || || || || || tt "
            "tt tt || || || || tt tt tt tt tt tt tt tt || || tt  b .. /7 ..  b tt  x  x cc cc cc  x x!  x  x cc  x  x x!  x cc cc cc  x  x tt  b .. /7 ..  b tt || || || || || || || || || || || || || || || "
            "|| || || || || || || tt tt tt tt tt tt tt || || tt tt .. /7 .. tt tt ||  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
            "|| || || || || || || || tt tt tt tt tt tt || || tt  b .. /7 ..  b tt ||  x  x cc cc cc cc cc cc cc cc cc cc cc cc cc  x  x || tt  b .. /7 ..  b tt || || || || || || || || || || || || || || || "
            "|| || || || || || || || || || || || || || || || tt tt .. /7 .. tt tt || ||  x  x  x cc cc cc cc cc cc cc cc cc  x  x  x || || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
            "|| || || || || || || || || || || || || || || || tt  b .. /7 ..  b tt || || || ||  x w+  x cc cc cc cc cc  x  x  x || || || || tt  b .. /7 ..  b tt || || || || || || || || || || || || || || || "
            "|| || || || || || || || || || || || || ||  r  r tt tt .. /7 .. tt tt || || || || || tt  x w+  x cc  x w+  x tt || || || || tt tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
            "|| || || || || || || || || || || || || || || || tt  b .. /7 ..  b tt tt tt tt tt tt tt  b .. .. /7 .. ..  b tt tt tt tt tt tt tt  b .. /7 ..  b tt tt tt tt tt tt tt || || || || || || || || || "
            "|| || || || || || || tt tt tt tt || || || || || tt tt .. /7 .. .. tt  b tt  b tt  b .. .. .. .. /7 .. .. .. ..  b tt  b tt  b tt .. .. /7 .. .. tt  b tt  b tt  b tt || || || || || || || || || "
            "|| || || || || tt  r tt  r  r tt  r  r || || || tt  b .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. tt tt || || || || || || || || || "
            "|| || ||  r  r tt tt tt || || tt tt  r  r  r || tt tt .. /8 /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /9 /d /d /d /d /d /2 ..  b tt || || || || || || || || || "
            "|| || ||  r || || || tt tt tt || tt tt ||  r || ||  b .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. tt tt || || || || || || || || || "
            "|| || || || tt tt tt tt .. .. tt || tt tt ||  r  r || || tt  b tt  b tt  b tt  b tt  b tt  b .. /7 .. tt  b tt  b tt  b tt  b tt  b tt  b tt  b tt tt  b /4 /2  b tt || || || || || || || || || "
            "||  r  r || tt tt || .. ..  b .. tt tt tt tt ||  r  r || tt tt tt tt tt tt tt tt tt tt tt tt .. /7 .. tt tt tt tt tt tt tt tt tt tt tt  x  x  x  x ws  x cc cc  x ws  x  x  x  x || || || || || "
            "|| || || || tt tt .. .. .. .. .. tt .. tt tt || ||  r || || || || || || || || || || || || tt .. /7 .. tt || || || || || || || || || ||  x cc cc cc cc cc cc cc cc cc cc cc cc  x || || || || || "
            "|| tt tt tt tt .. .. .. .. .. .. .. .. .. tt tt || tt tt || || || || || || || || || || || tt .. /7 .. tt || || || || || || || || || ||  x cc cc cc cc cc cc cc cc cc cc cc cc  x || || || || || "
            "|| tt tt tt ..  b .. .. .. .. .. .. ..  b .. tt .. tt tt tt tt tt tt || || || || || || || tt .. /7 .. tt || || || || || || || || || ||  x cc cc  0 cc cc cc cc cc cc  0 cc cc  x || || || || || "
            "|| || || tt .. .. .. .. ..  a .. .. .. .. .. tt ||  r || || || || tt || || || || || || || tt .. /7 .. tt || || || || || || || || || ||  x cc cc  0 cc cc  &  & cc cc  0 cc cc  x || || || || || "
            "||  r || tt tt tt tt .. .. .. .. .. .. tt tt || ||  r || || || || tt || || || || || || || tt .. /7 .. tt || || || || || || || ||  x  x  x cc cc  0 cc cc cc cc cc cc  0 cc cc  x  x  x || || || "
            "||  r || || || .. tt .. .. .. .. .. .. tt tt ||  r || || ||  r  r tt  r  r || || || || tt tt .. /7 .. tt || || || || || || || ||  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x || || || "
            "||  r  r || tt tt tt  b .. .. ..  b .. tt tt ||  r || ||  r  r tt tt tt  r  r || || || tt tt .. /7 .. tt || || || || || || || ||  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x || || || "
            "|| ||  r || tt tt tt tt tt .. .. .. .. tt ||  r  r || ||  r tt tt || tt ||  r || || || tt tt .. /7 .. tt || || || || || || || ||  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x || || || "
            "|| || || || || || || || tt tt tt tt tt || tt  r || || ||  r tt || || || ||  r || || || tt tt .. /7 .. || || || || || || || ||  x  x cc  x  x  @  @  @  @  @  @  @  @  @  @  x  x cc  x  x || || "
            "|| || ||  r  r || || || tt tt tt || tt tt ||  r || || ||  r tt tt || || ||  r || || || tt tt .. /7 .. tt || || || || || || ||  x cc cc cc  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x || || "
            "|| || || ||  r  r ||  r  r  r ||  r  r  r  r || || || ||  r  r || || || tt  r || || || tt tt .. /7 .. tt || || || || || || ||  x cc cc cc  x .W .H .I .T .E  @ .S .T .A .G  x cc cc cc  x || || "
            "|| || || || || || || || || || || || || || || || || || || ||  r  r  r  r tt || || || tt tt tt .. /7 .. tt || || || || || || ||  x cc cc cc  x  x  x  x  x  x  x  x  x  x  x  x cc cc cc  x || || "
            "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || tt tt tt .. /7 .. tt tt || || || || || ||  x  x  x  x  x || || || || || || || || || ||  x  x  x  x  x || || "
            "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || tt tt .. .. /7 .. .. tt tt || || || || || || || || || || || || || || || || || || || || || || || || || || || "
          )
        )
        #f #f #f #f
        ;; subplaces
        nil
        ;; neighbors
        nil
        ;; contents
        (list
          (list
            (kern-tag 'mg-3
              (bind
                (kern-mk-obj t_moongate 1)
                (list
                  'ord
                  #t
                  '()
                  #t
                  #f
                )
              ) ;; bind
            ) ;; kern-tag
          60 32)
          (list
            (bind
              (kern-mk-obj t_teleporter 1)
              (list
                'p_pit_of_death
                5
                5
              )
            ) ;; bind
          60 32)
          (list
            (bind
              (kern-mk-obj t_lever 1)
              (list
                #f
                'gt-b-1
                #f
              )
            ) ;; bind
          63 31)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          7 13)
          (list
            (kern-tag 'gt-b-1
              (bind
                (kern-mk-obj t_terrain_blitter 1)
                (list
                  'p_green_tower
                  60
                  30
                  5
                  5
                  'm_campsite
                )
              ) ;; bind
            ) ;; kern-tag
          62 32)
          (list
            (bind
              (kern-mk-obj t_ladder_down 1)
              (list
                'p_green_tower_lower
                32
                32
              )
            ) ;; bind
          32 32)
          (list
            (kern-mk-obj t_dagger 1)
          63 32)
          (list
            (kern-mk-obj t_green_potion 2)
          63 32)
          (list
            (kern-mk-obj t_mushroom 1)
          63 32)
          (list
            (bind
              (kern-mk-obj t_lever 1)
              (list
                #f
                'portcullis-1
                #f
              )
            ) ;; bind
          36 31)
          (list
            (kern-mk-field f_sleep_perm -1)          63 33)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          48 10)
          (list
            (kern-tag 'portcullis-1
              (bind
                (kern-mk-obj t_portcullis 1)
                (list
                  #f
                  '()
                  #f
                )
              ) ;; bind
            ) ;; kern-tag
          37 32)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          41 32)
          (list
            (bind
              (kern-mk-obj t_trap_door 1)
              (list
                'p_green_tower_lower
                54
                11
              )
            ) ;; bind
          54 11)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          32 22)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          4 6)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #t
                #f
              )
            ) ;; bind
          54 12)
          (list
            (kern-tag 'mg-4
              (bind
                (kern-mk-obj t_moongate 1)
                (list
                  'ord
                  #f
                  '()
                  #f
                  #f
                )
              ) ;; bind
            ) ;; kern-tag
          20 32)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          11 6)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          32 42)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          48 4)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          23 32)
          (list
            (kern-tag 'ws-d-1
              (bind
                (kern-mk-obj t_door 1)
                (list
                  #f
                  0
                  'ws-d-2
                  #f
                  #f
                  #f
                )
              ) ;; bind
            ) ;; kern-tag
          51 49)
          (list
            (kern-tag 'ws-d-2
              (bind
                (kern-mk-obj t_door 1)
                (list
                  #f
                  0
                  'ws-d-1
                  #f
                  #f
                  #f
                )
              ) ;; bind
            ) ;; kern-tag
          52 49)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          48 6)
          (list
            (bind
              (kern-mk-char
                'ch_shroom
                "Shroom"
                sp_human
                oc_druid
                s_companion_druid
                2
                20 30 20
                0 0
                0 0
                0 0
                0 0
                30 0
                150 9
                'shroom-conv
                sch_shroom
                nil
                (list
                  t_dagger
                )
                ;; hooks
                (list
                )
              )
              (list
                #f
                #f
              )
            ) ;; bind
          61 31)
        ) ;; end of objects
        (list
          'green-tower-pre-entry-hook
        )
      ) ;; end of place p_green_tower

    9) ;; end above neighbor
  )
  ;; contents
  (list
    (list
      (bind
        (kern-mk-obj t_ladder_up 1)
        (list
          'p_green_tower
          32
          32
        )
      ) ;; bind
    32 32)
    (list
      (kern-mk-char
        'ch_olin
        "Olin the Ghast"
        sp_ghast
        nil
        s_ghost
        2
        20 30 22
        0 1
        10 5
        0 0
        0 0
        58 0
        140 8
        nil
        nil
        nil
        nil
        ;; hooks
        (list
        )
      )
    59 32)
  ) ;; end of objects
  (list
  )
) ;; end of place p_green_tower_lower

(kern-mk-place 'p_wilderness "The Great Wild"
  nil ;; sprite
  (kern-mk-map 'm_wilderness 40 40 pal_expanded
    (list
      "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
      "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
      "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
      "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- -- -- -- -- -- -- -- -- __ __ __ __ __ __ __ "
      "__ __ __ __ __ __ __ __ -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- .. .. .. .. .. .. .. .. .. -- -- -- -- -- __ __ __ "
      "__ __ __ __ __ __ __ -- -- .. .. .. .. .. tt tt tt tt .. {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ tt tt .. .. .. %% {{ -- __ __ __ "
      "__ __ __ -- -- -- -- -- -- .. .. .. tt tt tt {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ tt tt .. .. .. -- __ __ __ "
      "__ __ __ -- .. .. .. .. tt tt  ! tt {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt .. .. .. -- -- .. __ "
      "__ __ __ -- .. .. tt tt tt || {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ tt .. .. .. .. .. -- .. .. "
      ".. __ __ -- .. .. tt tt {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ {{ {{ {{ tt .. .. .. .. -- .. .. "
      ".. __ __ -- .. tt tt {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ tt tt tt {{ {{ {{ {{ {{ {{ {{ tt .. /0 /d /d -- /d /a "
      ".. __ __ -- .. .. tt tt {{ {{ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ {{ {{ tt tt tt .. .. .. .. .. .. .. .. .. .. /6 .. .. -- .. .. "
      ".. -- -- -- .. .. .. .. tt {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ {{ {{ tt tt tt tt .. .. /7 .. .. -- -- .. "
      "__ -- .. .. .. .. .. .. .. tt tt {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ {{ {{ {{ {{ tt .. .. /7 .. .. .. -- .. "
      "__ -- .. .. .. .. .. .. .. .. {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt .. .. /7 .. .. .. -- __ "
      "__ -- .. .. .. .. {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ tt tt .. .. /7 .. .. .. -- __ "
      "__ -- .. .. .. {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ tt tt tt .. .. .. /7 .. .. -- -- __ "
      "__ -- .. .. .. .. {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ tt tt tt .. .. .. .. .. /7 .. -- -- __ __ "
      "-- -- .. .. .. .. .. .. .. {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt tt tt tt tt .. .. .. .. tt tt tt /7 tt -- -- __ __ "
      "-- -- tt tt tt tt .. tt tt tt {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt tt .. .. .. .. tt tt tt tt || || /7 || || -- -- -- "
      "|| ~~ || || tt tt .. .. tt tt tt tt .. {{ {{ {{ ^^ ^^ ^^ ^^ ^^ {{ {{ tt tt .. .. tt tt || || || || || /8 /2 || || ~~ tt "
      "|| ~~ ~~ || || tt .. .. tt tt .. .. .. .. .. {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ tt .. .. tt || || || || || || || /7 || || ~~ ~~ "
      ".. .. ~~ .. .. .. .. tt tt tt tt tt tt tt .. .. .. {{ {{ ^^ {{ {{ {{ .. .. .. .. tt || || ||  b tt  b tt /b .. || || ~~ "
      ".. || ~~ ~~ || || tt tt tt tt ~~ ~~ ~~ -- -- -- .. .. {{ {{ {{ -- -- -- .. .. .. tt tt || || tt tt .. .. .. .. || ~~ ~~ "
      ".. || || ~~ tt tt tt tt .. %% %% ~~ ~~ -- __ -- -- .. .. .. .. .. -- -- .. .. .. .. tt || ||  b tt  b tt .. .. ~~ ~~ .. "
      "|| || || ~~ tt tt tt .. .. .. %% %% ~~ -- __ __ -- -- -- -- .. .. -- -- -- .. .. .. tt || || || || || || .. .. ~~ .. .. "
      "|| || || ~~ tt tt .. .. .. %% .. %% %% -- __ __ __ __ __ -- -- -- -- -- .. .. .. .. tt tt tt || || || || || -- -- -- || "
      "|| || || ~~ .. .. .. %% %% .. %% %% ~~ -- __ __ __ __ __ __ __ __ __ -- -- .. .. .. .. .. tt tt tt -- -- -- -- __ -- -- "
      "-- || -- -- ~~ ~~ %% %% ~~ %% %% ~~ -- -- __ __ __ __ __ __ __ __ __ -- -- -- -- -- -- -- -- -- -- -- __ __ __ __ __ -- "
      "-- -- -- __ __ -- -- -- ~~ ~~ ~~ ~~ -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
      "__ __ __ __ __ -- -- -- %% %% %% %% -- -- __ __ ^^ ^^ ^^ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
      "__ __ __ __ __ __ -- -- -- .. .. .. -- -- -- -- -- -- ^^ -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
      "__ __ __ __ __ __ __ -- -- -- ^^ {{ ^^ {{ {{ {{ ^^ -- ^^ ^^ ^^ -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
      "__ __ __ __ __ __ __ __ __ -- -- {{ {{ {{ ^^ {{ ^^ -- {{ {{ ^^ ^^ -- -- __ __ __ __ __ -- ^^ ^^ ^^ __ __ __ __ __ __ __ "
      "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ^^ ^^ -- ^^ ^^ ^^ ^^ -- -- __ __ __ __ __ -- --  ! ^^ ^^ ^^ ^^ -- __ __ __ "
      "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^  !  !  ! ^^  ! -- -- __ __ "
      "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^  !  !  ! ^^ -- __ __ "
      "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ^^  !  ! ^^ ^^ ^^ ^^ __ __ __ "
      "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ^^  ! ^^ ^^ __ __ __ __ __ __ "
      "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- -- __ __ __ __ __ __ __ "
    )
  )
  #t #f #t #f
  ;; subplaces
  (list
    (list
      (kern-mk-place 'p_gregors_hut "Gregor's Hut"
        s_hamlet ;; sprite
        (kern-mk-map 'm_gregors_hut 32 32 pal_expanded
          (list
            "|| || || || tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. tt tt tt tt "
            "|| || || || tt .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. tt tt tt tt tt tt tt "
            "|| || tt tt tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. tt || || || tt tt tt "
            "|| tt tt tt tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. tt tt || || || tt tt tt "
            "tt tt tt tt tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. tt tt || || tt || || tt .. "
            "tt .. .. .. tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. tt tt tt || tt tt tt || tt .. "
            ".. .. .. .. tt tt tt tt .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. tt tt || || tt || || tt .. "
            ".. .. .. .. /0 /d /d /d /d /d /d /d /d /d /d /d /9 /d /d /d /d /d /d /2 tt tt || || || tt tt .. "
            ".. .. .. /0 /a tt tt tt .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /8 /d /d /2 tt tt tt .. .. "
            ".. .. .. /7 tt tt tt tt tt  r  r ws  r  r  r ws  r  r  r  r  r  b  b  b  b  b /8 /d /2 tt .. .. "
            ".. .. .. /7 tt tt tt tt tt  r cc cc cc cc cc cc cc  r cc cc  r .. .. .. .. ..  b .. /7 .. .. .. "
            ".. .. .. /7 .. tt tt tt tt  r cc cc cc cc cc cc cc  r cc cc  r tt .. .. .. ..  b .. /7 .. .. .. "
            ".. .. .. /7 .. .. tt || ||  r cc cc  [  @  ] cc cc  r cc cc  r tt tt .. .. ..  b .. /7 .. .. .. "
            ".. .. .. /7 .. .. tt || ||  r cc cc cc cc cc cc cc  r cc cc  r tt tt tt .. ..  b .. /7 .. .. .. "
            ".. .. .. /7 .. ..  r  r  r  r cc cc cc cc cc cc cc cc cc cc  r tt tt || tt ..  b .. /7 .. .. .. "
            ".. .. .. /7 .. .. ws cc cc  r  r cc  r  &  r  r  r  r  r  r  r tt tt tt .. ..  b .. /7 .. .. .. "
            "/d /d /d /6 .. ..  r cc cc cc cc cc cc cc cc cc cc  r .. ..  r .. .. .. .. ..  b .. /4 /d /d /d "
            ".. .. .. /7 .. ..  r  r  r  r cc cc cc cc cc cc cc  r .. ..  r .. .. .. .. ..  b .. /7 tt tt tt "
            ".. .. .. /7 .. ..  r cc cc  r cc cc cc cc cc cc cc  r .. .. .. .. .. .. .. ..  b tt /7 tt tt tt "
            ".. .. .. /7 .. .. ws cc cc cc cc cc cc cc cc cc cc  r .. .. .. .. .. .. .. ..  b tt /7 tt tt %% "
            ".. .. .. /7 .. ..  r  r  r  r  r ws  r cc  r ws  r  r ..  r  r  b  b  b  b  b /0 /d /a tt %% %% "
            ".. .. .. /7 .. .. .. .. .. .. .. tt tt /7 .. tt tt tt /4 /d /d /d /d /d /d /d /a tt tt %% %% %% "
            ".. .. .. /7 .. .. .. .. .. .. .. .. .. /7 .. .. .. .. /7 .. .. .. .. .. .. tt tt tt %% %% ~~ ~~ "
            ".. .. .. /8 /d /d /d /d /d /d /d /d /d /9 /d /1 /d /d /a .. .. .. .. .. tt tt tt %% ~~ ~~ ~~ %% "
            ".. .. .. .. .. .. .. tt tt tt .. .. .. .. .. /7 .. .. tt .. .. .. tt tt tt tt tt %% ~~ %% %% %% "
            "tt tt .. .. .. .. .. tt tt tt .. .. .. .. .. /7 .. .. tt tt tt .. .. .. tt ~~ ~~ ~~ ~~ %% %% %% "
            "|| tt tt .. .. .. .. tt tt tt tt .. .. .. .. /7 .. .. tt tt tt tt tt .. .. ~~ .. tt %% %% tt .. "
            "|| || tt tt .. .. .. .. tt tt tt .. .. .. .. /7 .. .. .. tt tt tt tt ~~ ~~ -- .. .. .. tt tt .. "
            "|| || tt tt tt .. .. .. .. .. .. .. .. .. .. /8 /2 .. .. tt tt tt ~~ ~~ ~~ ~~ tt tt .. tt || .. "
            "|| || || tt tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. tt tt tt ~~ tt tt tt tt tt .. .. .. .. "
            "|| || || || || tt tt .. .. .. .. .. .. .. .. .. /7 .. .. tt tt ~~ ~~ tt .. .. tt tt || .. .. .. "
            "|| || || || || || tt tt .. .. .. .. .. .. .. .. /7 .. .. tt tt ~~ tt tt .. tt tt || || .. .. .. "
          )
        )
        #f #f #f #f
        ;; subplaces
        nil
        ;; neighbors
        nil
        ;; contents
        (list
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          17 14)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          13 20)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          18 20)
          (list
            (kern-mk-char
              'ch_slurmok
              "Slurmok"
              sp_yellow_slime
              oc_wizard
              s_yellow_slime
              3
              4 14 3
              0 1
              10 5
              0 0
              0 0
              26 0
              240 8
              nil
              nil
              nil
              nil
              ;; hooks
              (list
                (list
                  ef_poison_immunity
                  '()
                  2
                  0
                )
              )
            )
          17 26)
          (list
            (kern-mk-char
              'ch_thorald_greybeard
              "Thorald Greybeard"
              sp_human
              oc_wizard
              s_companion_wizard
              3
              20 30 22
              0 1
              10 5
              0 0
              0 0
              39 0
              240 8
              nil
              nil
              nil
              (list
                t_rpg
              )
              ;; hooks
              (list
              )
            )
          23 26)
          (list
            (bind
              (kern-mk-obj tf_ew_bridge 1)
              '()
            ) ;; bind
          25 26)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          9 16)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          11 15)
          (list
            (bind
              (kern-mk-obj t_door 1)
              (list
                #f
                0
                '()
                #f
                #f
                #f
              )
            ) ;; bind
          9 19)
        ) ;; end of objects
        (list
        )
      ) ;; end of place p_gregors_hut

    3 17) ;; coords of p_gregors_hut
    (list
      p_green_tower
    35 23) ;; coords of p_green_tower
  ) ; end of subplaces
  ;; neighbors
  nil
  ;; contents
  (list
    (list
      (kern-mk-field f_sleep_perm -1)    34 24)
    (list
      (kern-mk-party t_goblin_horde 4
        (kern-mk-vehicle t_ship 1 100)
      )
    3 28)
    (list
      (kern-mk-party t_skeleton_brigade 4
        nil
      )
    25 19)
    (list
      (kern-tag 'mg-2
        (bind
          (kern-mk-obj t_moongate 1)
          (list
            'ord
            #f
            '()
            #f
            #f
          )
        ) ;; bind
      ) ;; kern-tag
    35 25)
    (list
      (kern-mk-obj t_goblin_generator 1)
    20 20)
    (list
      (kern-mk-field f_poison 19)    34 22)
    (list
      (kern-tag 'mg-1
        (bind
          (kern-mk-obj t_moongate 1)
          (list
            'ord
            #f
            '()
            #f
            #f
          )
        ) ;; bind
      ) ;; kern-tag
    32 23)
    (list
      (kern-mk-vehicle t_ship 1 100)
    36 26)
    (list
      (kern-mk-field f_fire 19)    34 23)
    (list
      (kern-mk-party t_slime_glob 4
        nil
      )
    36 20)
  ) ;; end of objects
  (list
  )
) ;; end of place p_wilderness

(kern-mk-player
  'player
  s_companion_fighter
  "Walk"
  "sounds/walk.wav"
  1000 500 3
  nil
  m_campsite
  nil
  nil ; player's vehicle
  (kern-mk-container
    nil
    ;; trap
    nil
    ;; contents
    (list
      (list 1 t_rpg)
      (list 1 the-goblin-lexicon)
      (list 1 t_poison_immunity_potion)
      (list 10 mandrake)
      (list 10 nightshade)
      (list 10 blood_moss)
      (list 10 sulphorous_ash)
      (list 10 black_pearl)
      (list 10 spider_silk)
      (list 10 ginseng)
      (list 10 garlic)
      (list 25 t_oil)
      (list 100 t_arrow)
      (list 1 t_dagger)
      (list 1 in_vas_por_ylem)
      (list 1 in_flam_hur)
      (list 1 sanct_lor)
      (list 3 in_sanct)
      (list 3 vas_lor)
      (list 10 in_lor)
      (list 1 in_zu)
      (list 2 an_xen_exe)
      (list 2 sanct)
      (list 2 an_sanct)
      (list 2 in_nox_por)
      (list 2 kal_xen)
      (list 2 an_xen_corp)
      (list 10 vas_rel_por)
      (list 10 test_recur)
    )
  )
  (list
    ch_thorald_greybeard
    ch_slurmok
  )
)
;;--------------
;; Miscellaneous
;;--------------
(kern-set-frame s_frame_ulc s_frame_urc s_frame_llc s_frame_lrc s_frame_td s_frame_tu s_frame_tl s_frame_tr s_null s_frame_horz s_frame_vert s_frame_endl s_frame_endr)
(kern-set-cursor ls_whirlpool)
(kern-set-crosshair t_crosshair)
(kern-set-ascii ss_little_sprites 32)
(kern-set-clock 0 0 0 0 12 48)
;; ---------
;; Astronomy
;; ---------
(kern-mk-astral-body
  'sun	; tag
  "Fyer (the sun)"	; name
  1	; distance
  1	; minutes_per_phase
  4	; minutes_per_degress
  0	; initial_arc
  0	; initial_phase
  nil	; gifc
  (list
    (list s_sun 255 "full")
  )
)
(bind-astral-body
  (kern-mk-astral-body
    'lumis	; tag
    "Lumis"	; name
    0	; distance
    120	; minutes_per_phase
    4	; minutes_per_degress
    90	; initial_arc
    0	; initial_phase
    'source-moon-ifc
    (list
      (list s_new_moon 0 "new")
      (list s_wax_quarter_moon 32 "1/4 waxing")
      (list s_wax_half_moon 64 "1/2 waxing")
      (list s_wax_three_quarter_moon 96 "3/4 waxing")
      (list s_full_moon 128 "full")
      (list s_wane_three_quarter_moon 96 "3/4 waning")
      (list s_wane_half_moon 64 "1/2 waning")
      (list s_wane_quarter_moon 32 "1/4 waning")
    )
  )
  (list
    'mg-1
    'mg-2
    'mg-3
    'mg-4
    'mg-1
    'mg-2
    'mg-3
    'mg-4
  )
) ;; bind-astral-body
(bind-astral-body
  (kern-mk-astral-body
    'ord	; tag
    "Ord"	; name
    0	; distance
    60	; minutes_per_phase
    2	; minutes_per_degress
    180	; initial_arc
    1	; initial_phase
    nil	; gifc
    (list
      (list s_new_moon 0 "new")
      (list s_wax_quarter_moon 32 "1/4 waxing")
      (list s_wax_half_moon 64 "1/2 waxing")
      (list s_wax_three_quarter_moon 96 "3/4 waxing")
      (list s_full_moon 128 "full")
      (list s_wane_three_quarter_moon 96 "3/4 waning")
      (list s_wane_half_moon 64 "1/2 waning")
      (list s_wane_quarter_moon 32 "1/4 waning")
    )
  )
  (list
    'mg-1
    'mg-2
    'mg-3
    'mg-4
    'mg-1
    'mg-2
    'mg-3
    'mg-4
  )
) ;; bind-astral-body
(kern-add-reveal 0)
(kern-add-quicken 0)
(kern-add-time-stop 0)
(kern-add-magic-negated 0)
(kern-add-xray-vision 0)

;; ----------------------------------------------------------------------------
;; The diplomacy table. Each entry defines the attitude of the row to the
;; column. Note that attitudes are not necessarily symmetric. Negative values
;; are hostile, positive are friendly.
;; ----------------------------------------------------------------------------
(kern-mk-dtable
 ;;    none play men orks accu
 (list  0    0    0   0   -1   ) ;; none
 (list  0    2    1   0   -1   ) ;; play
 (list -1    1    2  -1   -2   ) ;; men
 (list -1    0   -1   2   -1   ) ;; orks
 (list -1   -1   -1  -1    2   ) ;; accu
 )
