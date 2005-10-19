
;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(kern-mk-map
 'm_prison 32 32 pal_expanded
 (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx ,, ,, ,, ,, ,, ,, ,, 00 .. .. .. 00 ee ,, ,, ee 00 .. .. .. 00 ,, ,, ,, 00 !! cc !! 00 ee xx "
      "xx ,, -- -- -- -- ,, ,, 00 .. .. .. 00 ,, ,, ,, ,, 00 .. .. .. 00 ,, ,, ,, 00 !! cc !! 00 ,, xx "
      "xx ,, == == ee -- ,, ,, 00 00 ,, 00 00 ,, ,, ,, ,, 00 00 ,, 00 00 ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, -- -- -- -- ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, 00 ff cc ff 00 ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, aa ,, ,, ,, ,, ,, ,, ,, ,, aa ,, ,, ,, ,, ,, 00 ff cc ff 00 ee xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx ,, xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx [[ .T .O .R .T .U .R .E ]] xx ,, xx [[ .C .H .A .M .B .E .R ]] xx xx xx xx xx xx xx "
      "xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx "
      "xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx ,, ,, pp ,, pp ,, ,, xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx !! -- ,, ,, ,, ,, ,, ,, ,, -- !! xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx -- -- ,, ,, pp ,, pp ,, ,, -- -- xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx !! -- ,, ,, ,, ,, ,, ,, ,, -- !! xx xx xx xx xx xx xx xx ,, ,, ,, xx "
      "xx xx xx xx xx xx xx xx xx -- -- ,, ,, pp ,, pp ,, ,, -- -- xx xx xx xx xx xx xx xx xx xx ,, xx "
      "xx xx xx xx xx xx xx xx xx !! -- ,, ,, ,, ,, ,, ,, ,, -- !! xx xx xx xx xx xx xx xx xx xx ,, xx "
      "xx xx xx xx xx xx xx xx xx xx xx ,, ,, pp ,, pp ,, ,, xx xx xx xx xx xx xx xx xx xx xx xx ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx xx xx xx xx xx [[ .P .R .I .S .O .N @@ .C .E .L .L .S ]] xx ,, ,, xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ,, ,, xx xx xx xx xx xx xx xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx xx "
      "xx xx ,, xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx ,, xx xx xx xx ,, xx ,, xx ,, xx xx xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx xx xx xx xx xx "
      "xx ,, ,, .. ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx xx xx xx xx xx "
      "xx x! ,, xx xx xx xx xx xx xx xx xx x! ee xx xx ,, xx xx xx ,, xx xx xx ,, xx xx xx ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ee ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx x! .. xx xx xx xx xx xx xx xx xx x! ee xx xx ,, xx xx xx ,, xx xx xx ,, xx xx xx ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx xx xx xx xx xx "
      "xx ,, xx ,, xx xx xx ,, xx ,, xx ,, xx xx xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
))

;;----------------------------------------------------------------------------
;; NPC's
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place
 'p_prison     ; tag
 "Prison"      ; name
 s_hamlet      ; sprite
 m_prison      ; map
 #f              ; wraps
 #t              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 (list ; objects
  (put (mk-ladder-up 'p_glasdrin 49 34) 28 14)

  ;; cell doors
  (put (kern-tag 'pp1 (mk-portcullis)) 12 27)
  (put (kern-tag 'pp2 (mk-portcullis)) 14 27)
  (put (kern-tag 'pp3 (mk-portcullis)) 16 26)
  (put (kern-tag 'pp4 (mk-portcullis)) 20 26)
  (put (kern-tag 'pp5 (mk-portcullis)) 24 26)
  (put (kern-tag 'pp6 (mk-portcullis)) 16 28)
  (put (kern-tag 'pp7 (mk-portcullis)) 20 28)
  (put (kern-tag 'pp8 (mk-portcullis)) 24 28)

  ;; levers
  (put (mk-lever 'pp1) 1 30)
  (put (mk-lever 'pp2) 3 30)
  (put (mk-lever 'pp3) 7 24)
  (put (mk-lever 'pp4) 9 24)
  (put (mk-lever 'pp5) 11 24)
  (put (mk-lever 'pp6) 7 30)
  (put (mk-lever 'pp7) 9 30)
  (put (mk-lever 'pp8) 11 30)

  ;; doors
  (put (mk-door) 2 23)
  (put (mk-locked-door) 6 25)
  (put (mk-locked-door) 6 29)
  (put (mk-locked-door) 27 27)

  (put (kern-mk-obj F_energy_perm 1) 26 27)

  )

 nil ; hooks
 (list  ;; edge entrances
  (list east  0 9)
  (list south 9 0) 
  (list north 9 18)
  (list west  18 9)
  )
 )

