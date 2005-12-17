(kern-mk-map 
 'm_ankh_shrine 31 31 pal_expanded
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr ** ** ** ** ** ** ** ** ** rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr ** ** ** ** ** ** ** ** ** ** ** rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr ** ** xx xx xx xx xx xx xx ** ** rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr ** ** xx pp ,, ,, ,, pp xx ** ** rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr ** ** xx ,, cc cc cc ,, xx ** ** rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr ** ** xx ,, cc cc cc ,, xx ** ** rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr ** ** xx ,, cc cc cc ,, xx ** ** rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr ** ** xx pp ,, cc ,, pp xx ** ** rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr ** ** xx xx ,, cc ,, xx xx ** ** rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr ** ** ** ** ,, cc ,, ** ** ** ** rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr ** ** ** ,, cc ,, ** ** ** rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr ** ** ,, cc ,, ** ** rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr ** ** ,, cc ,, ** ** rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr ** ** ,, cc ,, ** ** rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr ** ** ,, cc ,, ** ** rr rr rr rr rr rr rr rr rr rr rr rr rr "
  ))


;;----------------------------------------------------------------------------
;; Include files
;;----------------------------------------------------------------------------
(kern-load "talking-ankh.scm")
(kern-load "demon-gate.scm")

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place
 'p_ankh_shrine     ; tag
 "Ankh Shrine"      ; name
 s_shrine      ; sprite
 m_ankh_shrine      ; map
 #f              ; wraps
 #f              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 (list ; objects
  (put (mk-talking-ankh) 14 21)
  (put (mk-demon-gate) 0 0)
  )
  nil ; hooks
 (list  ;; edge entrances
  (list east  14 30)
  (list south 14 30) 
  (list north 14 30)
  (list west  14 30)
  )
 )
