;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_void_temple 19 19 pal_expanded
    (list
      "cc cc cc cc cc cc cc .. .. .. .. .. cc cc cc cc cc cc cc "
      "cc cc cc cc cc cc cc cc .. .. .. cc cc cc cc cc cc cc cc "
      "cc cc tt tt tt ** ** ** bb .. bb ** ** ** tt tt tt cc cc "
      "cc cc tt tt ** ** ** ** ** cc ** ** ** ** ** tt tt cc cc "
      "cc cc tt ** ** ** ** ** ** cc ** ** ** ** ** ** tt cc cc "
      "cc cc ** ** ** ** ** ** ** cc ** ** ** ** ** ** ** cc cc "
      "cc cc ** ** ** ** ** ** bb .. bb ** ** ** ** ** ** cc cc "
      ".. cc ** ** ** ** ** .. .. .. .. .. ** ** ** ** ** cc .. "
      ".. .. bb ** ** ** bb .. .. .. .. .. bb ** ** ** bb .. .. "
      ".. .. .. cc cc cc .. .. .. .. .. .. .. cc cc cc .. .. .. "
      ".. .. bb ** ** ** bb .. .. .. .. .. bb ** ** ** bb .. .. "
      ".. cc ** ** ** ** ** .. .. .. .. .. ** ** ** ** ** cc .. "
      "cc cc ** ** ** ** ** ** bb .. bb ** ** ** ** ** ** cc cc "
      "cc cc ** ** ** ** ** ** ** cc ** ** ** ** ** ** ** cc cc "
      "cc cc tt ** ** ** ** ** ** cc ** ** ** ** ** ** tt cc cc "
      "cc cc tt tt ** ** ** ** ** cc ** ** ** ** ** tt tt cc cc "
      "cc cc tt tt tt ** ** ** bb .. bb ** ** ** tt tt tt cc cc "
      "cc cc cc cc cc cc cc cc .. .. .. cc cc cc cc cc cc cc cc "
      "cc cc cc cc cc cc cc .. .. .. .. .. cc cc cc cc cc cc cc "
    )
  )
;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Special Objects
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_void_temple ; tag
 "Void Temple"   ; name
 s_shrine              ; sprite
 m_void_temple  ; map
 #f               ; wraps
 #f                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 
 ;; neighbors
 (list
  )
 
 ;; objects
 (list
  (put (kern-mk-obj t_rune_d 1) 9 9)

  (put (kern-mk-obj F_sleep_perm 1)  15 9)
  (put (kern-mk-obj F_fire_perm 1)   14 9)
  (put (kern-mk-obj F_energy_perm 1) 13 9)

  (put (kern-mk-obj F_sleep_perm 1)  3 9)
  (put (kern-mk-obj F_fire_perm 1)   4 9)
  (put (kern-mk-obj F_energy_perm 1) 5 9)

  (put (kern-mk-obj F_sleep_perm 1)  9 3)
  (put (kern-mk-obj F_fire_perm 1)   9 4)
  (put (kern-mk-obj F_energy_perm 1) 9 5)

  (put (kern-mk-obj F_sleep_perm 1)  9 15)
  (put (kern-mk-obj F_fire_perm 1)   9 14)
  (put (kern-mk-obj F_energy_perm 1) 9 13)

  )

 nil ; hooks
 nil ; edge entrances
 )
