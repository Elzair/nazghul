;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_traps_3 19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx xx xx ,, x! xx xx xx x! ,, x! xx xx xx x! ,, xx xx xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx xx xx ,, x! xx xx xx x! ,, x! xx xx xx x! ,, xx xx xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
    )
  )
;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Special Objects
;;----------------------------------------------------------------------------
(kern-load "traps_3_mechs.scm")

;;----------------------------------------------------------------------------
;; Other dungeon rooms
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_traps_3 ; tag
 "The Thief's Ladder III"   ; name
 nil              ; sprite
 m_traps_3  ; map
 #f               ; wraps
 #t                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 nil ;; neighbors 
 ;; objects
 (list

  ;; monster generators
  (put (mk-mongen2 980 1 'is-spider? 'mk-queen-spider nil) 6 3)
  (put (mk-mongen2 980 3 'is-skeleton? 'mk-skeleton nil)   6 15)
  (put (mk-mongen2 980 1 'is-yellow-slime? 'mk-yellow-slime nil) 15 15)
  (put (mk-mongen2 980 3 'is-bandit? 'mk-bandit nil) 15 3)
  
  ;; portcullisses
  (put (kern-tag 't3_pc1  (mk-open-portcullis) ) 6 3)
  (put (kern-tag 't3_pc2  (mk-open-portcullis) ) 6 9)
  (put (kern-tag 't3_pc3  (mk-open-portcullis) ) 6 15)
  (put (kern-tag 't3_pc4  (mk-open-portcullis) ) 12 3)
  (put (kern-tag 't3_pc5  (mk-open-portcullis) ) 12 9)
  (put (kern-tag 't3_pc6  (mk-open-portcullis) ) 12 15)
  (put (kern-tag 't3_pc7  (mk-portcullis) ) 3 6)
  (put (kern-tag 't3_pc8  (mk-portcullis) ) 9 6)
  (put (kern-tag 't3_pc9  (mk-portcullis) ) 15 6)
  (put (kern-tag 't3_pc10 (mk-portcullis) ) 3 12)
  (put (kern-tag 't3_pc11 (mk-portcullis) ) 9 12)
  (put (kern-tag 't3_pc12 (mk-portcullis) ) 15 12)

  ;; levers
  (put (mk-lever-with-id 't3_ctrl 0)  3  3)
  (put (mk-lever-with-id 't3_ctrl 1) 15  3)
  (put (mk-lever-with-id 't3_ctrl 2)  3  9)
  (put (mk-lever-with-id 't3_ctrl 3) 15  9)
  (put (mk-lever-with-id 't3_ctrl 4)  3 15)
  (put (mk-lever-with-id 't3_ctrl 5)  9 15)
  (put (mk-lever-with-id 't3_ctrl 6) 15 15)

  ;; special control mechanism
  (put (kern-tag 't3_ctrl 
                 (mk-t3-ctrl (list 't3_pc1
                                   't3_pc2
                                   't3_pc3
                                   't3_pc4
                                   't3_pc5
                                   't3_pc6
                                   't3_pc7
                                   't3_pc8
                                   't3_pc9
                                   't3_pc10
                                   't3_pc11
                                   't3_pc12)
                             (list (list #f #t #f #t #f #t #t #f #t #f #t #f)
                                   (list #t #t #t #t #t #t #f #f #f #f #f #f)
                                   (list #t #f #t #f #t #f #f #t #f #t #f #t)
                                   (list #f #t #f #t #f #t #f #t #f #t #f #t)
                                   (list #f #f #f #f #f #f #t #t #t #t #t #t)
                                   (list #t #f #t #f #t #f #t #f #t #f #t #f)
                                   (list #f #f #t #t #t #t #t #t #t #t #f #f)
                                   ))) 0 0)


  ;; ladder back up
  (put (mk-ladder-up 'p_traps_2 9 9) 9 9)

  )
 nil ; hooks
 nil ; edge entrances
 )
