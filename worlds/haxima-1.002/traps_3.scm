;; ----------------------------------------------------------------------------
;; Level 3 of the Thief's Ladder
;; ----------------------------------------------------------------------------
(kern-load "traps_3_mechs.scm")
(mk-dungeon-room
 'p_traps_3 "Fun with Levers"
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

 ;; monster generators
 (put (spawn-pt 'queen-spider) 6 3)
 (put (spawn-pt 'skeletal-warrior)   6 15)
 (put (spawn-pt 'yellow-slime) 15 15)
 (put (spawn-pt 'bandit) 15 3)
 
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
 (put (mk-ladder-down 'p_traps_4 9 9) 9 3)

 )

(mk-place-music p_traps_3 'ml-dungeon-adventure)

(kern-place-add-on-entry-hook p_traps_3 'quest-thiefrune-den3)
