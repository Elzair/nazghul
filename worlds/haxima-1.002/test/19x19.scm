;;----------------------------------------------------------------------------
;; A basic 19x19 place for testing.
;;
;; This will just fill the map viewer for the standard setting.
;;----------------------------------------------------------------------------

(kern-mk-place 
 'p_19x19 ; tag
 "19x19 test place" ; name
 s_shrine ; sprite (for appearance in wilderness)
 (kern-mk-map 
  'm_char_setup 19 19 pal_expanded
  (list
      "xx xx xx xx xx xx x! xx xx xx xx xx x! xx xx xx xx xx xx "
      "x! ,, ,, ,, ,, ,, ,, ,, !! !! !! ,, ,, ws ,, ,, ,, ,, x! "
      "xx ,, ,, ,, ,, ,, ,, ,, !! !! !! ,, ,, xx ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, !! !! !! ,, ,, xx ws ws ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, == == == ,, ,, xx ,, ws ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, == == == ,, ,, ws ws ws ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, == == == ,, ,, xx ws xx xx ws xx "
      "xx ,, ,, ,, ,, ,, ,, ,, !! !! !! ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, !! !! !! ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, !! !! !! ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, !! !! !! ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, !! !! !! ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, !! !! !! ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, !! !! !! ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, !! !! !! ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, !! !! !! ,, ,, ,, ,, ,, ,, ,, xx "
      "x! ,, ,, ,, ,, ,, ,, ,, !! !! !! ,, ,, ,, ,, ,, ,, ,, x! "
      "xx ,, ,, ,, ,, ,, ,, ,, !! !! !! ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx xx xx xx xx x! xx xx xx xx xx x! xx xx xx xx xx xx "
   ))
 #f ; wrapping
 #f ; underground (no sunlight)
 #f ; wilderness (larger scale)
 #f ; wilderness combat (temp map)
 nil ; subplaces
 nil ; neighbors
 
 ;; *** contents of the place ***
 (list)
 nil ;; hooks
 nil
 )
