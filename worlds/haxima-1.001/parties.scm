;; ----------------------------------------------------------------------------
;; Parties
;;
;; These are used to generate random wilderness encounters.
;; ----------------------------------------------------------------------------

(kern-mk-party-type
 't_goblin_horde     ;; tag
 "horde of goblins"  ;; name
 s_orc               ;; sprite
 nil                 ;; formation
 (list               ;; member characters
  (list 
   sp_goblin         ;; species
   oc_raider         ;; occupation
   s_orc             ;; sprite
   "1d6"             ;; number
   nil               ;; special ai
   )
  )
 )

(kern-mk-party-type
 't_skeleton_brigade    ;; tag
 "brigade of skeletons" ;; name
 s_skeleton             ;; sprite
 nil                    ;; formation
 (list                  ;; member characters
  (list 
   sp_skeleton          ;; species
   oc_raider            ;; occupation
   s_skeleton           ;; sprite
   "1d8"                ;; number
   nil                  ;; special ai
   )
  )
 )
 
(kern-mk-party-type
 't_slime_glob
 "glob of slimes"
 s_yellow_slime
 nil
 (list
  (list
   sp_yellow_slime
   nil
   s_yellow_slime
   "1d2"
   'yellow-slime-ai
   )))

(kern-mk-party-type
 't_wood_spiders      ;; tag
 "clutter of spiders" ;; name
 s_spider             ;; sprite
 nil                  ;; formation
 (list                ;; members
  (list sp_spider nil s_spider "1d4" 'spider-ai)
  ))
 
(kern-mk-party-type
 't_queen_wood_spiders ;; tag
 "clutter of spiders"  ;; name
 s_spider              ;; sprite
 nil                   ;; formation
 (list                 ;; members
  (list sp_spider nil s_spider "1d4" 'spider-ai)
  (list sp_queen_spider nil s_queen_spider "1" 'spider-ai)
  ))
 