;;----------------------------------------------------------------------------
;; Gregor
;;
;; Gregor is one of the first characters the player will meet. He starts out
;; in the moongate clearing in the shrine room.
;;----------------------------------------------------------------------------
(kern-load "gregor.scm")
(bind 
 (kern-mk-char 'ch_gregor ; tag
               "Gregor"              ; name
               sp_human            ; species
               nil                 ; occ
               s_townsman          ; sprite
               faction-men         ; starting alignment
               0 10 5              ; str/int/dex
               0 0                 ; hp mod/mult
               0 0                 ; mp mod/mult
               30 0 9 9            ; hp/xp/mp/lvl
               'gregor-conv        ; conv
               nil ;sch_gregor     ; sched
               nil                 ; special ai
               nil                 ; container
               nil                 ; readied
               )
 (gregor-mk #f #f))

;;-----------------------------------------------------------------------------
;; Make a chest with some items to get the player started. This will be dropped
;; on the map in the shrine room.
;;-----------------------------------------------------------------------------
(define warchest
  (kern-mk-container
   t_small_wooden_chest ;; type
   nil ;; trap

   (list

    ;; Food
    (list 10 t_mushroom)

    ;; Reagents
    (list 10 garlic)
    (list 10 ginseng)
    (list 10 sulphorous_ash)

    ;; Arms
    (list 1 short-sword)
    (list 1 wooden-buckler)

    ;; Items
    (list 3 heal-potion)
    (list 3 cure-poison-potion)
    (list 2 poison-bolt-scroll-type)
    (list 1 death-bolt-scroll-type)

    ;; Hints/instructions
    (list 1 basic-survival-manual)
    )
   ))

;;----------------------------------------------------------------------------
;; Moongate Clearing
;;
;; This is where the player starts out.
;;----------------------------------------------------------------------------
(kern-mk-place 'p_moongate_clearing "Moongate Clearing"
  s_shrine ;; sprite
  (kern-mk-map 'm_moongate_clearing 23 28 pal_expanded
    (list
    "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ tt tt tt tt tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ";  //  0
    "^^ {{ {{ {{ ^^ ^^ ^^ ^^ tt tt tt || || ^^ ^^ ^^ {{ {{ {{ ^^ {{ {{ ^^ ";  //  1
    "^^ {{ {{ {{ {{ ^^ tt tt tt || tt tt || || ^^ ^^ {{ ^^ {{ ^^ ^^ {{ ^^ ";  //  2
    "^^ {{ {{ {{ {{ {{ tt || || || || tt || || || ^^ || ^^ {{ {{ ^^ {{ ^^ ";  //  3
    "^^ ^^ {{ {{ {{ tt || || || || tt tt tt || || || || ^^ ^^ {{ {{ {{ ^^ ";  //  4
    "^^ ^^ ^^ ^^ tt tt || || || tt tt tt tt tt tt || || || ^^ ^^ {{ ^^ ^^ ";  //  5
    "^^ ^^ ^^ ^^ || || || || tt tt tt b tt tt tt || || || || ^^ ^^ ^^ ^^ ";  //  6
    "^^ ^^ ^^ ^^ || || || tt tt b .. .. .. b tt tt || || || || ^^ ^^ || ";  //  7
    "^^ ^^ ^^ ^^ || || tt tt tt tt .. .. .. tt tt tt tt tt || || || || || ";  //  8
    "^^ ^^ || || || || tt tt b .. .. .. .. .. b tt tt tt tt tt tt tt tt ";  //  9
    "^^ || || || || || tt tt .. .. .. .. .. .. .. tt tt tt tt tt tt tt tt ";  // 10
    "{{ || || || || || tt b .. .. .. .. .. .. .. b tt tt tt tt tt tt {{ ";  // 11
    "{{ {{ || || || tt tt tt .. .. .. .. .. .. .. tt tt tt {{ {{ tt {{ {{ ";  // 12
    "^^ {{ {{ {{ {{ tt tt tt b .. .. .. .. .. b tt tt {{ {{ {{ {{ {{ {{ ";  // 13
    "^^ ^^ ^^ {{ {{ {{ tt tt tt tt .. .. .. tt tt tt {{ ^^ ^^ {{ {{ ^^ ^^ ";  // 14
    "^^ ^^ ^^ ^^ {{ {{ {{ tt tt b .. .. .. b tt tt {{ ^^ ^^ ^^ {{ {{ ^^ ";  // 15
    "^^ ^^ .. ^^ ^^ ^^ {{ {{ tt tt .. .. .. tt tt tt tt {{ ^^ ^^ {{ {{ ^^ ";  // 16
    "^^ .. .. .. .. ^^ {{ {{ tt b .. .. .. b tt tt {{ {{ {{ {{ {{ ^^ ^^ ";  // 17
    "^^ .. .. .. .. ^^ {{ {{ tt tt .. .. .. tt tt tt tt tt tt {{ ^^ ^^ ^^ ";  // 18
    "^^ .. .. .. .. ^^ {{ {{ tt b .. .. .. b tt b tt b tt tt ^^ ^^ ^^ ";  // 19
    "^^ ^^ .. ^^ ^^ ^^ {{ {{ tt tt .. .. .. .. .. .. .. .. tt tt ^^ ^^ ^^ ";  // 20
    "^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ tt b .. .. .. .. .. .. .. .. b tt ^^ ^^ ^^ ";  // 21
    "^^ ^^ ,H ,A ,I ,L ^^ ^^ tt tt .. .. .. .. .. .. .. .. tt tt ^^ ^^ ^^ ";  // 22
    "^^ .. .. .. .. .. .. ^^ tt tt b .. b tt b .. .. .. b tt tt ^^ ^^ ";  // 23
    "^^ .. .. .. .. .. .. .. .. .. .. .. tt tt tt .. .. .. tt tt tt tt ^^ ";  // 24
    "^^ .. .. .. .. .. .. ^^ tt || tt tt tt tt b .. .. .. b tt tt tt tt ";  // 25
    "^^ ,S ,E ,E ,K ,E ,R ^^ || || || tt tt tt .. .. .. .. .. tt tt tt tt ";  // 26
    "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ tt || tt tt tt b .. .. .. .. .. b tt tt tt ";  // 27
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  ;; neighbors
  nil

  ;; *** contents of the place ***
  (list
   (list (kern-tag 'black-gate (mk-moongate nil)) 11 11)
   (list ch_gregor 1 23)
   (list warchest 1 24)
   )

  nil ;; hooks
  (list  ;; edge entrances
   (list north 16 27)
   (list east  0  11)
   (list west 22 10)
   )
) ;; end of place p_moongate_clearing

;;----------------------------------------------------------------------------
;; Startup - this is a one-time only script that runs when the player starts
;; the game for the first time (or whenever he starts over from scratch,
;; loading the game from this file). It sets up the story a bit.
;;
;; The camera should center on the moongate clearing. Then, a gate should rise
;; from the ground, pause, then sink back down, leaving the player's sleep
;; sprite on the ground. Another pause, and then the player should wake up.
;;----------------------------------------------------------------------------
(define blackgate-stages
  (list (list '()                       0)
        (list s_moongate_quarter        32)
        (list s_moongate_half           64)
        (list s_moongate_three_quarters 96)
        (list s_moongate_full           128)))

(define (start-scene kplayer)
  (moongate-animate black-gate blackgate-stages)
  (kern-obj-put-at ch_wanderer 11 11)
  (kern-log-msg "You awaken to a quiet clearing."))

(kern-set-start-proc start-scene)
