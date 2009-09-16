;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define tooth-lvl 2)
(define tooth-species sp_rat)
(define tooth-occ oc_wrogue)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In the monster village of Kun.
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_tooth
               (list 0 0 campfire-4 "sleeping")
               (list 6 0 black-market-counter "working")
               (list 19 0 cantina-12 "idle")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (tooth-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Tooth is a nervous ratling, running a black market and pawn shop 
;; in the monster village of Kun.  It appears that he is suffering from 
;; the surplus (or lack) of some stimulant.
;;----------------------------------------------------------------------------

;; Basics...
(define (tooth-hail knpc kpc)
  (kern-log-msg "You meet a shifty old rat with one enormous incisor. He shakes and shivers nervously. "
                "Too much caffeine? Black lotus withdrawal? You aren't sure.")
  (say knpc "Psst. Looking for a deal? I got deals. All kinds of deals! Good deals! Bad deals! "
       "You name it, you make it, it's your call! No questions asked. No answers given.")
  )

(define (tooth-default knpc kpc)
  (say knpc "Don't know. Don't know about that. Information? Not my game.")
  )

(define (tooth-name knpc kpc)
  (say knpc "Tooth. I'm well-known! Well-respected. My customers love me. "
       "They always come back. Tell their friends about me? No way! "
       "Deals are too good. They keep it to themselves. I'm a well-kept secret! A well-known secret!")
  )

(define (tooth-join knpc kpc)
  (say knpc "Can't, mate. Got my shop to run. Bills to pay. Guards to dodge. Too busy! Need the action!")
  )

(define (tooth-job knpc kpc)
  (say knpc "Deals! Deals! Deals! Deals! Deals! Let's make one. Right now. "
       "Right here. Let's do it. Ready? Here we go.")
  (tooth-trade knpc kpc)
  )

(define (tooth-bye knpc kpc)
  (say knpc "Going so soon? Leaving already? I've got more stuff! Lots more! "
       "Good stuff! But it won't stay! No! It flies off the shelves! Leave at your own risk...!"
       "[He goes on like this as you walk away]")
  )

(define tooth-merch-msgs
  (list nil ;; closed
        "Look here, look at this marvelous stuff! All quality! Rare, valuable, hard-to-find!" ;; buy
        "Got stuff? I'll take it. Where did you get it? I don't want to know." ;; sell
        "Everything reasonably priced! How do I stay in business? I don't know! See for yourself!" ;; trade
        "Is that all? I've got more!" ;; sold-something
        "Wait! Where you going? Come back, have another look! Did you see my collection of scrolls?" ;; sold-nothing
        "Come back when you have more! I'm always looking!" ;; bought-something
        "I need the good stuff! Magical stuff, gems, artifacts!" ;; bought-nothing
        "Don't forget the basics! Oil, gems, picklocks!" ;; traded-something
        "Wait! Where you going? Come back, have another look! Did you see my collection of scrolls?" ;; traded-nothing
        ))

(define tooth-catalog
  (list
   (list t_picklock            5 "Unlock doors! Takes a bit of skill!")
   (list t_gem                20 "Find hidden rooms and passages! Thieves love 'em!")
   (list t_grease             25 "In a tight squeeze? Grease is the word!")

   (list t_oil                 5 "Perfect for fighting slimes!")
   (list t_slime_vial         30 "Kal Xen Nox in a bottle! Great for escaping the guards!")
   (list t_arrow               3 "Gotta have lots of arrows!")
   (list t_bolt                3 "Can't have too many bolts!")
   (list t_smoke_bomb          4 "Make a clean getaway under cover!")

   (list t_spiked_helm       300 "Use your head in a fight!")
   (list t_spiked_shield     300 "Rare item! For aggressive fighters!")

   (list t_dagger_4           (* 4 65) "Very nice! Small, easy to conceal, but with a fierce bite!")
   (list t_sword_2            (* 2 85) "Better than the average sword! Look at that keen edge!")
   (list t_sword_4            (* 4 85) "The finest! The very best! A weapon for the true artist of death!")
   (list t_morning_star_2     (* 2 105) "A truly awesome weapon! Lay waste to whole parties!")

   (list t_leather_helm_2     (* 2 100) "For the wrogue who needs that little bit of extra protection!")
   (list t_chain_coif_4       (* 4 100) "See that tight weave? The banded reinforcement? Turns any blade and softens the blow!")
   (list t_iron_helm_4        (* 4 150) "Very rare! Blows to the head will feel like pillows!")

   (list t_armor_leather_2    (* 2 150) "Want to pilfer a Gint armory? Steal gems from trolls? You'll need something light but effective like this!")
   (list t_armor_leather_4    (* 4 150) "This once belonged to an old wrogue! He died of old age on a bed of gold coins! Now you can, too!")
   (list t_armor_chain_4      (* 4 330) "This is perfect for the scrapper who likes to be in the thick without getting slowed down!")
   (list t_armor_plate_4      (* 4 660) "Shrug off the blows of gints and trolls in this amazing plate armour! There's nothing like it anywhere else!")

   (list t_xen_corp_scroll    (* 7 base-scroll-cost) "The assassin's favorite! Kills instantly!")
   (list t_sanct_lor_scroll   (* 7 base-scroll-cost) "The wrogue's favorite! Perfect for sneaking in and out unseen!")
   (list t_an_xen_ex_scroll   (* 6 base-scroll-cost) "If you can't beat them, have them join you with this charm spell!")
   (list t_in_ex_por_scroll   (* 4 base-scroll-cost) "Magic doors won't stop you when you carry plenty of these!")
   (list t_wis_quas_scroll    (* 4 base-scroll-cost) "Perfect for finding hidden doors (and invisible foes)!")
   (list t_in_quas_xen_scroll (* 7 base-scroll-cost) "Ever wish there was two of you? With this scroll there can be!")
   (list t_an_tym_scroll      (* 8 base-scroll-cost) "For when you really get into trouble this scroll will stop time and let you get out!")
   ))

(define (tooth-trade knpc kpc) (conv-trade knpc kpc "trade" tooth-merch-msgs tooth-catalog))
(define (tooth-buy   knpc kpc) (conv-trade knpc kpc "buy"   tooth-merch-msgs tooth-catalog))
(define (tooth-sell  knpc kpc) (conv-trade knpc kpc "sell"  tooth-merch-msgs tooth-catalog))

(define tooth-conv
  (ifc nil

       ;; basics
       (method 'default tooth-default)
       (method 'hail tooth-hail)
       (method 'bye  tooth-bye)
       (method 'job  tooth-job)
       (method 'name tooth-name)
       (method 'join tooth-join)

       (method 'trad tooth-trade)
       (method 'buy  tooth-buy)
       (method 'sell tooth-sell)
       (method 'deal tooth-trade)
       ))

(define (mk-tooth)
  (bind 
   (kern-mk-char 
    'ch_tooth           ; tag
    "Tooth"             ; name
    tooth-species         ; species
    tooth-occ              ; occ
    s_rat     ; sprite
    faction-men      ; starting alignment
    0 4 1            ; str/int/dex
    0  ; hp bonus
    0 ; hp per-level bonus
    0 ; mp off
    1 ; mp gain
    max-health ; hp
    -1                  ; xp
    max-health ; mp
    0
    tooth-lvl
    #f               ; dead
    'tooth-conv         ; conv
    sch_tooth           ; sched
    'townsman-ai              ; special ai
    nil
    nil              ; readied
    )
   (tooth-mk)))
