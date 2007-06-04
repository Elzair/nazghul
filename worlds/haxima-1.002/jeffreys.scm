;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_jeff
               (list 0  0  gcj-bed      "sleeping")
               (list 7  0  gs-altar    "idle")
               (list 8  0  ghg-s6      "eating")
               (list 9  0  gc-hall     "working")
               (list 12 0  ghg-s3      "eating")
               (list 13 0  gc-train    "working")
               (list 18 0  ghg-s3      "eating")
               (list 19 0  ghg-hall    "idle")
               (list 21 0  gcj-bed      "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (jeff-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (jeff-hail knpc kpc)
  (say knpc "[You meet a splendid paladin] Well-met, sir."))

(define (jeff-default knpc kpc)
  (say knpc "I cannot help you with that."))

(define (jeff-name knpc kpc)
  (say knpc "I am Commander Jeffreys."))

(define (jeff-join knpc kpc)
  (say knpc "I already have a job."))

(define (jeff-job knpc kpc)
  (say knpc "I command the paladins of Glasdrin."))

(define (jeff-bye knpc kpc)
  (say knpc "Fare thee well."))

;; Special
(define (jeff-comm knpc kpc)
  (say knpc "I am the highest-ranking official in Glasdrin, "
       "save the Stewardess. The military arm of Glasdrin reports to me."))

(define (jeff-mili knpc kpc)
  (say knpc "Glasdrin is unbeatable on land. All citizens of Glasdrin are "
       "required to serve a tour. Since everyone has received basic training "
       "they can be called to duty in a crisis."))

(define (jeff-pala knpc kpc)
  (say knpc "The paladins of Glasdrin are the finest warriors the Shard has "
       "ever seen. Individually their skills vary, of course, but their "
       "strength comes from fighting as a unit."))

(define (jeff-skil knpc kpc)
  (say knpc "Yes, whether they be a raw recruit or the Warritrix herself, "
       "when fighting with others in a unit the paladins of Glasdrin are "
       "nigh unbeatable."))

(define (jeff-warr knpc kpc)
  (say knpc "The Warritrix is a living treasure; the most cunning, versatile "
       "warrior of the age. I've seen her beat men twice her size and "
       "slay fearsome beasts. At the moment she has been called away on an "
       "errand."))

(define (jeff-erra knpc kpc)
  (say knpc "[He looks a bit uneasy] Yes, she took a squad to the Lost Halls. "
       "It's odd that she hasn't reported back yet... "
       "Normally I would send a search party, "
       "but at the moment I haven't the troops to spare."))

(define (jeff-sear knpc kpc)
  (say knpc "[He grows exasperated] I can spare no one to search for the "
       "Warritrix! Now, if you don't mind, I'm a busy man...")
  (kern-conv-end)
  (if (is-player-party-member? ch_ini)
      (say ch_ini "Something smells rotten in Denmark. "
           "We've got to find her!"))
  )

;; Townspeople...
(define (jeff-glas knpc kpc)
  (say knpc "A mighty city, Glasdrin has never fallen to invaders."))

(define (jeff-ange knpc kpc)
  (say knpc "Angela is a most gracious, hospitable woman."))

(define (jeff-patc knpc kpc)
  (say knpc "Doc Patch is an experienced medik and head of our hospital."))

(define (jeff-stew knpc kpc)
  (say knpc "The Stewardess bears the weight of leadership well."))

(define (jeff-ini knpc kpc)
  (say knpc "Lt. Inago is a natural warrior and a fine officer."))

(define (jeff-jess knpc kpc)
  (say knpc "Jess is a cheery lass, and a welcome sight at the end of a "
       "hard day as she pours a glass."))

(define (jeff-ches knpc kpc)
  (say knpc "We miss his mighty arm in battle, but his weapons and armor "
       "serve us well, too."))

(define (jeff-lost knpc kpc)
  (say knpc "The Lost Halls are very dangerous. No place for amateurs. "
       "I'd advise you to stay away! Now good day, sir!")
  (kern-conv-end)
  (if (is-player-party-member? ch_ini)
      (say ch_ini "Don't worry. I know where to find the Lost Halls. "
           "We'll need to get a ship and sail to ["
           (loc-x lost-halls-loc) " "
           (loc-y lost-halls-loc) "]."))
  )

(define jeff-conv
  (ifc glasdrin-conv

       ;; basics
       (method 'default jeff-default)
       (method 'hail jeff-hail)
       (method 'bye jeff-bye)
       (method 'job jeff-job)
       (method 'name jeff-name)
       (method 'join jeff-join)

       (method 'comm jeff-comm)
       (method 'mili jeff-mili)
       (method 'pala jeff-pala)
       (method 'warr jeff-warr)
       (method 'erra jeff-erra)
       (method 'glas jeff-glas)
       (method 'ange jeff-ange)
       (method 'lost jeff-lost)
       (method 'patc jeff-patc)
       (method 'stew jeff-stew)
       (method 'ini jeff-ini)
       (method 'inag jeff-ini)
       (method 'jess jeff-jess)
       (method 'ches jeff-ches)
       ))

(define (mk-jeffreys)
  (bind 
   (kern-mk-char 'ch_jeffreys       ; tag
                 "Jeff"             ; name
                 sp_human            ; species
                 oc_warrior          ; occ
                 s_companion_paladin ; sprite
                 faction-men         ; starting alignment
                 2 1 1               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health speed-human-hvy-armor 5  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'jeff-conv         ; conv
                 sch_jeff           ; sched
                 'townsman-ai                 ; special ai
                 nil                 ; container
                 (list t_armor_chain
                       t_chain_coif
                       t_sword
                       ))         ; readied
   (jeff-mk)))
