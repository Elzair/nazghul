;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Oparine.
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_henry
               (list 0  0  bilge-water-bed     "sleeping")
               (list 8  0  bilge-water-counter "working")
               (list 23 0  bilge-water-bed     "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (henry-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Henry is a one-handed tavern-keeper, doing trade in Oparine.
;; He was once a mighty mariner.
;;----------------------------------------------------------------------------

;; Basics...
(define (henry-hail knpc kpc)
  (say knpc "[You meet a cheery man with a hook for one hand] "
       "Welcome, matey!"))

(define (henry-default knpc kpc)
  (say knpc "Ye be askin' the wrong salt about that!"))

(define (henry-name knpc kpc)
  (say knpc "I's Henry! Henry the barkeep."))

(define (henry-join knpc kpc)
  (say knpc "I'd join ye in a 'eartbeat, lad, but oo's goin' to keep the "
       "tankards full 'round here if I be goin'? 'Twouldn't be right!"))

(define (henry-job knpc kpc)
  (say knpc "Why I's a minister! I fills the mugs 'ith 'oly sacraments!"))

(define (henry-bye knpc kpc)
  (say knpc "Keep a weather eye!"))

(define henry-catalog
  (list
   (list t_food 5 "My famous clam chowder will warm you to your toes.")
   (list t_beer 5 "Aye, drink and be merry, fer tomorro' mornin' you'll be wishin' you were dead!")
   (list t_wine 7 "I keep some o' the fancy-shmancy stuff fer the occasional gen'leman.")
   ))

(define henry-merch-msgs
  (list "I'll serve ye when my hangover's gone."
        "Let the good times roll!"
        "I've no interest in used food or drink."
        "Now we're talkin'!"
        "Don't drink and sail!"
        "If yer just lookin' for someplace to loiter, there's a dock outside."
        "I suppose I can make stew from this."
        "Good. Fine."
        "Come back when you need a refill!"
        "If yer just lookin' for someplace to loiter, there's a dock outside."
        ))

;; Trade...
(define (henry-buy knpc kpc) (conv-trade knpc kpc "buy" henry-merch-msgs henry-catalog))
(define (henry-sell knpc kpc) (say knpc "I've no use for solicitors."))

;; Hook...
(define (henry-hook knpc kpc)
  (say knpc "Oy, lost me arm, me did, fightin' fearsome monsters of "
       "the deep."))

(define (henry-mons knpc kpc)
  (say knpc "The kraken took me arm, snapped it right off 'ith 'is nasty "
       "beak, 'e did! The kraken won't shoot from afar like the cowardly "
       "sea serpent. No, 'e likes to munch right through the decks and get "
       "in snugly!"))

(define (henry-serp knpc kpc)
  (say knpc "The sea serpent can crush a ship 'ith its coils, "
       "but it's a craven beast, and prefers to spit fireballs from afar."))

;; Townspeople...
(define (henry-opar knpc kpc)
  (say knpc "It's a fair enough place."))

(define (henry-gher knpc kpc)
  (say knpc "So ye've 'eard the legend o' Ghastly Ghertie! "
       "If ye don't mind a bit o' unsolicited advice from an old seabird, "
       "steer clear o' ghosts! 'ungry they is!")
	(quest-data-assign-once 'questentry-ghertie))

(define (henry-ghos knpc kpc)
  (say knpc "The sea is full of the dead, and Ghertie did her share o' "
       "puttin' 'em there. I've seen nights on full moons where they rise "
       "up an' prowl the waves like cats! Dreadful is the dead, "
       "an' Henry's in no 'urry to be joinin' 'em!")
	(quest-data-assign-once 'questentry-ghertie))

(define (henry-alch knpc kpc)
  (say knpc "Aye, 'e's a queer one."))

(define (henry-bart knpc kpc)
  (say knpc "[loudly] Bart's the finest shipwright on the Peninsula! "
       "[leaning in closer and more softly] And the only shipwright, too!"))

(define (henry-seaw knpc kpc)
  (say knpc "A lovely lass, but strange as a fish on land!"))

(define (henry-osca knpc kpc)
  (say knpc "A pathetic, misplaced soul, but 'e drinks 'is share."))

(define henry-conv
  (ifc basic-conv

       ;; basics
       (method 'default henry-default)
       (method 'hail henry-hail)
       (method 'bye  henry-bye)
       (method 'job  henry-job)
       (method 'name henry-name)
       (method 'join henry-join)
       
       ;; trade
       (method 'trad henry-buy)
       (method 'buy  henry-buy)
       (method 'sell henry-sell)
       (method 'sacr henry-buy)

       ;; hand
       (method 'hook henry-hook)
       (method 'hand henry-hook)
       (method 'mons henry-mons)
       (method 'deep henry-mons)
       (method 'sea  henry-serp)
       (method 'serp henry-serp)

       ;; town & people
       (method 'opar henry-opar)
       (method 'alch henry-alch)
       (method 'gher henry-gher)
       (method 'ghas henry-gher)
       (method 'bart henry-bart)
       (method 'witc henry-seaw)
       (method 'osca henry-osca)

       ))

(define (mk-henry)
  (bind 
   (kern-mk-char 'ch_henry           ; tag
                 "Henry"             ; name
                 sp_human            ; species
                 nil                 ; occ
                 s_townsman          ; sprite
                 faction-men         ; starting alignment
                 1 0 0               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 6  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'henry-conv         ; conv
                 sch_henry           ; sched
                 'townsman-ai                 ; special ai
                 (mk-inventory (list (list 1 t_dagger)))     ; container
                 nil                 ; readied
                 )
   (henry-mk)))
