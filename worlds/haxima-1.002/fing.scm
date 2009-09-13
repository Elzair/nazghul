;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Oparine.
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_fing
               (list 0  0  sea-witch-bay        "idle")
               (list 6  0  sea-witch-shore      "idle")
               (list 8  0  sea-witch-bay        "idle")
               (list 20 0  sea-witch-shore      "idle")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (fing-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Fing is a male Nixie, who is a Prince among his people.
;; He dwells in Oparine, to be close to his true love, the human woman Lia.
;;----------------------------------------------------------------------------

;; Basics...
(define (fing-hail knpc kpc)
  (say knpc "[You meet a nixie] Hail, Landman."))

(define (fing-default knpc kpc)
  (say knpc "Perhaps another Landman would know about that."))

(define (fing-name knpc kpc)
  (say knpc "I am Fing."))

(define (fing-join knpc kpc)
  (say knpc "I cannot stray far from these shores."))

(define (fing-job knpc kpc)
  (say knpc "I am a prince among the Seamen of this valley... er, bay."))

(define (fing-bye knpc kpc)
  (say knpc "Farewell, Landman."))

;; Shores...
(define (fing-shor knpc kpc)
  (say knpc "I must stay by the shore so I can be near my love."))

(define (fing-love knpc kpc)
  (say knpc "Although my beloved cannot leave the land, "
       "she is a princess among the Sea People. "
       "She is kind and true, and has not despaired even with her curse."))

(define (fing-sea knpc kpc)
  (say knpc "There are many kingdoms under the sea, many ruins, and caves, "
       "sunken ships and great treasures. There are magicians, "
       "and warriors, and mighty beasts! No offense, but the dry land "
       "must be very dull in comparison."
       ))

(define (fing-curs knpc kpc)
  (say knpc "It is a matter for the Sea People."))

;; Townspeople...
(define (fing-opar knpc kpc)
  (say knpc "It's okay for you Landman, I suppose."))

(define (fing-gher knpc kpc)
  (say knpc "We admired her from below! So quick, so brutal! "
       "Like a tempest disguised as a woman. Her crew, it seems, came to "
       "a bad end."))

(define (fing-crew knpc kpc)
  (say knpc "Ghertie's crew sailed east to an island, went ashore, and never "
       "returned. Her ship is no more."))

(define (fing-alch knpc kpc)
  (say knpc "He speaks to my love but I am not jealous. "
       "He is too old and fat for any love potion to decieve her eyes!"))

(define (fing-osca knpc kpc)
  (say knpc "I know him not."))

(define (fing-henr knpc kpc)
  (say knpc "A brave Landman, from what I hear."))

(define (fing-bart knpc kpc)
  (say knpc "I have not seen many goblins. I think they fear the sea. "
       "He is an oddity among them."))


(define fing-conv
  (ifc nil

       ;; basics
       (method 'default fing-default)
       (method 'hail fing-hail)
       (method 'bye fing-bye)
       (method 'job fing-job)
       (method 'name fing-name)
       (method 'join fing-join)
       
       ;; Shores
       (method 'shor fing-shor)
       (method 'love fing-love)
       (method 'sea fing-sea)
       (method 'deep fing-sea)
       (method 'bay  fing-sea)
       (method 'curs fing-curs)

       ;; town & people
       (method 'opar fing-opar)
       (method 'alch fing-alch)
       (method 'gher fing-gher)
       (method 'crew fing-crew)
       (method 'osca fing-osca)
       (method 'henr fing-henr)
       (method 'bart fing-bart)
       (method 'lia  fing-love)

       ))

(define (mk-fing)
  (bind 
   (kern-mk-char 'ch_fing           ; tag
                 "Fing"             ; name
                 sp_nixie           ; species
                 oc_warrior         ; occ
                 s_nixie_civilian    ; sprite
                 faction-men         ; starting alignment
                 1 2 0               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 3  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'fing-conv         ; conv
                 sch_fing           ; sched
                 'townsman-ai                 ; special ai
                 (mk-inventory (list (list 10 t_spear)))                ; container
                 nil                 ; readied
                 )
   (fing-mk)))
