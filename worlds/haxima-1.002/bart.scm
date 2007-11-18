;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Oparine.
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_bart
               (list 0  0  black-barts-bed      "sleeping")
               (list 11 0  black-barts-ship     "working")
               (list 18 0  bilge-water-hall     "idle")
               (list 23 0  black-barts-bed      "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (bart-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Bart is a hard-drinking goblin shipwright who dwells in Oparine.
;;----------------------------------------------------------------------------

;; Basics...
(define (bart-hail knpc kpc)
  (say knpc "[You meet a taciturn goblin who smells of whiskey] "
       "Unh."))

(define (bart-default knpc kpc)
  (say knpc "Eh?"))

(define (bart-name knpc kpc)
  (say knpc "Bart."))

(define (bart-join knpc kpc)
  (say knpc "[He looks at you oddly and shakes his head]"))

(define (bart-job knpc kpc)
  (say knpc "Bart make ship. Good ship."))

(define (bart-bye knpc kpc)
  (say knpc "Ja."))

;; Trade...
(define (bart-trade knpc kpc)

  (define (buy-ship)
    (let* ((town (loc-place (kern-obj-get-location knpc)))
           (town-loc (kern-place-get-location town))
           (ship-loc (loc-offset town-loc east)))
      (if (ship-at? ship-loc)
          (say knpc "Hunh. No room at dock. Somebody need move ship first.")
          (begin
            (kern-obj-relocate (mk-ship) ship-loc nil)
            (take-player-gold oparine-ship-price)
            (say knpc "Ship ready ready outside town.")
            ))))

  (define (sell-ship)
    (let* ((town (loc-place (kern-obj-get-location knpc)))
           (town-loc (kern-place-get-location town))
           (ship-loc (loc-offset town-loc east))
           (kship (kern-place-get-vehicle ship-loc)))
      (if (null? kship)
          (say knpc "Bart no see ship. Park at dock, come back.")
          (begin
            (say knpc "You want sell that junk ship? Bart give " 
                 oparine-ship-tradein-price
                 ". Ok?")
            (if (kern-conv-get-yes-no? kpc)
                (begin
                  (say knpc "Bart too generous, you lucky.")
                  (kern-obj-remove kship)
                  (give-player-gold oparine-ship-tradein-price))
                (say knpc "Hunh. That ship sink soon."))))))

  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "Not work now. Now drink!")
      (begin
        (say knpc "You want buy ship?")
        (if (yes? kpc)
            (begin
              (say knpc "Ship " oparine-ship-price " gold. Want ship?")
              (if (kern-conv-get-yes-no? kpc)
                  (if (player-has-gold? oparine-ship-price)
                      (buy-ship)
                      (begin
                        (say knpc "You not have gold! You try cheat Bart? "
                             "[He spits on the ground]")
                        (kern-conv-end)))
                  (say knpc "Fine. You swim.")))
            (begin
              (say knpc "You want sell ship?")
              (if (yes? kpc)
                  (sell-ship)
                  (begin
                    (say knpc "Then what hell you bother Bart for?")
                    (kern-conv-end))))))))

;; Drink...
(define (bart-drink knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "Let's drink!")
      (say knpc "Work now. Drink later.")))


;; Townspeople...
(define (bart-opar knpc kpc)
  (say knpc "Smells like fish."))

(define (bart-gher knpc kpc)
  (say knpc "Tu-gu. Bad ghost.[He makes a strange sign in the air]"))

(define (bart-alch knpc kpc)
  (say knpc "Hi-lu-to. He take low road from burning city. "
       "Goblins cousins remember."))

(define (bart-seaw knpc kpc)
  (say knpc "She smell like fish."))

(define (bart-osca knpc kpc)
  (say knpc "Tu-to. Bart not know man-words."))

(define (bart-henr knpc kpc)
  (say knpc "Bart friend. Make good drink! Bart love drink."))

(define bart-conv
  (ifc basic-conv

       ;; basics
       (method 'default bart-default)
       (method 'hail bart-hail)
       (method 'bye bart-bye)
       (method 'job bart-job)
       (method 'name bart-name)
       (method 'join bart-join)
       
       ;; drink
       (method 'drin bart-drink)

       ;; trade
       (method 'trad bart-trade)
       (method 'ship bart-trade)
       (method 'buy bart-trade)
       (method 'sell bart-trade)

       ;; town & people
       (method 'opar bart-opar)
       (method 'alch bart-alch)
       (method 'gher bart-gher)
       (method 'witc bart-seaw)
       (method 'osca bart-osca)
       (method 'henr bart-henr)
       (method 'ja   bart-bye)

       ))

(define (mk-bart)
  (bind 
   (kern-mk-char 'ch_bart           ; tag
                 "Bart"             ; name
                 sp_forest_goblin    ; species
                 nil                 ; occ
                 s_fgob_civilian     ; sprite
                 faction-men         ; starting alignment
                 0 0 0               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 1  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'bart-conv         ; conv
                 sch_bart           ; sched
                 'townsman-ai                 ; special ai
                 (mk-inventory (list (list 1 t_axe)))                 ; container
                 nil                 ; readied
                 )
   (bart-mk)))
