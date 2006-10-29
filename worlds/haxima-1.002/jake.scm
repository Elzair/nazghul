;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define jake-lvl 2)
(define jake-species sp_gint)
(define jake-occ nil)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(define jake-bed )
(kern-mk-sched 'sch_jake
               (list 0  0 cantina-counter-zzz "sleeping")
               (list 9  0 cantina-counter "working")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (jake-mk) (list #t))
(define (jake-left? gob) (car gob))
(define (jake-left! gob val) (set-car! gob val))

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

(define (left-head? knpc)
  (jake-left? (gob knpc)))
(define (left-head! knpc)
  (jake-left! (gob knpc) #t)
  (say knpc "RIGHT HERE!"))
(define (right-head! knpc)
  (jake-left! (gob knpc) #f)
  (say knpc "How may I be of service?"))

;; Basics...
(define (jake-hail knpc kpc)
  (kern-log-msg "You meet an enormous figure with two heads. One is rough-looking, the other somewhat... "
                "well, prissy is probably not too strong a word.")
  (if (left-head? knpc)
      (say knpc "HAIL, MANLING!")
      (say knpc "Well met, little sir.")
      ))

(define (jake-default knpc kpc)
  (if (left-head? knpc)
      (say knpc "HELL, I DON'T KNOW! ASK PERCY!")
      (say knpc "A conundrum, to be sure.")
      ))

(define (jake-name knpc kpc)
  (if (left-head? knpc)
      (say knpc "I'M JAKE! AND THIS OTHER IS PERCY! [The left head jerks to indicate the right]")
      (say knpc "I am Percival. And my constant companion here likes to be called 'Jake'.  [The left head nods to the right]")
      ))

(define (jake-join knpc kpc)
  (if (left-head? knpc)
      (say knpc "HAR! HAR! HAR!")
      (say knpc "Oh, I'm afraid not. A most gracious offer, though, and all that.")
      ))

(define (jake-job knpc kpc)
  (if (left-head? knpc)
      (say knpc "I'M THE BOUNCER, OF COURSE! NOW DRINK SOMETHING OR GET OUT!")
      (begin
        (say knpc "I am the proprieter and bartender. Would you care for a drink?")
        (if (yes? kpc)
            (jake-trade knpc kpc)
            (say knpc "Please reconsider, I offer the finest.")
            ))))

(define (jake-bye knpc kpc)
  (if (left-head? knpc)
      (say knpc "BYE!")
      (say knpc "Farewell, do come again.")
      ))

(define (jake-jake knpc kpc)
  (if (left-head? knpc)
      (say knpc "YEAH? WHADDAYA WANT?")
      (begin
        (say knpc "Do you actually WANT to talk to Jake?")
        (if (yes? kpc)
            (left-head! knpc)))
      ))

(define (jake-perc knpc kpc)
  (if (left-head? knpc)
      (begin
        (say knpc "WHA? YOU WANNA TALK TO PERCY NOW?")
        (if (yes? kpc)
            (right-head! knpc)
            ))
      (say knpc "Yes, that's me. Can I help you?")
      ))

(define (jake-drin knpc kpc)
  (if (left-head? knpc)
      (say knpc "TALK TO PERCY!")
      (jake-trade knpc kpc)))

;; Trade...
(define (jake-trade knpc kpc)
  (kern-conv-trade knpc kpc
                   (list t_food 7)
                   (list t_beer 4)
                   (list t_wine 6)
                   )
  )

;; Town & Townspeople

;; Quest-related

(define jake-conv
  (ifc basic-conv

       ;; basics
       (method 'default jake-default)
       (method 'hail jake-hail)
       (method 'bye jake-bye)
       (method 'job jake-job)
       (method 'name jake-name)
       (method 'join jake-join)
       
       ;; trade
       (method 'drin jake-drin)
       (method 'trad jake-trade)
       (method 'buy jake-trade)
       (method 'sell jake-trade)

       ;; town & people
       (method 'jake jake-jake)
       (method 'perc jake-perc)
       ))

(define (mk-jake)
  (bind 
   (kern-mk-char 
    'ch_jake           ; tag
    "Jake&Percival"             ; name
    jake-species         ; species
    jake-occ              ; occ
    s_gint     ; sprite
    faction-men      ; starting alignment
    0 0 0            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    max-health ; hp
    -1                   ; xp
    max-health ; mp
    jake-lvl
    #f               ; dead
    'jake-conv         ; conv
    sch_jake           ; sched
    'townsman-ai              ; special ai
    nil              ; container
    nil              ; readied
    )
   (jake-mk)))
