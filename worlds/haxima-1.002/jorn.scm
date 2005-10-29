;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define jorn-lvl 5)
(define jorn-species sp_human)
(define jorn-occ oc_bandit)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(define jorn-bed gt-jorn-bed)
(define jorn-mealplace gt-ws-tbl1)
(define jorn-workplace gt-jorn-hut)
(define jorn-leisureplace gt-ws-hall)
(kern-mk-sched 'sch_my
               (list 0  0 jorn-bed          "sleeping")
               (list 11 0 jorn-mealplace    "eating")
               (list 12 0 jorn-workplace    "working")
               (list 18 0 jorn-mealplace    "eating")
               (list 19 0 jorn-leisureplace "idle")
               (list 24 0 jorn-workplace    "working")               
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (jorn-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (jorn-hail knpc kpc)
  (say knpc "[You meet a rough-looking, surly man] What do you want?"))

(define (jorn-default knpc kpc)
  (say knpc "Go bother someone else."))

(define (jorn-name knpc kpc)
  (say knpc "I'm Jorn. Heard of me?")
  (if (yes? kpc)
      (say knpc "Good, then you've been warned.")
      (say knpc "Too bad. Don't make me make an example out of you.")))

(define (jorn-join knpc kpc)
  (say knpc "[He laughs coarsely]"))

(define (jorn-job knpc kpc)
  (say knpc "None of your business."))

(define (jorn-bye knpc kpc)
  (say knpc "[He ignores your departure]"))


;; Town & Townspeople

;; Quest-related
(define (jorn-pira knpc kpc)
  (say knpc "You're starting to get on my nerves."))

(define (jorn-ring knpc kpc)
  (if (not (in-inventory? knpc t_skull_ring))
      (say knpc "[He looks puzzled] Eh... where'd it go?")
      (begin
        (say knpc "[He gives you a cold look] What of it? Do you want it?")
        (if (no? kpc)
            (say knpc "Then quit staring at it.")
            (begin
              (say knpc "Well, you're going to have to cut it off my finger "
                   "to get it. What do you think of that? Are you ready to "
                   "try and cut it off my finger?")
              (if (no? kpc)
                  (say knpc "[He sneers] I didn't think so.")
                  (begin
                    (say knpc "[With a roar, too fast for you to see, "
                         "he draws his sword and thrusts at you in the same "
                         "motion!]")
                    (kern-being-set-base-faction knpc faction-outlaw)
                    (kern-conv-end))))))))

(define jorn-conv
  (ifc basic-conv

       ;; basics
       (method 'default jorn-default)
       (method 'hail jorn-hail)
       (method 'bye jorn-bye)
       (method 'job jorn-job)
       (method 'name jorn-name)
       (method 'join jorn-join)
       
       ;; other responses
       (method 'pira jorn-pira)
       (method 'gher jorn-pira)
       (method 'merc jorn-pira)
       (method 'ring jorn-ring)

       ))

(define (mk-jorn)
  (bind 
   (kern-char-force-drop
   (kern-char-arm-self
    (kern-mk-char 
    'ch_my           ; tag
    "Jorn"             ; name
    jorn-species     ; species
    jorn-occ         ; occ
    s_brigand        ; sprite
    faction-men      ; starting alignment
    0 0 0            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    (max-hp jorn-species jorn-occ jorn-lvl 0 0) ; hp
    0                ; xp
    (max-mp jorn-species jorn-occ jorn-lvl 0 0) ; mp
    jorn-lvl
    #f               ; dead
    'jorn-conv       ; conv
    sch_my           ; sched
    'spell-sword-ai  ; special ai

    ;; container
    (mk-chest nil
              (mk-contents
               (add-content 1 t_skull_ring)
               (add-content 1 t_sword_2)
               (add-content 1 t_dagger_4)
               (add-content 1 t_armor_leather_2)
               (add-content 1 t_leather_helm_2)
               (add-content 67 t_gold_coins)
               (add-content 3 t_picklock)
               (add-content 3 t_heal_potion)
               ))


    nil              ; readied
    ))
   #t)
   (jorn-mk)))