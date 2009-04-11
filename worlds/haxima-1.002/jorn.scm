;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define jorn-lvl 5)
(define jorn-species sp_human)
(define jorn-occ oc_wrogue)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Green Tower.
;;----------------------------------------------------------------------------
(define jorn-bed gt-jorn-bed)
(define jorn-mealplace gt-ws-tbl1)
(define jorn-workplace gt-jorn-hut)
(define jorn-leisureplace gt-ws-hall)
(kern-mk-sched 'sch_jorn
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

(define (jorn-on-death knpc)
	(kern-obj-put-at (kern-mk-obj t_skull_ring_j 1) (kern-obj-get-location knpc))
	)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Jorn is a bravo and former pirate, currently living in Green Tower.
;; He was once a member of the crew of the Merciful Death, 
;; and is sought for vengeance by the ghost Ghertie.
;;----------------------------------------------------------------------------

;; Basics...
(define (jorn-hail knpc kpc)
  (say knpc "[You meet a rough-looking, surly man] What do you want?"))

(define (jorn-default knpc kpc)
  (say knpc "Go bother someone else."))

(define (jorn-name knpc kpc)
  (say knpc "I'm Jorn. Heard of me?")
   (quest-data-update 'questentry-ghertie 'jorn-loc 1)
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
      (quest-data-update 'questentry-ghertie 'jorn-loc 1)
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
                    (kern-conv-end))))))

(define jorn-conv
  (ifc basic-conv

       ;; basics
       (method 'default jorn-default)
       (method 'hail jorn-hail)
       (method 'bye  jorn-bye)
       (method 'job  jorn-job)
       (method 'name jorn-name)
       (method 'join jorn-join)
       
       ;; other responses
       (method 'pira jorn-pira)
       (method 'gher jorn-pira)
       (method 'merc jorn-pira)
       (method 'ring jorn-ring)

       ))

(define (mk-jorn)
	(let ((knpc
		(kern-mk-char 
			'ch_jorn           ; tag
			"Jorn"             ; name
			jorn-species     ; species
			jorn-occ         ; occ
			s_brigand        ; sprite
			faction-men      ; starting alignment
			2 0 1            ; str/int/dex
			0 0              ; hp mod/mult
			0 0              ; mp mod/mult
			max-health ; hp
			-1                ; xp
			max-health ; mp
			0
			jorn-lvl
			#f               ; dead
			'jorn-conv       ; conv
			sch_jorn           ; sched
			'spell-sword-ai  ; special ai

			;; container
			(mk-inventory (list
				(list 1 t_sword_2)
				(list 1 t_dagger_4)
				(list 1 t_armor_leather_2)
				(list 1 t_leather_helm_2)
				(list 67 t_gold_coins)
				(list 3 t_picklock)
				(list 3 t_heal_potion)
			))
			nil              ; readied
		)))
		(bind knpc  (jorn-mk))
		(kern-char-force-drop knpc #t)
		(kern-char-arm-self knpc)
		(kern-obj-add-effect knpc 
			ef_generic_death
			'jorn-on-death)
		knpc
	))
