;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define meaney-lvl 6)
(define meaney-species sp_human)
(define meaney-occ nil)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In the Poorhouse, near Oparine.
;;----------------------------------------------------------------------------
(define meaney-bed poorh-bed1)
(define meaney-mealplace poorh-sup1)
(define meaney-workplace poorh-hall)
(define meaney-leisureplace poorh-dining)
(kern-mk-sched 'sch_meaney
               (list 0  0 meaney-bed          "sleeping")
               (list 5  0 meaney-mealplace    "eating")
               (list 6  0 meaney-workplace    "working")
               (list 12 0 meaney-mealplace    "eating")
               (list 13 0 meaney-workplace    "working")
               (list 18 0 meaney-mealplace    "eating")
               (list 19 0 meaney-leisureplace "idle")
               (list 21 0 meaney-bed          "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (meaney-mk) (list 0 #t))
(define (meaney-get-donated meaney) (car meaney))
(define (meaney-donated? meaney) (> (meaney-get-donated meaney) 
                                    0))
(define (meaney-donate! meaney q) (set-car! meaney (+ (car meaney) q)))
(define (meaney-has-ring meaney) (cadr meaney))
(define (meaney-remove-ring meaney) (set-car! (cdr meaney) #f))

(define (meaney-on-death knpc)
	(if  (meaney-has-ring (kobj-gob-data knpc))
		(kern-obj-put-at (kern-mk-obj t_skull_ring_m 1) (kern-obj-get-location knpc))
	))

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Meaney is a monk of the Order of the Crossroad, 
;; living in the Poorhouse near Oparine.
;; He was once a pirate, in the crew of the Merciful Death,
;; and is sought for vengeance by the ghost captain Ghertie.
;;----------------------------------------------------------------------------

;; Basics...
(define (meaney-hail knpc kpc)
  (say knpc "I greet thee, traveler."))

(define (meaney-default knpc kpc)
  (say knpc "I cannot help thee with that."))

(define (meaney-name knpc kpc)
  (say knpc "I am brother Meaney.")
  (quest-data-update 'questentry-ghertie 'meaney-loc 1))

(define (meaney-join knpc kpc)
  (say knpc "My duty is to the poor and afflicted."))

(define (meaney-job knpc kpc)
  (say knpc "I run the poor house, "
       "where we care for the sick and destitute."))

(define (meaney-bye knpc kpc)
  (say knpc "Farewell."))

;; Second-tier responses
(define (meaney-get-donation knpc kpc)
  (define (rejected)
    (cond ((> (kern-player-get-gold) 0)
           (say knpc "I see. Perhaps another time. [He turns away sadly]")
           (kern-conv-end))
          (else
           (say knpc "Perhaps you also are in great need."))))
  (let ((meaney (kobj-gob-data knpc)))
    (if (not (meaney-donated? meaney))
        (begin
          (say knpc "Wilt thou donate some gold to help the poor?")
          (if (kern-conv-get-yes-no? kpc)
              (let ((q (get-gold-donation knpc kpc)))
                (if (> q 0)
                    (begin
                      (say knpc "Bless you, stranger! "
                           "Your kindness will be remembered.")
                      (meaney-donate! meaney q))
                    (rejected)))
              (rejected))))))

(define (meaney-poor knpc kpc)
  (say knpc "At the poor house we aid widows and orphans mostly. "
       "Art thou in great need?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "Then we gladly share what little food we have with thee.")
      (meaney-get-donation knpc kpc)))

(define (meaney-sick knpc kpc)
  (say knpc "We heal any who come here in need. Do you need healing?")
  (if (kern-conv-get-yes-no? kpc)
      (meaney-trade knpc kpc)
      (meaney-get-donation knpc kpc)))

(define (meaney-brot knpc kpc)
  (say knpc "I am a monk of the Order of the Crossroad. "
       "My order was founded by the Wanderer Davis over seven centuries ago "
       "to minister to the poor."))

;; Trade...
(define (meaney-trade knpc kpc)
  (if (trade-services knpc kpc
                      (list
                       (svc-mk "Heal" 0 heal-service)
                       (svc-mk "Cure" 0 cure-service)
                       ))
      (begin
        (say knpc "How else can I aid you?")
        (meaney-trade knpc kpc))
      (begin
        (say knpc "Will there be anything else?")
        (if (kern-conv-get-yes-no? kpc)
            (meaney-trade knpc kpc)
            (meaney-get-donation knpc kpc)))))

;; Town & Townspeople

;; Quest-related
(define (meaney-pira knpc kpc)
	(quest-data-update 'questentry-ghertie 'meaney-loc 1)
	(say knpc "Yes, I was once a pirate, long ago. "
		"I sailed with Ma Ghertie on the Merciful Death. "
		"Now I spend my life in penance to the poor."))

(define (meaney-gher knpc kpc)
  (say knpc "Ma Ghertie treated us crew like family. "
       "She was a ruthless brigand and thoroughly wicked. "
       "No doubt she deserved to die, "
       "but not at the hands of her own Bully Boys."))

(define (meaney-pena knpc kpc)
  (say knpc "I am guilty of many heinous crimes, "
       "and all the bloodletting was for naught. "
       "I betrayed my captain, and was betrayed in turn."))

(define (meaney-betr knpc kpc)
  (say knpc "The whole crew conspired to murder our captain and divide the loot. "
       "We were certain she was going to murder us all first if we did not. "
       "The first mate, the cook and I did the deed while she lay drunk, "
       "but when we returned to the dock the ship had sailed without us."))

(define (meaney-firs knpc kpc)
  (say knpc "The first mate was an evil wretch named Jorn. "
       "I heard he is a bandit now, somewhere in the great forest to the east. "
       "You might ask around Green Tower.")
       (quest-data-update 'questentry-ghertie 'jorn-forest 1))

(define (meaney-cook knpc kpc)
  (say knpc "Gholet is no doubt either dead or rotting in prison somewhere. "
       "The last time I saw him he stopped here for the night. "
       "When I awoke the next morning I found the lock on our donation box broken. "
       "The box was even more empty than usual.")
       (quest-data-update 'questentry-ghertie 'gholet-prison 1)
       )

(define (meaney-ring knpc kpc)
  (if (not (meaney-has-ring (kobj-gob-data knpc)))
      (say knpc "I hope I never see that cursed thing again.")
      (begin
        (say knpc "Yes, I wear the ring of Ma Ghertie's Bully Boys, "
             "the crew of the Merciful Death. Yes, I am a murdering pirate. ")
        (prompt-for-key)
        (say knpc "Legend says the Wanderer Luto traveled the countryside, "
              "dispensing justice at the point of his sword. Have you come to "
              "slay me for my crimes?")
        (if (yes? kpc)
            (begin
              (say knpc "Then be done with it, I am ready. [He stands with head bowed].")
              (kern-conv-end))
            (begin
              (say knpc "You are merciful. "
                    "But it is time I parted ways with this ring, "
                    "which means I must part ways with this finger. "
                    "I have not the courage to cut it off, "
                    "will you do it?")
              (if (yes? kpc)
                  (begin
                    (say knpc "[With a quick motion you grab his hand and hack off the "
                          "finger] Ah! There, it is off, and I am free from its curse. "
                          "I thank you, Wanderer. Know that if you ever need aid "
                          "or healing, I will do what I can for you.")
			(skullring-m-get nil kpc)
			(meaney-remove-ring (kobj-gob-data knpc))
                    )
                  (say knpc "I wish I could be rid of the wretched thing!")))))))

(define meaney-conv
  (ifc basic-conv

       ;; basics
       (method 'default meaney-default)
       (method 'hail meaney-hail)
       (method 'bye  meaney-bye)
       (method 'job  meaney-job)
       (method 'name meaney-name)
       (method 'join meaney-join)
       
       ;; trade
       (method 'trad meaney-trade)
       (method 'buy  meaney-trade)
       (method 'sell meaney-trade)

       ;; town & people

       ;; other responses
       (method 'poor meaney-poor)
       (method 'dest meaney-poor)
       (method 'sick meaney-sick)
       (method 'heal meaney-sick)
       (method 'affl meaney-sick)
       (method 'brot meaney-brot)
       (method 'hous meaney-job)

       ;; pirate quest replies
       (method 'pira meaney-pira)
       (method 'gher meaney-gher)
       (method 'ma   meaney-gher)
       (method 'capt meaney-gher)
       (method 'pena meaney-pena)
       (method 'betr meaney-betr)
       (method 'firs meaney-firs)
       (method 'cook meaney-cook)
       (method 'ring meaney-ring)
       (method 'skul meaney-ring)
       (method 'ghol meaney-cook)
       (method 'jorn meaney-firs)
       ))

(define (mk-meaney)
	(let ((knpc
    (kern-mk-char 
     'ch_meaney           ; tag
     "Meaney"             ; name
     meaney-species         ; species
     meaney-occ              ; occ
     s_companion_shepherd  ; sprite
     faction-men      ; starting alignment
     1 2 1            ; str/int/dex
     0 0              ; hp mod/mult
     0 0              ; mp mod/mult
     max-health ; hp
     -1                   ; xp
     max-health ; mp
     0
     meaney-lvl
     #f               ; dead
     'meaney-conv         ; conv
     sch_meaney           ; sched
     'townsman-ai              ; special ai

     ;;..........container (and contents)
     (mk-inventory
      (list
       (list 1 t_dagger)
       ))
     nil              ; readied
     )))
 (kern-char-force-drop knpc #t)
  (bind knpc (meaney-mk))
  (kern-obj-add-effect knpc 
           ef_generic_death
           'meaney-on-death)
	  knpc) )
  