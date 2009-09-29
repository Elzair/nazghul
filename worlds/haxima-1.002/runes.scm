;; rune sprites

;; also used where external events trigger quest
(define (rune-basic-quest questtag questicon)
   	(quest-data-complete questtag)
	(quest-data-assign-once questtag)
	(quest-data-update-with questtag 'done 1 (grant-party-xp-fn 50))
	(qst-set-icon! (quest-get questtag) questicon)
	)
	    
(define (rune-basic-get kobj kchar questtag)
  ;; Get the sprite before picking up the object, because picking it up will
  ;; deallocate it.
  (let ((sprite (kern-obj-get-sprite kobj)))
    (kobj-get kobj kchar)
    (rune-basic-quest questtag sprite)))

;; rune interface: when a rune is used on a special altar, it transforms the
;; alter and signals the demon gate mechanism
(define (rune-use ktype kuser)
  (let ((loc (get-target-loc kuser 1)))
    (cond ((null? loc) 
           result-no-target)
          ((eqv? (kern-place-get-terrain loc) t_rune_altar)
           (shake-map 5)
           (kern-log-msg "A LOCK IS RELEASED!")
           (kern-obj-remove-from-inventory kuser ktype 1)
           (kern-place-set-terrain loc t_active_altar)
           (send-signal kuser demon-gate 'on)
           result-ok)
          (else 
           result-not-here))))

(define rune-ifc
  (ifc obj-ifc
       (method 'use rune-use)
       ))

;; special extended interface for rune of leadership: summon the ghost of the
;; warritrix when the player picks it up
(define (rune-li-get kobj kchar)
  (kern-log-msg "An apparition appears!")
  (kern-obj-put-at (mk-warritrix)
                   (kern-obj-get-location kobj))
  (let ((sprite (kern-obj-get-sprite kobj)))
    (kern-obj-remove kobj)
    (kobj-get (kern-mk-obj t_rune_l 1) kchar)
    (rune-basic-quest 'questentry-rune-l sprite))
  )
(define rune-li-ifc
  (ifc rune-ifc
       (method 'get rune-li-get)))

;; trigger quest update
(define (rune-l-get kobj kchar)
	(rune-basic-get kobj kchar 'questentry-rune-p)
	)
(define rune-l-ifc
  (ifc rune-ifc
       (method 'get rune-l-get)))

;; trigger quest update
(define (rune-k-receive ktype kchar)
	(quest-data-update-with 'questentry-thiefrune 'recovered 1 (quest-notify (grant-party-xp-fn 50)))
	(quest-data-update 'questentry-rune-k 'player-got-rune 1)
	)
(define (rune-k-get kobj kchar)
	(rune-k-receive kobj kchar)
	(kobj-get kobj kchar)
	)
(define rune-k-ifc
  (ifc rune-ifc
       (method 'get rune-k-get)
       (method 'receive rune-k-receive)
       ))
       
;; trigger quest update
(define (rune-p-get kobj kchar)
	(rune-basic-get kobj kchar 'questentry-rune-p)
	)
(define rune-p-ifc
  (ifc rune-ifc
       (method 'get rune-p-get)))

;; trigger quest update
(define (rune-w-get kobj kchar)
	(rune-basic-get kobj kchar 'questentry-rune-w)
	)
(define rune-w-ifc
  (ifc rune-ifc
       (method 'get rune-w-get)))

;; trigger quest update
(define (rune-f-get kobj kchar)
	(rune-basic-get kobj kchar 'questentry-rune-f)
	)
(define rune-f-ifc
  (ifc rune-ifc
       (method 'get rune-f-get)))
       
;; trigger quest update
(define (rune-d-get kobj kchar)
	(rune-basic-get kobj kchar 'questentry-rune-d)
	)
(define rune-d-ifc
  (ifc rune-ifc
       (method 'get rune-d-get)))

;; trigger quest update
(define (rune-c-get kobj kchar)
	(rune-basic-get kobj kchar 'questentry-rune-c)
	)
(define rune-c-ifc
  (ifc rune-ifc
       (method 'get rune-c-get)))

;; trigger quest update
(define (rune-s-get kobj kchar)
	(rune-basic-get kobj kchar 'questentry-rune-s)
	)
(define rune-s-ifc
  (ifc rune-ifc
       (method 'get rune-s-get)))

;; rune types
(mk-quest-obj-type 't_rune_k "Rune of Knowledge" s_runestone_k layer-item rune-k-ifc)
(mk-quest-obj-type 't_rune_p "Rune of Power" s_runestone_p layer-item rune-p-ifc)
(mk-quest-obj-type 't_rune_s "Rune of Skill" s_runestone_s layer-item rune-s-ifc)
(mk-quest-obj-type 't_rune_c "Rune of Curiosity" s_runestone_c layer-item rune-c-ifc)
(mk-quest-obj-type 't_rune_f "Rune of Freedom" s_runestone_f layer-item rune-f-ifc)
(mk-quest-obj-type 't_rune_w "Rune of Wisdom" s_runestone_w layer-item rune-w-ifc)
(mk-quest-obj-type 't_rune_d "Rune of Discretion" s_runestone_d layer-item rune-d-ifc)
(mk-quest-obj-type 't_rune_l "Rune of Leadership" s_runestone_l layer-item rune-l-ifc)
(mk-quest-obj-type 't_rune_l_init "Rune of Leadership" s_runestone_l layer-item rune-li-ifc)

;; list of all rune types
(define rune-types 
  (list t_rune_k
        t_rune_p
        t_rune_s
        t_rune_c
        t_rune_l
        t_rune_f
        t_rune_w
        t_rune_d))

;; check if kpc has all the runes in inventory
(define (has-all-runes? kpc)
  (all-in-inventory? kpc rune-types))
