;; ----------------------------------------------------------------------------
;; effects.scm - generic effects procedures used in multiple places
;; ----------------------------------------------------------------------------

(define (mk-effect tag name sprite exec apply rm restart hook sym ddc cum dur)
  (kern-mk-effect tag 
                  name
                  "undef" 
                  sprite
                  exec 
                  apply 
                  rm 
                  restart
                  hook 
                  sym 
                  ddc 
                  s_null 
                  cum 
                  dur))

;; apply-time-scaled damage account for damaging effects applied at wilderness
;; scale or when camping or loitering. At higher time scales I think it's not
;; so nice to kill characters in one turn by applying the full damage for the
;; time scale. However, you have to apply *some* extra damage or its
;; incongruent.
(define (time-scaled-damage-factor)
  (if (> (kern-ticks-per-turn) 1)
      10
      1))

(define (poison-exec fgob obj)
  (if (obj-is-char? obj)
      (kern-obj-apply-damage obj "poisoned" (* 1 (time-scaled-damage-factor)))))

;; ------------------------------------------------------------------
;; Accumulating duration effects support
;; Should probably have a 'remove from list', but
;; it might be a better way elsewhere anyway
;; -------------------------------------------------------------------

(define (effect-list-lookup-loop fxlist target)
	(if (null? fxlist)
		nil
		(if (equal? (caar fxlist) target)
			(cadar fxlist)
			(effect-list-lookup-loop (tail fxlist) target)
		)))
		
(define (effect-list-lookup fxlist target)
	(let ((result (effect-list-lookup-loop (tail fxlist) target)))
		(if (null? result)
			(car (cdr (car (tail (set-cdr! fxlist (append (list (list target (list 0))) (tail fxlist)))))))
			result
		)))

;; ----------------------------------------------------------------------------
;; Poison & Disease Immunities
;;
;; These work by attaching an effect to the "add-hook-hook", which runs
;; whenever any new effect is applied. If anything tries to apply a poison
;; effect, for example, the poison immunity effect will catch it and block the
;; application.
;; ----------------------------------------------------------------------------
(define (poison-immunity-exec fgob effect)
  (if (eqv? effect ef_poison) #t #f))

(define (disease-immunity-exec fgob effect)
  (if (eqv? effect ef_disease) #t #f))

(define (paralysis-immunity-exec fgob effect)
  (if (eqv? effect ef_paralyze) #t #f))

(define (charm-immunity-exec fgob effect)
  (if (eqv? effect ef_charm) #t #f))

(define (sleep-immunity-exec fgob effect)
  (if (eqv? effect ef_sleep) #t #f))

;; ----------------------------------------------------------------------------
;; sleep
;;
;; The sleep effect is largely implemented in the kernel, and applies only to
;; character types. It expires naturally when the character makes a saving
;; throw. Note that this sleep effect is completely different than camping or
;; resting, which is managed entirely by the kernel.
;; ----------------------------------------------------------------------------
(define (sleep-exec fgob kobj)
  (if (not (obj-is-char? kobj))
      (kern-obj-remove-effect kobj ef_sleep)
      (let ((kchar kobj))
        (if (> (kern-dice-roll "1d20")
               19)
            (begin
              (kern-obj-remove-effect kchar ef_sleep)
              (kern-char-set-sleep kchar #t)
              )))))
    
(define (sleep-reset fgob kobj)
  (if (obj-is-char? kobj)
     (kern-char-set-sleep kobj #t)))

(define (sleep-rm fgob kobj)
  (if (obj-is-char? kobj)
      (kern-char-set-sleep kobj #f)))

;; ----------------------------------------------------------------------------
;; paralyze
;;
;; The paralyze effect rolls to expire each turn. If the roll fails, the
;; character loses its turn. If it succeeds, the effect removes itself from the
;; character. Treats a natural roll of 20 as success.
;; ----------------------------------------------------------------------------
(define (can-paralyze? kobj)
  (and (obj-is-char? kobj)
       (not (species-is-immune-to-paralyze? (kern-char-get-species kobj)))))

(define (paralyze-apply fgob kobj)
  (kern-log-msg (kern-obj-get-name kobj) " paralyzed!"))

(define (paralyze-exec fgob kobj)
  (if (not (obj-is-char? kobj))
      (kern-obj-remove-effect kobj ef_paralyze)
      (let ((kchar kobj)
            (droll (kern-dice-roll "1d20")))
        (if (or (= droll 20)
                (> droll
                   dc-escape-paralyze))
            (begin
              (kern-log-msg "Paralysis wears off of " (kern-obj-get-name kchar))
              (kern-obj-remove-effect kchar ef_paralyze)
              #f)
            (begin
              (kern-obj-set-ap kchar 0)
              #f)))))

(mk-effect 'ef_paralyze "Paralyzed" s_paralyse 'paralyze-exec 'paralyze-apply nil 'paralyze-apply 
           "keystroke-hook" "Z" 0 #f 15)

(define (paralyze kobj)
  (if (can-paralyze? kobj)
      (begin
        (kern-obj-add-effect kobj ef_paralyze nil))))

;;----------------------------------------------------------------------------
;; disease
;;
;; Drains life until victim is near death
;;----------------------------------------------------------------------------
(define (disease-exec fgob kobj)
  (if (not (obj-is-char? kobj))
      (kern-obj-remove-effect kobj ef_disease)
      (let ((kchar kobj))
        (let ((dmgroll (* (time-scaled-damage-factor) 
                          (kern-dice-roll "1d5")))
              (maxdmg (- (kern-char-get-hp kchar) 
                         (kern-dice-roll "1d10"))))
          (cond ((> dmgroll maxdmg)
                 (kern-log-msg (kern-obj-get-name kchar) " fights off Disease")
                 (kern-obj-apply-damage kchar "disease" maxdmg)
                 (kern-obj-remove-effect kchar ef_disease)
                 )
                (else
                 (kern-obj-apply-damage kchar "disease" dmgroll)))))))

;; ----------------------------------------------------------------------------
;; ensnare
;;
;; The ensnare effect rolls against a character's strength each turn. If the
;; roll fails, the character loses its turn. If it succeeds, the effect removes
;; itself from the character. Also treat a natural roll of 20 as success.
;; ----------------------------------------------------------------------------
(define (can-ensnare? kobj)
  (and (obj-is-char? kobj)
       (not (species-is-immune-to-ensnare? (kern-char-get-species kobj)))))

(define (ensnare-apply fgob kobj)
  (kern-log-msg (kern-obj-get-name kobj) " stuck in web!"))

(define (destroy-webs-at loc)
  (define (destroy-web web)
    (kern-obj-remove web))
  (map destroy-web (find-object-types-at loc web-type))
  (map destroy-web (find-object-types-at loc F_web_perm))
  )

(define (ensnare-exec fgob kobj)
  (println "ensnare-exec")
  (if (not (can-ensnare? kobj))
      (kern-obj-remove-effect ef_ensnare)
      (let ((kchar kobj)
            (droll (kern-dice-roll "1d20")))
        ;; special case -- paralysis prevents struggling against the ensnare
        (println "droll=" droll)
        (if (not (is-paralyzed? kchar))
            (if (or (= droll 20)
                    (> (+ (kern-char-get-strength kchar) 
                          droll)
                       dc-escape-ensnare))
                (begin
                  (kern-log-msg (kern-obj-get-name kchar) " breaks free of web!")
                  (kern-obj-remove-effect kchar ef_ensnare)
                  (destroy-webs-at (kern-obj-get-location kchar))
                  #t)
                (begin
                  (kern-log-msg (kern-obj-get-name kchar) " struggles in the web!")
                  (kern-obj-set-ap kchar 0)
                  #f))))))

(mk-effect 'ef_ensnare "Ensnared" s_tangle 'ensnare-exec 'ensnare-apply nil 'ensnare-apply
           "keystroke-hook" "E" 0 #f 15)

(define (is-ensnared? kobj)
  (in-list? ef_ensnare (kern-obj-get-effects kobj)))

(define (ensnare kobj)
  (if (can-ensnare? kobj)
      (begin
        (kern-obj-add-effect kobj ef_ensnare nil))))

;;----------------------------------------------------------------------------
;; poison immunity
(define (has-poison-immunity? kobj)
  (let ((effects (kern-obj-get-effects kobj)))
    (or (in-list? ef_poison_immunity effects)
        (in-list? ef_temporary_poison_immunity effects))))

;;----------------------------------------------------------------------------
;; fire immunity
(define (has-fire-immunity? kobj)
  (let ((effects (kern-obj-get-effects kobj)))
    (or (in-list? ef_fire_immunity effects)
        (in-list? ef_temporary_fire_immunity effects))))

;;----------------------------------------------------------------------------
;; magical kill immunity
(define (has-magical-kill-immunity? kobj)
  (let ((effects (kern-obj-get-effects kobj)))
    (or (in-list? ef_magical_kill_immunity effects)
        (in-list? ef_temporary_magical_kill_immunity effects))))

(define (has-sleep-immunity? kobj)
  (println "has-sleep-immunity?")
  (let ((effects (kern-obj-get-effects kobj)))
    (or (in-list? ef_sleep_immunity effects)
        (in-list? ef_temporary_sleep_immunity effects))))

;; ----------------------------------------------------------------------------
;; light
;;
;; Light works by increasing the effected object's light value when the effect
;; is applied, and decreasing it when the effect is removed. It does this in a
;; two-step process. The first step is an effect which runs on the special
;; ----------------------------------------------------------------------------

(define temp-light-power (list 0))

(define (temp-light-power-set power)
	(set-car! temp-light-power power))

(define (light-rm fgob kobj)
  (kern-log-msg "Light spell wore off")
  (kern-obj-dec-light kobj (caar fgob))
  (temp-light-power-set (caar fgob)))

(define (light-apply fgob kobj)
	(kern-obj-inc-light kobj (caar fgob))
	)

;a function with a working power->time calculation would be nicer
(define (light-effect-getdecr current)
	(if (< current 300)
		50
		(if (< current 600)
			5
			(floor (/ current 20))
	)))

(define (light-dim power light-time current-time kobj)
	(if (<= current-time light-time)
		power
		(let ((decrlight (light-effect-getdecr power)))
			(if (> decrlight power)
				0
				(light-dim (- power decrlight) (+ light-time 1) current-time kobj)
			))))

(define (light-exec fgob kobj)
	(let* ((light-time (cadar fgob))
		(current-time (kern-get-total-minutes))
		(power (caar fgob))
		(newpower (light-dim power light-time current-time kobj)))
		(cond ((= newpower power) nil)
			((<= newpower 0) (kern-obj-remove-effect kobj ef_light))
			(else
				(set-car! fgob (list newpower (kern-get-total-minutes)))
				(kern-obj-dec-light kobj (- power newpower))
				))))

(define (light-apply-new target power)
	(temp-light-power-set 0)
	(kern-log-enable #f)
	(kern-obj-remove-effect target ef_light)
	(kern-log-enable #t)
	(let ((fxgob (list (list (+ power (car temp-light-power)) (kern-get-total-minutes)))))
		(kern-obj-add-effect target ef_light fxgob)
	))
	
;; ----------------------------------------------------------------------------
;; torchlight
;;
;; This is just like light but it's called out especially because it's
;; vulnerable to the douse effect, where as normal light is not.
;; ----------------------------------------------------------------------------
(define torchlight-amount 1024)

(define (torchlight-rm fgob kobj)
  (kern-log-msg "A torch flickers out!")
  (kern-obj-dec-light kobj torchlight-amount))

(define (torchlight-apply fgob kobj)
  (kern-obj-inc-light kobj torchlight-amount))

;; ----------------------------------------------------------------------------
;; Weak light
;;
;; A silent, weak version of torchlight for NPCs.
;; ----------------------------------------------------------------------------
(define weaklight-amount 256)

(define (weaklight-rm fgob kobj)
  (kern-obj-dec-light kobj weaklight-amount))

(define (weaklight-apply fgob kobj)
  (kern-obj-inc-light kobj weaklight-amount))

;; ----------------------------------------------------------------------------
;; Protection
;;
;; Used by the In Sanct spell.
;; ----------------------------------------------------------------------------
(define (protection-rm fgob kobj)
  (if (obj-is-char? kobj)
      (kern-char-add-defense kobj -10)))

(define (protection-apply fgob kobj)
  (if (obj-is-char? kobj)
      (kern-char-add-defense kobj 10)))

;; ----------------------------------------------------------------------------
;; Charm
;;
;; Used by the An Xen Ex spell.
;; ----------------------------------------------------------------------------
(define (charm-mk faction) (list faction))

(define (charm-faction charm) (car charm))

(define (charm-rm charm kchar)
  (if (obj-is-char? kchar)
      (begin
        (kern-being-set-current-faction kchar
                                        (kern-being-get-base-faction kchar))
        (kern-log-msg (kern-obj-get-name kchar)
                      " recovers from charm!")
        )))

(define (charm-apply charm kchar)
  (if (obj-is-char? kchar)
      (kern-being-set-current-faction kchar (charm-faction charm))))

;; ----------------------------------------------------------------------------
;; Loot Drop
;;
;; Used to generate loot when an NPC is killed. The hook-fx given to the gob is
;; executed when the effect runs, taking the unfortunate npc as its parm.
;; ----------------------------------------------------------------------------
(define (loot-drop-mk hook-fx . hook-fx-parms) (list 'loot-drop-gob hook-fx hook-fx-parms))

(define (loot-drop-hook-fx gob) (cadr gob))
(define (loot-drop-hook-fx-parms gob) (caddr gob))

(define (loot-drop-exec fgob kobj)
  (if (not (obj-is-char? kobj))
      (kern-obj-remove-effect kobj ef_loot_drop)
      (let ((kchar kobj))
        (apply (eval (loot-drop-hook-fx fgob)) 
               (cons kchar
                     (loot-drop-hook-fx-parms fgob))))))

;; ----------------------------------------------------------------------------
;; Invisibility
;;
;; Used by the Sanct Lor spell. Note: the kernel's kern-obj-set-visible proc
;; increments/decrements a visibility counter, naturally handling cumulative
;; invisibility effects.
;; ----------------------------------------------------------------------------
(define (invisibility-rm fgob kobj)
  (kern-obj-set-visible kobj #t))

(define (invisibility-apply fgob kobj)
  (kern-obj-set-visible kobj #f))

;; ----------------------------------------------------------------------------
;; Slime Split
;;
;; A special feature of the slime species. When a slime takes damage it rolls
;; to clone itself.
;; ----------------------------------------------------------------------------
(define (split-gob-mk npc-type-tag) (list npc-type-tag))
(define (split-gob-npc-type-tag gob) (car gob))

(define (split-exec fgob kobj)
  (let ((loc (kern-obj-get-location kobj)))
    (if (not (kern-place-is-wilderness? (loc-place loc)))
        (begin
          (if (> (kern-dice-roll "1d20") 12)
              (let* (
					(orighp (kern-char-get-hp kobj))
					(orighproll (string-append "1d" (number->string (kern-char-get-max-hp kobj))))
					(hurtclone (< (kern-dice-roll orighproll) orighp))
					(origlevel (kern-char-get-level kobj))
					(clonelevel 
						(if (= origlevel 1)
							1
							(if hurtclone
								origlevel
								(- origlevel 1))))
					(clone (mk-npc (split-gob-npc-type-tag fgob)
                                   clonelevel)))
                (kern-being-set-base-faction clone
                                   (kern-being-get-base-faction kobj))
				(if hurtclone
					(kern-char-set-hp clone orighp))
                (kern-log-msg (kern-obj-get-name kobj) " divides!")
                (kern-obj-put-at clone (pick-loc loc clone)))
              )))))

;; ----------------------------------------------------------------------------
;; Grow Head
;;
;; A special feature of the hydra species. When a hydra takes damage it gains
;; experience, accelerating its advancement.
;; ----------------------------------------------------------------------------
(define (grow-head-exec fgob kobj)
  (if (obj-is-char? kobj)
      (kern-char-add-experience kobj (kern-dice-roll "2d20"))))

;; ----------------------------------------------------------------------------
;; Spider Calm
;;
;; Used by the An Xen Bet spell to prevent spiders from attacking.
;; ----------------------------------------------------------------------------
(define (spider-calm-rm fgob kchar)
  (kern-dtable-dec (kern-being-get-current-faction kchar)
                   faction-spider)
  (kern-log-msg (kern-obj-get-name kchar) " seems less friendly to spiders"))

(define (spider-calm-apply fgob kchar)
  (kern-dtable-inc (kern-being-get-current-faction kchar)
                   faction-spider)
  (kern-log-msg (kern-obj-get-name kchar) " makes spiders seem friendlier"))

;;----------------------------------------------------------------------------
;; Drunk
;;
;; Every keystroke at start-of-turn, roll to make the victim move in a random
;; direction as if staggering. If roll succeeds end the victim's turn. Ending
;; the turn prevents cumulative drinks from causing more than one "stagger" per
;; turn.
;;----------------------------------------------------------------------------
(define (drunk-exec fgob kchar)
  (if (> (kern-dice-roll "1d20") 16)
      (if (stagger kchar)
          (begin
            (kern-log-msg (kern-obj-get-name kchar) " staggers!")
            (end-turn kchar)))))

(define (drunk-apply fgob kchar)
  (kern-log-msg (kern-obj-get-name kchar) " feels tipsy!"))

(define (drunk-rm fgob kchar)
  (kern-log-msg (kern-obj-get-name kchar) " has a hangover!"))

;;-----------------------------------------------------------------
;; Graphics update
;; for stuff that changes appearance with time
;; requires update-gfx ifc
;;-----------------------------------------------------------

(define (update-graphics fgob kobj)
	(if (kobj-can? kobj 'update-gfx)
		(send-signal nil kobj 'update-gfx)
	))

;;----------------------------------------------------------------------------
;; Ready/Unready hooks
;;----------------------------------------------------------------------------
(define (uses-paper-doll? kobj)
  (and (obj-is-char? kobj)
       (eqv? (kern-char-get-species kobj)
             sp_human)))

(define (ktype-get-sprite ktype)
  (println "ktype: " (kern-type-get-name ktype) " gob: " (kern-type-get-gob ktype))
  (let ((gob (kern-type-get-gob ktype)))
    (if (null? gob)
        nil
        gob)))

(define (rebuild-humanoid-sprite khum)
  (re-mk-composite-sprite (cons (kern-sprite-strip-decorations 
                                 (kern-obj-get-sprite khum))
                                (filter notnull?
                                        (map ktype-get-sprite
                                             (kern-char-get-arms khum))))))

(define (ready-equip fgob kobj karms slot)
  (if (uses-paper-doll? kobj)
      (begin
        (kern-obj-set-sprite kobj (rebuild-humanoid-sprite kobj))
        (kern-map-set-dirty))))

(define (unready-equip fgob kobj karms slot)
  (ready-equip fgob kobj karms slot))

;; ----------------------------------------------------------------------------
;; Cleanup tentacles (for sludge krakens when they die). Note that this is a
;; hack, in that ALL tentacles in the current place are cleaned up, whether
;; they "belong" to the dying sludge kraken or not.
;; ----------------------------------------------------------------------------
(define (cleanup-tentacles fgob kobj)
  (map kern-char-kill
       (filter is-sludge-tentacle?
               (kern-place-get-beings (loc-place (kern-obj-get-location kobj))))))

;; ----------------------------------------------------------------------------
;; Effects Table
;; ----------------------------------------------------------------------------

;; Start-of-turn hooks
(mk-effect 'ef_poison                 "Poison"        s_poison      'poison-exec nil                 nil              nil                 "start-of-turn-hook" "P" 0   #f  -1)
(mk-effect 'ef_sleep                  "Sleep"         s_sleep       'sleep-exec  nil                 'sleep-rm        'sleep-reset        "start-of-turn-hook" "S" 0   #f  60)
(mk-effect 'ef_light                  "Magical light" s_light       'light-exec  'light-apply        'light-rm        'light-apply        "start-of-turn-hook" "L" 0   #t  -2)
(mk-effect 'ef_torchlight             "Torchlight"    s_torchlight  nil          'torchlight-apply   'torchlight-rm   'torchlight-apply   "start-of-turn-hook" "T" 0   #f  60)
(mk-effect 'ef_weaklight              "Torchlight"    s_torchlight  nil          'weaklight-apply    'weaklight-rm    'weaklight-apply    "start-of-turn-hook" "T" 0   #f  60)
(mk-effect 'ef_protection             "Protection"    s_protect     nil          'protection-apply   'protection-rm   'protection-apply   "start-of-turn-hook" "p" 0   #t  10)
(mk-effect 'ef_charm                  "Charm"         s_charm       nil          'charm-apply        'charm-rm        'charm-apply        "start-of-turn-hook" "C" 0   #f   5)
(mk-effect 'ef_invisibility           "Invisible"     s_invis       nil          'invisibility-apply 'invisibility-rm 'invisibility-apply "start-of-turn-hook" "N" 0   #t  10)
(mk-effect 'ef_permanent_invisibility "Invisible"     s_invis       nil          'invisibility-apply 'invisibility-rm 'invisibility-apply "start-of-turn-hook" "N" 0   #t  -1)
(mk-effect 'ef_spider_calm            "Spider calm"   s_spider_calm nil          'spider-calm-apply  'spider-calm-rm   nil                "start-of-turn-hook" ""  0   #f  60) 
(mk-effect 'ef_disease                "Diseased"      s_disease    'disease-exec  nil                 nil              nil                "start-of-turn-hook" "D" 0   #f  -2)

;; Add-hook hooks
(mk-effect 'ef_poison_immunity               "Poison immunity"    s_im_poison   'poison-immunity-exec    nil nil nil "add-hook-hook" "I" 0   #f  -1)
(mk-effect 'ef_temporary_poison_immunity     "Poison immunity"    s_im_poison   'poison-immunity-exec    nil nil nil "add-hook-hook" "I" 0   #f  60)
(mk-effect 'ef_disease_immunity              "Disease immunity"   s_im_disease  'disease-immunity-exec   nil nil nil "add-hook-hook" "E" 0   #f  -1)
(mk-effect 'ef_temporary_disease_immunity    "Disease immunity"   s_im_disease  'disease-immunity-exec   nil nil nil "add-hook-hook" "E" 0   #f  60)
(mk-effect 'ef_paralysis_immunity            "Paralysis immunity" s_im_paralyse 'paralysis-immunity-exec nil nil nil "add-hook-hook" "z" 0   #f  -1)
(mk-effect 'ef_temporary_paralysis_immunity  "Paralysis immunity" s_im_paralyse 'paralysis-immunity-exec nil nil nil "add-hook-hook" "z" 0   #f  60)
(mk-effect 'ef_charm_immunity                "Charm immunity"     s_im_charm    'charm-immunity-exec     nil nil nil "add-hook-hook" "c" 0   #f  -1)
(mk-effect 'ef_temporary_charm_immunity      "Charm immunity"     s_im_charm    'charm-immunity-exec     nil nil nil "add-hook-hook" "c" 0   #f  60)
(mk-effect 'ef_sleep_immunity                "Sleep immunity"     s_im_sleep    'sleep-immunity-exec     nil nil nil "add-hook-hook" "s" 0   #f  -1)
(mk-effect 'ef_temporary_sleep_immunity      "Sleep immunity"     s_im_sleep    'sleep-immunity-exec     nil nil nil "add-hook-hook" "s" 0   #f  60)

;; Nil hooks
(mk-effect 'ef_fire_immunity                   "Fire immunity"       s_im_fire  nil nil nil nil "nil-hook" "F" 0 #f  -1)
(mk-effect 'ef_temporary_fire_immunity         "Fire immunity"       s_im_fire  nil nil nil nil "nil-hook" "F" 0 #f  15)
(mk-effect 'ef_magical_kill_immunity           "Magic kill immunity" s_im_death nil nil nil nil "nil-hook" "K" 0 #f  -1)
(mk-effect 'ef_temporary_magical_kill_immunity "Magic kill immunity" s_im_death nil nil nil nil "nil-hook" "K" 0 #f  15)

;; Keystroke hooks
(mk-effect 'ef_drunk "Drunk" s_drunk 'drunk-exec 'drunk-apply 'drunk-rm nil "keystroke-hook" "A" 0 #t 60)

;; On-damage hooks
(mk-effect 'ef_split               "Split"          nil 'split-exec     nil nil nil             "on-damage-hook" ""  0 #f  -1)
(mk-effect 'ef_grow_head           "XP from damage" nil 'grow-head-exec nil nil 'grow-head-exec "on-damage-hook" "H" 0 #f  -1)
(mk-effect 'ef_temporary_grow_head "XP from damage" nil 'grow-head-exec nil nil 'grow-head-exec "on-damage-hook" "H" 0 #f  15)

;; Update-graphics hook
(mk-effect 'ef_graphics_update nil nil 'update-graphics nil nil 'update-graphics "start-of-turn-hook" "" 0 #f -1)

;; Ready-equip hooks
(mk-effect 'ef_ready_equip nil nil 'ready-equip nil nil nil "ready-equip-hook" "" 0 #f -1)

;; Unready-equip hooks
(mk-effect 'ef_unready_equip nil nil 'unready-equip nil nil nil "unready-equip-hook" "" 0 #f -1)

;; On-death hooks
(mk-effect 'ef_loot_drop         nil nil 'loot-drop-exec    nil nil nil "on-death-hook" "" 0 #f -1)
(mk-effect 'ef_cleanup_tentacles nil nil 'cleanup-tentacles nil nil nil "on-death-hook" "" 0 #f -1)

;;----------------------------------------------------------------------------
;; Effect Test Procedures
;;----------------------------------------------------------------------------

(define (is-poisoned? kobj)
  (in-list? ef_poison (kern-obj-get-effects kobj)))

(define (is-paralyzed? kobj)
  (in-list? ef_paralyze (kern-obj-get-effects kobj)))

(define (is-diseased? kobj)
  (in-list? ef_disease (kern-obj-get-effects kobj)))

(define (is-asleep? kobj)
  (in-list? ef_sleep (kern-obj-get-effects kobj)))

(define (is-charmed? kobj)
  (in-list? ef_charm (kern-obj-get-effects kobj)))

(define (is-invisible? kobj)
  (in-list? ef_invisibility (kern-obj-get-effects kobj)))

(define (is-disabled? kobj)
  (let ((effects (kern-obj-get-effects kobj)))
    (println "effects=" effects)
    (if (null? effects)
        #f
        (foldr (lambda (x effect)
                 (or x
                     (in-list? effect effects)))
               #f
               (list ef_paralyze ef_sleep ef_charm ef_ensnare)))))

(define (not-disabled? kobj)
  (not (is-disabled? kobj)))

;; ----------------------------------------------------------------------------
;; Effect Application Procedures
;; ----------------------------------------------------------------------------

;; Used by spells:
(define (apply-poison obj)
  (cond ((and (obj-is-char? obj)
           (not (has-poison-immunity? obj)))
         (kern-log-msg (kern-obj-get-name obj) " poisoned!")
         (kern-obj-add-effect obj ef_poison nil)))
  obj)

;; Used by species that are inherently immune:
(define (apply-poison-immunity kobj)
  (kern-obj-add-effect kobj ef_poison_immunity nil)
  kobj)

(define (apply-sleep kobj)
  (cond ((and (obj-is-char? kobj)
              (not (has-sleep-immunity? kobj)))
         (kern-char-set-sleep kobj #t)
         (kern-obj-add-effect kobj ef_sleep nil)))
  kobj)

(define (make-invisible kobj)
  (kern-obj-add-effect kobj ef_permanent_invisibility nil)
  kobj)

(define (apply-acid kchar)
  (if (obj-is-char? kchar)
      (let ((arms (kern-char-get-arms kchar)))
        (if (null? arms)
            (kern-log-msg "Acid has no effect!")
            (let ((ktype (random-select arms)))
              (kern-log-msg "Acid dissolves 1 " (kern-type-get-name ktype) 
                            " held by " (kern-obj-get-name kchar))
              (kern-char-unready kchar ktype)
              (kern-obj-remove-from-inventory kchar ktype 1))))))

(define (generic-burn obj dice)
  (if (and (kern-obj-is-being? obj)
           (not (has-fire-immunity? obj)))
      (begin
        (if (kern-obj-is-being? obj)
            (kern-log-msg (kern-obj-get-name obj) " burned!"))
        ;; FIXME: multiply damage by kern-ticks-per-turn?
        (kern-obj-apply-damage obj "burning" (kern-dice-roll dice)))))

(define (burn obj)
  (generic-burn obj "2d3+2"))

(define (great-burn obj)
  (generic-burn obj "10d8+20"))

;; fixme: what about the player party? probably not safe to just remove it from
;; the map...
(define (chasm-fall kobj)
  (cond ((and (not (can-fly? kobj))
              (not (is-abstract? kobj)))
         (kern-log-msg (kern-obj-get-name kobj) " drops into the abyss!")
         (if (obj-is-char? kobj)
             (kern-char-kill kobj)
             (kern-obj-remove kobj)))))

(define (magical-kill obj)
  (if (and (kern-obj-is-char? obj)
           (not (has-magical-kill-immunity? obj)))
      (kern-char-kill obj)))
  

(define (slip obj)
  (let ((mmode (kern-obj-get-mmode obj)))
    (if (eqv? mmode mmode-walk)
        (if (< (kern-dice-roll "1d20") 5)
            (let ((dir (kern-obj-get-dir obj)))
              (if (not (and (= 0 (car dir))
                            (= 0 (cadr dir))))
                  (begin
                    (kern-obj-move obj (- (car dir)) (- (cadr dir)))
                    (kern-log-msg "Slipped!")
                    (kern-obj-apply-damage obj "slipped" (kern-dice-roll "1d4")))))))))

(define (apply-lightning obj)
  (if (kern-obj-is-being? obj)
      (kern-log-msg (kern-obj-get-name obj) " shocked!"))
  ;; FIXME: multiply damage by kern-ticks-per-turn?
  (kern-obj-apply-damage obj "shocked" (kern-dice-roll "2d8")))

;; Drop a random temporary field on the object's location
(define (apply-random-field kobj)
  (kern-obj-put-at (kern-mk-obj (random-select (list  F_fire 
                                                      F_poison 
                                                      F_sleep 
                                                      F_energy))
                                1)
                   (kern-obj-get-location kobj)))
                   
;; Prismatic -- pick a random effect. This isn't quite what I want, I'd rather
;; go through the powers layer, but that requires me to know who my caster
;; is. This was written to be used by a weapon like a prismatic wand, and the
;; missile procedures don't get the user/caster as a parm (yet).
(define (apply-prismatic kobj)
  (if (or (not (kern-obj-is-being? kobj))
          (contest-of-skill 8 (occ-ability-magicdef kobj)))
      (let ((selection (random-select (list 'paralyze 
                                            'apply-acid
                                            'apply-poison
                                            'burn
                                            'slip
                                            'apply-lightning
                                            'apply-random-field
                                            ))))
        (println selection)
        (apply (eval selection)
               (list kobj)))))
   
                  

;;----------------------------------------------------------------------------
;; Misc stuff -- not sure where to put this
(define (douse ktarg)
  (kern-obj-remove-effect ktarg ef_torchlight))

(define (wind-trap ktarg)
  (kern-log-msg "A gust of wind!")
  (douse ktarg)
  #f ;; prevents removal of trigger
  )
