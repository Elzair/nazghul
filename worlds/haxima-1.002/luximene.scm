;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define lux-lvl 7)
(define lux-species sp_ghast)
(define lux-occ oc_wizard)

;;----------------------------------------------------------------------------
;; Schedule
;;
;; No schedule (found in Green Tower Tombs)
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (lux-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Luximene is a lich (or later, a shade conjured up from his skull),
;; sitting in rulership over the small realm of his treasury in 
;; ruined Kurpolis.  In life, he was a great king who conquered and united 
;; the Shard, and was ultimately overthrown by a coalition of those who 
;; became known as the Wise.
;;----------------------------------------------------------------------------

;; Basics...
(define (lux-hail knpc kpc)
  (say knpc "[You meet a shade who burns with malice] What do you want, "
       "mortal?"))

(define (lux-default knpc kpc)
  (say knpc "Do not waste my time."))

(define (lux-name knpc kpc)
  (say knpc "I am Luximene the Great, Lord of the Middle Realm."))

(define (lux-rune knpc kpc)
  (say knpc "[He suddenly looks disturbed] That is a key, one of eight, "
       "made by the Wise to lock the Demon Gate."))

(define (lux-gate knpc kpc)
  (say knpc "Each key was given to a Keeper, each Keeper was forbidden to meet with any other, "
       "and to pass their legacy on to an heir. Art thou a Keeper?")
  (if (yes? kpc)
      (begin
        (say knpc "Then why dost thou ask me of this Rune? Did thy master not explain?")
        (if (yes? kpc)
            (say knpc "[He chuckles malignantly] Thou thinkest me a fool, "
                 "and art a fool for thinking so. The Rune will be thy undoing, "
                 "and the undoing of thine Age!")
            (say knpc "Cursed is the Age wherein Men forget their vows.")))
      (begin
        (say knpc "Art thou a Thief?")
        (if (yes? kpc)
            (say knpc "Doomed is the Age where even thieves know not the value of what they steal.")
            (say knpc "Then thou hast acquired the Rune by chance, "
                 "and the end of thine Age is nigh.")))))

(define (lux-age knpc kpc)
  (say knpc "When the Demon Gate was sealed, the Age of Wizards ended. "
       "It was I who conquered and united the Shard, and The Age of Luximene began. "
       "After me each ruler was worse than the one before, and my empire fell into ruin, "
       "and the present age, the Lost Age, began. Do you wish to know a great secret?")
  (if (yes? kpc)
      (say knpc "Then I will tell you, for you are the Last Keeper.")
      (say knpc "\n[The room fills with his terrible laughter]\n"
	   "But it is thine to know,\n"
           "Last of the Keepers!"))
  (say knpc "The Lost Age will end when the Demon Gate is opened."))

(define (lux-keep knpc kpc)
  (say knpc "Thou art the Last Keeper. You have a Rune, and know its meaning. "
       "Now all the Runes are thine to Keep. Doest thou have them all?")
  (if (yes? kpc)
      (say knpc "Be thou certain, Keeper. The Gate will not open without them all.")
      (say knpc "Find them. One honorable man will save an Age from disgrace, "
           "though perhaps not himself.")))

(define (lux-wise knpc kpc)
  (say knpc "The Wise?  [He laughs]\n"
       "Rebels against the ruler of an Age!\n"
       "\n"
       "The Warrior Argus,\n"
       "  who defeated my armies\n"
       "  and led their expedition\n"
       "The Wizard Xileph,\n"
       "  who counseled rebellion\n"
       "  and countered my sorceries\n"
       "The Wrogue Narthax,\n"
       "  whose thefts made bold mine enemies\n"
       "  and found my secret passages\n"
       "The Wright Weilend,\n"
       "  who outfitted the quest\n"
       "  against my stronghold\n"
       "\n"
       "Yet in my defeat, was wrought their DOOM!"
       "\n")
  (say knpc "\n[He gazes at you in expectation]\n"
       "What?  Know ye not these names?")
  (if (yes? kpc)
      (say knpc "Then marvel, at my legend!")
      (begin
	(say knpc
	     "\n[His cold eyes glint in anger]\n"
	     "Ah, rather you refer to those called Wise of this Age.\n"
	     "I know nothing of such trifling matters.\n"
	     "Begone, mortal!")
	(kern-conv-end)
	))
  )

(define (lux-accu knpc kpc)
  (say knpc "Accursed?\n"
       "In each Age there are those who seek power, "
       "who will not be swayed from that search by warning or scruples.\n"
       "\n"
       "Some will be destroyed by this search, "
       "while others land in thrall to those few who gain true power.\n"
       "\n"
       "Such must be, the worms of which you speak.")
  )


(define lux-conv
  (ifc basic-conv

       ;; basics
       (method 'default lux-default)
       (method 'hail lux-hail)
       (method 'name lux-name)

       (method 'rune lux-rune)
       (method 'key  lux-rune) ;; A synonym

       (method 'demo lux-gate) ;; A synonym
       (method 'gate lux-gate)

       (method 'wise lux-wise)
       (method 'accu lux-accu)

       (method 'age  lux-age)
       (method 'keep lux-keep)

       ))

(define (mk-luximene)
  (bind 
   (kern-mk-char 
    'ch_lux           ; tag
    "Luximene"             ; name
    lux-species         ; species
    lux-occ              ; occ
    s_ghost     ; sprite
    faction-men      ; starting alignment
    0 0 0            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    max-health ; hp
    -1                   ; xp
    max-health ; mp
    0
    lux-lvl
    #f               ; dead
    'lux-conv         ; conv
    nil           ; sched
    nil              ; special ai
    nil              ; container
    nil              ; readied
    )
   (lux-mk)))

(define (mk-lich-king)
  (let ((kchar 
         (bind 
          (kern-char-force-drop
           (kern-mk-char 
            'ch_lich_king           ; tag
            "Lich King" ; name
            sp_lich         ; species
            oc_wizard              ; occ
            s_lich     ; sprite
            faction-monster      ; starting alignment
            10 10 10            ; str/int/dex
            10 1              ; hp mod/mult
            0  0              ; mp mod/mult
            max-health ; hp
            -1                   ; xp
            max-health ; mp
            0
            8
            #f               ; dead
            nil              ; conv
            nil             ; sched
            'lich-ai        ; special ai
            (mk-inventory
             (list (list 1 t_morning_star)
                   (list 1 t_armor_chain_4)
                   (list 1 t_chain_coif_4)
                   (list 100 t_gold_coins)
                   (list 3 t_mana_potion)
                   (list 3 t_heal_potion)
                   (list 1 t_lich_skull)
                   (list 1 t_lichs_blood)
                   ))
            nil              ; readied
            )
           #t)
          (lux-mk))))
    (map (lambda (eff) (kern-obj-add-effect kchar eff nil))
         undead-effects)
    kchar))

