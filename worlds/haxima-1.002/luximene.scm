;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define lux-lvl 7)
(define lux-species sp_ghast)
(define lux-occ oc_wizard)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (lux-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
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
        (say knpc "Then why dost thou ask me of this Rune? Did they master not explain?")
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
      (say knpc "[The room fills with his terrible laughter] But it is thine to know, "
           "Last of the Keepers!"))
  (say knpc "The Lost Age will end when the Demon Gate is opened."))

(define (lux-keep knpc kpc)
  (say knpc "Thou art the Last Keeper. You have a Rune, and know its meaning. "
       "Now all the Runes are thine to Keep. Doest thou have them all?")
  (if (yes? kpc)
      (say knpc "Be thou certain, Keeper. The Gate will not open without them all.")
      (say knpc "Find them. One honorable man will save an Age from disgrace, "
           "though perhaps not himself.")))


(define lux-conv
  (ifc basic-conv

       ;; basics
       (method 'default lux-default)
       (method 'hail lux-hail)
       (method 'name lux-name)
       (method 'rune lux-rune)
       (method 'gate lux-gate)
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
            8
            #f               ; dead
            nil              ; conv
            nil             ; sched
            'lich-ai        ; special ai
            (mk-chest
             'lightning-trap
             (list (list 1 t_morning_star)
                   (list 1 t_armor_chain_4)
                   (list 1 t_chain_coif_4)
                   (list 100 t_gold_coins)
                   (list 3 t_mana_potion)
                   (list 3 t_heal_potion)
                   (list 1 t_lich_skull)
                   ))
            nil              ; readied
            )
           #t)
          (lux-mk))))
    (map (lambda (eff) (kern-obj-add-effect kchar eff nil))
         undead-effects)
    kchar))

