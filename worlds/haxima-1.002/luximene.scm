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
  (say knpc "Ask a wizard, not a king."))

(define lux-conv
  (ifc basic-conv

       ;; basics
       (method 'default lux-default)
       (method 'hail lux-hail)
       (method 'name lux-name)
       (method 'rune lux-rune)
       (method 'gate lux-gate)

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
    (max-hp lux-species lux-occ lux-lvl 0 0) ; hp
    0                   ; xp
    (max-mp lux-species lux-occ lux-lvl 0 0) ; mp
    lux-lvl
    #f               ; dead
    'lux-conv         ; conv
    sch_my           ; sched
    nil              ; special ai
    nil              ; container
    nil              ; readied
    )
   (lux-mk)))

;; Luximene begins the game as a Lich King, when defeated he drops his skull,
;; which can be used with the Necromancer to summon his shade.
(mk-obj-type 't_lich_skull "King Luximenes skull" s_magic layer-item obj-ifc)

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
            (max-hp lux-species lux-occ 8 10 10) ; hp
            0                   ; xp
            (max-mp lux-species lux-occ 8 10 10) ; mp
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
         skel-effects)
    kchar))

