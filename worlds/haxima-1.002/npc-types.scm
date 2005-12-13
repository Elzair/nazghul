;;----------------------------------------------------------------------------
;; NPC type constructors
;;----------------------------------------------------------------------------

(load "yellow-slime.scm")
(load "troll.scm")
(load "spider.scm")

;;----------------------------------------------------------------------------
;; Local Constants
;;----------------------------------------------------------------------------
(define default-level 1)

;;----------------------------------------------------------------------------
;; Local Procedures
;;----------------------------------------------------------------------------

;; mk-stock-char -- convenience wrapper for kern-mk-char. Handles the
;; boilerplate associated with first-time "stock" character creations. A stock
;; character is a monster, guard or similar cannon-fodder NPC, with no
;; interesting conversation, no schedule of appointments, etc.
(define (mk-stock-char name species occupation sprite faction ai container 
                       arms conv)
  ;;(println "mk-stock-char")
  (kern-mk-char
   nil ;;..........tag
   name ;;.........name
   species ;;.......species
   occupation ;;...occupation
   sprite ;;.......sprite
   faction ;;....;.faction
   0 ;;............custom strength modifier
   0 ;;............custom intelligence modifier
   0 ;;............custom dexterity modifier
   0 ;;............custom base hp modifier
   0 ;;............custom hp multiplier (per-level)
   0 ;;............custom base mp modifier
   0 ;;............custom mp multiplier (per-level)
   (max-hp species occupation default-level 0 0) ;;.current hit points
   0  ;;...........current experience points
   (max-mp species occupation default-level 0 0) ;;.current magic points
   default-level  ;;............current level
   #f ;;...........dead?
   conv ;;.........conversation (optional)
   nil ;;..........schedule (optional)
   ai ;;...........custom ai (optional)
   container ;;....container (and contents)
   arms ;;.........readied arms (in addition to the container contents)
   nil ;;..........hooks in effect
   ))

;; Curried version of mk-stock-char for characters without an occupation, ai,
;; container or armamenets
(define (mk-animal name species sprite)
  (mk-stock-char name species nil sprite faction-none nil nil nil nil))

(define (mk-readied-items . items)
  items)

(define (mk-at-level ctor-tag lvl-dice . args)
  (set-level (apply (eval ctor-tag) args) 
             (kern-dice-roll lvl-dice)))

;; npct -- NPC type
(define (mk-npct name spec occ spr traps equip eff ai faction conv)
  (list name spec occ spr traps equip eff ai faction conv))
(define (npct-name npct) (car npct))
(define (npct-spec npct) (cadr npct))
(define (npct-occ npct) (caddr npct))
(define (npct-spr npct) (cadddr npct))
(define (npct-traps npct) (list-ref npct 4))
(define (npct-eqp npct) (list-ref npct 5))
(define (npct-effects npct) (list-ref npct 6))
(define (npct-ai npct) (list-ref npct 7))
(define (npct-faction npct) (list-ref npct 8))
(define (npct-conv npct) (list-ref npct 9))



;; npcg -- generic NPC gob
(define (npcg-mk type) 
  (list 'npcg 
        type 
        #f  ;; taunted
        #f  ;; spawned
        nil ;; post
        ))
(define (npcg-type npcg) (cadr npcg))
(define (npcg-taunted? npcg) (caddr npcg))
(define (npcg-spawned? npcg) (cadddr npcg))
(define (npcg-is-type? npcg type) (equal? type (npcg-type npcg)))
(define (npcg-set-taunted! npcg val) (set-car! (cddr npcg) val))
(define (npcg-set-spawned! npcg val) (set-car! (cdddr npcg) val))
(define (npcg-set-post! npcg val) (set-car! (list-tail npcg 4) val))
(define (npcg-has-post? npcg) (not (null? (npcg-get-post npcg))))
(define (npcg-get-post npcg) (list-ref npcg 4))
(define (is-npcg? gob) (eq? (car gob) 'npcg))

(define (kbeing-is-npc-type? kbeing type)
  (let ((npcg (gob kbeing)))
    (and (not (null? npcg))
         (is-npcg? npcg)
         (npcg-is-type? npcg type))))

(define (kbeing-was-spawned? kbeing)
  (let ((npcg (gob kbeing)))
    ;;(println "kbeing-was-spawned?" npcg)
    (and (not (null? npcg))
         (is-npcg? npcg)
         (npcg-spawned? npcg))))
  
;; mk-npc -- create a kernel character of the given type, faction and level
(define (mk-npc npct-tag lvl)
  ;;(println "mk-npc:" npct " " faction " " lvl)g
  (let* ((npct (eval npct-tag))
         (npc (bind
               (set-level
                (kern-char-arm-self
                 (mk-stock-char
                  (npct-name npct)
                  (npct-spec npct)
                  (npct-occ npct)
                  (npct-spr npct)
                  (npct-faction npct)
                  (npct-ai npct)
                  (mk-chest
                   (random-select (npct-traps npct))
                   (filter notnull?
                           (map (lambda (x)
                                  (apply roll-to-add x))
                                (npct-eqp npct))))
                  nil
                  (npct-conv npct)))
                lvl)
               (npcg-mk npct-tag))))
    ;; revisit -- will this work or will effects need to be symbol-tags?
    (map (lambda (eff) (kern-obj-add-effect npc eff nil))
         (npct-effects npct))
    npc))

;; spawn-npc -- like mk-npc but mark the npc as spawned (this allows monster
;; managers to periodically clean up old spawned NPC's)
(define (spawn-npc npct-tag lvl)
  (let ((kchar (mk-npc npct-tag lvl)))
    (npcg-set-spawned! (gob kchar) #t)
    kchar))

;;----------------------------------------------------------------------------
;; trap packages
(define no-traps (list nil))
(define basic-traps  (list nil 'burn 'spike-trap))
(define wizard-traps (list nil 'poison-trap 'sleep-trap 'lightning-trap))
(define wrogue-traps (list nil 'self-destruct-trap 'bomb-trap 'sleep-trap 'poison-trap 'spike-trap 'sleep-trap 'burn))

;;----------------------------------------------------------------------------
;; effect packages
(define slime-effects  (list ef_poison_immunity ef_slime_split))
(define skel-effects (list ef_poison_immunity ef_fire_immunity))
(define hydra-effects (list ef_poison_immunity ef_grow_head))
(define drag-effects (list ef_fire_immunity))

;;----------------------------------------------------------------------------
;; equipment packages for different types of npcs
(define wizard-equip 
  (list (list 100 "1"     t_dagger)
        (list 100 "1d2-1" t_heal_potion)
        (list 100 "1d2+1" t_mana_potion)
        (list 100 "1d20"  t_gold_coins)
        (list 10  "1d3"   t_food)
        (list 10  "1"     t_cure_potion)
        (list 10  "1"     t_poison_immunity_potion)
        (list 20  "1d5"   sulphorous_ash)
        (list 20  "1d5"   ginseng)
        (list 20  "1d5"   garlic)
        (list 10  "1d3"   spider_silk)
        (list 10  "1d3"   blood_moss)
        (list 10  "1d3"   black_pearl)
        (list 5   "1d2"   nightshade)
        (list 5   "1d2"   mandrake)
        (list 5   "1"     t_in_mani_corp_scroll)
        (list 5   "1"     t_xen_corp_scroll)
        (list 10  "1"     t_in_quas_xen_scroll)
        (list 10  "1"     t_an_xen_exe_scroll)
        (list 20  "1"     t_in_an_scroll)
        (list 20  "1"     t_vas_mani_scroll)
        ))
(define archer-equip 
  (list (list 100 "1"     t_bow)
        (list 100 "1d6"   t_arrow)
        (list 100 "1"     t_dagger)
        (list 100 "1d10"  t_gold_coins)
        (list 20  "1d3"   t_food)
        ))
(define stalker-equip 
  (list (list 100 "2"     t_dagger)
        (list 100 "2"     t_dagger)
        (list 100 "1d15"  t_gold_coins)
        (list 30  "1d3"   t_food)
        ))
(define slinger-equip 
  (list (list 100 "1"     t_sling)
        (list 100 "1d10"  t_gold_coins)
        (list 20  "1d3"   t_food)
        ))
(define berserker-equip 
  (list (list 100 "2"     t_axe)         
        (list 100 "1d2"   t_heal_potion)
        (list 100 "1d15"  t_gold_coins)
        (list 30  "1d3"   t_food)
        ))
(define ranger-equip
  (list (list 100 "1"     t_sword)
        (list 100 "1"     t_bow)
        (list 100 "20"    t_arrow)
        (list 100 "1"     t_leather_helm)
        (list 100 "1"     t_armor_leather)
        (list 100 "1d3-1" t_heal_potion)
        ))
(define skeletal-warrior-equip
  (list (list 100 "1"     t_sword)
        (list 100 "1"     t_shield)
        (list 100 "1"     t_iron_helm)
        (list 100 "1d20"  t_gold_coins)
        ))
(define spear-thrower-equip
  (list (list 100 "1d20"  t_spear)
        (list 100 "1"     t_iron_helm)
        (list 100 "1"     t_axe)
        (list 100 "1d20"  t_gold_coins)
        ))
(define death-knight-equip
  (list (list 100 "1"     t_2h_axe)
        (list 100 "1"     t_armor_plate)
        (list 100 "1"     t_iron_helm)
        (list 100 "1d20"  t_gold_coins)
        (list 100 "1d3-1" t_mana_potion)
        ))
(define knight-equip
  (list (list 100 "1"     t_2h_sword)
        (list 100 "1"     t_armor_plate)
        (list 100 "1"     t_iron_helm)
        (list 100 "1d20"  t_gold_coins)
        (list 100 "1d3-1" t_heal_potion)
        ))
(define squire-equip
  (list (list 100 "1"     t_crossbow)
        (list 100 "1d10"  t_bolt)
        (list 100 "1"     t_dagger)
        (list 100 "1"     t_armor_chain)
        (list 100 "1"     t_chain_coif)
        (list 100 "1d10"  t_gold_coins)
        (list 100 "1d2-1" t_heal_potion)
        ))
(define halberdier-equip
  (list (list 100 "1"     t_halberd)
        (list 100 "1"     t_chain_coif)
        (list 100 "1"     t_armor_chain)
        (list 100 "1d3-1" t_heal_potion)
        (list 10  "1"     t_vas_mani_scroll)
        (list 10  "1"     t_in_an_scroll)
        (list 50  "1d5"   t_food)
        ))
(define crossbowman-equip
  (list (list 100 "1"     t_crossbow)
        (list 100 "10"    t_bolt)
        (list 100 "1"     t_chain_coif)
        (list 100 "1"     t_dagger)
        (list 100 "1"     t_armor_chain)
        (list 100 "1d3-1" t_heal_potion)
        (list 10  "1"     t_vas_mani_scroll)
        (list 10  "1"     t_in_an_scroll)
        (list 50  "1d5"   t_food)
        ))
(define wrogue-equip
  (list (list 100 "1"     t_dagger)
        (list 100 "1"     t_armor_leather)
        (list 100 "1"     t_leather_helm)
        (list 100 "2d6-2" t_gold_coins)
        (list 100 "1d3-1" t_picklock)
        (list 50  "1d5"   t_food)
        (list 50  "1"     t_sword)
        (list 10  "1d2"   t_oil)
        (list 50  "1d10"  t_arrow)
        (list 50  "1"     t_bow)
        (list 10  "1"     t_in_ex_por_scroll)
        (list 10  "1"     t_wis_quas_scroll)
        (list 5   "1"     t_sanct_lor_scroll)
        (list 5   "1"     t_an_tym_scroll)
        (list 5   "1"     t_vas_rel_por_scroll)
        (list 20  "1"     t_mana_potion)
        (list 10  "1"     t_cure_potion)
        (list 10  "1"     t_poison_immunity_potion)
        (list 10  "1d3"   t_torch)
        ))
(define medik-equip
  (list (list 100 "1d3"   t_heal_potion)
        (list 100 "1d2"   t_mana_potion)
        (list 25  "1d2"   t_cure_potion)
        (list 25  "1d2"   t_vas_mani_scroll)
        (list 20  "1d5"   sulphorous_ash)
        (list 20  "1d5"   ginseng)
        (list 20  "1d5"   garlic)
        (list 10  "1d3"   spider_silk)
        (list 10  "1d3"   blood_moss)
        (list 10  "1d3"   black_pearl)
        (list 5   "1d2"   nightshade)
        (list 5   "1d2"   mandrake)
        (list 100 "1"     t_chain_coif)
        (list 100 "1"     t_staff)
        (list 100 "1"     t_armor_chain)
        ))
(define troll-equip
  (list (list 100 "1d3" t_thrown_boulder)
        (list 25  "1d3" t_food)
        ))
(define geomancer-equip
  (list (list 50  "1d3"   t_gem)
        (list 50  "1d20"  t_gold_coins)
        (list 100 "1d3-1" t_mana_potion)
        ))
(define gint-warrior-equip
  (list (list 100 "1"     t_2h_axe)
        (list 100 "1"     t_2h_sword)
        (list 100 "4d25"  t_gold_coins)
        (list 100 "1d5"   t_food)
        (list 100 "1d3-1" t_heal_potion)
        ))
(define reaper-equip
  (list (list 100 "1d5"   t_torch)
        ))
(define headless-equip
  (list (list 100 "1"     t_axe)
        (list 100 "1"     t_shield)
        (list 100 "1d5-1" t_gold_coins)
        ))
(define dragon-equip
  (list (list 100 "1d100+19" t_gold_coins)
        (list 100 "1d20"     t_food)
        (list 100 "1d5-1"    t_gem)
        ))
(define zorn-equip
  (list (list 100 "1d20+9" t_gold_coins)
        ))
(define craven-archer-equip
  (list (list 100 "1"     t_bow)
        (list 100 "20"    t_arrow)
        (list 100 "1"     t_armor_plate)
        (list 100 "1"     t_iron_helm)
        (list 100 "1d20"  t_gold_coins)
        (list 100 "1d3-1" t_mana_potion)
        ))

;; npc types
;;      scheme variable                 name                       species          occup.     sprite             chest traps  equipment              effects       ai               faction
;;      ======================          ========================== ================ ========== ================== ============ ====================== ============= ==============   ========
(define forest-goblin-shaman   (mk-npct "a forest goblin shaman"   sp_forest_goblin oc_wizard  s_orc              wizard-traps wizard-equip           nil           'shaman-ai       faction-forest-goblin nil))
(define forest-goblin-hunter   (mk-npct "a forest goblin hunter"   sp_forest_goblin oc_warrior s_orc              basic-traps  archer-equip           nil           'generic-ai      faction-forest-goblin nil))
(define forest-goblin-stalker  (mk-npct "a forest goblin stalker"  sp_forest_goblin oc_warrior s_orc              basic-traps  stalker-equip          nil           'generic-ai      faction-forest-goblin nil))
(define cave-goblin-slinger    (mk-npct "a cave goblin slinger"    sp_cave_goblin   oc_warrior s_orc              basic-traps  slinger-equip          nil           'generic-ai      faction-cave-goblin   nil))
(define cave-goblin-berserker  (mk-npct "a cave goblin berserker"  sp_cave_goblin   oc_warrior s_orc              basic-traps  berserker-equip        nil           'generic-ai      faction-cave-goblin   nil))
(define cave-goblin-priest     (mk-npct "a cave goblin priest"     sp_cave_goblin   oc_wizard  s_orc              wizard-traps wizard-equip           nil           'priest-ai       faction-cave-goblin   nil))
(define ranger                 (mk-npct "a ranger"                 sp_human         oc_warrior s_companion_ranger basic-traps  ranger-equip           nil           'ranger-ai       faction-men           'ranger-conv))
(define skeletal-spear-thrower (mk-npct "a skeletal spear-thrower" sp_skeleton      oc_warrior s_skeleton         basic-traps  spear-thrower-equip    skel-effects  'generic-ai      faction-monster       nil))
(define skeletal-warrior       (mk-npct "a skeletal warrior"       sp_skeleton      oc_warrior s_skeleton         basic-traps  skeletal-warrior-equip skel-effects  'generic-ai      faction-monster       nil))
(define death-knight           (mk-npct "a death knight"           sp_skeleton      oc_warrior s_knight           basic-traps  death-knight-equip     nil           'death-knight-ai faction-monster       nil))
(define craven-archer          (mk-npct "a craven archer"          sp_skeleton      oc_warrior s_knight           basic-traps  craven-archer-equip    nil           'craven-archer-ai faction-monster      nil))
(define halberdier             (mk-npct "a halberdier"             sp_human         oc_warrior s_guard            no-traps     halberdier-equip       nil           'guard-ai        faction-men           nil))
(define crossbowman            (mk-npct "a crossbowman"            sp_human         oc_warrior s_guard            no-traps     crossbowman-equip      nil           'guard-ai        faction-men           nil))
(define medik                  (mk-npct "a medik"                  sp_human         oc_wizard  s_companion_wizard no-traps     medik-equip            nil           'medik-ai        faction-men           nil))
(define yellow-slime           (mk-npct "a yellow slime"           sp_yellow_slime  nil        s_yellow_slime     nil          nil                    slime-effects 'yellow-slime-ai faction-monster       nil))
(define green-slime            (mk-npct "a green slime"            sp_green_slime   nil        s_slime            nil          nil                    slime-effects 'animal-ai       faction-monster       nil))
(define giant-spider           (mk-npct "a giant spider"           sp_spider        nil        s_spider           nil          nil                    nil           'spider-ai       faction-monster       nil))
(define queen-spider           (mk-npct "a queen spider"           sp_queen_spider  nil        s_queen_spider     nil          nil                    nil           'spider-ai       faction-monster       nil))
(define bandit                 (mk-npct "a bandit"                 sp_human         oc_wrogue  s_brigand          wrogue-traps wrogue-equip           nil           'std-ai          faction-outlaw        nil))
(define troll                  (mk-npct "a troll"                  sp_troll         oc_warrior s_troll            no-traps     troll-equip            nil           'std-ai          faction-troll         nil))
(define troll-geomancer        (mk-npct "a troll geomancer"        sp_troll         oc_wizard  s_troll            no-traps     geomancer-equip        nil           'std-ai          faction-troll         nil))
(define gint-warrior           (mk-npct "a gint warrior"           sp_gint          oc_warrior s_ettin            basic-traps  gint-warrior-equip     nil           'std-ai          faction-gint          nil))
(define gint-mage              (mk-npct "a gint mage"              sp_gint          oc_wizard  s_ettin            wizard-traps wizard-equip           nil           'shaman-ai       faction-gint          nil))
(define bull                   (mk-npct "a bull"                   sp_bull          nil        s_bull             nil          nil                    nil           'animal-ai       faction-none          nil))
(define kraken                 (mk-npct "kraken"                   sp_kraken        nil        s_kraken           nil          nil                    nil           'kraken-ai       faction-monster       nil))
(define sea-serpent            (mk-npct "sea serpent"              sp_sea_serpent   nil        s_sea_serpent      nil          nil                    nil           'sea-serpent-ai  faction-monster       nil))
(define dryad                  (mk-npct "dryad"                    sp_dryad         nil        s_reaper           nil          reaper-equip           nil           'spell-sword-ai  faction-monster       nil))
(define wolf                   (mk-npct "wolf"                     sp_wolf          nil        s_wolf             nil          nil                    nil           'animal-ai       faction-monster       nil))
(define gazer                  (mk-npct "gazer"                    sp_gazer         oc_wizard  s_gazer            wizard-traps nil                    nil           'gazer-ai        faction-monster       nil))
(define headless               (mk-npct "headless"                 sp_headless      oc_warrior s_headless         basic-traps  headless-equip         nil           'std-ai          faction-monster       nil))
(define wisp                   (mk-npct "wisp"                     sp_wisp          nil        s_wisp             nil          nil                    nil           'std-ai          faction-monster       nil))
(define dragon                 (mk-npct "dragon"                   sp_dragon        nil        s_dragon           wizard-traps dragon-equip           drag-effects  'dragon-ai       faction-monster       nil))
(define zorn                   (mk-npct "zorn"                     sp_zorn          oc_wrogue  s_zorn             wrogue-traps zorn-equip             nil           'std-ai          faction-monster       nil))
(define demon                  (mk-npct "demon"                    sp_demon         nil        s_demon            basic-traps  nil                    nil           'std-ai          faction-monster       nil))
(define hydra                  (mk-npct "hydra"                    sp_hydra         nil        s_hydra            no-traps     nil                    hydra-effects 'hydra-ai        faction-monster       nil))
(define lich                   (mk-npct "lich"                     sp_lich          oc_wizard  s_lich             wizard-traps wizard-equip           skel-effects  'spell-sword-ai  faction-monster       nil))
(define warlock                (mk-npct "warlock"                  sp_human         oc_wizard  s_wizard           wizard-traps wizard-equip           nil           'spell-sword-ai  faction-monster       nil))
(define ghast                  (mk-npct "ghast"                    sp_ghast         nil        s_ghost            nil          nil                    nil           'std-ai          faction-monster       nil))
(define corrupt-halberdier     (mk-npct "a halberdier"             sp_human         oc_warrior s_guard            no-traps     halberdier-equip       nil           'guard-ai        faction-monster       nil))
(define corrupt-crossbowman    (mk-npct "a crossbowman"            sp_human         oc_warrior s_guard            no-traps     crossbowman-equip      nil           'guard-ai        faction-monster       nil))
(define knight                 (mk-npct "a knight"                 sp_human         oc_warrior s_knight           no-traps     knight-equip           nil           'guard-ai        faction-trigrave      'knight-conv))
(define squire                 (mk-npct "a squire"                 sp_human         oc_warrior s_guard            no-traps     squire-equip           nil           'guard-ai        faction-trigrave      'knight-conv))

;;define                        (mk-npct "                          sp_              oc_        s_                 nil          nil                    nil           'std-ai           ))

;;----------------------------------------------------------------------------
;; Type queries
;;----------------------------------------------------------------------------
(define (is-species? kchar species)
  (eqv? (kern-char-get-species kchar) species))

(define (is-occ? kchar occ)
  (eqv? (kern-char-get-occ kchar) occ))

(define (is-yellow-slime? kchar)
  (is-species? kchar sp_yellow_slime))

(define (is-green-slime? kchar)
  (println "is-green-slime?")
  (is-species? kchar sp_green_slime))

(define (is-spider? kchar)
  (or (is-species? kchar sp_spider)
      (is-species? kchar sp_queen_spider)))

(define (is-troll? kchar)
  (is-species? kchar sp_troll))

(define (is-goblin? kchar)
  (or (is-species? kchar sp_cave_goblin)
      (is-species? kchar sp_forest_goblin)))

(define (is-skeleton? kchar)
  (is-species? kchar sp_skeleton))

(define (is-death-knight? kchar)
  (and (is-species? kchar sp_skeleton)
       (is-occ? kchar oc_warrior)))

(define (is-bandit? kchar)
  (is-occ? kchar oc_wrogue))

(define (is-halberdier? kchar)
  (is-guard-type? kchar 'halberdier))

(define (is-crossbowman? kchar)
  (is-guard-type? kchar 'crossbowman))

(define (is-gint? kchar)
  (is-species? kchar sp_gint))

(define (is-forest-goblin-shaman? kchar)
  (and (is-species? kchar sp_forest_goblin)
       (is-occ? kchar oc_wizard)))

(define (is-forest-goblin-hunter? kchar)
  (and (is-species? kchar sp_forest_goblin)
       (is-occ? kchar oc_warrior)))

;; fixme -- same as forest-goblin-hunter above
(define (is-forest-goblin-stalker? kchar)
  (and (is-species? kchar sp_forest_goblin)
       (is-occ? kchar oc_warrior)))

(define (is-skeletal-warrior? kchar)
  (let ((gob (kobj-gob-data kchar)))
    (and (not (null? gob))
         (eq? (car gob)
              'skeletal-warrior))))

(define (post-guard kguard x y)
  (npcg-set-post! (gob kguard) (list x y))
  kguard)

