;;----------------------------------------------------------------------------
;; pgroup -- one type of npc within an npc party type
(define (pgroup-mk npct dice)
  (list 'pgroup npct dice))
(define (pgroup-npct pgrp) (cadr pgrp))
(define (pgroup-dice pgrp) (caddr pgrp))
(define (pgroup-size pgrp)
  (define (loop n sum)
    (if (= n 0)
        sum
        (loop (- n 1)
              (+ sum (max 0 (kern-dice-roll (pgroup-dice pgrp))))
              )))
  (loop (length (kern-party-get-members (kern-get-player)))
        0)
  )
(define (pgroup-generate pgrp)
  (println " pgroup-generate")
  (define (loop n)
    (if (<= n 0)
        nil
        (cons (mk-npc (pgroup-npct pgrp) (calc-level))
              (loop (- n 1)))))
  (loop (pgroup-size pgrp)))

;;----------------------------------------------------------------------------
;; ptype -- npc party type
(define (ptype-mk name sprite faction dc scarce . groups)
  (list 'ptype sprite faction groups name dc scarce))
(define (ptype-sprite ptype) (cadr ptype))
(define (ptype-faction ptype) (caddr ptype))
(define (ptype-groups ptype) (cadddr ptype))
(define (ptype-name ptype) (list-ref ptype 4))
(define (ptype-dc ptype) (list-ref ptype 5))
(define (ptype-scarcity ptype) (list-ref ptype 6))
(define (ptype-generate ptype)
  (println "ptype-generate")
  (let ((kparty (kern-mk-party)))
    (kern-being-set-name kparty (ptype-name ptype))
    (kern-obj-set-sprite kparty (ptype-sprite ptype))
    (kern-being-set-base-faction kparty (ptype-faction ptype))
    (map (lambda (pgroup)
           (map (lambda (kchar)
                  (kern-party-add-member kparty kchar))
                (pgroup-generate pgroup)))
         (ptype-groups ptype))
    kparty
    ))

;;----------------------------------------------------------------------------
;; mk-npc-party
(define (mk-npc-party ptype) 
  (println "mk-npc-party")
  (ptype-generate (eval ptype)))

;;----------------------------------------------------------------------------
;; NPC PARTY TYPES
(define forest-goblin-party-l1
  (ptype-mk "goblin stalker" s_orc faction-forest-goblin 1 2
            (pgroup-mk 'forest-goblin-stalker "1")
            ))
            
(define forest-goblin-party-l2
  (ptype-mk "goblin scouting party" s_orc faction-forest-goblin 2 2
            (pgroup-mk 'forest-goblin-stalker "1")
            (pgroup-mk 'forest-goblin-hunter  "1")
            ))
            
(define forest-goblin-party-l3 
  (ptype-mk "goblin hunting party" s_orc faction-forest-goblin 3 2
            (pgroup-mk 'forest-goblin-stalker "1d2")
            (pgroup-mk 'forest-goblin-hunter  "1d2")
            (pgroup-mk 'wolf "1d2")
            ))

(define forest-goblin-party-l4 
  (ptype-mk "goblin tribe" s_orc faction-forest-goblin 4 2
            (pgroup-mk 'forest-goblin-stalker "1d2")
            (pgroup-mk 'forest-goblin-hunter  "1d2")
            (pgroup-mk 'wolf "1d2")
            (pgroup-mk 'forest-goblin-shaman "1")
            ))

(define bandit-party 
  (ptype-mk "bandit gang" s_brigand faction-outlaw 3 5
            (pgroup-mk 'bandit "1d2")
            ))

(define troll-party-l2
  (ptype-mk "band of trolls" s_troll faction-troll 3 5
            (pgroup-mk 'troll "1")
            ))

(define troll-party-l3
  (ptype-mk "troll with goblin slaves" s_troll faction-troll 3 5
            (pgroup-mk 'troll "1")
            (pgroup-mk 'forest-goblin-stalker "1d3-1")
            ))

(define green-slime-party-l2
  (ptype-mk "ooze of slimes" s_slime faction-monster 2 3
            (pgroup-mk 'green-slime "1d3")
            ))

(define yellow-slime-party-l3
  (ptype-mk "yellow slime colony" s_yellow_slime faction-monster 3 3
            (pgroup-mk 'green-slime "1d3")
            (pgroup-mk 'yellow-slime "1")
            ))

(define hydra-party-l5
  (ptype-mk "hydra with slimes" s_hydra faction-monster 5 3
            (pgroup-mk 'green-slime "1d3")
            (pgroup-mk 'yellow-slime "1d2")
            (pgroup-mk 'hydra "1")
            ))

(define dryad-party-l3
  (ptype-mk "dryad grove" s_reaper faction-monster 3 5
            (pgroup-mk 'dryad "1")
            ))

(define dryad-party-l4
  (ptype-mk "dryad grove with wolves" s_reaper faction-monster 4 5
            (pgroup-mk 'dryad "1")
            (pgroup-mk 'wolf "1d3")
            ))

(define dryad-party-l5
  (ptype-mk "dryad grove with shamans" s_reaper faction-monster 5 5
            (pgroup-mk 'dryad "1")
            (pgroup-mk 'forest-goblin-shaman "1d3")
            ))

(define wolf-party-l1 
  (ptype-mk "wolf pack" s_wolf faction-monster 2 5
            (pgroup-mk 'wolf "1")
            ))

(define wolf-party-l2 
  (ptype-mk "wolf pack" s_wolf faction-monster 2 5
            (pgroup-mk 'wolf "1d3")
            ))

(define skeleton-party-l2
  (ptype-mk "skeleton brigade" s_skeleton faction-monster 2 5
            (pgroup-mk 'skeletal-warrior "1")
            ))

(define skeleton-party-l3
  (ptype-mk "skeleton brigade" s_skeleton faction-monster 3 5
            (pgroup-mk 'skeletal-warrior "1")
            (pgroup-mk 'skeletal-spear-thrower "1")
            ))

(define skeleton-party-l4 
  (ptype-mk "skeleton brigade" s_skeleton faction-monster 4 5
            (pgroup-mk 'skeletal-warrior "1d2")
            (pgroup-mk 'skeletal-spear-thrower "1d3")
            ))

(define lich-party-l5
  (ptype-mk "lich with undead servants" s_lich faction-monster 5 5
            (pgroup-mk 'lich "1")
            (pgroup-mk 'skeletal-warrior "1d2")
            (pgroup-mk 'skeletal-spear-thrower "1d3")
            ))

(define wisp-party-l5
  (ptype-mk "swarm of wisps" s_wisp faction-none 5 5
            (pgroup-mk 'wisp "1d3")
            ))

(define ghast-party 
  (ptype-mk "haunt of ghasts" s_ghost faction-monster 1 2
            (pgroup-mk 'ghast "1d3")
            ))

(define dragon-party-l6
  (ptype-mk "lone dragon" s_dragon faction-monster 6 1
            (pgroup-mk 'dragon "1")
            ))

(define dragon-party-l7
  (ptype-mk "dragon with cave goblins" s_dragon faction-monster 7 1
            (pgroup-mk 'dragon "1")
            (pgroup-mk 'cave-goblin-berserker "1d2")
            (pgroup-mk 'cave-goblin-slinger "1d2")
            (pgroup-mk 'cave-goblin-priest "1")
            ))

(define dragon-party-l8
  (ptype-mk "flock of dragons" s_dragon faction-monster 8 1
            (pgroup-mk 'dragon "1d3")
            ))

(define gint-party-l4
  (ptype-mk "gint scouting party" s_ettin faction-gint 4 5
            (pgroup-mk 'gint-warrior "1")
            ))

(define gint-party-l5
  (ptype-mk "gint hunting party" s_ettin faction-gint 5 5
            (pgroup-mk 'gint-warrior "1")
            (pgroup-mk 'cave-goblin-slinger "1d2")
            (pgroup-mk 'wolf "1d2")
            ))

(define gint-party-l6
  (ptype-mk "gint war party" s_ettin faction-gint 6 3
            (pgroup-mk 'gint-warrior "1d2")
            (pgroup-mk 'troll "1d2")
            (pgroup-mk 'cave-goblin-slinger "1d3")
            (pgroup-mk 'wolf "1d2")
            ))

(define kraken-party-l3
  (ptype-mk "school of kraken" s_kraken faction-monster 3 5
            (pgroup-mk 'kraken "1d2")
            ))

(define sea-serpent-party-l3
  (ptype-mk "school of sea serpents" s_sea_serpent faction-monster 3 5
            (pgroup-mk 'sea-serpent "1d2")
            ))

(define nixie-party-l2
  (ptype-mk "school of nixies" s_nixie faction-monster 2 3
            (pgroup-mk 'nixie-warrior "1d2")
            ))

(define nixie-party-l3
  (ptype-mk "school of nixies" s_nixie faction-monster 3 5
            (pgroup-mk 'nixie-warrior "1d2")
            (pgroup-mk 'kraken "1")
            ))

(define nixie-party-l4
  (ptype-mk "school of nixies" s_nixie faction-monster 4 5
            (pgroup-mk 'nixie-warrior "1d2")
            (pgroup-mk 'sea-serpent "1")
            ))

(define spider-party-l3
  (ptype-mk "clutch of spiders" s_spider faction-spider 3 5
            (pgroup-mk 'giant-spider "1d2")
            ))

(define spider-party-l4
  (ptype-mk "clutch of spiders" s_spider faction-spider 4 5
            (pgroup-mk 'giant-spider "1d2")
            (pgroup-mk 'queen-spider "1")
            ))
