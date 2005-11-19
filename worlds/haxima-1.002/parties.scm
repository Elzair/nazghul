;; ----------------------------------------------------------------------------
;; Parties
;;
;; These are used to generate random wilderness encounters.
;; ----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Local Procedures
;;----------------------------------------------------------------------------
(define (mk-group species sprite quantity factory)
  (list species sprite quantity factory))

(define (mk-groups . groups)
  groups)

;; holdover procedures until party generation is revisited
(define (mk-goblin-raider)
  (mk-npc 'cave-goblin-berserker faction-cave-goblin 4))
(define (mk-skeletal-warrior)
  (mk-npc 'skeletal-warrior faction-monster 4))

;;----------------------------------------------------------------------------
;; Party Types
;;----------------------------------------------------------------------------
(kern-mk-party-type
 't_goblin_horde ;;.....tag
 "horde of goblins" ;;..name
 s_orc ;;...............sprite (used in wilderness)
 nil ;;.................formation (optional)
 ;;.....................sub-groups that comprise the party
 (mk-groups (mk-group sp_cave_goblin s_orc "1d6" 'mk-goblin-raider))
 )

(kern-mk-party-type
 't_skeleton_brigade ;;.........tag
 "brigade of skeletons" ;;......name
 s_skeleton ;;............ .....sprite (used in wilderness)
 nil ;;.........................formation (optional)
 ;;.............................subgroups  that comprise the party
 (mk-groups (mk-group sp_skeleton s_skeleton "1d8" 'mk-skeletal-warrior)))

(kern-mk-party-type
 't_slime_glob ;;.......tag
 "glob of slimes" ;; ...name
 s_yellow_slime ;;......sprite (used in wilderness)
 nil ;;.................formation (optional)
 ;;.....................subgroups  that comprise the party
 (mk-groups (mk-group sp_yellow_slime s_yellow_slime "1d2" 'mk-yellow-slime)))

(kern-mk-party-type
 't_wood_spiders      ;; tag
 "clutter of spiders" ;; name
 s_spider             ;; sprite
 nil                  ;; formation
 (mk-groups (mk-group sp_spider s_spider "1d4" 'mk-wood-spider)))
 
(kern-mk-party-type
 't_queen_wood_spiders ;; tag
 "clutter of spiders"  ;; name
 s_spider              ;; sprite
 nil                   ;; formation
 (mk-groups (mk-group sp_spider s_spider "1d4" 'mk-wood-spider)
            (mk-group sp_queen_spider s_queen_spider "1" 'mk-queen-spider)))

(kern-mk-party-type
 't_bandit_gang        ;; tag
 "gang of bandits"     ;; name
 s_brigand             ;; sprite
 nil                   ;; formation
 (mk-groups (mk-group sp_human s_brigand "1d4" 'mk-bandit)))
