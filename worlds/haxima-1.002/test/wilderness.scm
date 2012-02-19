;; Test the In Quas Xen (clone) spell using a scroll.
;;
;; Setup a place with the player and two other npcs and several scrolls in
;; posession. The npcs should have immobile ai's so they stay put. One npc
;; should be just in range and the other just out of range. Have the player ai
;; use the scroll on itself first. It should validate the clone. Then have the
;; player ai use the scroll on the npc in range and validate its clone. Finally
;; it should use the scroll on the npc out of range and verify that no clone
;; exists.
;;
;; Validating a clone means checking that a) it has the same equipment as the
;; original, b) has a lower level than the caster and c) has mana and hp <=
;; caster.

;; Customize
(if (not (defined? 'include-sun))
    (define include-sun #t))

;; Load generic data
(load "haxima.scm")
(kern-load "test/island.scm")
(load "test/tower.scm")
(load "test/player.scm")
(load "test/cave.scm")

;; Startup proc
(define (create-char kplayer)
 (kern-obj-put-at kplayer (list p_island 9 5)))

(kern-add-hook 'new_game_start_hook 'create-char)

;;----------------------------------------------------------------------------

(give player t_in_quas_xen_scroll 10)
(give player t_an_tym_scroll 10)

(give player t_armor_plate 1)
(give player t_iron_helm 1)
(kern-char-ready ch_wanderer t_armor_plate t_iron_helm)

(set-level ch_wanderer 9)


(kern-obj-put-at (mk-npc-party 'pirate-party-l3) (loc-mk p_island 0 0))
(kern-obj-put-at (mk-npc-party 'forest-goblin-party-l4) (loc-mk p_island 5 3))
;(kern-obj-put-at (mk-npc-party 'forest-goblin-party-l4) (loc-mk p_island 5 2))
;(kern-obj-put-at (mk-npc-party 'forest-goblin-party-l1) (loc-mk p_island 5 7))
;(kern-obj-put-at (mk-npc-party 'forest-goblin-party-l1) (loc-mk p_island 7 5))
;(kern-obj-put-at (mk-npc-party 'forest-goblin-party-l1) (loc-mk p_island 3 5))
;(kern-obj-put-at (mk-npc-party 'forest-goblin-party-l1) (loc-mk p_island 1 1))

(kern-place-set-subplace p_tower (list p_island 5 5))
(kern-obj-put-at (mk-dungeon 'p_cave 7 12) (list p_island 10 5))
(kern-obj-put-at (mk-ladder-down 'p_cave 7 14) (list p_tower 8 9))
