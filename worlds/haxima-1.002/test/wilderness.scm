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
(load "test/player.scm")

;; Startup proc
(define (create-char kplayer)
 (kern-obj-put-at kplayer (list p_island 10 10)))

(kern-add-hook 'new_game_start_hook 'create-char)

;;----------------------------------------------------------------------------

(give player t_in_quas_xen_scroll 10)
(set-level ch_wanderer 9)

;(let ((kchar (mk-npc 'paladin 9)))
;  (kern-obj-put-at kchar (loc-mk p_island 5 5)))