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
(load "test/19x19.scm")
(load "test/player.scm")

;; Startup proc
(define (create-char kplayer)
 (kern-obj-put-at kplayer (list p_19x19 12 10)))

(kern-add-hook 'new_game_start_hook 'create-char)

;;----------------------------------------------------------------------------
(give player t_in_quas_xen_scroll 10)
(give player t_an_xen_ex_scroll 10)
(give player t_xen_corp_scroll 10)

(give player sulphorous_ash 100)
(give player ginseng 100)
(give player garlic 100)
(give player spider_silk 100)
(give player blood_moss 100)
(give player black_pearl 100)
(give player nightshade 100)
(give player mandrake 100)
(give player t_royal_cape 100)

(set-level ch_wanderer 9)


(kern-mk-char 
 'ch_thorald ; tag
 "Thorald Greybeard"   ; name
 sp_human              ; species
 oc_wrogue             ; occ
 s_companion_wizard    ; sprite
 faction-player        ; starting alignment
 0 10 2                ; str/int/dex
 0 1                   ; hp mod/mult
 10 5                  ; mp mod/mult
 240 -1 8 0 8             ; hp/xp/mp/AP_per_turn/lvl
 #f                    ; dead
 'conv-thorald         ; conv
 nil                   ; sched
 'thorald-ai           ; special ai
 nil                   ; container
 (list t_sling
       t_armor_leather
       )
 nil)

(kern-party-add-member player ch_thorald)


;;(let ((kchar (mk-npc 'headless 1))) (kern-obj-put-at kchar (loc-mk p_19x19 10 10)))
;;(let ((kchar (mk-npc 'wolf 1))) (kern-obj-put-at kchar (loc-mk p_19x19 4 9)))
;;(let ((kchar (mk-npc 'wolf 1))) (kern-obj-put-at kchar (loc-mk p_19x19 4 8)))
;;(let ((kchar (mk-npc 'wolf 1))) (kern-obj-put-at kchar (loc-mk p_19x19 4 10)))

(let* ((kmir (kern-tag 'mirror (mk-mirror 's_mirror_bg_flagstones)))
       (ksen (mk-char-sensor 'mirror)))
  (kern-obj-put-at ksen (loc-mk p_19x19 11 10))
  (kern-obj-put-at kmir (loc-mk p_19x19 11 9)))