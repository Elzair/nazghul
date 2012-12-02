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
;(kern-load "test/island.scm")
(kern-load "test/bigmap.scm")
(load "test/tower.scm")
(load "test/player.scm")
(load "test/cave.scm")

;; Startup proc
(define (create-char kplayer)
 (kern-obj-put-at kplayer (list p_world 256 256)))

(kern-add-hook 'new_game_start_hook 'create-char)

;;----------------------------------------------------------------------------

(give player t_in_quas_xen_scroll 10)
(give player t_an_tym_scroll 10)

(give player t_armor_plate 1)
(give player t_iron_helm 1)
(kern-char-ready ch_wanderer t_armor_plate t_iron_helm)

(set-level ch_wanderer 9)
(kern-obj-add-food player 1000)

;(kern-obj-put-at (mk-npc-party 'pirate-party-l3) (loc-mk p_island 0 0))
;(kern-obj-put-at (mk-npc-party 'forest-goblin-party-l4) (loc-mk p_island 5 3))
;(kern-obj-put-at (mk-npc-party 'forest-goblin-party-l4) (loc-mk p_island 5 2))
;(kern-obj-put-at (mk-npc-party 'forest-goblin-party-l1) (loc-mk p_island 5 7))
;(kern-obj-put-at (mk-npc-party 'forest-goblin-party-l1) (loc-mk p_island 7 5))
;(kern-obj-put-at (mk-npc-party 'forest-goblin-party-l1) (loc-mk p_island 3 5))
;(kern-obj-put-at (mk-npc-party 'forest-goblin-party-l1) (loc-mk p_island 1 1))

;(kern-place-set-subplace p_tower (list p_island 5 5))
;(kern-obj-put-at (mk-dungeon 'p_cave 7 12) (list p_island 10 5))
;(kern-obj-put-at (mk-ladder-down 'p_cave 7 14) (list p_tower 8 9))

(if #t
    (begin
      (kern-obj-put-at 
       (mk-chest
        nil ;; trap
        
        '(
         
         ;; Food
         ( 1000 t_food)
         
         ;; Gold
         ( 1000 t_gold_coins)
         
         ;; Reagents
         ( 55 sulphorous_ash)
         ( 55 ginseng)
         ( 55 garlic)
         ( 55 spider_silk)
         ( 53 blood_moss)
         ( 53 black_pearl)
         ( 51 nightshade)
         ( 51 mandrake)
         
         ;; Items
         ( 9 t_vas_mani_scroll)
         ( 9 t_in_ex_por_scroll)
         ( 9 t_sanct_lor_scroll)
         ( 9 t_wis_quas_scroll)
         ( 9 t_xen_corp_scroll)
         ( 9 t_an_xen_ex_scroll)
         ( 9 t_vas_rel_por_scroll)
         ( 59 t_gem)
         ( 9 t_cure_potion)
         ( 9 t_mana_potion)
         ( 99 t_xp_potion)
         ( 99 t_str_potion)
         ( 99 t_dex_potion)
         ( 99 t_int_potion)
         ( 99 t_info_potion)
         ( 99 t_torch)
         ( 99 t_picklock)
         ( 9 t_pick)
         ( 2 t_dragons_blood)
         ( 2 t_hydras_blood)
         ( 2 t_shovel)
        ;; ( 1 t_rune_k)
         
         
         ;; Arms
	 ( 1 t_dagger)
	 ( 1 t_mace)
	 ( 1 t_axe)
	 ( 1 t_sword)
	 ( 1 t_2H_axe)
	 ( 1 t_2H_sword)
	 ( 1 t_morning_star)
	 ( 1 t_halberd)
	 ( 1 t_staff)

	 ( 1 t_dagger_4)
         ( 1 t_sword_4)
         ( 1 t_morning_star_2)
	 ( 1 t_stun_wand)
	 ( 1 t_doom_staff)
	 ( 1 t_eldritch_blade)
	 ( 1 t_mystic_sword)
	 ( 1 t_flaming_sword)

         ( 1 t_shield)
	 ( 1 t_scratched_shield)

	 ( 1 t_leather_helm)
	 ( 1 t_chain_coif)
	 ( 1 t_iron_helm)

	 ( 1 t_armor_leather)
	 ( 1 t_armor_chain)
	 ( 1 t_armor_plate)

         ( 1 t_iron_helm_4)
	 ( 1 t_armor_leather_4)
	 ( 1 t_armor_chain_4)
         ( 1 t_armor_plate_4)

	 ( 1 t_chrono)
	 ( 1 t_spiked_shield)

	 ( 1 t_sling)
	 ( 3 t_spear)

         ( 1 t_sling_4)
	 ( 1 t_magic_axe)

	 (20 t_oil)
	 (20 t_slime_vial)

	 (  1 t_self_bow)
	 (  1 t_bow)
	 (  1 t_long_bow)
	 (  1 t_great_bow)
	 (500 t_arrow)

	 (  1 t_lt_crossbow)
	 (  1 t_crossbow)
	 (  1 t_hvy_crossbow)
	 (  1 t_trpl_crossbow)
	 (500 t_bolt)

         ))
       (list p_world 256 257))
