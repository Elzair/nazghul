(kern-load "nate.scm")

(mk-19x19-town
 'p_bandit_hideout_l1 "Bandit Stockade" s_ruin
 (list
		"tt tt tt tt t5 t3 tt tt tt tt tt tt tt tt tc rr rr rr rr "
		"tt tt tt tt tt tt tt tt tt tt tt tt tc rr rr rr ~7 rr rr "
		"tt bb rr bb bb rr rr bb bb bb rr rr rr rr bb ~b __ ~d rr "
		"tt rr .. dd dd dd .. .. .. .. t7 rr rr bb ee ee ~e rr rr "
		"tt bb dd dd && dd dd dd dd .. ta t5 %b %% ee %c bb rr bb "
		"tc bb bb dd dd dd dd .. dd dd .. tt tt %a ee bb rr rr t3 "
		".. .. bb bb bb .. dd .. .. dd dd dd dd .. dd rr bb tb tt "
		".. .. .. .. rr dd dd xx xx w+ xx xx dd dd dd .. bb bb .. "
		"dd .. .. .. rr dd .. rr ,, ,, dd rr .. dd .. && bb bb dd "
		"dd dd .. dd .. dd .. ,, dd dd cc w+ cc dd dd .. bb dd dd "
		".. dd dd dd rr dd .. xx dd dd ,, xx .. .. bb bb rr bb dd "
		".. .. dd .. bb dd .. xx rr ,, xx xx bb bb bb dd rr bb .. "
		"t5 rr bb rr bb dd dd .. .. dd .. bb dd dd dd dd rr bb t3 "
		"tt bb dd dd dd dd dd dd dd dd bb bb dd dd .. dd bb t3 tt "
		"tt bb dd dd && dd dd .. .. .. bb dd dd .. dd dd rr tt tt "
		"tt bb bb dd dd dd bb bb bb bb bb bb dd dd dd bb rr tt tt "
		"tt t5 bb bb rr rr bb t3 tt tt t5 bb bb bb rr rr t3 tt tt "
		"tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tc ta "
		"tt tt tt tt tc ta tt tt tt tt tt tt tt tt tt tt tt t5 t3 "
  )
   (list ;;entrances
   	(list southwest 18 5)
   )
 (put (mk-ladder-down 'p_bandit_hideout_l2a 9 9) 9 9)
 (put (mk-door) 4 9)
 (put (kern-tag 'p_bhl1_p1 (mk-portcullis)) 7 9)
 (put (kern-tag 'p_bhl1_p2 (mk-portcullis)) 9 11)
 (put (mk-sense-trig 'generic-trig-exec 'p_bhl1_p1 'signal)  12 9)
 (put (mk-lever 'p_bhl1_p2)  10 9)

 ;; Release the hounds!
 (put (mk-step-trig 'terrain-changer 10 14 't_dirt) 9 6)

 ;; Bandits
 (put (mk-npc 'bandit 1) 3 4)
 (put (mk-npc 'footpad 2) 5 4)
 (put (mk-npc 'bandit 1) 3 14)
 (put (mk-npc 'footpad 2) 5 14)
 (put (mk-npc 'footpad 1) 5 8)
 (put (mk-npc 'footpad 1) 5 10)
 (put (mk-npc 'bandit 1) 15 9)
 (put (mk-npc 'bandit 1) 15 7)
 (put (mk-npc 'highwayman 2) 9 8)

 ;; Wolves in the pen
 (put (kern-being-set-base-faction (mk-npc 'wolf 1) 
                                   faction-outlaw) 13 14)
 (put (kern-being-set-base-faction (mk-npc 'wolf 1)
                                   faction-outlaw) 14 13)
 (put (mk-corpse) 13 13)

 ;; Traps
 (put (mk-caltrops) 8 12)

 )

(mk-place-music p_bandit_hideout_l1 'ml-outdoor-adventure)

(mk-dungeon-room
 'p_bandit_hideout_l2a "Stockade Barracks"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr bb dd dd dd dd dd dd dd dd dd dd dd dd dd dd rr rr "
  "rr bb dd dd dd bb dd xx xx xx xx bb dd dd dd dd bb dd rr "
  "rr dd dd dd dd dd dd xx ,, ,, xx xx xx xx xx dd dd dd rr "
  "rr dd dd xx xx xx xx xx ,, ,, ,, ,, ,, x! xx dd dd bb rr "
  "rr dd dd xx x! ,, ,, xx xx xx xx ,, ,, ,, xx dd && bb rr "
  "rr dd dd xx ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx bb bb bb rr "
  "rr dd dd xx ,, ,, ,, xx x! ,, xx xx ,, xx xx xx xx bb rr "
  "rr dd xx xx ?? xx xx xx ,, ,, ,, x! ,, xx ,, ,, xx dd rr "
  "rr dd xx ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx ,, ,, xx dd rr "
  "rr dd xx ,, ,, xx ,, x! ,, ,, ,, xx xx xx ,, xx xx dd rr "
  "rr dd xx ?? xx xx ,, xx xx ,, x! xx ,, ,, ,, xx dd dd rr "
  "rr dd dd dd xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, xx dd dd rr "
  "rr dd bb dd xx ,, ,, ,, xx xx xx xx ,, ,, x! xx dd dd rr "
  "rr dd dd dd xx x! ,, ,, ,, ,, ,, xx xx ~x xx xx dd dd rr "
  "rr dd dd dd xx xx xx xx xx ,, ,, xx dd dd dd dd dd dd rr "
  "rr rr dd dd dd dd dd bb xx xx xx xx dd dd dd dd dd rr rr "
  "rr rr rr dd dd dd bb bb bb bb bb bb dd dd dd dd rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (mk-ladder-up 'p_bandit_hideout_l1 9 9) 9 9)

 ;; Portculli and levers
 (put (kern-tag 'p_bh2_1 (mk-portcullis)) 8 14)
 (put (kern-tag 'p_bh2_2 (mk-portcullis)) 14 10)
 (put (mk-lever 'p_bh2_1) 8 3)
 (put (mk-lever 'p_bh2_2) 3 9)

 ;; Trapped doors
 (put (trap-door (mk-door) 'spike-trap)  9 7)
 (put (trap-door (mk-door) 'sleep-trap)  7 9)
 (put (trap-door (mk-door) 'poison-trap) 9 11)
 (put (trap-door (mk-door) 'bomb-trap)   11 9)
 (put (trap-door (mk-door) 'sleep-trap) 10 4)

 ;; Chests & prisoners
 ;; arms...
 (let ((kchest (mk-chest nil
                         '((10 t_oil)
                           (50 t_arrow)
                           (20 t_spear)
                           ))))
   (ifccall kchest 'add-trap 'sleep-trap)
   (ifccall kchest 'add-trap 'poison-trap)
   (put kchest 10 14))

 ;; medical...
 (let ((kchest (mk-chest nil
                         '((10 t_heal_potion)
                           (3 t_cure_potion)
                           (2 t_poison_immunity_potion)
                           (1 t_invisibility_potion)
                           (1 t_slime_vial)
                           ))))
   (ifccall kchest 'add-trap 'lightning-trap)
   (ifccall kchest 'add-trap 'self-destruct-trap)
   (put kchest 10 15))
 ;; wrogue supplies...
 (let ((kchest (mk-chest nil
                         '((5 t_torch)
                           (5 t_picklock)
                           (3 t_gem)
                           (1 t_pick)
                           (1 t_in_ex_por_scroll)
                           (1 t_wis_an_ylem_scroll)
                           (1 t_wis_quas_scroll)
                           ))))
   (ifccall kchest 'add-trap 'bomb-trap)
   (put kchest 9 15))

 (put (mk-corpse) 15 8)
 (put (mk-npc 'cave-goblin-berserker 4) 14 8)

 ;; Guards
 (put (mk-npc 'footpad 2) 6 6)
 (put (mk-npc 'highwayman 3) 4 6)
 (put (mk-npc 'footpad 2) 6 12)
 (put (mk-npc 'highwayman 3) 7 14)
 (put (mk-npc 'footpad 2) 12 12)
 (put (mk-npc 'footpad 2) 12 6)
 (put (mk-npc 'footpad 2) 2 15)
 (put (kern-being-set-base-faction (mk-npc 'wolf 1) 
                                   faction-outlaw) 9 14)

 ;; Traps
 (put (mk-step-trig 'wind-trap nil) 1 10)
 (put (mk-step-trig 'wind-trap nil) 7 1)
 (put (mk-caltrops) 4 1)

 ;; Cryptozoic denizens
 (put (spawn-pt 'giant-spider faction-spider) 16 16)
 (put (mk-corpse-with-loot) 12 16)
 (put (mk-corpse-with-loot) 17 15)

 ;; Nate
 (put (mk-nate) 16 3)
)

(mk-place-music p_bandit_hideout_l2a 'ml-dungeon-adventure)

;  (put (spawn-pt 'cave-goblin-slinger) 18 7)
;  (put (spawn-pt 'cave-goblin-slinger) 18 11)
;  (put (spawn-pt 'cave-goblin-berserker) 15 9)
;  (put (guard-pt 'crossbowman) 12 9)
;  (put (guard-pt 'crossbowman) 12 11)
;  (put (guard-pt 'halberdier) 10 10)

 ;; Make a test chest with a bunch of traps on it.
;  (let ((kchest (mk-chest nil
;                 '((1 t_sword)
;                   (5 t_arrow)
;                   (2 t_torch)))))
;    (map (lambda (trap)
;           (ifccall kchest 'add-trap trap))
;         (list 'sleep-trap
;               'poison-trap
;               'burn-trap
;               'spike-trap
;               'lightning-trap
;               'self-destruct-trap
;               'bomb-trap))
;    (put kchest 5 8))
;  (put (mk-caltrops) 6 9)
;  (put (mk-beartrap) 6 10)
