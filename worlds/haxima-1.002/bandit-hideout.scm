(mk-19x19-town
 'p_bandit_hideout_l1 "Bandit Stockade" s_forest
 (list
      "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt rr rr rr rr "
      "tt tt tt tt tt tt tt tt tt tt tt tt tt rr rr rr ~7 rr rr "
      "tt bb rr bb bb rr rr bb bb bb rr rr rr rr bb ~b __ ~d rr "
      "tt rr .. dd dd dd .. .. .. .. tt rr rr bb ee ee ~e rr rr "
      "tt bb dd dd && dd dd dd dd .. tt tt %% %% ee %% bb rr bb "
      "tt bb bb dd dd dd dd .. dd dd .. tt tt %% ee bb rr rr tt "
      ".. .. bb bb bb .. dd .. .. dd dd dd dd .. dd rr bb tt tt "
      ".. .. .. .. rr dd dd xx xx w+ xx xx dd dd dd .. bb bb .. "
      "dd .. .. .. rr dd .. rr ,, ,, dd rr .. dd .. && bb bb dd "
      "dd dd .. dd .. dd .. ,, dd dd cc w+ cc dd dd .. bb dd dd "
      ".. dd dd dd rr dd .. xx dd dd ,, xx .. .. bb bb rr bb dd "
      ".. .. dd .. bb dd .. xx rr ,, xx xx bb bb bb dd rr bb .. "
      "tt rr bb rr bb dd dd .. .. dd .. bb dd dd dd dd rr bb tt "
      "tt bb dd dd dd dd dd dd dd dd bb bb dd dd .. dd bb tt tt "
      "tt bb dd dd && dd dd .. .. .. bb dd dd .. dd dd rr tt tt "
      "tt bb bb dd dd dd bb bb bb bb bb bb dd dd dd bb rr tt tt "
      "tt tt bb bb rr rr bb tt tt tt tt bb bb bb rr rr tt tt tt "
      "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
      "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  )
 (put (mk-ladder-down 'p_bandit_hideout_l2a 9 9) 9 9)
 (put (mk-door) 4 9)
 (put (kern-tag 'p_bhl1_p1 (mk-portcullis)) 7 9)
 (put (kern-tag 'p_bhl1_p2 (mk-portcullis)) 9 11)
 (put (mk-sense-trig 'generic-trig-exec 'p_bhl1_p1 'signal)  12 9)
 (put (mk-sense-trig 'generic-trig-exec 'p_bhl1_p2 'signal)  10 9)
 (put (mk-caltrops) 6 9)
 (put (mk-beartrap) 6 10)

 ;; Make a test chest with a bunch of traps on it.
 (let ((kchest (mk-chest nil
                '((1 t_sword)
                  (5 t_arrow)
                  (2 t_torch)))))
   (map (lambda (trap)
          (ifccall kchest 'add-trap trap))
        (list 'sleep-trap
              'poison-trap
              'burn-trap
              'spike-trap
              'lightning-trap
              'self-destruct-trap
              'bomb-trap))
   (put kchest 5 8))

 )

(mk-dungeon-room
 'p_bandit_hideout_l2a "Stockade Barracks"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr bb dd dd dd dd dd dd dd dd dd dd dd dd dd dd rr rr "
  "rr bb dd dd dd && dd xx xx xx xx bb dd dd dd dd bb dd rr "
  "rr dd dd dd dd dd dd xx ,, ,, xx xx xx xx xx dd dd dd rr "
  "rr dd dd xx xx xx xx xx ,, ,, ,, ,, ,, x! xx dd dd bb rr "
  "rr dd dd xx x! ,, ,, xx xx xx xx ,, ,, ,, xx dd bb bb rr "
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
 (put (mk-portcullis) 8 14)
 (put (mk-portcullis) 14 10)

 ;; FIXME: make these trapped doors
 (let ((kdoor (mk-door)))
   (ifccall kdoor 'add-trap 'spike-trap)
   (put kdoor 9 7))

 (put (mk-door) 7 9)
 (put (mk-door) 9 11)
 (put (mk-door) 11 9)
 (put (mk-door) 10 4)

 ;; FIXME: let's get some pit traps that drop into another, deeper dungeon
)
;  (put (spawn-pt 'cave-goblin-slinger) 18 7)
;  (put (spawn-pt 'cave-goblin-slinger) 18 11)
;  (put (spawn-pt 'cave-goblin-berserker) 15 9)
;  (put (guard-pt 'crossbowman) 12 9)
;  (put (guard-pt 'crossbowman) 12 11)
;  (put (guard-pt 'halberdier) 10 10)

