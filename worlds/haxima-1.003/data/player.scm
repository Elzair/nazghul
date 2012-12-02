;; Load the initial player party.

;; Make the main character.
(kern-mk-char 'ch_wanderer    ; tag
              "The Wanderer" ; name
              sp_human       ; species
              oc_wanderer    ; occupation
              s_wanderer     ; sprite
              faction-player ; starting alignment
              6 6 6          ; starting str/int/dex
              pc-hp-off      ; base max hit points
              pc-hp-gain     ; max hit points gained per level
              pc-mp-off      ; base mana points
              pc-mp-gain     ; mana points gained per level
              9999999999     ; XXX?  max hit points (kernel will trim based on level)
              0              ; hit points (kernel will set to max shortly)
              9999999999     ; XXX? max mana points (kernel will trim based on level)
              0              ; XXX? mana points (kernel will set to max shortly)
              1              ; character level
              #f             ; dead?
              nil            ; conversation
              nil            ; schedule
              nil            ; special ai
              nil            ; personal inventory
              nil            ; readied armaments
              )

;; Make the party
(kern-mk-player 'player                    ; tag
                s_wanderer                 ; sprite
                "Walk"                     ; movement description
                nil                        ; movement sound
                1                          ; food
                0                          ; gold
                (* 60 60 5)                ; turns to next meal (5 hours)
                nil                        ; formation
                nil                        ; campsite map
                nil                        ; campsite formation
                nil                        ; vehicle
                (kern-mk-inventory nil)    ; inventory
                nil                        ; party members (should be nil to start)
                )

(kern-party-add-member player ch_wanderer)
