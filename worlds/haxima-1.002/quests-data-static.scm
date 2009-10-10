;;----------------------------------------------------
;; this is a collection place for updates to quests
;;
;; the function is an arbitary method call, but the ones here tend to
;; basically boil down to retreiving a bunch of flags from the quest payload
;; and updating the quest via (qst-set-descr! text)
;;
;; the boilerplate at the front of the methods just makes the quest data easily accessible
;; via a tbl-flag? method without needing to go through several level of redirection each time
;;

;;---------------
;; bandits
(define (quest-bandits-update)
  (println "quest-bandits-update")
  (let* ((quest (quest-data-get 'questentry-bandits))
         (quest-tbl (car (qst-payload quest)))
         (header (kern-ui-paginate-text
                  "Gregor, an old charcoal burner, has asked for your help in dealing with some troublesome bandits that have been plaguing the great forest."
                  "")))
    (println "quest-tbl:" quest-tbl)
    (define (tbl-flag? tag) 
      (not (null? (tbl-get quest-tbl tag)))
      )
    (qst-set-descr! quest
                    (cond ((tbl-flag? 'done)
                           (kern-ui-paginate-text
                            "You have delivered Nate to the Rangers of Green Tower and he is now their problem."
                            ))
                          ((tbl-flag? 'nate-given-to-jailor)
                           (append header
                                   (kern-ui-paginate-text
                                    "Take the jailor's receipt back to Deric, the commander of the Green Tower rangers, to collect the reward."
                                    )))
                          ((tbl-flag? 'captured-nate-and-talked-to-deric)
                           (append header
                                   (kern-ui-paginate-text
                                    "Deliver Nate to the jailor in Green Tower and he will give you a receipt."
                                    )))
                          ((tbl-flag? 'captured-nate)
                           (append header
                                   (kern-ui-paginate-text 
                                    "Take Nate back to Green Tower and talk to the Ranger commander."
                                    )))
                          ((tbl-flag? 'talked-to-deric)
                           (append header
                                   (kern-ui-paginate-text 
                                    "Deric, commander of the rangers, was not much help. But he did give you signed orders to enlist one of his men."
                                    )))
                          (else
                           (append header
                                   (kern-ui-paginate-text
                                    "Gregor said to ask about the bandits in Green Tower."
                                    )))
                          )

		)
	))

;;---------------
;; whereami

    (define (quest-whereami-update)
      (let* ((quest (quest-data-get 'questentry-whereami))
             (quest-tbl (car (qst-payload quest)))
             (qp-shard (tbl-get quest-tbl 'shard))
             (qp-wanderer (tbl-get quest-tbl 'wanderer)))

        (qst-set-descr! quest
                        
                        (if (not (null? (tbl-get quest-tbl 'nossifer)))

                            (kern-ui-paginate-text
                             "You have found yourself on the Shard, a small fragment of a world, that floats surrounded by a great void."
                             ""
                             "The demon lord Nossifer arranged for your arrival, to set in motion events that would allow his own passage to the Shard."
                             ""
                             "Now the Demon Gate is open, and the only thing left in Nossifer's way is you..."
                             )
                            
                            (append
                             ;;1 where
                             (cond ((null? qp-shard)
                                    (kern-ui-paginate-text
                                     "You have found yourself in a world you have no knowledge of, with barest impressions of what might have gone before."
                                     ""
                                     ))
                                   ((equal? 1 qp-shard)
                                    (kern-ui-paginate-text
                                     "You have found yourself in a world you have no knowledge of. The inhabitants refer to it as the Shard."
                                     ""
                                     ))
                                   (#t (kern-ui-paginate-text
                                        "You have found yourself on the Shard, a small fragment of a world, that floats surrounded by a great void."
                                        ""
                                        ))
                                   )


                             ;; how
                             (if (and (null? qp-wanderer) (null? qp-shard))
                                 (kern-ui-paginate-text "Where are you?")
                                 nil
                                 )
                             
                             (cond ((null? qp-wanderer)
                                    (kern-ui-paginate-text
                                     "How and why are you here?"
                                     "And what are you going to do now?"
                                     ))
                                   ((equal? 1 qp-wanderer)
                                    (kern-ui-paginate-text
                                     "Others like you have in the past been found stumbling into this world. The inhabitants know you as 'Wanderers'."
                                     ""
                                     "Now you are here, what are you going to do?"
                                     ))
                                   (#t (kern-ui-paginate-text
                                        "Wanderers like yourself, who have occasionally stumbled upon this world, have in the past been responsible for great deeds."
                                        ""
                                        "How will you make your place?"
                                        ))
                                   )

                             )
                            )
                        )
	))

;;-----------------------
;; calltoarms

(define (quest-calltoarms-update)
	(let* ((quest (quest-data-get 'questentry-calltoarms))
			(quest-tbl (car (qst-payload quest)))
			(header (kern-ui-paginate-text
					"You have recieved an urgent message to contact someone called the Enchanter as soon as possible."
					"")))
		(define (tbl-flag? tag) (not (null? (tbl-get quest-tbl tag))))
		(qst-set-descr! quest
		
(cond ((tbl-flag? 'done)
		(kern-ui-paginate-text
			"You have allied yourself with the Enchanter, one of the Wise who watch over the shard."
		))
	((tbl-flag? 'talked)
		(append header
		(kern-ui-paginate-text
			"You have met with the Enchanter, but you and he did not come to any agreement."
		)))
	((tbl-flag? 'tower)
		(append header
		(kern-ui-paginate-text
			"You have found the Enchanter's Tower in the Fens. However, getting inside could be more difficult than reaching it."
		)))
	((tbl-flag? 'directions)
		(append header
		(kern-ui-paginate-text
			"The Enchanter's Tower may be found in the Fens, a swampland north of the town of Trigrave, in the western part of the Shard."
		)))
	(#t
		(append header
		(kern-ui-paginate-text
			"The message suggests that you ask the caretaker of the clearing that you arrived in for directions."
		)))
)

		)
	))

;; give some xp for reaching the tower
(define (quest-calltoarms-tower kplace kplayer)
	(quest-data-update-with 'questentry-calltoarms 'tower 1 (quest-notify (grant-xp-fn 5)))
	)

;;-----------------------
;; thiefrune

;; TODO: make the theft appear not to happen until the player is on the scene, by altering convs based on quest status

(define (quest-thiefrune-update)
	(let* ((quest (quest-data-get 'questentry-thiefrune))
			(quest-tbl (car (qst-payload quest)))
			(header (kern-ui-paginate-text
					"The Enchanter has asked you to investigate a theft from his tower."
					"")))
		(define (tbl-flag? tag) (not (null? (tbl-get quest-tbl tag))))
		(qst-set-descr! quest
		
(cond ((tbl-flag? 'recovered) 
		(append header
		(kern-ui-paginate-text
			"You have tracked down the thief and retrieved a rune belonging to the Enchanter."
		)))
	((tbl-flag? 'talked)
		(append header
		(kern-ui-paginate-text
			"The culprit, Mouse, seems to be willing to negotiate."
		)))
	((tbl-flag? 'den5)
		(append header
		(kern-ui-paginate-text
			"You have found the thief's hideout in Bole, and breached the defensive upper levels."
		)))
	((tbl-flag? 'den4)
		(append header
		(kern-ui-paginate-text
			"You have found the thief's hideout in Bole, and have passed three levels of traps."
		)))
	((tbl-flag? 'den3)
		(append header
		(kern-ui-paginate-text
			"You have found the thief's hideout in Bole, and have passed two levels of traps."
		)))
	((tbl-flag? 'den2)
		(append header
		(kern-ui-paginate-text
			"You have found the thief's hideout in Bole, but have only passed the first level."
		)))
	((tbl-flag? 'den1)
		(append header
		(kern-ui-paginate-text
			"You have found the thief's hideout in Bole, but have yet to breach its defenses."
		)))
	((tbl-flag? 'bole)
		(append header
		(kern-ui-paginate-text
			"The ^c+mthief^c- has been seen heading northeast through the Great Forest. Bole would be the most likely place to look."
		)))
	((tbl-flag? 'tower)
		(append header
		(kern-ui-paginate-text
			"The ^c+mthief^c- has been seen heading east through the mountain passes. The Green Tower would be the best place to start looking."
		)))
	(#t
		(append header
		(kern-ui-paginate-text
			"The ^c+mthief^c- has been tracked as far as Trigrave. The townsfolk there may be able to give you further information."
		)))
)

		)
	))

;; give some xp for getting through the dungeon
(define (quest-thiefrune-den1 kplace kplayer)
	(quest-data-update 'questentry-thiefrune 'tower 1)
	(quest-data-update 'questentry-thiefrune 'bole 1)
	(quest-data-update-with 'questentry-thiefrune 'den1 1 (quest-notify (grant-party-xp-fn 10)))
	)
(define (quest-thiefrune-den2 kplace kplayer)
	(quest-data-update-with 'questentry-thiefrune 'den2 1 (quest-notify (grant-party-xp-fn 10)))
	)
(define (quest-thiefrune-den3 kplace kplayer)
	(quest-data-update-with 'questentry-thiefrune 'den3 1 (quest-notify (grant-party-xp-fn 10)))
	)
(define (quest-thiefrune-den4 kplace kplayer)
	(quest-data-update-with 'questentry-thiefrune 'den4 1 (quest-notify (grant-party-xp-fn 10)))
	)
(define (quest-thiefrune-den5 kplace kplayer)
	(quest-data-update-with 'questentry-thiefrune 'den5 1 (quest-notify (grant-party-xp-fn 10)))
	)


;;-----------------------
;; runeinfo

;; TODO: runes are unidentified until you check in with abe?

(define (quest-runeinfo-update)
	(let* ((quest (quest-data-get 'questentry-runeinfo))
			(quest-tbl (car (qst-payload quest)))
			(header (kern-ui-paginate-text
				"The stolen rune that you recovered must have great significance to prompt it's theft. The Enchanter has given you the task of seeking out this reason."
				""
				)))
		(define (tbl-flag? tag) (not (null? (tbl-get quest-tbl tag))))
		(qst-set-descr! quest
		
(cond 
	((tbl-flag? 'done)
		(kern-ui-paginate-text
			"The rune you carry is one of the Keys to the Demon Gate. The gate itself lies somewhere in the north. It was sealed by the wise in ages past, and the Keys were scattered and hidden."
		))
	((tbl-flag? 'gate)
		(append header
		(kern-ui-paginate-text
			"The runes are also known as the Keys to the Demon Gate. The gate itself lies somewhere in the north. It was sealed by the wise in ages past, and the Keys were scattered and hidden."
		)))
	((tbl-flag? 'keys)
		(append header
		(kern-ui-paginate-text
			"The runes are also known as the Keys to the Demon Gate. What that means remains unknown."
		)))
	((tbl-flag? 'abe)
		(append header
		(kern-ui-paginate-text
			"The Alchemist has advised you that ^c+mAbe^c-, at the Green Tower, has studied the nature of the runes."
		)))
	(#t
		(append header
		(kern-ui-paginate-text
			"He suggests that you start with the ^c+mAlchemist^c-, who may be found at Oparine."
		)))
)

		)
	))


;;-----------------------
;; dragon blood

(define (quest-dragon-update quest)
	(let* ((quest-tbl (car (qst-payload quest)))
			(header (kern-ui-paginate-text
				"The Alchemist has offered to trade you information on the wherabouts of a Rune, in exchange for the blood of a dragon."
				)))
		(define (tbl-flag? tag) (not (null? (tbl-get quest-tbl tag))))
		(qst-set-descr! quest
		
(cond 
	((tbl-flag? 'done)
		(kern-ui-paginate-text
			"The Alchemist has traded you information on the wherabouts of a Rune, in exchange for the blood of a dragon."
		))
	((in-inventory? (car (kern-party-get-members (kern-get-player))) t_dragons_blood 1)
		(append header
		(kern-ui-paginate-text
			""
			"You have a vial of dragon's blood in your possession."
		)))
	((tbl-flag? 'sea)
		(append header
		(kern-ui-paginate-text
			""
			"He suggests that the Fire Sea may be the best place to seek them out."
		)))
	(#t
		header
		)
)

		)
	))

;;-----------------------
;; deeps rune

(define (quest-rune-p-update)
	(let* ((quest (quest-data-get 'questentry-rune-p))
			(quest-tbl (car (qst-payload quest))))
		(define (tbl-flag? tag) (not (null? (tbl-get quest-tbl tag))))
		(qst-set-descr! quest
		
(cond 
	((tbl-flag? 'done)
		(kern-ui-paginate-text
			"A rune was found in the depths of Kurpolis."
		))
	(#t
		(kern-ui-paginate-text
			"The Alchemist provided you with information on a rune buried in the deeps of Kurpolis:"
			""
		   "\"The paladins have built several fortifications in the deeps of Kurpolis. One of the runes was buried in the foundations of the deepest fort.\""
		   ""
	   		"\"A pick and shovel may be enough to get it out again, but it might be difficult with a dozen paladins breathing down your neck.\""
		))
)

		)
	))
	
	
;;-----------------------
;; spider rune

(define (quest-rune-f-update)
	(let* ((quest (quest-data-get 'questentry-rune-f))
			(quest-tbl (car (qst-payload quest))))
		(define (tbl-flag? tag) (not (null? (tbl-get quest-tbl tag))))
		(qst-set-descr! quest
		
(cond 
	((tbl-flag? 'done)
		(kern-ui-paginate-text
			"The lost rune of King Clovis was retrieved from the Spider Queen Angriss."
		))
	((and (tbl-flag? 'angrisslair) (tbl-flag? 'angriss))
		(kern-ui-paginate-text
			"King Clovis once possessed a rune, but he fell during the Goblin Wars."
			""
			"Information from the goblins has lead you to Angriss' lair, which may be where the rune lies now."
		))	
	((tbl-flag? 'angriss)
		(kern-ui-paginate-text
			"King Clovis once possessed a rune, but he fell during the Goblin Wars."
			""
		   "The goblin Kama has given you a clue to its location- Something (maybe a spider?) can be found where the southern edge of the Great Forest meets the mountains in the east."
		))		
	((tbl-flag? 'kama)
		(kern-ui-paginate-text
			"King Clovis once possessed a rune, but he fell during the Goblin Wars."
			""
		   "The goblins may know what became of it. The goblin Kama may be able to help you, if you can find him."
		))		
	((tbl-flag? 'gen)
		(kern-ui-paginate-text
			"King Clovis once possessed a rune, but he fell during the Goblin Wars."
			""
		   "The goblins may know what became of it. Amongst humans, the ranger Gen has the greatest knowledge of their kind."
		))		
	(#t
		(kern-ui-paginate-text
			"King Clovis once possessed a rune, but he fell during the Goblin Wars."
			""
		   "The goblins may know what became of it, but would they tell you?"
		))
)

		)
	))
	
(define (quest-rune-f-lair kplace kplayer)
	(if (not (null? (quest-data-getvalue 'questentry-rune-f 'angriss)))
		(quest-data-update-with 'questentry-rune-f 'angrisslair 1 (quest-notify nil))
	))

;;-----------------------
;; void rune

;; todo add more sections? info on voidships, etc?

(define (quest-rune-d-update)
	(let* ((quest (quest-data-get 'questentry-rune-d))
			(quest-tbl (car (qst-payload quest))))
		(define (tbl-flag? tag) (not (null? (tbl-get quest-tbl tag))))
		(qst-set-descr! quest
		
(cond 
	((tbl-flag? 'done)
		(kern-ui-paginate-text
			"A rune was recovered from the Temple of the Void."
		))
	(#t
		(kern-ui-paginate-text
			"Legends tell of a temple in the void, which housed a rune."
		))
)

		)
	))

;;-----------------------
;; pirate rune

(define (quest-rune-c-update)
	(let* ((quest (quest-data-get 'questentry-rune-c))
			(quest-tbl (car (qst-payload quest))))
		(define (tbl-flag? tag) (not (null? (tbl-get quest-tbl tag))))
		(qst-set-descr! quest
		
(cond 
	((tbl-flag? 'done)
		(kern-ui-paginate-text
			"A rune was recovered from the wreck of the Merciful Death."
		))
	((tbl-flag? 'shiploc)
		(append
			(kern-ui-paginate-text
			"The pirate captain Ghertie once held a rune aboard her ship, the Merciful Death."
			""
			)
			(kern-ui-paginate-text (string-append "Her ghost has revealed the location of the ship: [" (number->string merciful-death-x) ", " (number->string merciful-death-y) "]."))
			(if (tbl-flag? 'shipraise)
				(kern-ui-paginate-text
				""
				"The ship may be raised with the Vas Uus Ylem spell, mixed using mandrake, blood moss and spider silk."
				)
				nil
			)
		))
	((tbl-flag? 'info)
		(kern-ui-paginate-text
			"The pirate captain Ghertie once held a rune aboard her ship, the Merciful Death."
			""
			"Her ghost now haunts Oparine, seeking ^c+mrevenge^c- upon her crew."
		))
	(#t
		(kern-ui-paginate-text
			"The pirate captain Ghertie once held a rune."
		))
)

		)
	))

;;-----------------------
;; pirate quest

(define (quest-ghertie-update)
	(let* ((quest (quest-data-get 'questentry-ghertie))
			(quest-tbl (car (qst-payload quest))))
		(define (tbl-flag? tag) (not (null? (tbl-get quest-tbl tag))))
		(qst-set-descr! quest
		
(cond 
	((tbl-flag? 'done)
		(kern-ui-paginate-text
			(string-append "The ghost of Ghertie the pirate once haunted the inn at Oparine. In exchange for her revenge, she revealed the location of her sunken ship: [" (number->string merciful-death-x) ", " (number->string merciful-death-y) "].")
		))
	((tbl-flag? 'questinfo)
		(append
			(kern-ui-paginate-text
				"The ghost of Ghertie the pirate haunts the inn at Oparine. She seeks revenge on the survivors of the crew that betrayed her."
				""
				"Gholet, Jorn and Meaney each wear a ring marking them as Ghertie's crew. Ghertie will accept the rings as proof that you have hunted them down, in exchange for the location of her sunken treasure."
			)
			(if (and (tbl-flag? 'ring-jorn)
						(tbl-flag? 'ring-meaney)
						(tbl-flag? 'ring-gholet))
				(kern-ui-paginate-text
					""
					"You have retrieved all three skull rings."
				)
				(append
					(cond 
						((tbl-flag? 'ring-gholet)
							(kern-ui-paginate-text
								""
								"You have retrieved a skull ring from Gholet."
							))
						((tbl-flag? 'gholet-price)
							(kern-ui-paginate-text
								""
								"Gholet is held in the dungeons beneath Glasdrin, and will exchange the ring for a dozen lockpicks."
							))
						((tbl-flag? 'gholet-dungeon)
							(kern-ui-paginate-text
								""
								"Gholet is held in the dungeons beneath Glasdrin."
							))
						((tbl-flag? 'gholet-prison)
							(kern-ui-paginate-text
								""
								"If Gholet can be found anywhere it will be in some prison or another."
							))
						(#t nil)
					)
					(cond 
						((tbl-flag? 'ring-jorn)
							(kern-ui-paginate-text
								""
								"You have retrieved a skull ring from Jorn."
							))
						((tbl-flag? 'jorn-loc)
							(kern-ui-paginate-text
								""
								"Jorn can be found at the Green Tower's White Stag Inn."
							))
						((tbl-flag? 'jorn-forest)
							(kern-ui-paginate-text
								""
								"Jorn is a bandit somewhere in the Great Forest."
							))
						(#t nil)
					)
					(cond 
						((tbl-flag? 'ring-meaney)
							(kern-ui-paginate-text
								""
								"You have retrieved a skull ring from Meaney."
							))
						((tbl-flag? 'meaney-loc)
							(kern-ui-paginate-text
								""
								"Meaney runs the Poor House, north of Oparine."
							))
						(#t nil)
					)
				)
			)
		))
	(#t
		(kern-ui-paginate-text
			(string-append
				(if (tbl-flag? 'ghertieid)
					"The ghost of Ghertie the pirate" 
					"A ghost")
				(if (tbl-flag? 'ghertieloc)
					"haunts the inn at Oparine. "
					"may be found in Oparine. ")
				(if (tbl-flag? 'revenge)
					"She seeks ^c+mrevenge^c-, and may aid you if you take care of her unfinished business."
					(string-append "Why does "
						(if (tbl-flag? 'ghertieid) "she" "it")
						"linger in undeath?"
					)
				))
		))
)

		)
	))


;;------------------------------------------------------------------------ 
;; Warritrix rune
;; 

(define (quest-rune-l-update)
	(let* ((quest (quest-data-get 'questentry-rune-l))
			(quest-tbl (car (qst-payload quest))))
		(define (tbl-flag? tag) (not (null? (tbl-get quest-tbl tag))))
		(qst-set-descr! quest
		
(cond 
	((tbl-flag? 'done)
		(kern-ui-paginate-text
			"A rune was recovered from the fallen Warritrix."
		))
	((and (tbl-flag? 'located) (tbl-flag? 'know-hall))
		(kern-ui-paginate-text
			(string-append
			"One of the runes is carried by the Warritrix. "
			"She may be found in the Lost Halls, at [" 
			(number->string (loc-x lost-halls-loc)) "," (number->string (loc-y lost-halls-loc))
			"]."
			)
		))
	((and (tbl-flag? 'located) (tbl-flag? 'approx-hall))
		(kern-ui-paginate-text
			(string-append
			"One of the runes is carried by the Warritrix. "
			"She may be found in the Lost Halls, on the Shard's south coast."
			)
		))
	((tbl-flag? 'located)
		(kern-ui-paginate-text
			"One of the Runes is carried by the Warritrix, who may be found at the Lost Halls."
		))
	(#t
		(kern-ui-paginate-text
			"One of the Runes is carried by the Warritrix."
		))
)		

		)
	))

;;-----------------------
;; absalot rune

(define (quest-rune-s-update)
	(let* ((quest (quest-data-get 'questentry-rune-s))
			(quest-tbl (car (qst-payload quest))))
		(define (tbl-flag? tag) (not (null? (tbl-get quest-tbl tag))))
		(qst-set-descr! quest
		
(cond 
	((tbl-flag? 'done)
		(kern-ui-paginate-text
			"A rune was recovered from the Absalot ruins."
		))
	((tbl-flag? 'silasinfo)
		(kern-ui-paginate-text
			"Silas has a rune hidden in the Absalot ruins."
		))
	(#t
		(kern-ui-paginate-text
			"A rune may be found somewhere in Absalot."
		))
)

		)
	))

;;-----------------------
;; enchanter rune
;; tracks if the player has gotten the rune back from the enchanter

(define (quest-rune-k-update)
	(let* ((quest (quest-data-get 'questentry-rune-k))
			(quest-tbl (car (qst-payload quest))))
		(define (tbl-flag? tag) (not (null? (tbl-get quest-tbl tag))))
		(qst-set-descr! quest
		
(cond 
	((tbl-flag? 'entrusted-with-rune)
		(kern-ui-paginate-text
			"The Enchanter has entrusted the Rune of Knowledge into your care."
		))
	((tbl-flag? 'player-got-rune)
		(kern-ui-paginate-text
			"The Rune of Knowledge belonged to the Enchanter, but has since fallen into your hands."
		))
	((tbl-flag? 'ench-should-have-rune)
		(kern-ui-paginate-text
			"The Rune of Knowledge belongs to the Enchanter. You may be able to convince him to turn it over to you."
		))
	(#t
		(kern-ui-paginate-text
			"The Rune of Knowledge belonged to the Enchanter."
		))
)

		)
	))

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search for the Wise quest group

(define (quest-wise-init)
	(quest-data-assign-once 'questentry-wise)
	(map (lambda (tag)
		 (if (not (null? (quest-data-getvalue tag 'name)))
			(quest-data-assign-once tag)
		))
		(list 'questentry-enchanter 'questentry-warritrix  'questentry-alchemist
						'questentry-the-man 'questentry-engineer  'questentry-necromancer)
	))

(define (quest-wise-subinit tag)
	(quest-data-update tag 'name 1)
	(if (quest-data-assigned? 'questentry-wise)
		(quest-data-assign-once tag)
	)
	)

;; TODO- merge lost hall location info with rune quest	
(define (quest-warritrix-update)
	(let* ((quest (quest-data-get 'questentry-warritrix))
			(quest-tbl (car (qst-payload quest))))
		(define (tbl-flag? tag) (not (null? (tbl-get quest-tbl tag))))
		(qst-set-descr! quest
		
(cond
	((tbl-flag? 'avenged)
		(kern-ui-paginate-text
			"The Warritrix was the most cunning warrior of the age, but was slain in an ambush in the Lost Halls."
			""
			"You have since brought her betrayers to justice."
		))
	((tbl-flag? 'found)
		(kern-ui-paginate-text
			"The Warritrix was the most cunning warrior of the age, but was slain in an ambush in the Lost Halls."
		))
	((tbl-flag? 'slain)
		(cond
			((tbl-flag? 'lost-hall-loc)
				(kern-ui-paginate-text
					(string-append
					"The Warritrix was the most cunning warrior of the age, but her spirit has been seen in the Void."
					""
					"Her mortal remains may still lie in the Lost Halls, at ["
					(number->string (loc-x lost-halls-loc)) "," (number->string (loc-y lost-halls-loc))
					"]."
					)
				))
			((tbl-flag? 'lost-hall)
				(kern-ui-paginate-text
					"The Warritrix was the most cunning warrior of the age, but her spirit has been seen in the Void."
					""
					"Her mortal remains may still lie in the Lost Halls."
				))
			(#t
				(kern-ui-paginate-text
					"The Warritrix was the most cunning warrior of the age, but her spirit has been seen in the Void."
					""
					"When alive, she served the city of Glasdrin."
				))
		))
	((tbl-flag? 'lost-hall-loc)
		(kern-ui-paginate-text
			(string-append
			"The Warritrix is the most cunning warrior of the age. She is overdue from her mission the the Lost Halls, which can be found at ["
			(number->string (loc-x lost-halls-loc)) "," (number->string (loc-y lost-halls-loc))
			"]."
			)
		))
	((tbl-flag? 'lost-hall)
		(kern-ui-paginate-text
			"The Warritrix is the most cunning warrior of the age. A mission has taken her to the Lost Halls."
		))
	((tbl-flag? 'assignment)
		(kern-ui-paginate-text
			"The Warritrix is the most cunning warrior of the age. She is currently away from Glasdrin on a mission."
		))
	((tbl-flag? 'general-loc)
		(kern-ui-paginate-text
			"The Warritrix is the most cunning warrior of the age. She can normally be found around Glasdrin."
		))
	((tbl-flag? 'common)
		(kern-ui-paginate-text
			"The Warritrix is the most cunning warrior of the age."
		))
	(#t
		(kern-ui-paginate-text
			"The Warritrix is one of the wise."
		))
)

		)
	))
