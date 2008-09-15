(let
	((questdata (tbl-mk)))
	
	(tbl-set! questdata "Character Creation"
(qst-mk "Character Creation"
	'(
		"Move to the moongate at the north side of"
		"the room to begin the game."
		""
		"Along the way you will be prompted for your"
		"characters name. You may also customize your"
		"attributes by talking to the statues in the"
		"room."
	)
	'quest-assign-always
	'quest-status-inprogress
	's_quest_start
	0
))

	(tbl-set! questdata "Where am I?"
(qst-mk "Where am I?"
	'(
		"You have found yourself in a world you have no"
		"knowledge of, with barest impressions of what"
		"might have gone before."
		""
		"Where are you?"
		"How and why are you here?"
		"And what are you going to do now?"
	)
	'quest-assign-always
	'quest-status-inprogress
	's_quest_start
	(tbl-mk)
))

	(tbl-set! questdata "An Urgent Summons"
(qst-mk "An Urgent Summons"
	'(
		"You have recieved an urgent message to contact"
		"someone called the Enchanter as soon as possible."
		""
		"The message suggests that you ask the caretaker of"
		"the clearing that you arrived in."
	)
	'quest-assign-always
	'quest-status-inprogress
	's_quest_start
	0
))
	
	(tbl-set! (gob (kern-get-player)) 'questdata questdata)

)




