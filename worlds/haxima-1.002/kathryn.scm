;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define kathryn-start-lvl  6)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_kathryn
               (list 0  0  bole-bed-kathryn "sleeping")
               (list 9  0  bole-table-1 "eating")
               (list 10 0  bole-courtyard   "idle")
               (list 12 0  bole-table-1 "eating")
               (list 13 0  bole-dining-hall "idle")
               (list 18 0  bole-table-1 "eating")
               (list 19 0  bole-dining-hall "idle")
               (list 23 0  bole-bed-kathryn "sleeping")
               )

;;----------------------------------------------------------------------------
;; Special objects
;;----------------------------------------------------------------------------
(mk-reusable-item 
 't_kathryns_letter "Letter" s_scroll 1
 (lambda (kletter kuser)
   (kern-ui-page-text
   "Letter"
   "K,"
   "The Enchanter has one of the Runes. Acquire "
   "it by any means necessary, and leave no one "
   "to tell the tale. Not even a ghost."
   "--S")))


;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (kathryn-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------
(define (kathryn-hail knpc kpc)
  (say knpc "[You meet a lovely woman who regards you with contempt]. Yes?"))

(define (kathryn-default knpc kpc)
  (say knpc "[She studiously ignores you]"))

(define (kathryn-name knpc kpc)
  (say knpc "I don't give my name to vagabonds. Say, why don't you go PUMP "
       "one of the locals for information? I saw some farm animals on the "
       "way in..."))

(define (kathryn-join knpc kpc)
  (say knpc "[She chokes with laughter]"))

(define (kathryn-job knpc kpc)
  (say knpc "[Sneering] I'm a nun. Blowjobs are 500 gold."))


(define (kathryn-blowjob knpc kpc)
  (say knpc "It's called SARCASM. Beat it, creep."))

(define (kathryn-clients knpc kpc)
  (say knpc "My clients prefer to remain anonymous. All you need to know is "
       "that you don't want to piss them off."))

(define (kathryn-things knpc kpc)
  (say knpc "I'm afraid a thug like you wouldn't understand."))

(define (kathryn-thief knpc kpc)
  (say knpc "[She gives you a cunning look] You know, perhaps we can help "
       "each other. I came here on behalf of my clients to purchase an item "
       "in good faith from an anonymous seller."))

(define (kathryn-seller knpc kpc)
  (say knpc "Imagine my shock when I discovered the item was stolen! "
       "Of course, I demanded that he hand over the item immediately so I "
       "could return it to its rightful owner. No sooner did I make my "
       "intentions clear when he vanished before my eyes!"))

(define (kathryn-vanish knpc kpc)
  (say knpc "Poof! Gone! The villian must have a ring of invisibility. "
       "Thud and I have been searching for him everywhere."))

(define (kathryn-search knpc kpc)
  (define (do-join)
    (say knpc "Excellent! Let's ask Thud to join us and we'll see if "
         "anyone around here knows about a thief.")
    (kern-char-join-player knpc)
    (if (in-inventory? knpc t_wis_quas_scroll)
        (begin
          (say knpc "Oh, this scroll may come in handy. "
               "You take it, I really don't understand these magical "
               "thingies very well.")    
          (kern-obj-remove-from-inventory knpc t_wis_quas_scroll 1)
          (kern-obj-add-to-inventory knpc kpc t_wis_quas_scroll 1)))
    (kern-conv-end))
  (say knpc "It seems we have a common goal. Join us, and when we catch the "
       "villian we'll all return the item. You can keep any reward. "
       "Knowing that we have done the right thing will "
       "be enough for me and Thud. What do you say, tough guy, will you join "
       "us?")
  (if (kern-conv-get-yes-no? kpc)
      (do-join)
      (begin
        (say knpc "[She becomes distressed] Oh, please won't you help me! "
             "My masters are powerful and cruel! If I don't return with the "
             "item they seek there will be nowhere I can hide in this life "
             "or the next! Won't you please join me?")
        (if (kern-conv-get-yes-no? kpc)
            (do-join)
            (begin
              (say knpc "[She leans close with parted lips and lidded eyes] "
                   "Okay, tough guy, you win. I don't want the silly item. "
                   "I just find you irresistible. Let me come with you, "
                   "pretty-please? I can be oh... so... grateful...")
              (if (kern-conv-get-yes-no? kpc)
                  (do-join)
                  (say knpc "[She turns red and shrieks with rage] Fool! "
                       "You have no idea who you are dealing with! What you "
                       "seek draws the gaze of the darkest wizards in the "
                       "land! That thief will know eternal torment when we "
                       "catch him! And YOU HAD BETTER STAY OUT OF OUR WAY!")
                  (kern-conv-end)))))))

(define (kathryn-tavern knpc kpc)
  (say knpc "Nice place. If you're a cockroach."))

(define (kathryn-companion knpc kpc)
  (say knpc "Thud? He's my... cousin."))

(define (kathryn-cousin knpc kpc)
  (say knpc "A DISTANT cousin."))

(define (kathryn-bill knpc kpc)
  (say knpc "The village idiot? He's probably out using his tool in the "
       "forest. Why don't you go give him a hand?"))

(define (kathryn-hackle knpc kpc)
  (say knpc "Crazy bitch? Lives across the creek? I'm afraid she can't cure "
       "ugly. Sorry."))

(define (kathryn-may knpc kpc)
  (say knpc "The innkeeper? Nosy old crone. Obviously you don't have money, "
       "but if you did I'd warn you to sleep with it under your pillow."))

(define (kathryn-melvin knpc kpc)
  (say knpc "The cook? A dirty old man and a drunk."))

(define (kathryn-sorceress knpc kpc)
  (say knpc "[She covers her mouth in mock fright] Ooh! You guessed my dirty "
       "little secret! Now beat it or I'll use you as a torch."))

(define (kathryn-scro knpc kpc)
  (if (is-player-party-member? knpc)
      (if (in-inventory? knpc t_wis_quas_scroll)
          (begin
           (say knpc "It's my last one. Take it... but don't waste it.")
           (kern-obj-remove-from-inventory knpc t_wis_quas_scroll 1)
           (kern-obj-add-to-inventory knpc kpc t_wis_quas_scroll 1))
          (say knpc "I already gave you my last one!"))
      (say knpc "A scroll? [She laughs] Shall I turn out my pockets for you? "
           "Mind your own business.")))


(define kathryn-conv
  (ifc nil
       (method 'default kathryn-default)
       (method 'hail kathryn-hail)
       (method 'bye (lambda (knpc kpc) (say knpc "Good riddance.")))
       (method 'job kathryn-job)
       (method 'name kathryn-name)
       (method 'join kathryn-join)

       (method 'blow kathryn-blowjob)
       (method 'bill kathryn-bill)
       (method 'clie kathryn-clients)
       (method 'comp kathryn-companion)
       (method 'cous kathryn-cousin)
       (method 'fait kathryn-seller)
       (method 'hack kathryn-hackle)
       (method 'item kathryn-things)
       (method 'inn  kathryn-tavern)
       (method 'may  kathryn-may)
       (method 'meet kathryn-thief)
       (method 'melv kathryn-melvin)
       (method 'nun  kathryn-blowjob)
       (method 'ring kathryn-search)
       (method 'sear kathryn-search)
       (method 'sell kathryn-seller)
       (method 'sorc kathryn-sorceress)
       (method 'tave kathryn-tavern)
       (method 'thud kathryn-companion)
       (method 'thie kathryn-thief)
       (method 'thin kathryn-things)
       (method 'vani kathryn-vanish)
       (method 'vill kathryn-search)
       (method 'scro kathryn-scro)
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-kathryn)
  (bind 
   (kern-char-force-drop
    (kern-char-arm-self
     (kern-mk-char 
      'ch_kathryn ;;..tag
      "Kathryn" ;;....name
      sp_human ;;.....species
      oc_wizard ;;....occupation
      s_wizard ;;.....sprite
      faction-men ;;..faction
      0 ;;............custom strength modifier
      4 ;;............custom intelligence modifier
      0 ;;............custom dexterity modifier
      2 ;;............custom base hp modifier
      1 ;;............custom hp multiplier (per-level)
      4 ;;............custom base mp modifier
      2 ;;............custom mp multiplier (per-level)
      (max-hp sp_human oc_wizard kathryn-start-lvl 0 0) ;; current hit points
      0  ;;...........current experience points
      (max-mp sp_human oc_wizard kathryn-start-lvl 0 0) ;; current magic points
      kathryn-start-lvl  ;;..current level
      #f ;;...........dead?
      'kathryn-conv ;;conversation (optional)
      sch_kathryn ;;..schedule (optional)
      'spell-sword-ai ;;...custom ai (optional)
      ;;..............container (and contents)
      (mk-chest
       nil
       (mk-contents
        (add-content 1 t_kathryns_letter)
        (add-content 100 t_gold_coins)
        (add-content 5 sulphorous_ash )
        (add-content 5 ginseng )
        (add-content 5 garlic )
        (add-content 3 spider_silk )
        (add-content 3 blood_moss )
        (add-content 2 black_pearl )
        (add-content 1 nightshade )
        (add-content 1 mandrake )
        (add-content 1 t_wis_quas_scroll)
        ))
      ;;..............readied arms (in addition to the container contents)
      (list
       t_staff
       )
      nil ;;..........hooks in effect
      ))
    #t)
   (kathryn-mk)))
