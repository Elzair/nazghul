;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_ini
               (list 0  0  gi-bed      "sleeping")
               (list 5  0  gs-altar    "idle")
               (list 6  0  gc-train    "working")
               (list 12 0  ghg-s5      "eating")
               (list 13 0  gc-hall     "working")
               (list 18 0  ghg-s5      "eating")
               (list 19 0  ghg-hall    "idle")
               (list 21 0  gi-bed      "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (ini-mk) (list #f))
(define (ini-will-join? ini) (car ini))
(define (ini-will-join! ini) (set-car! ini #t))

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (ini-hail knpc kpc)
  (say knpc "[You meet a morose paladin] Hi."))

(define (ini-default knpc kpc)
  (say knpc "I can't help you with that."))

(define (ini-name knpc kpc)
  (say knpc "I am Inago, but everyone calls me Ini."))

(define (ini-join knpc kpc)
  (let ((ini (kobj-gob-data knpc)))
    (if (ini-will-join? ini)
        (begin
          (say knpc "I thank you! Let's not waste any time finding "
               "the Warritrix!")
          (kern-conv-end)
          (join-player knpc))
        (say knpc "[Sigh] My duty is with the paladins."))))
        

(define (ini-job knpc kpc)
  (say knpc "I'm a paladin. But I don't like it very much."))

(define (ini-bye knpc kpc)
  (say knpc "So long."))

(define (ini-warr knpc kpc)
  (say knpc "[He straightens up a bit] Something is amiss! The Warritrix has "
       "been gone too long with no word. The Commander should have sent out "
       "search parties by now. Instead he sits idly by, pretending to be "
       "distracted by other problems. I sense foul play. Will you search for "
       "her?")
  (if (kern-conv-get-yes-no? kpc)
      (begin
        (say knpc "I would join you! I know the deeps well, and though my "
             "duty is here, I won't obey an order that leaves her to die.")
        (ini-will-join! (kobj-gob-data knpc)))
      (say knpc "Someone must do something! The Realm owes her a great "
           "debt.")))

;; Paladin...
(define (ini-pala knpc kpc)
  (say knpc "I've been a paladin my whole life. I'm not very good at it; "
       "I get sick before and after every battle. I'm surprised they "
       "let me stay in, but I guess they need the warm bodies. "
       "I would have quit long ago but I don't know what else to do."
       ))

(define (ini-quit knpc kpc)
  (say knpc "I've managed to save some pay. I'll retire soon, buy a farm "
       "near Trigrave, get away from this place. Just think: no more long "
       "marches, no more sleeping on stony ground in the lightless deep, "
       "no more waking up to monsters eating your squad for breakfast."
       ))

;; Townspeople...
(define (ini-glas knpc kpc)
  (say knpc "Kind of a dreary place, don't you think?")
  (kern-conv-get-yes-no? kpc)
  (say knpc "I've always wanted to visit Green Tower, see the trees."))

(define (ini-ange knpc kpc)
  (say knpc "A modest lady. I once saw her spit a cave goblin with a dagger."))

(define (ini-spit knpc kpc)
  (say knpc "She was assigned to my squad on a standard patrol in the "
       "hills. We'd just barely survived an encounter with gints when some "
       "cave goblins decided we looked like easy pickings. "
       "It was dicey there for a while."))

(define (ini-patc knpc kpc)
  (say knpc "I owe him my life."))

(define (ini-life knpc kpc)
  (say knpc "I was killed once. We were in the deeps on patrol, dead tired "
       "after fleeing a party of death knights that killed our medik, "
       "and we camped right in the middle of a party of sleeping trolls. ")
  (prompt-for-key)
  (say knpc
       "They woke up first. One minute I was fighting for my life, "
       "and the next I was in the hospital looking up at Doc Patch. "
       "Nobody else survived from my squad."
       ))

(define ini-conv
  (ifc glasdrin-conv

       ;; basics
       (method 'default ini-default)
       (method 'hail ini-hail)
       (method 'bye ini-bye)
       (method 'job ini-job)
       (method 'name ini-name)
       (method 'join ini-join)

       (method 'warr ini-warr)
       (method 'pala ini-pala)
       (method 'quit ini-quit)
       (method 'glas ini-glas)
       (method 'ange ini-ange)
       (method 'spit ini-spit)
       (method 'dagg ini-spit)
       (method 'patc ini-patc)
       (method 'life ini-life)
       ))
       
(define (mk-ini)
  (bind 
   (kern-mk-char 'ch_ini           ; tag
                 "Ini"             ; name
                 sp_human            ; species
                 oc_warrior          ; occ
                 s_companion_paladin ; sprite
                 faction-men         ; starting alignment
                 5 0 5               ; str/int/dex
                 3 1                 ; hp mod/mult
                 3 1                 ; mp mod/mult
                 30 0 0 3            ; hp/xp/mp/lvl
                 #f                  ; dead
                 'ini-conv         ; conv
                 sch_ini           ; sched
                 nil                 ; special ai
                 nil                 ; container
                 (list t_armor_chain
                       t_chain_coif
                       t_halberd
                       ))         ; readied
   (ini-mk)))
