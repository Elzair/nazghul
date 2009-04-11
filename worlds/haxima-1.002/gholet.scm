;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define gholet-lvl 4)
(define gholet-species sp_human)
(define gholet-occ nil)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In the Prison level under Glasdrin
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (gholet-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Gholet is a former pirate, now imprisoned in the Prison below Glasdrin.
;; He is one of the surviving crew of the Merciful Death, 
;; and is sought for vengeance by the ghost Ghertie.
;;----------------------------------------------------------------------------

;; Basics...
(define (gholet-hail knpc kpc)
  (say knpc "'Ello, guv'nah"))

(define (gholet-default knpc kpc)
  (say knpc "Right you are, guv'nah, that's what I like about you!"))

(define (gholet-name knpc kpc)
  (say knpc "Gholet's me 'andle, but take no account o' me, guv'nah")
  (quest-data-update 'questentry-ghertie 'gholet-dungeon 1))

(define (gholet-join knpc kpc)
  (say knpc "And leave behind 'is 'ere life of luxury? Tosh!"))

(define (gholet-job knpc kpc)
  (say knpc "Why, I's a cook. Or I was, at any rate, 'fore I took up the new "
       "vocation you see me at 'ere."))

(define (gholet-bye knpc kpc)
  (say knpc "Nice chattin' with ya, guv'nah"))

;; Tier 2 replies
(define (gholet-cook knpc kpc)
  (say knpc "I was a cook on a famous luxury liner! The Merciful Death, "
       "ever 'ear of her?")
  (if (yes? kpc)
      (say knpc "Oh. Well, I told you she was famous!")
      (say knpc "Oh, she was quite-well known in her time. "
           "Don't know what became of 'er.")))

(define (gholet-merc knpc kpc)
  (say knpc "Oh, yeah, she was captained by a marvelous woman. "
       "Ghertie was her name."))

(define (gholet-gher knpc kpc)
  (say knpc "A real peach. Couldn't ask for a nicer boss."))

(define (gholet-voca knpc kpc)
  (say knpc "Aye, this is the easiest job I ever had."))

(define (gholet-mean knpc kpc)
  (say knpc "'Aven't seen old Meaney in ages. "
       "Last I 'eard 'e was running a poor 'ouse."))

(define (gholet-jorn knpc kpc)
  (say knpc "Oh, now there's a right dangerous man, now guv'nah. "
       "Best let sleeping dogs lie, as I like to say. "))

(define (gholet-dog knpc kpc)
  (say knpc "From what I 'ear, that dog lies at the White Stag.")
  (quest-data-update 'questentry-ghertie 'jorn-loc 1))

;; Quest-related
(define (gholet-ring knpc kpc)

  (if (not (in-inventory? knpc t_skull_ring_g))
      (say knpc "Ring? What ring?")
      (begin

        (define (take-picklocks)
          (if (< (num-in-inventory kpc t_picklock) 12)
		(begin
		(quest-data-update-with 'questentry-ghertie 'gholet-price 1 (quest-notify nil))
              (say knpc "Mmm. Bit of a problem, guv'nah, "
                   "you don't have enough picklocks. "
                   "But I'll keep the ring 'ere on layaway until you do.")
		   )
              (begin
                (say knpc "Right you are, 'ere you go, and there I go, "
                     "right as rain, right as rain! Enjoy your new ring, "
                     "guv'nah!")
                (kern-obj-remove-from-inventory kpc t_picklock 12)
                (kern-obj-add-to-inventory knpc t_picklock 12)
                (kern-obj-remove-from-inventory knpc t_skull_ring_g 1)
		(skullring-g-get nil kpc)
		)))

        (say knpc "Oh, that old thing? It got to itchin', "
             "I must be allergic to it. "
             "So I took it off. 'Ad to take the whole finger, off, actually, "
             "bit of a mess. Would you like to see it?")
        (if (yes? kpc)
            (begin
              (say knpc "Don't blame you. Not at all. "
                   "It's quite the curious item! "
                   "But one good turn deserves another, "
                   "don't you agree, guv'nah?")
              (if (yes? kpc)
                  (begin
                    (say knpc "Of course you do! Youse a fair man, guv'nah! "
                         "You know tit-for-tat, scratch each other's back! "
                         "A dozen picklocks. A dozen picklocks and you can have the ring. "
                         "Agreed?")
                    (if (yes? kpc)
                        (take-picklocks)
                        (begin
			(quest-data-update-with 'questentry-ghertie 'gholet-price 1 (quest-notify nil))
                          (say knpc "That's my price. "
                               "Come back when you're ready to pay.")
                          (kern-conv-end))))
                  (begin
                    (say knpc "Piss off, then.")
                    (kern-conv-end))))
            (say knpc "Don't screw with me. "
                 "I know you're 'ere for the ring or you wouldn't "
                 "'ave asked.")))))
      
(define gholet-conv
  (ifc basic-conv

       ;; basics
       (method 'default gholet-default)
       (method 'hail gholet-hail)
       (method 'bye  gholet-bye)
       (method 'job  gholet-job)
       (method 'name gholet-name)
       (method 'join gholet-join)
       
       ;; other responses
       (method 'cook gholet-cook)
       (method 'merc gholet-merc)
       (method 'gher gholet-gher)
       (method 'voca gholet-voca)
       (method 'ring gholet-ring)

       (method 'mean gholet-mean)
       (method 'jorn gholet-jorn)
       (method 'dog  gholet-dog)
       ))

(define (mk-gholet)
  (bind 
   (kern-char-force-drop
   (kern-mk-char 
    'ch_my           ; tag
    "Gholet"             ; name
    gholet-species         ; species
    gholet-occ              ; occ
    s_brigand     ; sprite
    faction-men      ; starting alignment
    1 0 3            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    max-health; hp
    -1                   ; xp
    max-health ; mp
    0
    gholet-lvl
    #f               ; dead
    'gholet-conv         ; conv
    nil              ; sched
    nil              ; special ai
    ;;..........container (and contents)
    (mk-inventory
              (list
               (list 1 t_skull_ring_g)
               ))
    nil              ; readied
    )
   #t)
  (gholet-mk)))
