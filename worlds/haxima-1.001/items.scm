;;----------------------------------------------------------------------------
;; item.scm - types that support the 'use' method in addition to the default
;;            'get' method
;;----------------------------------------------------------------------------

(define heal-potion-amount 10)

;;-----------------------------------------------------------------------------
;; heal potion
;;-----------------------------------------------------------------------------
(define (heal-potion-effect item user)
  (let ((target (kern-ui-select-party-member)))
    (if (not (null? target))
        (begin
          (kern-obj-heal target heal-potion-amount)
          (kern-obj-remove-from-inventory user item 1)
          ))))

(define heal-potion-ifc
  (ifc obj-ifc
       (method 'use heal-potion-effect)))

(mk-obj-type 'heal-potion "healing potion" s_kg_potion_red_f33_2 layer-item 
             heal-potion-ifc)

;; ----------------------------------------------------------------------------
;; cure potion
;; ----------------------------------------------------------------------------
(define (green-potion-effect item user)
  (let ((target (kern-ui-select-party-member)))
    (if (not (null? target))
        (begin
          (kern-obj-remove-effect target ef_poison)
          (kern-obj-remove-from-inventory user item 1)
          ))))

(define green-potion-ifc
  (ifc obj-ifc
       (method 'use green-potion-effect)))

(mk-obj-type 'cure-poison-potion "cure poison potion" s_kg_potion_green_f33_2 layer-item 
             green-potion-ifc)

;; ----------------------------------------------------------------------------
;; sleep potion
;; ----------------------------------------------------------------------------
(define (sleep-potion-use kitem kuser)
  (kern-print "Using a sleep potion... Hmm... Not implemented yet!"))

(define sleep-potion-ifc
  (ifc obj-ifc
       (method 'use sleep-potion-use)))

(mk-obj-type 't_sleep_potion "purple potion" s_purple_potion layer-item
             sleep-potion-ifc)

;; ----------------------------------------------------------------------------
;; poison immunity potion
;; ----------------------------------------------------------------------------
(define (use-poison-immunity-potion item user)
  (let ((target (kern-ui-select-party-member)))
    (if (not (null? target))
        (begin
          (kern-obj-add-effect target ef_temporary_poison_immunity nil)
          (kern-obj-remove-from-inventory user item 1)
          ))))

(define poison-immunity-potion-ifc
  (ifc obj-ifc
       (method 'use use-poison-immunity-potion)))

(mk-obj-type 't_poison_immunity_potion "bubbly potion" s_yellow_potion
             layer-item poison-immunity-potion-ifc)


;; ----------------------------------------------------------------------------
;; Sample scroll: Gen's Goblin Lexicon
;; ----------------------------------------------------------------------------
(define (goblin-lexicon-use lexicon user)
  (kern-ui-page-text
   "Goblin Lexicon"
   "I compiled these notes to help others learn the goblin language. I hope it is useful."
   "--Gen"
   ""
   "Bo.....My, Myself"
   "Cho....Mankind"
   "Da.....Abode, World"
   "Eh.....'What?'"
   "Gu.....Spirit, Ancestor"
   "Ha.....Good, Yes, Skillful"
   "Hi.....Magic"
   "Ka.....Kill, Destroy, End"
   "Ki.....Health, Life-Force, Power"
   "Ma.....Forest, Hidden Ways"
   "Me.....Duty, Job, Destiny"
   "Na.....Yours, Yourself"
   "Nu.....Give Birth, Create, Begin"
   "No.....Name"
   "Ru.....Ancient, Primordal, Deep, Cave"
   "To.....Individual"
   "Tu.....Bad, No, Useless"
   "Zu.....Watch, Seek"
   ))

(define goblin-lexicon-ifc
  (ifc obj-ifc
       (method 'use goblin-lexicon-use)))

(mk-obj-type 'the-goblin-lexicon "A Goblin Lexicon" s_scroll1 layer-item goblin-lexicon-ifc)

;;----------------------------------------------------------------------------
;; Player manual
;;----------------------------------------------------------------------------
(define (basic-survival-manual-use manual user)
  (kern-ui-page-text
   "Basic Survival Manual"
   "by Gorn the Wayward"
   ""
   "Dear Pilgrim,"
   ""
   "Welcome to the Shard! Not the brightest spot "
   "on our Journey, but if you're here then it is"
   "The Way that brought you here, and it did so "
   "for a reason. You'll have to figure out what "
   "that reason is if you want to continue the "
   "Journey."
   ""
   "I recommend you seek the Hermit. Usually "
   "there's a local yokel standing around who "
   "knows where to find him (or her!). It may "
   "have been ten years or a hundred since the "
   "last Pilgrim stood where you are now (with "
   "that same stunned expression on their "
   "face!), but there's always a Hermit."
   ""
   "Meanwhile, I've jotted down some quick notes "
   "about how to survive in the Shard. They'll "
   "get you started. Just remember: you don't "
   "know who is friend of foe yet, and that can "
   "always change anyway, so don't be too quick "
   "with the sword or too careless with your "
   "mouth. "
   ""
   "Good luck!"
   ""
   "And don't forget: SEEK THE HERMIT!"
   ))

(define basic-survival-manual-ifc
  (ifc obj-ifc
       (method 'use basic-survival-manual-use)))

(mk-obj-type 'basic-survival-manual "Basic Survival Manual" s_book_red_4 layer-item basic-survival-manual-ifc)