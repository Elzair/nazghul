;;----------------------------------------------------------------------------
;; item.scm - types that support the 'use' method in addition to the default
;;            'get' method
;;----------------------------------------------------------------------------

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

(mk-obj-type 't_green_potion "green potion" s_green_potion layer-item 
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
