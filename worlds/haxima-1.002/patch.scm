;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Glasdrin
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_patch
               (list 0  0  gdp-bed "sleeping")
               (list 7  0  ghg-s1  "eating")
               (list 8  0  gh-ward "working")
               (list 11 0  ghg-s1  "eating")
               (list 12 0  gh-ward "working")
               (list 17 0  ghg-s1  "eating")
               (list 18 0  gc-hall "idle")
               (list 21 0  gdp-hut "idle")
               (list 22 0  gdp-bed "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (patch-mk) (list 'townsman))

;;----------------------------------------------------------------------------
;; Conv
;; 
;; An old wizard living in Glasdrin, who works as a healer.
;; He wears an eyepatch, and is known as "Doc Patch".
;;----------------------------------------------------------------------------

;; Basics...
(define (patch-hail knpc kpc)
  (say knpc "[You meet a sprightly old wizard with an eyepatch] "
       "Hail, traveler."))

(define (patch-default knpc kpc)
  (say knpc "I can't help you with that."))

(define (patch-name knpc kpc)
  (say knpc "People call me Doc Patch."))

(define (patch-join knpc kpc)
  (say knpc "No, my duty is here, Wanderer."))

(define (patch-job knpc kpc)
  (say knpc "I run the hospital. Do you need healing?")
  (if (kern-conv-get-yes-no? kpc)
      (patch-trade knpc kpc)
      (say knpc "Well if you ever do, I can fix you up.")))

(define (patch-bye knpc kpc)
  (say knpc "Keep well!"))

;; Trade...
(define (patch-trade knpc kpc)
  (if (trade-services knpc kpc
                      (list
                       (svc-mk "Heal" 30 heal-service)
                       (svc-mk "Cure" 30 cure-service)
                       (svc-mk "Resurrect" 100 resurrect-service)))
      (begin
        (say knpc "What else can I do for you?")
        (patch-trade knpc kpc))
      (begin
        (say knpc "Will there be anything else?")
        (if (kern-conv-get-yes-no? kpc)
            (patch-trade knpc kpc)
            (say knpc "Very well.")))))
  
;; Patch...
(define (patch-patc knpc kpc)
  (say knpc "I lost my eye in Kurpolis. It's so dark there I didn't really "
       "need it. [He winks. With his good eye.]"))

(define (patch-kurp knpc kpc)
  (say knpc "The paladins keep an outpost in the dungeon, and "
       "I did a tour of duty down there in my youth."))

(define (patch-tour knpc kpc)
  (say knpc "All citizens of Glasdrin are required to serve a tour of duty. "
       "I was a medik."))

(define (patch-medi knpc kpc)
  (say knpc "A medik is a mage that specializes in healing arts. Paladin "
       "squads are all assigned a medik to support them in battle. "
       "We get some basic combat training, but I only had to use my dagger "
       "once."))

(define (patch-dagg knpc kpc)
  (say knpc "Yep, I carried that dagger for six months and finally got to use "
       "it... peeling potatoes for kitchen duty."))

(define (patch-dung knpc kpc)
  (say knpc "Monsters breed in the deeps, and it's better to stop them there "
       "before they can get to the surface. Don't you agree?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "You're damned tootin'! So the paladins keep a company there "
           "to patrol the middle levels. If you ever find yourself in need "
           "of help down there, seek them out. They'll have a medik.")
      (say knpc "Harumph! I suppose you'd rather let them run amok over "
           "the countryside!")))

(define (patch-doc knpc kpc)
  (say knpc "I'm a medik. Are you in need of healing?")
  (if (kern-conv-get-yes-no? kpc)
      (patch-trade knpc kpc)
      (say knpc "Ok. Come by the hospital if you ever need assistance. "
           "I know how you adventurers get torn up in fights!")))

(define (patch-hosp knpc kpc)
  (say knpc "Yep. The paladins of Glasdrin are always getting in scraps. "
       "I get the odd adventurer and sick villager in, too."))

;; Townspeople...

(define patch-conv
  (ifc glasdrin-conv

       ;; basics
       (method 'default patch-default)
       (method 'hail patch-hail)
       (method 'bye  patch-bye)
       (method 'job  patch-job)
       (method 'name patch-name)
       (method 'join patch-join)
       
       ;; trade
       (method 'trad patch-trade)
       (method 'heal patch-trade)
       (method 'cure patch-trade)
       (method 'resu patch-trade)
       (method 'help patch-trade)

       ;; patch
       (method 'patc patch-patc)
       (method 'kurp patch-kurp)
       (method 'tour patch-tour)
       (method 'medi patch-medi)
       (method 'dagg patch-dagg)
       (method 'dung patch-dung)
       (method 'doc  patch-doc)
       (method 'hosp patch-hosp)
       (method 'outp patch-dung)

       ;; town & people

       ))

(define (mk-patch)
  (bind 
   (kern-mk-char 'ch_patch           ; tag
                 "Patch"             ; name
                 sp_human            ; species
                 oc_wizard           ; occ
                 s_companion_wizard  ; sprite
                 faction-glasdrin         ; starting alignment
                 1 3 0               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 6            ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'patch-conv         ; conv
                 sch_patch           ; sched
                 'townsman-ai                 ; special ai
                 (mk-inventory (list (list 1 t_staff)))                 ; container
                 (list t_dagger)                 ; readied
                 )
   (patch-mk)))
