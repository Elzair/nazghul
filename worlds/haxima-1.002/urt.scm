;; ----------------------------------------------------------------------------
;; Urt
;;
;; Urt is a troll ensnared and paralyzed in the abandoned cellar. Although
;; paralyzed, he can speak.
;; ----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------
(define urt-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "(looks confused)")))
       (method 'hail (lambda (knpc kpc) (say knpc "(grunts)")))
       (method 'bye (lambda (knpc kpc) (say knpc "(grunts)")))
       (method 'job (lambda (knpc kpc) (say knpc "Stuck")))
       (method 'name (lambda (knpc kpc) (say knpc "Urt")))

       (method 'eat (lambda (knpc kpc) (say knpc "Urt eat man")))
       (method 'food (lambda (knpc kpc) (say knpc "All eater food for other")))
       (method 'girl (lambda (knpc kpc) (say knpc "She HRENGI, UG THROGNI")))
       (method 'hills (lambda (knpc kpc) (say knpc "VLARDOGNI in hills, "
                                              "kill trolls. Trolls leave.")))
       (method 'hren (lambda (knpc kpc) (say knpc "Urt not know man-word")))
       (method 'man (lambda (knpc kpc) (say knpc "Urt look for man-child. " 
                                            "She yummy, tender. "
                                            "She trick Urt. "
                                            "URN SEGESTRUM, UG VIGIDUS!")))
       (method 'sege (lambda (knpc kpc) (say knpc "'eater' in troll-word")))
       (method 'spiders (lambda (knpc kpc) (say knpc "Urt hate spiders")))
       (method 'stuck (lamda (knpc kpc) (say knpc "Spiders eat Urt")))
       (method 'thro (lambda (knpc kpc) (say knpc "Urt not know man-word")))
       (method 'troll (lambda (knpc kpc) (say knpc "Trolls hungry. "
                                              "Food in hills."
                                              "Home in hills.")))
       (method 'ug (lambda (knpc kpc) (say knpc "'will be' in troll-word")))
       (method 'urn (lambda (knpc kpc) (say knpc "'was' in troll-word")))
       (method 'vigi (lambda (knpc kpc) (say kpnc "'food' in troll-word")))
       (method 'vlar (lambda (knpc kpc) (say knpc "Urt not know man-word")))
       ))

