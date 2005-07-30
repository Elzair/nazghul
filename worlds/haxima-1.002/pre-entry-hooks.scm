
;; Note: this procedure cannot be defined in the session.scm file or it will
;; not be saved and therefore will not be available when reloading the game. In
;; general, nothing should be defined (using Scheme 'define') in the session
;; file, only kernel calls should be made. The exception is things that are
;; truly only needed once on initial startup (for example, I currently define
;; some time constants there that fit this category). Since in general
;; procedures do not have modifiable state they can be kept in files (like this
;; one) which are loaded by the session file.
(define (green-tower-pre-entry-hook kplace kpp)
  (kern-print "Enter GreenTower?\n")
  (kern-conv-get-yes-no? kpp))

(define (pit-of-death-pre-entry-hook kplace kpp)
  (kern-print "BWA-HA-HA-HA-HA!")
  #t)
