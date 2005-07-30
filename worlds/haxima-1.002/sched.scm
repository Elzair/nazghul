;; sched.scm -- schedules
;;
;; Intuitively a schedule should be associated with its character. However, a
;; schedule is read-only data, so it doesn't really belong in the session file
;; where the character is defined.

(kern-mk-sched 'sch_shroom
               (list 0  0  51 9  1  1  "sleeping")
               (list 8  0  40 11 3  3  "idle")
               (list 9  0  49 6  7  1  "working")
               (list 12 0  50 9  1  1  "eating")
               (list 13 0  49 6  7  1  "working")
               (list 18 0  56 54 1  1  "eating")
               (list 19 0  53 50 4  7  "idle")
               (list 21 0  51 9  1  1  "sleeping"))

