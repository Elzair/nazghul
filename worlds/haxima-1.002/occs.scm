;;----------------------------------------------------------------------------
;; occupations
(define (mk-occ tag name hit def dam arm xp)
  (kern-mk-occ tag name 1.0 0 0 0 0 hit def dam arm xp))

;;            /           /         / t  / f / e /   /
;;           /           /         / i  / e / g / r /
;;          /           / e       / h  / d / a / o /
;;         / g         / m       / -  / - / m / m /
;;        / a         / a       / o  / o / a / r / p
;;       / t         / n       / t  / t / d / a / x
(mk-occ 'oc_wizard   "wizard"   -1  -1  -1  -1   4)
(mk-occ 'oc_warrior  "warrior"   1   0   1   0   4)
(mk-occ 'oc_wright   "wright"    0   1   0   1   4)
(mk-occ 'oc_wrogue   "wrogue"    1   1   0   0   4)
(mk-occ 'oc_wanderer "wanderer"  5   5   5   5   8)
(mk-occ 'oc_ranger   "ranger"    1   1   0   0   4)
