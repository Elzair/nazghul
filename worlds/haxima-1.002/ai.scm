;;----------------------------------------------------------------------------
;; Generic AI used by kernel, reproduced here as a starting point
;;----------------------------------------------------------------------------
(define (ai-display args) nil)
(define (ai-newline) nil)

(define (ai-select-target kchar)
  (ai-display "ai-select-target")(ai-newline)
  (nearest-obj kchar (all-visible-hostiles kchar)))

(define (ai-wander kchar)
  (ai-display "ai-wander")(ai-newline)
  (kern-obj-wander kchar))

(define (in-range? karms dist)
  (ai-display "in-range?")(ai-newline)
  (<= dist (kern-arms-type-get-range karms)))

(define (has-ammo? kchar karms)
  (ai-display "has-ammo?")(ai-newline)
  (or (not (arms-type-needs-ammo? karms))
      (let ((ammo-type (kern-arms-type-get-ammo-type karms)))
        (ai-display "has-ammo?: ammo-type=")(ai-display ammo-type)(ai-newline)
        (or (null? ammo-type)
            (kern-obj-has? kchar ammo-type)))))

(define (weapon-blocked? karms dist)
  (ai-display "weapon-blocked?")(ai-newline)
  (and (< dist 2)
       (arms-type-is-blockable? karms)))

(define (ai-select-weapon katt kdef)  
  (let ((defdist (distance katt kdef)))
  (ai-display "ai-select-weapon:defdist=")(ai-display defdist)(ai-newline)
    (define (weapon-ok? karms)
      (ai-display "ai-select-weapon:weapon-ok?")(ai-newline)
      (and (in-range? karms defdist)
           (has-ammo? katt karms)
           (not (weapon-blocked? karms defdist))))
    (define (scan-weapons wlist)
      (ai-display "ai-select-weapon:scan-weapons")(ai-newline)
      (if (null? wlist) nil
          (let ((karms (car wlist)))
            (if (weapon-ok? karms)
                karms
                (scan-weapons (cdr wlist))))))
    (scan-weapons (kern-char-get-weapons katt))))

(define (ai-attack-target kchar ktarg)
  (ai-display "ai-attack-target")(ai-newline)
  (define (do-attack-loop retval)
    (let ((kweap (ai-select-weapon kchar ktarg)))
      (ai-display "ai-attack-target:kweap=")(ai-display kweap)(ai-newline)
      (if (null? kweap) retval
          (begin
            (kern-char-attack kchar kweap ktarg)
            (if (and (is-alive? ktarg)
                     (has-ap? kchar))
                (do-attack-loop #t)
                #t)))))
  (do-attack-loop #f))

(define (ai-pathfind-to-target kchar ktarg)
  (ai-display "ai-pathfind-to-target")(ai-newline)
  (pathfind kchar (kern-obj-get-location ktarg)))

(define (generic-ai kchar)
  (ai-display "generic-ai")(ai-newline)
  (let ((ktarg (ai-select-target kchar)))
    (ai-display "generic-ai: ktarg=")(ai-display ktarg)(ai-newline)
    (if (null? ktarg)
        (ai-wander kchar)
        (begin
          (taunt kchar ktarg)
          (or (ai-attack-target kchar ktarg)
              (ai-pathfind-to-target kchar ktarg))))))

;; Bandit AI --------------------------------------------------

(define bandit-taunts 
  (list 
   "Yer money or yer life!"
   "Have at 'cher!"
   "Yer a dead man, ye are!"
   "Oy!  You!  Gerrout!"
   "'Ave at 'im, boys!"
   "Circle round, we've got a dead one!"
   "Dibs on 'is boots!"
   "Stranger, meetcha couple my friends..."
   ))

(define (bandit-taunted? kbandit)
  (car (kobj-gob-data kbandit)))

(define (bandit-taunt kbandit ktarg)
  (taunt kbandit ktarg bandit-taunts)
  (set-car! (kobj-gob-data kbandit) #t))

(define (bandit-ai kchar)
  (let ((ktarg (ai-select-target kchar)))
    (if (null? ktarg)
        (ai-wander kchar)
        (begin
          (or (bandit-taunted? kchar)
              (bandit-taunt kchar ktarg))
          (or (ai-attack-target kchar ktarg)
              (ai-pathfind-to-target kchar ktarg))))))
