;; ----------------------------------------------------------------------------
;; simple, non-prioritized, generic search (not very efficient, so don't try
;; it on large search spaces)
;; ----------------------------------------------------------------------------
(define (search here? next start)
  (define (do-search queue visited)
    (if (null? queue) nil
        (let ((loc (car queue)))
          (if ((here? loc) loc)
              (do-search (append (cdr queue)
                                 (filter (lambda (v) (not (member v visited))) 
                                         (next loc)))
                         (append (list loc) visited)))))))
  (do-search (list start) nil))
