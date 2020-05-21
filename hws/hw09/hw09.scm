
; Tail recursion

(define (replicate x n)
  (define (replicate-helper x n s)
    (if (= n 0)
      s
      (replicate-helper x (- n 1) (cons x s))
    )
  )
    (replicate-helper x n '())
)

(define (accumulate combiner start n term)
  (define (accumulate-helper combiner start n term)
    (if (= n 0)
      start
      (accumulate-helper combiner (combiner start (term n)) (- n 1) term)
    )
  )
  (accumulate-helper combiner start n term)
)

(define (accumulate-tail combiner start n term)
  (define (accumulate-helper combiner start n term)
    (if (= n 0)
      start
      (accumulate-helper combiner (combiner start (term n)) (- n 1) term)
    )
  )
  (accumulate-helper combiner start n term)
)

; Streams

(define (map-stream f s)
    (if (null? s)
    	nil
    	(cons-stream (f (car s)) (map-stream f (cdr-stream s)))))

(define multiples-of-three
  (cons-stream 3 (map-stream (lambda (x) (+ x 3)) multiples-of-three) 
  )
)


(define (nondecreastream s)
  (cond ((null? s) nil)
    ((null? (cdr-stream s)) (cons-stream (list (car s)) nil))
    ((> (car s) (car (cdr-stream s))) (cons-stream (list (car s)) (nondecreastream (cdr-stream s))))
    (else (cons-stream (cons (car s) (car (nondecreastream (cdr-stream s)))) (cdr-stream (nondecreastream (cdr-stream s)))))
  )
)


(define finite-test-stream
    (cons-stream 1
        (cons-stream 2
            (cons-stream 3
                (cons-stream 1
                    (cons-stream 2
                        (cons-stream 2
                            (cons-stream 1 nil))))))))

(define infinite-test-stream
    (cons-stream 1
        (cons-stream 2
            (cons-stream 2
                infinite-test-stream))))