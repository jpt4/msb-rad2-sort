;;  msb binary radix sort
;;  UTC20150312 - UTC20150409
;;  jpt4

;;  dual-filter
(define (dual-filter ls pr acc rem)
  (cond
   [(null? ls) (cons (reverse acc) (cons (reverse rem) '()))]
   [(pr (car ls)) (dual-filter (cdr ls) pr (cons (car ls) acc) rem)]
   [else (dual-filter (cdr ls) pr acc (cons (car ls) rem))]))

;;  k-sigfigs?
(define (k-sigfigs? k)
  (lambda (n)
    (eq? (bitwise-length n) k)))

;;  with-k-sigfigs
(define (with-k-sigfigs ls p)
  (dual-filter ls (k-sigfigs? p) '() '()))

;;  main
(define (msb-rad2-sort ls)
  (main-aux ls 0 '()))
    

;;  auxiliary
(define (main-aux ls p acc)
  (cond
   [(null? ls) (flatten* (reverse acc))]
   [else
    (letrec* ([tmp (with-k-sigfigs ls p)]
	      [tar (car tmp)]
	      [tdr (cadr tmp)])
     (if (null? tar)
	 (main-aux tdr (add1 p) acc)
	 (main-aux tdr (add1 p) (cons (sort-aux tar p) acc))))]))
	   
;;  sort fixed bitlength
(define (sort-aux ls p)
  (cond
   [(< p 0) ls]
   [else
    (letrec* ([lh (with-even-at ls p)]
	      [l (car lh)]
	      [h (cadr lh)])
     (flatten* (cons (sort-aux l (sub1 p)) (sort-aux h (sub1 p)))))]))

;;  even at place value? pl := exponent of place-value
(define (even-at? pl)
  (lambda (n)
    (even? (bitwise-arithmetic-shift-right n pl))))

;;  with-even-at place value p
(define (with-even-at ls p)
  (dual-filter ls (even-at? p) '() '()))

;;  flatten list - unwrap one layer of inner parentheses
(define (flatten ls)
  (cond
   [(atom? ls) ls]
   [(deep-singleton? ls) (car ls)]
   [(null? (car ls)) (flatten (cdr ls))]
   [(atom? (car ls)) (cons (car ls) (flatten (cdr ls)))]
   [else (cons (flatten (caar ls)) (flatten (cons (cdar ls) (cdr ls))))]))

;;  deep-singleton? - list of one pair
(define (deep-singleton? ls)
  (and (pair? (car ls)) (equal? (length ls) 1)))

;;  flatten deep list - no inner parentheses
(define (flatten* ls)
  (cond
   [(atom? ls) ls]
   [(null? (car ls)) (flatten* (cdr ls))]
   [(atom? (car ls)) (cons (car ls) (flatten* (cdr ls)))]
   [(pair? (car ls))
    (flatten* (cons (caar ls) (cons (cdar ls) (cdr ls))))]))

;;  iota from a to b
(define (iota-subset a b)
  (if (equal? a b)
      `(,a)
      (cons a (iota-subset (add1 a) b))))

;;  high-to-low seq of bitlength b binary numbers
(define (hl-bin-seq b)
  (if (zero? b)
      '(#b0)
      (reverse (iota-subset (expt 2 (sub1 b)) (sub1 (expt 2 b))))))

;;  n random numbers [0,k)
(define (random-seq n k)
  (if (zero? n)
      '()
      (cons (random k) (random-seq (sub1 n) k))))
