#lang racket
(require racket/draw)

;;DEFINE n of grid, and coordinates of outlying block
(define bn 4) ;;Grid of size 2^bn (1-9)
(define bx 5) ;;x coordinate of missing block (starting at 0)
(define by 9) ;;y coordinate of missing block (starting at 0)
;;define end

;;Draw base grid and outlying block
(define target (make-bitmap (* 7 (expt 2 bn)) (* 7 (expt 2 bn))))
(define grid (new bitmap-dc% [bitmap target]))

(define (recurse n x y)
  (send grid draw-rectangle (* 7 x) (* 7 y) 7 7)
  (cond
    [(> x 0) (recurse n (- x 1) y)]
    [(> y 0) (recurse n (- (expt 2 n) 1) (- y 1))]
    [else #f]
  )
)
(define (build n x y)
  (send grid set-pen "black" 1 'solid)
  (send grid set-brush "blue" 'solid)
    (recurse n (- (expt 2 n) 1) (- (expt 2 n) 1))
  (send grid set-brush "red" 'solid)
    (send grid draw-rectangle (* 7 x) (* 7 y) 7 7)
)
;;Grid Draw end

;;Trimo
(define (trimo ox oy nx ny)
    (if (= nx 0)
      (if (= ny 0)
          ((lambda (NW)
           (send grid set-brush "cyan" 'solid)
           (send grid draw-rectangle (* 7 (+ 1 ox)) (* 7 oy) 7 7)
           (send grid draw-rectangle (* 7 (+ 1 ox)) (* 7 (+ 1 oy)) 7 7)
           (send grid draw-rectangle (* 7 ox) (* 7 (+ 1 oy)) 7 7)
          )1)
          ((lambda (NE)
           (send grid set-brush "green" 'solid)
           (send grid draw-rectangle (* 7 (+ 1 ox)) (* 7 oy) 7 7)
           (send grid draw-rectangle (* 7 (+ 1 ox)) (* 7 (+ 1 oy)) 7 7)
           (send grid draw-rectangle (* 7 ox) (* 7 oy) 7 7)
          )1)
      )
      (if (= ny 0)
          ((lambda (SW)
           (send grid set-brush "purple" 'solid)
           (send grid draw-rectangle (* 7 ox) (* 7 oy) 7 7)
           (send grid draw-rectangle (* 7 (+ 1 ox)) (* 7 (+ 1 oy)) 7 7)
           (send grid draw-rectangle (* 7 ox) (* 7 (+ 1 oy)) 7 7)
          )1)
          ((lambda (SE)
           (send grid set-brush "yellow" 'solid)
           (send grid draw-rectangle (* 7 ox) (* 7 oy) 7 7)
           (send grid draw-rectangle (* 7 (+ 1 ox)) (* 7 oy) 7 7)
           (send grid draw-rectangle (* 7 ox) (* 7 (+ 1 oy)) 7 7)
          )1)
      )
    )
  )
;;trimo end

;;the actual thing
(define (recurs n ox oy x y)
  (define onx (+ ox (/ (expt 2 n) 2))) ;;halfway x
  (define ony (+ oy (/ (expt 2 n) 2))) ;;halfway y
  (define on (/ (expt 2 n) 2)) ;;halfway n
  (cond
    [(= n 1) (trimo ox oy x y)] ;;base case
    [(> n 1) (if (> x (- on 1))
       (if (> y (- on 1))
          ((lambda (SE)
           (recurs (- n 1) onx ony (- x on) (- y on)) ;;SE
           (recurs (- n 1) ox ony (- on 1) 0) ;:SW
           (recurs (- n 1) onx oy 0 (- on 1)) ;;NE
           (recurs (- n 1) ox oy (- on 1) (- on 1)) ;;NW
          )1)
          ((lambda (NE)
           (recurs (- n 1) onx ony 0 0) ;;SE
           (recurs (- n 1) ox ony (- on 1) 0) ;:SW
           (recurs (- n 1) onx oy (- x on) y) ;;NE
           (recurs (- n 1) ox oy (- on 1) (- on 1)) ;;NW
          )1)
      )
      (if (> y (- on 1))
          ((lambda (SW)
           (recurs (- n 1) onx ony 0 0) ;;SE
           (recurs (- n 1) ox ony x (- y on)) ;:SW
           (recurs (- n 1) onx oy 0 (- on 1)) ;;NE
           (recurs (- n 1) ox oy (- on 1) (- on 1)) ;;NW
          )1)
          ((lambda (NW)
           (recurs (- n 1) onx ony 0 0) ;;SE
           (recurs (- n 1) ox ony (- on 1) 0) ;:SW
           (recurs (- n 1) onx oy 0 (- on 1)) ;;NE
           (recurs (- n 1) ox oy x y) ;;NW
          )1)
      )
    )
    ]
  )
)
;;thing end

;;execute functions
(build bn bx by)
(recurs bn 0 0 bx by)
   (send grid set-brush "red" 'solid)
   (send grid draw-rectangle (* 7 bx) (* 7 by) 7 7)

(send target save-file "grid.png" 'png)
;;functions end