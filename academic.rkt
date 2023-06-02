#lang racket/base
(require (submod slide-easy generic) racket/draw racket/class)
(provide (contract-out (current-theme-color (parameter/c (or/c string? (is-a?/c color%))))
                       (current-theme-font (parameter/c (or/c font-family/c (is-a?/c font%))))
                       (install-template (->* () (string?) any))))

(define current-theme-color
  (make-parameter "Firebrick"))
(define current-theme-font
  (make-parameter 'roman))

(define (install-template (prefix ""))
  (define type (string->symbol (string-append prefix "学术")))

  (define (add-prefix symbol) (string->symbol (string-append (symbol->string type) "/" (symbol->string symbol))))

  (define (create t . ls) (tag type (tag (add-prefix t) ls)))
  
  (define (handler p proc)
    (if (pict? p) p (proc p)))

  (define (n:->pict subtype) (->pict subtype))

  (define BLACK (make-object color% "Black"))
  
  (define (titlet s (color BLACK)) (text s (cons 'bold (cons color (current-theme-font))) (* 2 (current-font-size))))
  (define (normalt s (color BLACK)) (text s (cons color (current-theme-font)) (current-font-size)))
  (define (smallt s (color BLACK)) (text s (cons color (current-theme-font)) (floor (/ (current-font-size) 2))))

  (define background (blank 1000 700))
  
  (define (封面->pict title member)
    (define mb (handler member normalt))
    (define tt (handler title (lambda (str) (titlet str (make-object color% (current-theme-color))))))
    (lc-superimpose background (vl-append (current-gap-size) tt (hline (floor (/ (+ (pict-width tt) (pict-width mb)) 2)) (current-gap-size)) mb)))

  (define (节->pict index title)
    (define id (handler index titlet))
    (define tt (handler title (lambda (str) (titlet str (make-object color% (current-theme-color))))))
    (cc-superimpose background (vc-append (current-gap-size) id (hline (floor (/ (+ (pict-width tt) (pict-width id)) 2)) (current-gap-size)) tt)))

  (define (图示->pict intr image source)
    (cc-superimpose background (vc-append (current-gap-size)
                                          (handler intr (lambda (str) (titlet str (make-object color% (current-theme-color)))))
                                          image
                                          (handler source smallt))))

  (define (致谢->pict member)
    (封面->pict "THANKS FOR YOUR LISTENING" member))

  (define ((make-handler p) ls) (apply p ls))

  (define names (map add-prefix '(封面 节 图示 致谢)))
  (define funcs (map make-handler (list 封面->pict 节->pict 图示->pict 致谢->pict)))
  (define contracts (list (list/c elem/c elem/c)
                          (list/c elem/c elem/c)
                          (list/c elem/c pict? elem/c)
                          (list/c elem/c)))

  (define elem/c (or/c string? pict?))
  
  (map (lambda (n c f) (install n c f)) names contracts funcs)
  (install type tagged-object? n:->pict)
  
  create)
