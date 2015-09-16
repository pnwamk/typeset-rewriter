#lang racket/base

(require racket/match
         (only-in redex lw with-atomic-rewriter with-compound-rewriters)
         (for-syntax racket/base syntax/parse syntax/stx)
         (for-template racket/base))

(provide with-atomic-rewriters
         with-rewriters
         define-rw-context
         rw)

;; like 'with-compound-rewriters' but
;; for atomic rewriters
(define-syntax (with-atomic-rewriters stx)
  (syntax-parse stx
    [(_ () body) #'body]
    [(_ ([x rw] rst ...) body)
     #'(with-atomic-rewriter x rw
         (with-atomic-rewriters (rst ...)
           body))]))

;; allows for atomic + compound rewriters
(define-syntax (with-rewriters stx)
  (define-splicing-syntax-class atomic-rws
    (pattern (~seq #:atomic rws:expr))
    (pattern (~seq) #:with rws #'()))
  (define-splicing-syntax-class compound-rws
    (pattern (~seq #:compound rws:expr))
    (pattern (~seq) #:with rws #'()))
  (syntax-parse stx
    [(_ as:atomic-rws cs:compound-rws body)
     #'(with-atomic-rewriters as.rws
         (with-compound-rewriters cs.rws
           body))]))

(define-syntax (define-rw-context stx)
  (define-splicing-syntax-class atomic-rws
    (pattern (~seq #:atomic rws:expr))
    (pattern (~seq) #:with rws #'()))
  (define-splicing-syntax-class compound-rws
    (pattern (~seq #:compound rws:expr))
    (pattern (~seq) #:with rws #'()))
  (syntax-parse stx
    [(_ name:id as:atomic-rws cs:compound-rws)
     #'(define-syntax-rule (name body-expr)
         (with-rewriters #:atomic as.rws
                         #:compound cs.rws
                         body-expr))]))

;; matches a redex lw struct
;; with the specified symbol
(define-match-expander -literal
  (λ (stx)
    (syntax-case stx ()
      [(_ sym)
       #'(lw 'sym _ _ _ _ _ _)])))


;; used to match syntactic lists
;; in redex patterns
(define-match-expander -sexp
  (λ (stx)
    (syntax-case stx ()
      [(_ pats ...)
       #'(lw (or (list (lw "(" _ _ _ _ #f #f)
                       pats ...
                       (lw ")" _ _ _ _ #f #f))
                 (list (lw "[" _ _ _ _ #f #f)
                       pats ...
                       (lw "]" _ _ _ _ #f #f))
                 (list (lw "{" _ _ _ _ #f #f)
                       pats ...
                       (lw "}" _ _ _ _ #f #f)))
             _ _ _ _ _ _)])))

;; parse the inside of a rw pattern, conerting
;; each piece into the appropriate match forms
(define-for-syntax (parse-rw-internal stx)
  (syntax-parse stx
    [x:id #'x]
    [((~literal quote) sym)
     #'(-literal sym)]
    [(e ...)
     #`(-sexp #,@(stx-map parse-rw-internal #'(e ...)))]))

;; rw
;; rewriter builder
;; (rw ('name pat ...) => output)
;; ' is used to declare literal matches
;; anything else is matched like a match pattern variable
;; output is any expression
(define-syntax (rw stx)
  (syntax-parse stx
    [(_ (((~literal quote) name) . args) (~datum =>) output)
     #`(match-lambda
         [(or (list (lw "(" _ _ _ _ #f #f)
                    (-literal name) #,@(stx-map parse-rw-internal #'args)
                    (lw ")" _ _ _ _ #f #f))
              (list (lw "[" _ _ _ _ #f #f)
                    (-literal name) #,@(stx-map parse-rw-internal #'args)
                    (lw "]" _ _ _ _ #f #f))
              (list (lw "{" _ _ _ _ #f #f)
                    (-literal name) #,@(stx-map parse-rw-internal #'args)
                    (lw "}" _ _ _ _ #f #f)))
          output]
         [else (error 'rw "unrecognized case of ~a: ~a" 'name else)])]
    [_ (error 'rw "expected (rw ('name args ...) => output), got ~a" stx)]))
