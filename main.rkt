#lang racket/base

(require racket/match
         (only-in redex lw with-atomic-rewriter with-compound-rewriters)
         (for-syntax racket/base syntax/parse syntax/stx racket/format)
         (for-template racket/base))

(provide with-atomic-rws
         with-rws
         define-rw-context
         rw-lambda)

;; like 'with-compound-rewriters' but
;; for atomic rewriters
(define-syntax (with-atomic-rws stx)
  (syntax-parse stx
    [(_ () body) #'body]
    [(_ ([x rw] rst ...) body)
     #'(with-atomic-rewriter x rw
         (with-atomic-rws (rst ...)
           body))]))

;; allows for atomic + compound rewriters
(define-syntax (with-rws stx)
  (define-splicing-syntax-class atomic-rws
    (pattern (~seq #:atomic rws:expr))
    (pattern (~seq) #:with rws #'()))
  (define-splicing-syntax-class compound-rws
    (pattern (~seq #:compound rws:expr))
    (pattern (~seq) #:with rws #'()))
  (syntax-parse stx
    [(_ as:atomic-rws cs:compound-rws body)
     #'(with-atomic-rws as.rws
         (with-compound-rewriters cs.rws
           body))]))

;; allow easy definition of macro which
;; uses a particular set of rewriters for
;; executing the body expr
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
         (with-rws #:atomic as.rws
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

;; rw
;; rewriter builder
;; (rw name [`(name pats ...) => output] ...)
;; ' is used to declare literal matches
;; anything else is matched like a match pattern variable
;; output is any expression
(define-syntax (rw-lambda stx)
  (syntax-parse stx
    [(_ [((~literal quasiquote) (name:id . args)) (~datum =>) output]
        cases ...)
     #`(match-lambda
         #,(parse-rw-lambda-case #'name #'[(quasiquote (name . args)) => output])
         #,@(stx-map (λ (c) (parse-rw-lambda-case #'name c)) #'(cases ...))
         [else (error 'rw-lambda "no ~a case for ~a" 'name else)])]
    [_ (raise-syntax-error
        'rw
        (~a "expected: (rw [`(name pats ...) => output] ...)\n got: "
            (syntax->datum stx)
            "\n")
        stx)]))

;; parses an rw-lambda case
;; name: identifier? 
;; case: syntax?
;;       should be of form [`(rw-name pats ...) => output-exp]
;; output: syntax?
(define-for-syntax (parse-rw-lambda-case name case)
  (syntax-parse case
    [[((~literal quasiquote) (cname:id . args)) (~datum =>) output]
     (unless (free-identifier=? name #'cname)
       (raise-syntax-error
        'rw-case
        (~a "expected (rw [`(name pats ...) => output] ...),\n but case name "
            (syntax->datum name)
            " is not equal to "
            (syntax->datum #'cname))
        case))
     #`[(or (list
             (lw "(" _ _ _ _ #f #f)
             (-literal cname) #,@(stx-map parse-rw-case-internal #'args)
             (lw ")" _ _ _ _ #f #f))
            
            (list
             (lw "[" _ _ _ _ #f #f)
             (-literal cname) #,@(stx-map parse-rw-case-internal #'args)
             (lw "]" _ _ _ _ #f #f))
            
            (list
             (lw "{" _ _ _ _ #f #f)
             (-literal cname) #,@(stx-map parse-rw-case-internal #'args)
             (lw "}" _ _ _ _ #f #f)))
        output]]
    [_ (raise-syntax-error
        'rw-case
        (~a " expected (rw [`(name pats ...) => output] ...)\n got: "
            (syntax->datum case)
            "\n")
        case)]))


;; parse the inside of a rw-lambda case pattern, conerting
;; each piece into the appropriate match forms
(define-for-syntax (parse-rw-case-internal stx)
  (syntax-parse stx
    [((~literal quote) (~datum ...)) #'(lw '(... ...) _ _ _ _ _ _)]
    [(~datum ...) stx]
    [((~literal unquote) match-expr) #'match-expr]
    [sym:id
     #'(-literal sym)]
    [(e ...)
     #`(-sexp #,@(stx-map parse-rw-case-internal #'(e ...)))]
    [_ (raise-syntax-error
        'rw
        (~a "unrecognized rw pattern: " (syntax->datum stx))
        stx)]))
