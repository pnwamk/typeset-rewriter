# typeset-rewriter
Simple, low level rewriters for PLT Redex typesetting.

See the PLT Redex docs on [typesetting](http://docs.racket-lang.org/redex/The_Redex_Reference.html?q=redex#%28part._.Typesetting%29) for more details. These macros are just a thin layer on top of these mechanics.


Provides the following syntax:

### rw

Allows for simple compound-rewriter definitions. The compound expression rewriter for
an term 'name must be an s-expression ('name rest ...).

When writing patterns for rw definitions, quote things to be literally matched, all other
identifiers will bind like a standard match variable.

Example usage:
```racket
(define lambda-rw
  (rw ('lambda ([x ': t]) body)
      => (list "" "λ" x ":" t ". " body)))
```

This defines a rewriter which will match PLT Redex terms of the form `(lambda ([any : any]) any)`.
Our particular definition shows how you might use a rewriter to produce 
"more traditional" looking lambda-expressions as far as figures go: `λx:t.e`.

For more info on compound rewriters, see the redex docs. They are supposed to be functions 
with signature `(listof lw) -> (listof (or/c lw? string? pict?))` which is
"rewritten appropriately."

### with-atomic-rewriters

Like `with-compound-rewriters` for atomic rewriters.

Example usage:
```racket
(with-atomic-rewriters (['lambda "λ"] ['-> "→"]) 
 body-expr)
```

### with-rewriters

Allows for atomic and/or compound rewriters to be specified.

Example usage:
```racket
(with-rewriters
 #:atomic (['Env "Γ"] ['exp "e"] ['ty "τ"] ['-> "→"] ['integer "n"])
 #:compound (['lambda lambda-rw]
             ['typeof typeof-rw]
             ['extend extend-rw]
             ['lookup lookup-rw]
             ['val-type val-type-rw])
 body-expr)
```

### define-rw-context

Defines a macro to reuse sets of rewriters.

Example usage:
```racket
(define-rw-context with-rws
  #:atomic (['Env "Γ"] ['exp "e"] ['ty "τ"] ['-> "→"] ['integer "n"])
  #:compound (['lambda lambda-rw]
              ['typeof typeof-rw]
              ['extend extend-rw]
              ['lookup lookup-rw]
              ['val-type val-type-rw]))
              
(with-rws render-expr1)
(with-rws render-expr2)
```

