(module xml-pull mzscheme
  (require (lib "etc.ss")
           (lib "struct.ss")
           (only (lib "list.ss") first foldl)
           (planet "ssax.ss" ("lizorkin" "ssax.plt" 1 3))
           (planet "generator.ss" ("dyoo" "generator.plt" 2 0)))
  
  
  ;; This is an implementation of a XML pull-style parser.  I'm feeling a little whimsical,
  ;; so the abstractions here are named after food.

  
  ;; An morsel is one of the following:
  (define-struct morsel () #f)
  (define-struct (start-element morsel) (name attributes) #f)
  (define-struct (end-element morsel) (name attributes) #f)
  (define-struct (characters morsel) (s1 s2) #f)
  (define-struct (exhausted morsel) () #f)

  (provide (struct morsel ())
           (struct start-element (name attributes))
           (struct end-element (name attributes))
           (struct characters (s1 s2))
           (struct exhausted()))
  ;; Add more morsels here as necessary.  I wonder if we'll want a start-document or end-document
  ;; morsel?
  
  
  
  
  ;; The parser state is a structure:
  ;;
  ;; (make-pstate c l)
  ;;
  ;; where c is a boolean and l is an sexp.  We set collecting? to true
  ;; whenever we're interested in accumulating children into the pstate
  ;; structure.  l is used to accumulate the s-expressions when we're
  ;; in an interested mood.
  (define-struct pstate (collecting? lst))
  
  
  ;; pstate-extend: pstate sexp -> pstate
  ;; Accumulates a-sexp into the parser state a-state.
  (define (pstate-extend a-pstate a-sexp)
    (copy-struct pstate a-pstate 
                 [pstate-lst (cons a-sexp (pstate-lst a-pstate))]))
  
  
  ;; rev-normalize-string-children: (listof sexp) -> (listof sexp)
  ;; Does a shallow reveral of elements, and concatenates adjacent strings
  ;; together.
  (define rev-normalize-string-children ssax:reverse-collect-str-drop-ws)
  
  
  
  (provide current-namespace-translate)
  ;; current-namespace-translate: (parameterof (symbol -> symbol))
  ;; Used to do additional translation of namespaces to something convenient.
  (define current-namespace-translate 
    (make-parameter (lambda (ns) ns)))

  
  ;; elem-gi->symbol: elem-gi -> symbol
  ;; Translate an elem-gi (which is either itself a symbol or a pair of symbols)
  ;; into a single symbol.
  ;; FIXME: allow user to provide translational map of the namespace
  ;; to something that they like.
  (define (elem-gi->symbol elem-gi)
    (cond
      [(symbol? elem-gi) elem-gi]
      [else
       (string->symbol
        (string-append 
         (symbol->string ((current-namespace-translate) (car elem-gi))) 
         ":" 
         (symbol->string (cdr elem-gi))))]))

  
  ;; normalize-attributes: (listof (elem-gi . string)) -> (listof (list symbol string))
  ;; Forces attribute names to be symbols and restructures
  ;; each attribute name/value pair into a list.
  (define (normalize-attributes attributes)
    (reverse 
     (foldl (lambda (x acc)
              (cons (list (elem-gi->symbol (car x))
                          (cdr x))
                    acc))
            '()
            attributes)))

  
  (define ((new-level-handler yield) elem-gi attributes namespaces expected-content seed)
    (cond
      [(pstate-collecting? seed) 
       (copy-struct pstate seed [pstate-lst '()])]
      [else
       (let ([start-collecting?
              (yield (make-start-element (elem-gi->symbol elem-gi) 
                                         (normalize-attributes attributes)))])
         (cond
           [(eqv? start-collecting? #t)
            (copy-struct pstate seed 
                         [pstate-collecting? #t]
                         [pstate-lst '()])]
           [else seed]))]))

      
  
  (define ((finish-element-handler yield) elem-gi attributes namespaces parent-seed seed)

    (define (combine elem-gi attributes)
      (pstate-extend parent-seed
                     `(,(elem-gi->symbol elem-gi)
                        (@ ,@(normalize-attributes attributes))
                        ,@(rev-normalize-string-children (pstate-lst seed)))))
    (cond
      [(pstate-collecting? parent-seed)
       (combine elem-gi attributes)]
      [(and (not (pstate-collecting? parent-seed))
            (pstate-collecting? seed))
       (yield (first (pstate-lst (combine elem-gi attributes))))
       parent-seed]
      [else
       (yield (make-end-element (elem-gi->symbol elem-gi)
                                (normalize-attributes attributes)))
       parent-seed]))
  
  
  (define ((char-data-handler yield) s1 s2 seed)
    (cond
      [(pstate-collecting? seed) 
       (cond 
         [(string=? s2 "") (pstate-extend seed s1)]
         [else (pstate-extend (pstate-extend seed s1) s2)])]
      [else
       (yield (make-characters s1 s2))
       seed]))
  
  

  
  
;                                     
;                                     
;                  :@@$   :@@$        
;    @             @:     @:          
;   @@@@@   $@$:  @@@@@  @@@@@ @@@ @@@
;    @        -@   @      @     $-  $-
;    @     -$@$@   @      @     -$  $ 
;    @     $*  @   @      @      $*$: 
;    @: :$ @- *@   @      @       $$  
;    :@@$- -$$-@@ @@@@@  @@@@@    $*  
;                                 $   
;                               @@@@  
;                                     
;                                     
  
  ;; A taffy is a structure:
  ;;
  ;; (make-taffy g)
  ;;
  ;; where g is a generator.  We use a taffy to pull off morsels
  ;; of chewy XML.
  (define-struct taffy (g last-morsel))
  

  (provide start-xml-pull)
  ;; start-xml-pull: input-port -> taffy
  (define (start-xml-pull ip)
    (define-generator (start-parsing ip)
      (let ([parser
             (ssax:make-parser
              NEW-LEVEL-SEED (new-level-handler yield)
              FINISH-ELEMENT (finish-element-handler yield)
              CHAR-DATA-HANDLER (char-data-handler yield))])
              ;; don't know how to properly handle PI's yet...
        (parser ip (make-pstate #f '()))))
    (make-taffy (start-parsing ip) #f))

  
  (provide pull-morsel)
  ;; pull-morsel: taffy -> morsel
  (define (pull-morsel a-taffy)
    (let ([evt
           (generator-next (taffy-g a-taffy) 
                           (lambda (exn) (make-exhausted)) #f)])
      (set-taffy-last-morsel! a-taffy evt)
      evt))

  
  (provide pull-sexp)
  ;; pull-sexp: taffy -> (union sexp exhaused)
  (define (pull-sexp a-taffy)
    (unless (start-element? (taffy-last-morsel a-taffy))
      (error 'pull-sexp 
             "can only pull an sexp if the last morsel was a start-element, but was ~a"
             (taffy-last-morsel a-taffy)))
    (let ([evt
           (generator-next (taffy-g a-taffy)
                           (lambda (exn) (make-exhausted)) #t)])
      (set-taffy-last-morsel! a-taffy evt)
      evt))
  
  
  (provide pull-sexps/g)
  ;; pull-sexps/g: taffy symbol -> (generatorof sexp)
  (define-generator (pull-sexps/g a-taffy a-symbol)
    (let loop ([morsel (pull-morsel a-taffy)])
      (cond
        [(exhausted? morsel)
         (void)]
        [(and (start-element? morsel)
              (symbol=? a-symbol (start-element-name morsel)))
         (yield (pull-sexp a-taffy))
         (loop (pull-morsel a-taffy))]
        [else 
         (loop (pull-morsel a-taffy))]))))