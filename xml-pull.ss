(module xml-pull mzscheme
  (provide (all-defined))
  (require (lib "etc.ss")
           (lib "struct.ss")
           (lib "pretty.ss")
           (only (lib "list.ss") first foldl)
           (planet "ssax.ss" ("lizorkin" "ssax.plt" 1 3))
           (planet "generator.ss" ("dyoo" "generator.plt" 2 0)))
  
  
  ;; This is an implementation of a pull-style parser.

  
  
  ;; An event is one of the following:
  (define-struct event () #f)
  (define-struct (start-element event) (elem-gi attributes namespaces expected-content) #f)
  (define-struct (end-element event) (elem-gi attributes namespaces) #f)
  (define-struct (characters event) (s1 s2) #f)
  (define-struct (exhausted event) ())
  ;; add more events here as necessary
  
  
  
  
  ;; The parser state is a structure:
  ;;
  ;; (make-pstate c l)
  ;;
  ;; where c is a boolean and l is an sexp
  (define-struct pstate (collecting? lst))
  
  ;; pstate-extend: 
  (define (pstate-extend a-pstate thing)
    (copy-struct pstate a-pstate 
                 [pstate-lst (cons thing (pstate-lst a-pstate))]))
  
  
  (define normalize-strings ssax:reverse-collect-str-drop-ws)
  
  
  (define ((new-level-handler yield) elem-gi attributes namespaces expected-content seed)
    (cond
      [(pstate-collecting? seed) 
       (copy-struct pstate seed [pstate-lst '()])]
      [else
       (let ([start-collecting?
              (yield (make-start-element elem-gi attributes namespaces expected-content))])
         (cond
           [(eqv? start-collecting? #t)
            (copy-struct pstate seed 
                         [pstate-collecting? #t]
                         [pstate-lst '()])]
           [else seed]))]))

  
  ;; normalize-attributes: 
  (define (normalize-attributes attributes)
    (reverse 
     (foldl (lambda (x acc)
              (cons (list (elem-gi->symbol (car x))
                          (cdr x))
                    acc))
            '()
            attributes)))

  
  ;; elem-gi->symbol: elem-gi -> symbol
  (define (elem-gi->symbol elem-gi)
    (cond
      [(symbol? elem-gi) elem-gi]
      [else (string->symbol
             (string-append 
              (symbol->string (car elem-gi)) 
              ":" 
              (symbol->string (cdr elem-gi))))]))
  
  
  (define ((finish-element-handler yield) elem-gi attributes namespaces parent-seed seed)

    (define (combine elem-gi attributes)
      (pstate-extend parent-seed
                     `(,(elem-gi->symbol elem-gi)
                        (@ ,@(normalize-attributes attributes))
                        ,@(normalize-strings (pstate-lst seed)))))
    (cond
      [(pstate-collecting? parent-seed)
       (combine elem-gi attributes)]
      [(and (not (pstate-collecting? parent-seed))
            (pstate-collecting? seed))
       (yield (first (pstate-lst (combine elem-gi attributes))))
       parent-seed]
      [else
       (yield (make-end-element elem-gi attributes namespaces))
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
  (define-struct taffy (g))
  
  
  ;; start-xml-pull: input-port -> taffy
  (define (start-xml-pull ip)
    (define-generator (start-xml-pull ip)
      (let ([parser
             (ssax:make-parser
              NEW-LEVEL-SEED (new-level-handler yield)
              FINISH-ELEMENT (finish-element-handler yield)
              CHAR-DATA-HANDLER (char-data-handler yield))])
              ;; don't know how to properly handle PI's yet...
        (parser ip (make-pstate #f '()))))
    (make-taffy (start-xml-pull ip)))

  
  ;; pull-event: taffy -> event
  (define (pull-event a-taffy)
    (generator-next (taffy-g a-taffy) (lambda (exn) (make-exhausted)) #f))

  
  ;; pull-sexp: taffy -> sexp
  (define (pull-sexp a-taffy)
    (generator-next (taffy-g a-taffy) (lambda (exn) (make-exhausted)) #t))
  

  ;; pull-sexps/g: taffy symbol -> (generatorof sexp)
  (define-generator (pull-sexps/g a-taffy a-symbol)
    (let loop ([event (pull-event a-taffy)])
      (cond
        [(exhausted? event)
         (void)]
        [(and (start-element? event)
              (equal? a-symbol (start-element-elem-gi event)))
         (yield (pull-sexp a-taffy))
         (loop (pull-event a-taffy))]
        [else 
         (loop (pull-event a-taffy))])))
  

  (define (test-harness pulling-data trigger)
    (generator-for-each (lambda (x) (pretty-print x) (newline)) 
                        (pull-sexps/g pulling-data trigger)))
  
  (define (test-data)
    (start-xml-pull (open-input-string 
               #<<EOF
<test-xml>
<person>
    <name>Sue Rhee</name>
</person>
<person>
    <name>Dan Garcia</name>
</person>
<person>
    <name>Mike Clancy</name>
</person>
</test-xml>
EOF
               )))
  
  
  (define (test1)
    (test-harness (test-data) 'person))
  
  
  (define (test2)
    (test-harness 
     
     (start-xml-pull (open-input-file "~/Desktop/go_daily-termdb.rdf-xml")) 
     '(http://www.geneontology.org/dtds/go.dtd# . term)))

  
#;  (test1)

  (test2)
  
  
  )