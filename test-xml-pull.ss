(module test-xml-pull mzscheme
  (require (lib "pretty.ss")
           (lib "etc.ss")
           "xml-pull.ss"
           (planet "generator.ss" ("dyoo" "generator.plt" 2 0)))
  
  
    
;                              
;                              
;  @@@@@@@                     
;  @  @  @                @    
;     @    -@@$   :@@+@  @@@@@ 
;     @    $  -$  @$ -@   @    
;     @    @@@@@  :@@$-   @    
;     @    $         *@   @    
;     @    +:     @  :@   @: :$
;    @@@    $@@+  $+@@:   :@@$-
;                              
;                              
;                              
;                              
  
  (define (test-harness a-taffy a-symbol)
    (generator-for-each (lambda (x) (pretty-print x) (newline)) 
                        (pull-sexps/g a-taffy a-symbol)))
  
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
     'http://www.geneontology.org/dtds/go.dtd#:term))

  
  (define (test3)
    (parameterize
        ([current-namespace-translate
          (lambda (ns)
            (cond [(symbol=? ns 'http://www.geneontology.org/dtds/go.dtd#)
                   'go]
                  [(symbol=? ns 'http://www.w3.org/1999/02/22-rdf-syntax-ns#)
                   'rdf]
                  [else ns]))])
      (test-harness
       (start-xml-pull (open-input-file "~/Desktop/go_daily-termdb.rdf-xml"))
       'go:term)))
  
  (test1)

  #;(test2)

  #;(test3)
  )
