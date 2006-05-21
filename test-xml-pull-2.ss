(module test-xml-pull-2 mzscheme
  (require (lib "url.ss" "net")
           (lib "inflate.ss")
           (lib "pretty.ss")
           (lib "port.ss")
           (planet "xml-pull.ss" ("dyoo" "xml-pull.plt" 1 0))
           (planet "generator.ss" ("dyoo" "generator.plt" 2 0)))

  ;; wrap-gunzip: input-port input-port
  ;; Wraps an uncompressor around the input stream.
  (define (wrap-gunzip original-ip)
    (define-values (ip op) (make-pipe 32768))
    (thread (lambda () (gunzip-through-ports original-ip op)))
    ip)

  
  (define my-url
    (string->url "http://archive.godatabase.org/latest-termdb/go_daily-termdb.rdf-xml.gz"))
  (define my-input-port (wrap-gunzip (get-pure-port my-url)))
  
  (define my-taffy (start-xml-pull my-input-port))
  (define generated-terms (pull-sexps/g my-taffy 'http://www.geneontology.org/dtds/go.dtd#:term))

  ;; pretty-print the first 100 terms in the Gene Ontology
  (let loop ([i 0])
    (when (< i 100)
      (pretty-print (generator-next generated-terms))
      (loop (add1 i))))
  
  ;; If you want to print out all terms in the ontology, comment the previous expression
  ;; and uncomment this following one:
  #; (generator-for-each pretty-print generated-terms)
  )
