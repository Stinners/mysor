#lang racket 

(define example-file (open-input-file "example.gmi"))
(define lines (sequence->stream (in-lines example-file)))

(define header (stream-first lines))
(println header)

;; TODO figure out how to do this without prefab
(struct gmi-header (code status) #:prefab)

(define (parse-header header)
  (let ([code (substring header 0 2)]
        [status (substring header 3)])
    (gmi-header code status)))

(parse-header "20 text/gemini")
