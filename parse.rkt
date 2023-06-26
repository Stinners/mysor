#lang racket 

(define-syntax ~>
  (syntax-rules () 
    ([~> value] value)
    ([~> value ('λ def ...) rest ...] (~> ((λ def ...) value) rest ...))
    ([~> value (fn args ...) rest ...] (~> (fn value args ...) rest ...))
    ([~> value fn rest ...] (~> (fn value) rest ...))))

(define example-file (open-input-file "example.gmi"))
(define lines (sequence->stream (in-lines example-file)))

(define header (stream-first lines))
(println header)

(define (between value min max)
  (and (>= value min)
       (<= value max)))

(struct gmi-header (code status)
  #:guard (λ (code-str status name)
            (let ([code (string->number code-str)]) 
              (unless (between code 10 99)
                (error "Code must be a two digit integer"))
              (unless (>= 1024 (string-length status))
                (error "Status cannot be longer than 1024 characters"))
              (values code status))))

(define (parse-header header)
  (gmi-header (substring header 0 2)
              (substring header 3)))

(define (nth-char n line)
  (if (< n (string-length line))
      (string-ref line 0)
      #\nul))

(define (starts-with substr str)
  (let ([substr-len (string-length substring)]
        [str-len (string-length str)])
      (cond 
        [(> substr-len str-len)                    #f]
        [(equal? str (substring substr 0 str-len)) #t] 
        [else                                      #f])))
    
(define (parse-link line)
  (-> line 
      (substring 2)
      (string-trim #:repeat? #t)))

(define (match-line line)
  (let ([first-char (nth-char 0 line)])
    (match first-char
      [#\# 'header]
      [#\* 'list]
      [#\> 'quote]
      [#\= (if (equal? (#\> (nth-char 2 line)))
               'link 
               'text)]
      [#\` (if (starts-with "```" line)
               'preformat
               'text)]
      [_ 'text])))
  
(define (parse-line preformat? links line)
  (let* ([first-char (substring line 0 1)]
         [line-type (match-line line)] 
         [preformat? (xor preformat? (equal? line-type 'preformat))]
         [links (if (equal? line-type 'link)
                    (-> line (parse-link) (cons links))
                    links)])
    (values preformat? links line-type)))
       

(parse-header "20 text/gemini")
