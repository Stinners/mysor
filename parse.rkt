#lang racket 

(define-syntax ~>
  (syntax-rules () 
    ([~> value] value)
    ([~> value ('λ def ...) rest ...] (~> ((λ def ...) value) rest ...))
    ([~> value (fn args ...) rest ...] (~> (fn value args ...) rest ...))
    ([~> value fn rest ...] (~> (fn value) rest ...))))

(define-syntax ~>>
  (syntax-rules () 
    ([~>> value] value)
    ([~>> value ('λ def ...) rest ...] (~>> ((λ def ...) value) rest ...))
    ([~>> value (fn args ...) rest ...] (~>> (fn args ... value) rest ...))
    ([~>> value fn rest ...] (~>> (fn value) rest ...))))
    
(define (parse-link line)
  (-> line 
      (substring 2)
      (string-trim #:repeat? #t)))

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

(struct gmi-line (type text))

(struct parser-state (preformat? links lines))

(define (switch-preformat preformat? s)
  (if preformat (struct-copy parser-state s [preformat? (not (parser-state-preformat? s))]
                 s)))

(define (add-link line s)
  (let* ([line-type (gmi-line-type line)]
         [text (parse-link (gmi-text line))]))
  (if (equal? (gmi-line-type line) 'link
       (struct-copy parser-state s [links (cons (gmi-line-text) (parser-state-links s))]
           s))))

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

(define (get-line-type line)
  (let ([first-char (nth-char 0 line)])
    (match first-char
      [#\# 'header]
      [#\* 'list]
      [#\> 'quote]
      [#\= (if (equal? (#\> (nth-char 2 line)))
               'link 
               'text)]
      [#\` (if (starts-with "```" line)
               'preformat-switch
               'text)]
      [_ 'text])))

(define (parse-lines preformat? state)
  (match lines
    [empty (values links parsed-lines)]
    [(list line rest ...)
     (let* ([raw-line-type (get-line-type line)]
            [preformat? (xor preformat? (equal? raw-line-type 'preformat-switch))]
            [line-type (if preformat? 
                           'preformat 
                           raw-line-type)]
            [new-link (if (equal? line-type 'link)
                          (parse-link line)
                          #f)]
            [new-line (gmi-line line-type line)])
        (~>> state
             (switch-preformat preformat?)
             (add-link)
          ()))]))
       
(define example-file (open-input-file "example.gmi"))
(define lines-seq (sequence->stream (in-lines example-file)))

(define header (stream-first lines-seq))
(define lines (stream-rest lines-seq))

(define parsed-header (parse-header "20 text/gemini"))

