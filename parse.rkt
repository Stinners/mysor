#lang racket 

(require "util.rkt")

;; Helpers 

(define (between value min max)
  (and (>= value min)
       (<= value max)))

(define (parse-header header)
  (gmi-header (substring header 0 2)
              (substring header 3)))

(struct gmi-header (code status)
  #:guard (λ (code-str status name)
            (let ([code (string->number code-str)]) 
              (unless (between code 10 99)
                (error "Code must be a two digit integer"))
              (unless (>= 1024 (string-length status))
                (error "Status cannot be longer than 1024 characters"))
              (values code status))))

;; Structs

(struct gmi-text (text))
(struct gmi-link (text url))
(struct gmi-toggle (alt-text))
(struct gmi-pre (text))
(struct gmi-heading (text level))
(struct gmi-list (text))
(struct gmi-quote (text))

;; Parsing 

(struct parser-state (preformat? lines))

(define (update-lines state func)
  (struct-copy parser-state state
    [lines (func (parser-state-lines line))]))


(define (toggle-preformat line s)
  (if (string-prefix? "```" line)
    (struct-copy parser-state s [preformat? (not parser-state-preformat? s)])
    s))

;; Make Lines

(define (trim str) (string-trim str #:repeat? #t))

(define make-text gmi-text)
(define make-pre gmi-pre)

(define (make-link line)
  (let* ([parts (-> line 
                    (substring 2) 
                    trim
                    (string-split #:repeat? #t #:trim #t))]
         [url (first parts)]
         [friendly (-> parts cdr string-join)])
    (gmi-link friendly url)))

(define (make-toggle line)
  (-> line (substring 3) trim (gmi-toggle)))

(define (make-quote line)
  (-> line (substring 2) trim (gmi-quote)))

(define (make-lst line)
  (-> line (substring 2) trim (gmi-list)))

(define (make-heading line)
  (let* [trimmed (string-trim line "#" #:left? #t #:right? #f #:repeat? #t)]
        [level (- (string-length line) (string-length trimmed))]
    (gmi-heading (trim trimmed) level)))
  
;; High Level Functions

(define (make-line line)
  (cond 
    [(string-prefix? "=>"  line) (make-line line)]
    [(string-prefix? "```" line) (make-toggle line)]
    [(string-prefix? "#"   line) (make-header line)]
    [(string-prefix? "* "  line) (make-lst line)]
    [(string-prefix? "> "  line) (make-quote line)]
    [else                        (make-text line)]))

(define (parse-line state line)
  (let* ([state      (toggle-preformat line)]
         [preformat? (parser-state-preformat? state)]
         [line (if preformat?
                 (make-pre line)
                 (make-line line))])
    (update-lines state (λ (lines) (cons line lines)))))
         
(define parse text) 

