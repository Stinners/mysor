#lang racket

(require racket/tcp
         racket/port
         openssl
         net/url-string)

(define (prepare-url url)
  (let ([url (string->url url)])
    url))

;; Assume that we're passing in a valid URL struct with 
(define (make-request url)
  (define-values (c-in c-out) (ssl-connect "gemini.circumlunar.space" 1965))
  (display url c-out)
  (flush-output c-out)
  (define response (port->string c-in))
  (close-output-port c-out)
  (close-input-port c-in)
  response)


(prepare-url "gemini://gemini.circumlunar.space/\r\n")
