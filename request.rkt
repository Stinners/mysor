#lang racket

(require racket/tcp
         racket/port
         openssl
         net/url-string)

(define (prepare-url url)
  (let ([url (string->url url)])
    url))

(struct exn:invalid-url exn:fail (problem))

;; For now assume that we're passing in a valid URL string
(define (make-request url-string)
  (let*-values ([(url) (string->url url-string)]
                [(c-in c-out) (ssl-connect (url-host url) 1965)])
    (display url-string c-out)
    (display "\r\n" c-out)
    (let ([response (port->string c-in)])
      (close-output-port c-out)
      (close-input-port c-in)
      response)))

(display (make-request "gemini://gemini.circumlunar.space/"))
