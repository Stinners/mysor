#lang racket

(provide ~> ~>>)

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
