#!/usr/bin/env racket
#lang racket/base

(require "color.rkt"
         "protocol.rkt"
         "util.rkt"
         racket/contract/base
         racket/contract/region
         racket/cmdline
         racket/function
         racket/list)

(provide (all-defined-out))

(define/contract (cli-ping server)
  ((cons/c string? number?) . -> . void?)
  (define res (etqw-ping (car server) (cdr server)))
  (displayln
   (if res
       (format "~a ~ah,~ab/~a (~a:~a)"
               (strip-colors (hash-ref res 'name "(no name)"))
               (count (λ (p) (not (hash-ref (cdr p) 'bot))) (hash->list (hash-ref res 'players)))
               (count (λ (p) (hash-ref (cdr p) 'bot)) (hash->list (hash-ref res 'players)))
               (hash-ref res 'maxplayers "??")
               (car server)
               (cdr server))
       (format "(~a:~a is offline)" (car server) (cdr server))))
  (void))

(define (main)
  (command-line
   #:args rst
   (for-each cli-ping (if (empty? rst)
                          (find-favorites)
                          (map (curryr string->host-port 27733) rst)))))

(module+ main
  (main))
