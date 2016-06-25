#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (require etqw-ping) entry point. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "cli.rkt"
         "protocol.rkt"
         racket/contract)

(provide etqw-ping
         (rename-out [parse-response etqw-parse-response]))

(module+ main
  (main))
