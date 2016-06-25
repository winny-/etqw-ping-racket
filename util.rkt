#lang racket/base

(require racket/contract/base
         racket/contract/region
         racket/function
         racket/list
         racket/string)

(provide (all-defined-out))

(define/contract (hash-partition ht proc)
  (hash? (any/c any/c . -> . any/c) . -> . (values hash? hash?))
  (for/fold ([yes (hash)]
             [no (hash)])
            ([(k v) (in-hash ht)])
    (if (proc k v)
        (values (hash-set yes k v) no)
        (values yes (hash-set no k v)))))

(define/contract (etqw-game-dir)
  (-> path?)
  (unless (eq? (system-type 'os) 'unix)
    (error "This procedure only supports unix."))
  (build-path (find-system-path 'home-dir)
              ".etqwcl"))

(define/contract (etqw-player-dirs)
  (-> (listof path?))
  (with-handlers ([exn:fail:filesystem? (λ (e) '())])
    (filter directory-exists?
            (directory-list (build-path (etqw-game-dir)
                                        "sdnet")
                            #:build? #t))))

(define/contract (bytes/string->string b-or-s)
  ((or/c bytes? string?) . -> . string?)
  ((if (string? b-or-s) identity bytes->string/utf-8) b-or-s))

(define/contract (grep-favorite x)
  ((or/c input-port? string? bytes?) . -> . (listof (cons/c string? number?)))
  (map (λ (m) (cons (bytes/string->string (car m))
                    (string->number (bytes/string->string (cadr m)))))
       (regexp-match* #rx"favorite_([^:]+):([0-9]+)"
                      x
                      ; Omit the full match
                      #:match-select cdr)))

(define/contract (find-favorites [player-dir #f])
  ([] [(or/c #f path?)] . ->* . (listof (cons/c string? number?)))
  (apply append
         (map (λ (p) (call-with-input-file
                       (build-path p "base.dict")
                       grep-favorite))
              (if player-dir
                  (list player-dir)
                  (etqw-player-dirs)))))

(define/contract (string-count s ss)
  (string? string? . -> . exact-nonnegative-integer?)
  (length (regexp-match* (regexp-quote ss) s)))

(define/contract (string->host-port s default-port)
  (string? number? . -> . (or/c (cons/c string? number?) #f))
  (if (or (string-prefix? s "[") (= (string-count s ":") 1))
      (let ([m (regexp-match #rx"\\[?([0-9.:]+)]?:([0-9]+)" s)])
        (cons (cadr m) (string->number (caddr m))))
      (cons s default-port)))

(module+ test
  (require "test-util.rkt"
           rackunit)
  (check-values-equal? (hash-partition #hash((a . 1)
                                             (b . 2))
                                       (λ (k v) (eq? k 'a)))
                       (#hash((a . 1))
                        #hash((b . 2))))
  (check-values-equal? (hash-partition #hash((a . 1)
                                            (b . #f))
                                      (λ (k v) v))
                       (#hash((a . 1))
                        #hash((b . #f))))
  (check-values-equal? (hash-partition #hash() (λ (k v) #t))
                       (#hash()
                        #hash()))
  (check-equal? (bytes/string->string "hello") "hello")
  (check-equal? (bytes/string->string #"test") "test")
  (check-equal? (grep-favorite "favorite_1.2.3.4:27733") '(("1.2.3.4" . 27733)))
  (check-equal? (grep-favorite #"favorite_1.1.1.1:2222") '(("1.1.1.1" . 2222)))
  (check-equal? (grep-favorite (open-input-string "favorite_4.2.3.1:1234"))
                '(("4.2.3.1" . 1234)))
  (check-equal? (grep-favorite "badmatch") '())
  (check-equal? (string-count "ababa" "a") 3)
  (check-equal? (string-count "AabA" "c") 0)
  (check-equal? (string->host-port "1.1.1.1" 1234) '("1.1.1.1" . 1234))
  (check-equal? (string->host-port "1.2.3.4:5555" 4444) '("1.2.3.4" . 5555))
  (check-equal? (string->host-port "::1" 1111) '("::1" . 1111))
  (check-equal? (string->host-port "[::1]:2222" 3333) '("::1" . 2222)))

