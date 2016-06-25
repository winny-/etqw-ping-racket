#lang racket/base

(require racket/contract/base
         racket/contract/region
         racket/list
         racket/string)

(provide (all-defined-out))

(define colors
  '((#\1 . red)
    (#\2 . green)
    (#\3 . yellow)
    (#\4 . blue)
    (#\5 . light-blue)
    (#\6 . pink)
    (#\7 . white)
    (#\8 . grey)
    (#\9 . black)
    (#\0 . white)
    (#\a . orange)
    (#\b . green-blue)
    (#\c . dark-purple)
    (#\d . dark-orange)
    (#\e . purple)
    (#\f . pastel-blue)
    (#\g . mint)
    (#\h . dark-green)
    (#\i . red)
    (#\j . dark-red)
    (#\k . brown)
    (#\l . sand)
    (#\m . dark-yellow)
    (#\n . yellow)
    (#\o . light-yellow)
    (#\p . black)
    (#\q . red)
    (#\r . green)
    (#\s . yellow)
    (#\t . blue)
    (#\u . light-blue)
    (#\v . pink)
    (#\w . white)
    (#\x . grey)
    (#\y . black)
    (#\z . light-grey)))

(define/contract (strip-colors s)
  (string? . -> . string?)
  (regexp-replace* #rx"\\^.?" s ""))

(define/contract (char->token c)
  (char? . -> . (or/c symbol? #f))
  (define t (assq c colors))
  (if t (cdr t) #f))

(define/contract (tokenize-colors s)
  (string? . -> . (listof (or/c string? symbol?)))
  (reverse
   (map
    (λ (el) (if (list? el)
                (list->string (reverse el))
                el))
    (let loop ([ls (string->list s)] [tokens '()] [in-esc #f])
      (cond
        [(empty? ls) tokens]
        [(eq? (car ls) #\^) (loop (cdr ls) tokens #t)]
        [in-esc (loop (cdr ls)
                      (let ([t (char->token (car ls))])
                        ; valid token? add it to tokens, or ignore it completely.
                        (if t
                            (cons t tokens)
                            tokens))
                      #f)]
        ; Just another char, not an escape.
        [else
         (loop (cdr ls)
               ; What to do with the char we just popped:
               (if (and (not (empty? tokens)) (list? (car tokens)))
                   ; add it to the current list of chars.
                   (cons
                    (cons (car ls) (car tokens))
                    (cdr tokens))
                   ; or create a new one.
                   (cons (list (car ls))
                         tokens))
               #f)])))))

(define/contract (vt100-colors tokens)
  ((listof (or/c string? symbol?)) . -> . string?)
  (void))

(module+ test
  (require rackunit)
  (check-equal? (strip-colors "^6this ^/is ^1a ^3tes^^t.^") "this is a test.")
  (check-equal? (char->token #\6) 'pink)
  (check-false (char->token #\^))
  (check-equal? (tokenize-colors "^6this^/ is a test^z!^")
                '(pink "this is a test" light-grey "!")))
