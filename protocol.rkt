#lang racket/base

(require racket/bool
         racket/contract/base
         racket/contract/region
         racket/function
         racket/hash
         racket/list
         racket/match
         racket/port
         racket/string
         racket/tcp
         racket/udp)

(require "util.rkt")

(provide (all-defined-out))

(define/contract (etqw-ping host [port 27733] [timeout 5.0])
  ([string?] [port-number? (or/c #f (and/c real? (not/c negative?)))] . ->* . (or/c hash? #f))
  (define skt (udp-open-socket))
  (define res (make-bytes 65507))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (udp-connect! skt host port)
  (udp-send skt #"\xff\xffgetInfoEx\x00")
  (define v (sync/timeout timeout
                          (udp-receive!-evt skt res)))
  (udp-close skt)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define actual (if v
                     (subbytes res 0 (sub1 (car v)))
                     #f))
  (if actual
      (hash-set (parse-response actual) 'raw actual)
      #f))

(define/contract (parse-response res)
  (bytes? . -> . (or/c hash? #f))
  ; separate header/challenge from body
  (define-values (header body) (split-header-body res))
  (when (not header)
    (error 'bad-header "~v" res))
  (define-values (server-info after-server-info) (extract-server-info body))
  (define-values (players after-players) (extract-players after-server-info))
  (define-values (fixed after-fixed) (extract-fixed-server-info after-players))
  (define-values (extra after-extra) (extract-player-extras after-fixed))
  ; merge all the data together...
  (define (combiner k v1 v2)
    (hash-union (hash-remove v1 'id)
                v2))
  (hash-set (hash-union fixed server-info)
            'players (hash-union players extra #:combine/key combiner)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Layout of header:
; 0xffff infoExResponse 0x00 0xff*8 byte1
; 0x00 byte2 0x00 byte3 byte4 0x00 0x00
; (rest is body ?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (split-header-body bstr)
  (bytes? . -> . (values (or/c bytes? #f) (or/c bytes? #f)))
  (define m (regexp-match #px#"(\xff\xffinfoExResponse\x00\xff{8}.\x00.\x00..\x00\x00)(.*)" bstr))
  (if m
      (values (cadr m) (caddr m))
      (values #f #f)))

(define/contract (extract-next-key-value bstr)
  (bytes? . -> . (values (or/c bytes? #f) (or/c bytes? #f) bytes?))
  (define-values (k krest) (extract-next-field bstr))
  (define-values (v vrest) (extract-next-field krest))
  (values k v vrest))

(define/contract (extract-next-field bstr #:allow-empty [allow-empty #f])
  ([bytes?] [#:allow-empty boolean?] . ->* . (values (or/c bytes? #f) bytes?))
  (define ls (bytes->list bstr))
  (if (or (< (length ls) 2) (zero? (car ls)))
      (if allow-empty
          (values #f (list->bytes (cdr ls)))
          (values #f bstr))
      (let-values ([(l r) (splitf-at ls (negate zero?))])
        (values (list->bytes l) (if (empty? r)
                                    #""
                                    (list->bytes (cdr r)))))))

(define/contract (bytes->symbol bstr)
  (bytes? . -> . symbol?)
  (string->symbol
   (regexp-replace #rx"^(net|si)_"
                   (string-downcase (bytes->string/utf-8 bstr))
                   "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Layout of the server info field:
; key-value pairs separated by byte 0x0. When two
; adjacent 0x0 bytes are detected, the there is
; nothing further to extract.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (extract-server-info bstr)
  (bytes? . -> . (values hash? bytes?))
  (define (motd-symbol? sym)
    (string-prefix? (symbol->string sym) "motd_"))
  (define-values (info brest)
   (let loop ([bstr bstr] [acc (hash)])
     (define-values (k v remaining) (extract-next-key-value bstr))
     (cond
       ; we're done. period.
       [(not k) (values acc (subbytes bstr 2))]
       ; kv without a v? o_o probably an empty field. stupid.
       [(not v) (loop (subbytes remaining 1) acc)]
       [else
        (loop
         remaining
         (match (bytes->symbol k)
           ; known strings
           [(and sym (or 'adminname 'campaign 'email 'gamename 'irc
                         'map 'name 'rules 'version 'website 'bba_modver
                         (? motd-symbol?)))
            (hash-set acc sym (bytes->string/utf-8 v))]
           ; known booleans
           [(and sym (or 'adminstart 'allowlatejoin 'antilag 'antilagforgiving
                         'antilagonly 'bot_enable 'disableglobalchat
                         'disablevoting 'gamereviewreadywait 'needpass
                         'noproficiency 'privateclients 'pure
                         'serverpunkbusterenabled 'serverdedicated
                         'spectators 'teamdamage 'teamforcebalance))
            (hash-set acc sym (not (zero? (string->number (bytes->string/utf-8 v)))))]
           ; known numerics (integer or float)
           [(and sym (or 'maxplayers 'minplayers 'readypercent 'timelimit))
            (hash-set acc sym (string->number (bytes->string/utf-8 v)))]
           ; default to byte string
           [rest-k-sym (hash-set acc rest-k-sym v)]))])))
  (let-values ([(motd-ht rest-ht) (hash-partition info (λ (k v) (motd-symbol? k)))])
    (values
     (hash-set rest-ht 'motd (map cdr (sort (hash->list motd-ht)
                                            (λ (a b) (symbol<? (car a) (car b))))))
     brest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Layout of the fixed server info field:
; osmask - LE uint
; ranked - byte, 0x0 false, else is true
; timeleft - LE uint, number of ms left in match
; gamestate - byte
; servertype - byte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (extract-fixed-server-info bstr)
  (bytes? . -> . (values hash? bytes?))
  (define osmask (integer-bytes->integer bstr #f #f 0 4))
  (define ranked (not (zero? (bytes-ref bstr 4))))
  (define timeleft (integer-bytes->integer bstr #f #f 5 9))
  (define gamestate (bytes-ref bstr 9))
  (define servertype (if (zero? (bytes-ref bstr 10))
                         'regular
                         'tv))
  (values (hash-union
           (if (eq? servertype 'regular)
               (hash 'interested-clients (bytes-ref bstr 11))
               (hash 'connected-clients (integer-bytes->integer bstr #f #f 11 15)
                     'max-clients (integer-bytes->integer bstr #f #f 15 19)))
           (hash 'osmask osmask
                 'ranked ranked
                 'timeleft timeleft
                 'gamestate (find-gamestate gamestate)
                 'servertype servertype))
          (subbytes bstr (if (eq? servertype 'regular)
                             12
                             20))))

(define/contract (find-gamestate gs)
  (exact-nonnegative-integer? . -> . (or/c #f symbol?))
  (define s (findf (λ (pair) (= (cdr pair) gs)) gamestates))
  (implies s (car s)))

(define gamestates '((warmup . #b00000001)
                     (in-progress . #b00000010)
                     (reviewing . #b00000100)
                     (second-round . #b00001000))) ; stopwatch

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Layout of the a player field:
; id - byte, if it's 0x20 then no more players.
; ping - littleendian short, latency of player in ms.
; name - 0x0 terminated string
; clantag_pos - byte, not sure what this is for.
; bot - byte, 0x0 false, else is true.
;
; In etqwlib.py, ping is one byte, and the second
; byte is called rate. I don't think this is
; accurate, since it is common for clients to
; have latencies greater than 254.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (extract-next-player bstr)
  (bytes? . -> . (values hash? bytes?))
  (define id (bytes-ref bstr 0))
  (define ping (integer-bytes->integer bstr #f #f 1 3))
  (define-values (name namerest) (extract-next-field (subbytes bstr 3)))
  (define clantag_pos (bytes-ref namerest 0))
  (define-values (clantag clantagrest) (extract-next-field (subbytes namerest 1) #:allow-empty #t))
  (define bot (not (zero? (bytes-ref clantagrest 0))))
  (values
   (hash 'id id
         'ping ping
         'name (bytes->string/utf-8 name)
         'clantag_pos clantag_pos
         'clantag (if clantag (bytes->string/utf-8 clantag) #f)
         'bot bot)
   (subbytes clantagrest 1)))

(define/contract (extract-players bstr)
  (bytes? . -> . (values hash? bytes?))
  (let loop ([bstr bstr] [acc (hash)])
    (cond
      [(< (bytes-length bstr) 9) (values acc bstr)]
      [(= (bytes-ref bstr 0) #x20) (values acc (subbytes bstr 1))]
      [else
       (define-values (p prest) (extract-next-player bstr))
       (loop prest (hash-set acc (hash-ref p 'id) p))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Format of the extra player data:
; id - byte, matches id found in the player section.
; xp - 4 byte LE IEE floating point number.
; team - string of the team name
; kills - 2 byte LE uint, number of kills
; deaths - 2 byte LE uint, number of deaths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (extract-player-extras bstr)
  (bytes? . -> . (values hash? bytes?))
  (let loop ([bstr bstr] [acc (hash)])
    (cond
      [(< (bytes-length bstr) 8) (values acc bstr)]
      [(= (bytes-ref bstr 0) #x20) (values acc (subbytes bstr 1))]
      [else
       (define-values (pe perest) (extract-next-player-extra bstr))
       (loop perest (hash-set acc (hash-ref pe 'id) pe))])))

(define/contract (extract-next-player-extra bstr)
  (bytes? . -> . (values hash? bytes?))
  (define id (bytes-ref bstr 0))
  (define xp (floating-point-bytes->real bstr #f 1 5))
  (define-values (team trest) (extract-next-field (subbytes bstr 5) #:allow-empty #t))
  (define kills (integer-bytes->integer trest #f #f 0 4))
  (define deaths (integer-bytes->integer trest #f #f 4 8))
  (values
   (hash 'id id
         'xp xp
         'team (if team (bytes->string/utf-8 team) #f)
         'kills kills
         'deaths deaths)
   (subbytes trest 8)))
