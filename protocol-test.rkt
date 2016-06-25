#lang racket/base

(require "protocol.rkt"
         "test-util.rkt"
         racket/function
         racket/port
         rackunit)

(define test-response (call-with-input-file "test-response.bin" port->bytes))
(define parsed-test-response (call-with-input-file "test-response.rktd" read))
(define-values (h b) (split-header-body test-response))
(check-true (bytes? h))
(check-true (bytes? b))
(check-values-equal? (extract-next-field #"") (#f #""))
(check-values-equal? (extract-next-field #"\x00abadfield\x00") (#f #"\x00abadfield\x00"))
(check-values-equal? (extract-next-field #"\x00\x00notafield") (#f #"\x00\x00notafield"))
(check-values-equal? (extract-next-field #"agoodfield\x00ignored") (#"agoodfield" #"ignored"))
(check-values-equal? (extract-next-key-value #"") (#f #f #""))
(check-values-equal? (extract-next-key-value #"\x00malformed") (#f #f #"\x00malformed"))
(check-values-equal? (extract-next-key-value #"okay\x00\x00malformed") (#"okay" #f #"\x00malformed"))
(check-values-equal? (extract-next-key-value #"a\x00b\x00") (#"a" #"b" #""))
(check-equal? (bytes->symbol #"not_a_meme") 'not_a_meme)
(check-equal? (bytes->symbol #"") '||)
(check-equal? (bytes->symbol #"si_test") 'test)
(check-equal? (bytes->symbol #"net_other") 'other)
(define after-server-info
  (bytes-append
   #"\0,\0^[Joe_Blob\0\0\0\0\1\22\0WoDaN\0\0\0\0\0027\0^eKrish\0\0\0\0\3s\0Norse.\0\0\0\0"
   #"\4C\0^1theonething\0\1\0\0\22Z\0^1.^;e^7x^;e\0\0^7a^;im^1b^;ot^0\0\0 \a\0\0\0\0\36f\t\0"
   #"\2\0\0\0[f\nBstrogg\0\n\0\0\0\6\0\0\0\1\"\\8Astrogg\0\0\0\0\0\3\0\0\0\2h\374IC\0\0\0\0\0"
   #"\0\0\0\0\3\0\0\0\0\0\0\0\0\0\0\0\0\0\4Q/\312Bgdf\0\a\0\0\0\6\0\0\0\22\25\365\301Bgdf\0\2"
   #"\0\0\0\4\0\0\0 "))
(check-values-equal? (extract-server-info b) ((hash-ref parsed-test-response 'server-info)
                                              after-server-info))
(check-values-equal? (extract-next-player #"\0,\0^[Joe_Blob\0\0\0\0")
                     ((hash-ref parsed-test-response 'first-player)
                      #""))
;                                                 osmask     timeleft /-gamestate
;                                                 |....\ranked/....| /  /-servertype
;                                                 |.....\  | /.....| | / /-interested-clients
(check-values-equal? (extract-fixed-server-info #"\a\0\0\0\0\36f\t\0\2\0\0")
                     ((hash-ref parsed-test-response 'fixed-server-info)
                      #""))
(check-values-equal? (extract-next-player-extra #"\0[f\nBstrogg\0\n\0\0\0\6\0\0\0")
                     ((hash-ref parsed-test-response 'first-player-extra)
                      #""))

(check-equal? (parse-response test-response)
              (hash-ref parsed-test-response 'entire))
