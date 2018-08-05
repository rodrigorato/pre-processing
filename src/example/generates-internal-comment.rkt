#lang racket
(require "../preprocess.rkt")

; Example string to test with internal comments active token ";;""
(define ex-internal-comment
#<<END
//Another great idea from our beloved client
;;This is stupid but it’s what the client wants
for(int i = 0; i < MAX_SIZE; i++) {
END
)

; Defines the ";;" active token to be used for internal comments
(def-active-token ";;" (str)
  (match (regexp-match-positions "\n" str)
    ((list (cons start end)) (substring str end))
    (else "")))

(def-active-token "//" (str)
  (string-append ";;" str))

; Process the string and display it to the user
(displayln (process-string ex-internal-comment))

