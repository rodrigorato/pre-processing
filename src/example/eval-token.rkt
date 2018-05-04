#lang racket
(require "../preprocess.rkt")

; +--------------------+
; | EVAL TOKEN EXAMPLE |
; +--------------------+

; Example string to test with internal comments active token ";;""
(define ex-eval-token
#<<END
if (curYear > //eval (date-year (seconds->date (current-seconds)))) {
END
)

(define ns (make-base-namespace))

(def-active-token "//eval " (str)
  (call-with-input-string
    str
    (lambda (in)
      (string-append (~a (eval (read in) ns))
                     (port->string in)))))

; Process the string and display it to the user
(displayln (process-string ex-eval-token))

