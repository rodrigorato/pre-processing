#lang racket
(provide add-active-token def-active-token process-string)

; +------------------------------+
; | PRE-PROCESSOR IMPLEMENTATION |
; +------------------------------+

; Defines a hash map, mapping active tokens to functions
; An active token's function is what must be executed when it is found
(define active-tokens (make-hash))

; TODO: Not yet implemented
(define (def-active-token t)
  t)
; Adds a new mapping from token-name to token-function
; WARNING: if token-name is already saved, IT WILL BE OVERWRITTEN!
(define (add-active-token token-name token-function)
  (hash-set! active-tokens token-name token-function))

; TODO: Not yet implemented
(define (process-string s)
  s)

(displayln 
  (process-string
#<<END
alias display = System.out.print;
public class App {
    public void f() {
        display("f");
    }
}
END
  ))
