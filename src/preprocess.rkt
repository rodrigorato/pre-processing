#lang racket
(provide add-active-token def-active-token process-string)

; +------------------------------+
; | PRE-PROCESSOR IMPLEMENTATION |
; +------------------------------+

; Defines a hash map, mapping active tokens to functions
; An active token's function is what must be executed when it is found
(define active-tokens (make-hash))

; Gets an active token's function by its name
(define (get-active-token-function token-name)
  (hash-ref active-tokens token-name))

; Adds a new mapping from token-name to token-function
; WARNING: if token-name is already saved, IT WILL BE OVERWRITTEN!
(define (add-active-token token-name token-function)
  (hash-set! active-tokens token-name token-function))

; Defines a new syntax rule for def-active-token, parsing it and calling add-active-token
; Creates a new lambda from the given code, and adds that in add-active-token
(define-syntax-rule (def-active-token token-name token-func-args token-func-code)
  (add-active-token token-name (lambda token-func-args token-func-code)))

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
