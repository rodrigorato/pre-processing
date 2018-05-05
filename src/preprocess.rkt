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

; Iterates over the known tokens and applies them to s until no change is made
(define (process-string s)
  (let ((token-names (hash-keys active-tokens))
        (previous-s s))

    (for ([token token-names])
      (set! s (apply-token-proc token s)))

    (when (is-changed? s previous-s)
      (set! s (process-string s)))
    )
  s)

; Returns #t if the strings are different, #f otherwise
(define (is-changed? this that)
  (not (string=? this that)))

; Checks if a given token is to be applied over a string and returns the string accordingly
(define (apply-token-proc token str)
  (let ((token-positions (has-token? str token)))
    (when token-positions
       (for ([pos token-positions])
        (set! str 
          (string-append (substring str 0 (car pos)) 
                         ((hash-ref active-tokens token) (substring str (cdr pos)))))
        ))
    str))


; Checks if a given token is present in a given string
; If it is, returns a list of lists, where the inner ones are like (str-initial-pos str-final-pos)
; If it is not, it returns #f
(define (has-token? str token)
  (let ((positions (regexp-match-positions (regexp token) str)))
    (if (not (null? positions))
        positions
        #f)))

; +--------------------------+
; | Active Token Definitions |
; +--------------------------+

; Local Type Inference
; TODO - Implement
(def-active-token "var" (str)
  str)

; String Interpolation 
; TODO - Implement
(def-active-token "#" (str)
  str)

; Type Aliases
; TODO - Implement
(def-active-token "alias" (str)
  str)

