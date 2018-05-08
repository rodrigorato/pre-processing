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
        )
      )
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

; Finds type name of a given string which includes a type initialization using the "new" keyword
(define (type-of-new str)
  (let* ((good-str (string-replace str  #px"[\n\r\t]" ""))
        (new-keyword-pos (regexp-match-positions #rx"[\\w]*[ ]*=[ ]*new[ ]*" good-str))
         (type-str #f)
         )
    (cond
      [(not (equal? 0 (caar (regexp-match-positions #rx"[ \n\r\t]" good-str)))) (set! type-str #f)]
      [new-keyword-pos
      (let ((first-parenthesis-pos (regexp-match-positions #rx"new[\\s]*(.*?)[(]" good-str)))
        (when first-parenthesis-pos
          (set! type-str (substring good-str (cdar new-keyword-pos) (- (cdar first-parenthesis-pos) 1)))
        ))]
          [else (set! type-str #f)])
    type-str))

; Implements local type inference using type-of-new to find the type of a given initialization
(def-active-token "var" (str)
  (let ((type (type-of-new str))
        (final str))
    (if type
      (set! final (string-append type str))
      (set! final (string-append "var" str)))
    final))


; String Interpolation 

; rebuilds string by extracting the interpolated expressions,
; building a new string around them and then just adding the remainder of the string
(define (interpolate-string str)
  (let* ((scope-string (regexp-match #rx".*[\"];?" str))
         (s (car scope-string))
         (last-point 0)
         (new-s "")
         (interpol-pos (regexp-match-positions* #rx"#{(.*?)}" s))) ; get location of all interpolations
    (cond [(null? interpol-pos) (set! new-s str)] ; interpolation required but there is nothing to do
    [else (for ([p interpol-pos])
      ; rebuild string the right way
      (set! new-s
            (string-append
             new-s
             (substring s last-point (car p))
             "\" + ("
             (substring s (+ (car p) 2) (- (cdr p) 1))
             ") + \"" ))
      (set! last-point (cdr p)) ; store where we stoped the last time
    )
     ; no other occurences found so just add the rest of the string
    (set! new-s
          (string-append new-s
          (substring str (cdr (last interpol-pos)))))])
    new-s))


; interpolates string by using our own predefined function
(def-active-token "#" (str)
  ; check if we are at the start of a string
  (if (equal? 0 (car (first(regexp-match-positions "\"" str))))
    (interpolate-string str)
    (string-append "#" str))
  )

; Gets the value that must be associated with this alias name
(define (alias-value str) 
  (let ((definition-equals-pos (regexp-match-positions #rx"=[\\s]*" str))
        (value #f))
    (let ((semi-colon-pos (regexp-match-positions #rx"=[\\s]*(.*?)[;]" str)))
      (when semi-colon-pos
        (set! value (substring str (cdar definition-equals-pos) (- (cdar semi-colon-pos) 1)))
        ))
    (string-trim value)))

; Gets the alias name from a particular string containing the alias active token
(define (alias-name str)
  (string-trim (car (regexp-split #rx"=" str))))

; Given an alias name and its value, replace it along a given string and return it
; THIS IS WRONG TODO FIXME 
(define (replace-aliases str alias-name alias-value)
  (let* ((str-regex (string-append "[^_a-zA-Z0-9]" alias-name))
        (locations (regexp-match-positions* (regexp str-regex) str)); find locations of alias uses
        (last-pos 0)
        (new-s "")
        )
    (cond [(null? locations) (set! new-s str)]
    [else (for ([l locations])
      ; take string up until the place where alias is used and replace by true type
      (cond [(regexp-match? #px"[_a-zA-Z0-9]" (substring str (cdr l) (+ (cdr l) 1)))
             (set! new-s
            (string-append
             new-s
             (substring str last-pos (cdr l))
             ))
             ]
      [else (set! new-s
            (string-append
             new-s
             (substring str last-pos (+ (car l) 1))
             alias-value))])
      (set! last-pos (cdr l))
      )
    (set! new-s
          (string-append new-s
          (substring str (cdr (last locations)))))])
    new-s))

; Returns a string stripped from its first line
(define (string-after-semi-colon str) 
  (match (regexp-match-positions ";" str)
    ((list (cons start end)) (substring str end))
    (else "")))

; Type Aliases
; TODO - Implement
(def-active-token "alias" (str)
  (let* ((name (alias-name str))
        (value (alias-value str))
        (str-final (string-after-semi-colon str)))
    (set! str-final (replace-aliases str-final name value))
    str-final))
