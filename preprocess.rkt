#lang racket
(provide add-active-token def-active-token process-string)

; TODO: Not yet implemented
(define (add-active-token t) 
  t)

; TODO: Not yet implemented
(define (def-active-token t)
  t)

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
