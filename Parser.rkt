#lang racket

(define (parse2 textfile)

  ;Convert each line into a string
  (define (file-to-lines)
    (file->lines textfile #:mode 'text #:line-mode 'linefeed))


  ;Scanner function which will scan each token to see if its a legal character
  (define (scan-tokens lst)

    ;Function for determinig if a string only consists of letters
    (define (is-alphabetic? charlst)
      (if (empty? charlst)
          #t
          (if (char-alphabetic? (first charlst))
              (is-alphabetic? (rest charlst))
              #f)))

    (define (identify-token line linenumber)      
      (cond
        [(empty? line) #t]
        [else
         (let ([current-token (first line)])
           ;(display " ")
           ;(display current-token)
           (cond
             ;Determine if the token is a legal character
             [(number? (string->number current-token)) (identify-token (rest line) linenumber)]
             [(equal? current-token "if") (identify-token (rest line) linenumber)]
             [(equal? current-token "then") (identify-token (rest line) linenumber)]
             [(equal? current-token "read") (identify-token (rest line) linenumber)]
             [(equal? current-token "write") (identify-token (rest line) linenumber)]
             [(equal? current-token "goto") (identify-token (rest line) linenumber)]
             [(equal? current-token "gosub") (identify-token (rest line) linenumber)]
             [(equal? current-token "return") (identify-token (rest line) linenumber)]
             [(equal? current-token "+") (identify-token (rest line) linenumber)]
             [(equal? current-token "-") (identify-token (rest line) linenumber)]
             [(equal? current-token "=") (identify-token (rest line) linenumber)]
             [(equal? current-token ":") (identify-token (rest line) linenumber)]
             [(equal? current-token "$$") (identify-token (rest line) linenumber)]

             ;For parentheses, separate the parentheses from the following characters and append it to the list
             [(and (string-prefix? current-token "(") (> (string-length current-token) 1))
              (identify-token (list* (substring current-token 0 1) (substring current-token 1 (string-length current-token)) (rest line)) linenumber)]
             [(equal? current-token "(") (identify-token (rest line) linenumber)]

             [(and (string-suffix? current-token ")") (> (string-length current-token )1))
              (identify-token (list* (substring current-token 0 (sub1(string-length current-token))) (substring current-token (sub1(string-length current-token)) (string-length current-token)) (rest line)) linenumber)]
             [(equal? current-token ")") (identify-token (rest line) linenumber)]
             
             ;[(equal? current-token ")") (identify-token (rest line) linenumber)]
             ;[(equal? current-token " ") (identify-token (rest line) linenumber)]
             [(is-alphabetic? (string->list current-token)) (identify-token (rest line) linenumber)]
             [else #f]))]));End of identify-token function
             
    
    
     (cond
       [(empty? lst) #t]
       [else
        (let ([currentline (string-split (first lst))]
              [linenumber (first (string-split (first lst)))])
          (if (equal? (identify-token currentline linenumber) #t)
              (scan-tokens (rest lst))
              (printf "Scan Error on line: ~a" linenumber)))])); End of scan-tokens function
             
              
 
    ;(scan-tokens (file-to-lines))


  (if(equal? (scan-tokens (file-to-lines)) #t)
     (display "Scanning Passed")
     (display "Scanning Not Passed")))
  
        





      
  

