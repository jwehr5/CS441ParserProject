#lang racket

(define (parse textfile)

  ;Convert each line into a string
  (define (file-to-lines)
    (file->lines textfile #:mode 'text #:line-mode 'linefeed))

  ;Converts our file into a list of tokens
  (define (file-to-tokens file)
    (if(empty? file)
       file
       (append (string-split (first file)) (file-to-tokens (rest file)))))

  ;Function for determinig if a string only consists of letters
    (define (is-alphabetic? charlst)
      (if (empty? charlst)
          #t
          (if (char-alphabetic? (first charlst))
              (is-alphabetic? (rest charlst))
              #f)))


  ;Scanner function which will scan each token to see if its a legal character
  (define (scan-tokens lst)

    ;This function identifies each individual token.
    (define (identify-token line linenumber)      
      (cond

        ;If the list is empty then all tokens in the line were scanned sucessfully. 
        [(empty? line) #t]
        [else
         (let ([current-token (first line)])
           ;(display " ")
           ;(display current-token)
           (cond
             ;Determine if the token is legal. If it doesn't match one of these, return false.
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
             [(equal? current-token "(") (identify-token (rest line) linenumber)]
             [(equal? current-token ")") (identify-token (rest line) linenumber)]
             [(is-alphabetic? (string->list current-token)) (identify-token (rest line) linenumber)]
             [else #f]))]));End of identify-token function
             
    
    #|
    Each line in the program is an item in the list.
    Within each line break each string up and make it its own list and pass it into the identify-token function.
    For Example: ("10 read 20") becomes ("10" "read" "20").
    Grab the line number by taking the first item in the list.
    |#
     (cond
       
       ;If the list is empty then all lines were scanned sucessfully. 
       [(empty? lst) #t]
       [else
        (let ([currentline (string-split (first lst))]
              [linenumber (first (string-split (first lst)))])
          (if (equal? (identify-token currentline linenumber) #t)
              (scan-tokens (rest lst))
              (error "Scan Error on line:" linenumber)))])); End of scan-tokens function
             

  ;This part of the program handles the parsing process
  (define (program? file)

    ;An etail may begin with a +, -, =, or nothing.
    (define (etail? file linenumber)
      (if(or (equal? (first file) "+") (equal? (first file) "-") (equal? (first file) "="))
         (expr? (rest file) linenumber)
         file))
 
    
    ;A number may begin with a +, - or with no sign.
    (define (num? file)
      (if (or (equal? (first file) "+") (equal? (first file) "-"))
          (rest file)
          file))

    

    (define (expr? file linenumber)
      (cond

        ;An expr may begin with an id. If it does begin with an id, check for an etail.
        [(id? (string->list(first file))) (etail? (rest file) linenumber)]
        
        ;An expr may begin with an number. Call the num? function first to see if it begins with a + or -. Then check if its a number. If it's a number, call etail.
        [(number? (string->number(first (num? file)))) (etail? (rest file) linenumber) ]

        ;If the expr begins with an opening parenthesis, check to make sure it ends with a closing parenthesis after handling the expr in between.
        [(and (equal? (first file) "(") (equal? (first (expr? (rest file) linenumber)) ")")) (rest (expr? (rest file) linenumber))]
        [else (error "Syntax error on line:" linenumber)]))
      
      

    #|
    A stmt may begin with an id, if, read, write, goto, gosub, or return.
    If it begins with any one of those keywords then proceed appropriately to the next nonterminal
    |#
    (define (stmt? file linenumber)
      (cond
        [(and (equal? (first file) "if") (equal? (first (expr? (rest file) linenumber)) "then"))   (stmt? (rest (expr? (rest file) linenumber)) linenumber)]
        [(and (id? (string->list(first file))) (equal? (second file) "="))   (expr? (rest (rest file)) linenumber)]
        [(and (equal? (first file) "read") (id? (string->list(second file))))  (rest (rest file))]
        [(equal? (first file) "write") (expr? (rest file) linenumber)]
        [(and (or (equal? (first file) "goto") (equal? (first file) "gosub")) (idx? (string->list(second file))))  (rest(rest file))]
        [(equal? (first file) "return") (rest file)]
      [else (error "Syntax error on line:" linenumber)]))
      
        
    ;Check to make sure an identifier only consits of alphabetic characters.
    (define (id? str)
      (is-alphabetic? str))

    #|
    In a line number, the first digit can't be 0. If it is then return false.
    The function takes a number that has been broken up into individual digits.
    For the digit 0, char->integer returns 48 so the integer representation of the first digit should not be 48.
    |#
    (define (idx? numlst)
         (if (not (= (char->integer (first numlst)) 48))
             #t
             #f))
      
    ;A linetail may begin with a : or nothing.
    (define (linetail? file linenumber)
      (if (equal? (first file) ":")
          (stmt? (rest file) linenumber)
          file))
    

    (define (line? file linenumber)
       (linetail? (stmt? file linenumber)  linenumber))
        

    (define (linelist? file)
      (cond
        ;Before we jump into the line nonterminal, lets check to see if the next token is a line number.
        [(and (number? (string->number (first file))) (idx? (string->list (first file))))  (linelist? (line? (rest file) (first file)))]

        ;If the next token is $$, then we should be at the end of the program.
        [(equal? (first file) "$$") file]
        [else (error "Syntax Error: Missing Line Number")]))
        

    ;When we reach the end of the parse, the only thing that should be left in our list of tokens is $$.
    (if(equal? (linelist? file) '("$$"))
       "Accept"
       (error "Syntax Error: Missing $$ or Misplaced $$"))


); End of program? funtion
    
  

  
  (if(equal? (scan-tokens (file-to-lines)) #t)
      (program? (file-to-tokens (file-to-lines)))
     (display "Scanning Not Passed"))


 ); End of Parse function
  
        





      
  

