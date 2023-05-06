#lang racket
(require racket/port)
(require data/either)
(require data/monad)


; <--- Initial File Processing --->
; Opens input file, removes characters from file, converts string to uppercase, converts string to a list based on spaces
(define (process-file filename remove-words)
  (do
      [file-contents <- (read-file filename)]
      [first-line <- (get-first-line file-contents)]
      [split-contents <- (parse-contents file-contents remove-words)]
    (list split-contents first-line)))


; Opening file
(define (read-file filename)
  (if (file-exists? filename)
      (success
       (port->lines
        (open-input-file filename
                        #:mode 'text) #:line-mode 'return))
      (failure (string-append filename " does not exist!\n"))))


(define (get-first-line contents)
  (if (empty? contents)
      (failure "File has no content!\n")
      (success (first (regexp-split "\n" (first contents))))))


(define (parse-contents file-contents remove-words)
  (if (empty? remove-words)
      (failure "Pass in valid list of words/characters to remove\n")
      (success
       (regexp-split #px" +"
                     (remove-characters
                      (string-downcase
                       (first file-contents))
                      remove-words)))))


; Parameterized list of characters (chars) to remove from file (file)
(define (remove-characters file chars)
  (if (empty? chars)
      file
      (remove-characters (string-replace file (first chars) " ") (rest chars))))


; <--- Hash Creation/Processing Operations --->
; Creates hashmap of: (Key = word) (Value = count of word)
(define (hash-create input output)
  (if (empty? input)
      output
      (begin
        (if (hash-has-key? output (first input))
          (hash-set! output (first input) (+ (hash-ref output (first input)) 1))
          (hash-set! output (first input) 1))
        (hash-create (rest input) output))))


; Given a list of values from hashmap, sums all values and returns it
(define (hash-sum input total)
  (if (empty? input)
      total
      (hash-sum (rest input) (+ total (first input)))))


; Given a hashmap, list of keys, and the sum, creates frequency distribution for how often word is used
(define (hash-frequency all-hash keys sum)
  (if (empty? keys)
      all-hash
      (begin
        (hash-set! all-hash (first keys) (* (log10 (/ (hash-ref all-hash (first keys)) sum)) -1))
        (hash-frequency all-hash (rest keys) sum))))
        

(define (log10 n)
  (/ (log n) (log 10)))


; Generate hash of frequency of word
(define (generate-frequency file)
  (define file-hash (hash-create (first file) (make-hash '())))
  (define file-frequencies (hash-frequency file-hash (hash-keys file-hash) (hash-sum (hash-values file-hash) 0)))
  (list file-frequencies (second file)))


; <--- String/Character Removal Processing --->
; Adds spaces before/after a word so it won't remove substrings of words from a file (ex. remove "or" from "word")
(define (string-cushion original new-lst)
  (if (empty? original)
      new-lst
      (string-cushion (rest original) (append new-lst (list (string-append " " (first original) " "))))))


; Removes list of special characters and words to remove from input files
(define remove-from-file
  (if (failure? (process-file "stop_words_english.txt" '("\n")))
      (begin
        (display (from-failure #t (process-file "stop_words_english.txt" '("\n"))))
        '("\n" "?" "," "." "\"" "!" "-" ":" ";" "(" ")"))
      (append
       '("\n" "?" "," "." "\"" "!" "-" ":" ";" "(" ")")
       (string-cushion (first (process-file "stop_words_english.txt" '("\n"))) '()))))


; <--- Applying Processing to Input Files --->
; Saves all file names under the "Files/" folder as a list
(define input-file-names
  (map
   (lambda (file)
     (string-append "Files/" (path->string file)))
   (directory-list "Files/")))


(display "Processing file contents - this may take some time...\n")
(define input-file-contents
  (map
   process-file
   input-file-names
   (make-list (length input-file-names) remove-from-file)))


; Removes and displays failures in file processing accordingly
(define (remove-failures existing-list new-list)
  (if (empty? existing-list)
      new-list
      (if (failure? (first existing-list))
          (begin
            (display (from-failure #t (first existing-list)))
            (remove-failures (rest existing-list) new-list))
          (remove-failures (rest existing-list) (append new-list (list (first existing-list)))))))

(define formatted-input-file-contents
  (remove-failures input-file-contents '()))


(define frequencies
  (map
   generate-frequency
   formatted-input-file-contents))


; <--- User input --->
(define (user-input)
  (display "Search for...\n> ")
  (define input (regexp-split #px" +" (string-downcase (read-line (current-input-port) 'any))))
  (process-input input)
  (user-input))


(define (process-input input)
  (define matched-words (match-words input))
  (define ranked-results (rank-results matched-words))
  (display-results ranked-results))


; <--- Ranking of results --->
; Matches input words to input files
(define (match-words input)
  (map
   find-words
   (make-list (length frequencies) input)
   frequencies
   (make-list (length frequencies) 0)
   (make-list (length frequencies) 0)))


; Finds matching words in each file and sums up frequencies and tallies # of matches
(define (find-words input file sum matches)
  (if (empty? input)
      (list matches sum (second file))
      (if (hash-has-key? (first file) (first input))
          (find-words (rest input) file (+ sum (hash-ref (first file) (first input))) (+ matches 1))
          (find-words (rest input) file sum matches))))


; ranks results in descending order by matches, and ascending order by frequency
(define (rank-results input)
  (sort input
        (lambda (first-lst second-lst)
          (cond ((> (first first-lst) (first second-lst)) #t)
                ((< (first first-lst) (first second-lst)) #f)
                (else (< (second first-lst) (second second-lst)))))))


(define (display-results input)
  (for-each (lambda (result)
              (when (> (first result) 0)
                (display (string-append "--" (third result) "\n"))))
            input))


(define start-program (user-input))