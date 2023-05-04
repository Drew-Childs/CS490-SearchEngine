#lang racket
(require racket/port)
(require data/either)
(require data/monad)


; <--- Initial File Processing --->
; Opens input file, removes characters from file, converts string to uppercase, converts string to a list based on spaces
(define (process-file filename remove-words)
  (define file-contents (read-file filename))
  (define split-contents
    (regexp-split #px" +"
                  (remove-characters
                   (string-downcase
                    (apply string-append file-contents))
                   remove-words)))
  (list split-contents (first file-contents)))


; Opening file
(define (read-file filename)
  (port->lines
     (open-input-file filename
                       #:mode 'text) #:line-mode 'return))


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
        

; Function for calculating logbase10
(define (log10 n)
  (/ (log n) (log 10)))


; Helper function to generate hash of frequency of word
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
  (append
   '("\n" "?" "," "." "\"" "!" "-" ":" ";" "(" ")")
   (string-cushion (first (process-file "stop_words_english.txt" '("\n"))) '())))


; <--- Applying Processing to Input Files --->
; Saves all file names under the "Files/" folder as a list
(define input-file-names
  (map
   (lambda (file)
     (string-append "Files/" (path->string file)))
   (directory-list "Files/")))


; TODO - have this process all files and not just first 3
(display "Processing file contents - this may take some time...")
(define input-file-contents
  (map
   process-file
   (list(first input-file-names) (second input-file-names) (third input-file-names))
   (make-list 3 remove-from-file)))


(define frequencies
  (map
   generate-frequency
   input-file-contents))


(define (user-input message)
  (display "Search for...\n> ")
  (define input (regexp-split #px" +" (string-downcase (read-line (current-input-port) 'any))))
  (either display user-input (process-input input)))


(define (process-input input)
  (do
      [matched-words <- (match-words input)]
      [ranked-results <- (rank-results matched-words)]
      [ouptut-results <- (display-results ranked-results)]
    (success input)))


; for each file, send to find words and pass it the input and frequencies of that file
; return list of sum of frequencies and first lines in file (sum line)
(define (match-words input)
  (success
  (map
   find-words
   (make-list (length frequencies) input)
   frequencies
   (make-list (length frequencies) 0)
   (make-list (length frequencies) 0))))


; accept file and list of words as input
; find words in hash, add it to sum
; once reached end of input, return sum with first line of file
(define (find-words input file sum matches)
  (if (empty? input)
      (success (list sum matches (second file)))
      (if (hash-has-key? (first file) (first input))
          (find-words (rest input) file (+ sum (hash-ref (first file) (first input))) (+ matches 1))
          (find-words (rest input) file sum matches))))


; sort a list of sums (according to criteria)
; return a list of 
(define (rank-results input)
  (display input)
  (success "hello world"))


; given a list of strings, display them formatted
(define (display-results input)
  (success "beans world"))