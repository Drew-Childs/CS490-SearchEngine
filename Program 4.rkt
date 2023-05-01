#lang racket
(require racket/port)
(require pmap)


; <--- Initial File Processing --->
; Opens input file, removes characters from file, converts string to uppercase, converts string to a list based on spaces
(define (process-file filename remove-words)
  (display "here ")
  (regexp-split #px" +"
   (remove-characters
    (string-downcase
     (apply string-append
     (read-file filename)))
    remove-words)))


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
  (let
      ([file-hash (hash-create file (make-hash '()))])
    (hash-frequency file-hash
                    (hash-keys file-hash)
                    (hash-sum (hash-values file-hash) 0))))


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
   (string-cushion (process-file "stop_words_english.txt" '("\n")) '())))


; <--- Applying Processing to Input Files --->
; Saves all file names under the "Files/" folder as a list
(define input-file-names
  (map
   (lambda (file)
     (string-append "Files/" (path->string file)))
   (directory-list "Files/")))


; TODO - have this process all files and not just first 3
; Opening up each of the input files and removing appropriate characters/strings from them
(define input-file-contents
  (map
   process-file
   (append (list(first input-file-names)) (list(second input-file-names)) (list(third input-file-names)))
   (make-list 3 remove-from-file)))
  





;(define (read-input response)
;  (display "> ")
;  (define input (regexp-split #px" +" (read-line (current-input-port) 'any)))
;  (either display read-input 



(define frequencies
  (map
   generate-frequency
   input-file-contents))

(with-output-to-file "hash-table.txt"
  (lambda ()
    (write frequencies)))








; save hash to a file
; somehow associate hashes to their corresponding file
; check if hash file exists
;     -if not, generate new hash and save it
;     -if yes, continue
; ask user for input (implement monads)
; query hashes to find relevant results, and display them