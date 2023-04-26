#lang racket
(require data/either)
(require data/monad)
(require racket/port)


; <--- Initial file processing --->
; Opens input file, removes characters from file, converts string to uppercase, converts string to a list based on spaces
(define (process-file filename)
  (regexp-split #px" +"
   (string-upcase
    (remove-characters
     (first
      (read-file filename))
     '("\n" "\\?" "," "\\." "\"" "!" "-" ":" ";")))))


; Opening file
(define (read-file filename)
  (port->lines
     (open-input-file filename
                       #:mode 'text) #:line-mode 'return))


; Parameterized list of characters (chars) to remove from file (file)
(define (remove-characters file chars)
  (if (empty? chars)
      file
      (remove-characters (regexp-replace* (regexp (first chars)) file " ") (rest chars))))






; <--- file preprocessing --->
; scan in words that can't use from file

; read in each file
; convert to list
; remove elements that can't use
; generate hash with the count of how many times each word appeared in that file

; produce relative frequency (similar to author program)

; save all hashes in a data structure for quick access


; <--- file checking --->
; for each word user enters
;   for each file
;     if word appears in the file, retrieve it's calculated score


; <--- ranking --->
; Give priority to number of words matched in a given file
; If same amount of matches, give next priority to hightest calculated score of words


; <--- user interface --->
; ask user for word or words to search for
; display results described in ranking
; show first line of the file for each match


; <--- general --->
; don't forget to use Monads