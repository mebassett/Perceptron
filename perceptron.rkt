#lang racket
; Matthew Eric Bassett
; http://mebassett.gegn.net
; mebassett@gegn.net
; London, UK
; 4 Feb 2012
;
; Attempt at implementing a perceptron algorithm as per:
; Tommi Jaakkola, course materials for 6.867 Machine Learning, Fall 2006. 
; MIT OpenCourseWare (http://ocw.mit.edu/), Massachusetts Institute of Technology. 
; Downloaded on [3 Feb 2012].
;
; The training data is a bunch of 25x25 black/white images.  The perceptron is to
; identify which images are of a letter A and which ones are not.  I created the 
; data in GIMP, saved as an HTML table, and used sed to convert it into a list of
; pixel colors.  I would right a blog about it, but its now nearly 3am.  It's snowing
; outside and I'd like to go play in the snow, but I think sleep is more important 
; right now.

;initialize our perceptron.
(define sigma (build-vector 625 (λ (x) 1)))

;some maths functions
(define (sign x)
  (if (>= x 0)
      1
      -1))

(define (dot v1 v2)
  (apply + (vector->list (vector-map * v1 v2))))

(define (scalar-mult s v)
  (vector-map * 
              v 
              (build-vector (vector-length v) (λ (x) s))))

(define (vector-add v1 v2)
  (vector-map +
              v1
              v2))

(define (hexstr->number str)
  (string->number (string-append "#x" str)))

;reading the image data files
(define (img->vector filename)
  (define filelist (with-input-from-file (string->path filename)
                     (λ ()
                       (define (iter file-list line)
                         (if (eof-object? line)
                             file-list
                             (iter (append file-list (list line)) (read-line))))
                       (iter '() (read-line)))))
  (list->vector (map hexstr->number filelist)))

(define (filename->label name)
  (if (regexp-match? #rx"[0-9]+\\.\\1\\.img$" name)
      1
      -1))

;our perceptron stuff
(define (perceptron-label img)
  (sign (dot sigma img)))

(define (train-perceptron img-name)
  (define label (filename->label img-name))
  (define vec (img->vector img-name))
  (cond ((= (perceptron-label vec) label)
         (display "good!\n")) 
        (else 
         (set! sigma (vector-add sigma (scalar-mult label vec)))
         (display "bad! updated sigma, though.\n"))))

;how to train your perceptron...
(define (train)
  (for ([path (in-directory "images/training")])
    (when (regexp-match? #rx"[.]img$" path)
      (train-perceptron (path->string path)))))

;let's see what it thinks before training
(perceptron-label (img->vector "images/sample/test1.img"))
(perceptron-label (img->vector "images/sample/test2.img"))
(perceptron-label (img->vector "images/sample/test3.img"))

;I ran some tests and realized it gets a tad bit better if you train it on
;the same data several times.  There's one image it just can't get right.
;Maybe it needs more training data? or maybe this is just a silly way to do
;this
(map (λ (x) (train)) (build-list 10 values))

(perceptron-label (img->vector "images/sample/test1.img"))
(perceptron-label (img->vector "images/sample/test2.img"))
(perceptron-label (img->vector "images/sample/test3.img"))

;that really wasn't great.  it has a false postive.  oh well!  