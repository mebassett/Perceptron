#lang racket
(define sigma (build-vector 625 (λ (x) 1)))

(define (sign x)
    (if (>= x 0)
        1
        -1))

(define (dot v1 v2)
    (apply + (vector->list (vector-map * v1 v2))))

(define (hexstr->number str)
    (string->number (string-append "#x" str)))

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

(define (scalar-mult s v)
    (vector-map * v (build-vector (vector-length v) (λ (x) s))))

(define (perceptron img)
    (sign (dot sigma img)))

(define (train-perceptron img-name)
    (define y (filename->label img-name))
    (define vec (img->vector img-name))
    (cond ((= (perceptron vec) y)
           (display "good!\n"))
          (else 
           (set! sigma (vector-map + sigma (scalar-mult y vec)))
           (display "bad! updated sigma, though.\n"))))

;how to train your perceptron...
(define (train)
  (for ([path (in-directory "images/training")])
    (when (regexp-match? #rx"[.]img$" path)
      (train-perceptron (path->string path)))))

(perceptron (img->vector "images/sample/test1.img"))
(perceptron (img->vector "images/sample/test2.img"))
(perceptron (img->vector "images/sample/test3.img"))

(map (λ (x) (train)) (build-list 10 values))

(perceptron (img->vector "images/sample/test1.img"))
(perceptron (img->vector "images/sample/test2.img"))
(perceptron (img->vector "images/sample/test3.img"))