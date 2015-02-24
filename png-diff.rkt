#lang racket/base

(require racket/list)
(require racket/class)
(require racket/draw)
(require scheme/cmdline)


(define *version* (make-parameter "0.0.1"))
(define *file1* "") ;; первый файл
(define *file2* "") ;; второй файл
(define *output* "") ;; файл для вывода
(define *source* (make-parameter 1)) ;; исходный файл для рисования (1 или 2)
(define *xdif* (make-parameter 5))  ;;  смещение по х для рисования рамки
(define *ydif* (make-parameter 5)) ;; смещение по у для рисования рамки
(define *color* (make-parameter "red")) ;; имя цвета (строка)
(define *size* (make-parameter 10)) ;; имя цвета (строка)
(define *width* (make-parameter 1)) ;; имя цвета (строка)
(define *time* (make-parameter #f)) ;; выводить допинформацию по времени работы



(define (calc-list in-list size)
  (define (local min cur ilist result)
    (if (null? ilist)
        (append result (list (list min cur)))
        (if (> size (-  (car ilist) cur))
            (local min (car ilist) (cdr ilist) result)
            (local (car ilist)  (car ilist) (cdr ilist) (append result (list (list min cur)) )))))
  (if (> 1 (length in-list))
      in-list
      (local (car in-list)  (car in-list) (cdr in-list) (list))))

(define (diff filename1 filename2 output)
  (define (minX i1 i2)
    (min (send (send i1 get-bitmap) get-width) (send (send i2 get-bitmap) get-width)))
  (define (minY i1 i2)
    (min (send (send i1 get-bitmap) get-height) (send (send i2 get-bitmap) get-height)))
  (define (check-point x y i1 i2)
    (let [(color1 (make-object color%))
          (color2 (make-object color%))]
      (if (and (send i1 get-pixel x y color1) 
               (send i2 get-pixel  x y color2))
          (not 
           (and (= (send color1 red) (send color2 red))
                (= (send color1 blue) (send color2 blue))
                (= (send color1 green) (send color2 green))))
          #t)))
  
  (let ([image1 (read-bitmap filename1)]
        [image2 (read-bitmap filename2)]
        [f1 (new  bitmap-dc% )]
        [f2 (new  bitmap-dc% )]
        )
    
    
    (send f1 set-bitmap image1)
    (send f2 set-bitmap image2)
    
    
    (let* [(minX (minX f1 f2))
           (minY (minY f1 f2))
           (diff-list '())
           
           ]
      (do [(i 0 (+ 1 i))]
        [(= i minX)]
        (do [(j 0 (+ 1 j))]
          [(= j minY)]
          (when (check-point i j f1 f2)
            (set! diff-list (append diff-list (list (list i j))))
            ) 
          
          ))
      
      (let* [(xlist (calc-list (map (lambda (x) (car x)) diff-list) (*size*)))
             (rectList (map (lambda (xx) (list (car xx) (last xx))) ((lambda (a1) (map (lambda (z) ((lambda (x1 x2) (filter (lambda (x) (and (>= (car x) x1) (>= x2 (car x)))) a1))  (car z) (cadr z))) (calc-list (map (lambda (x) (car x)) a1) 5))) diff-list)))]
        (map (lambda (rect)
               (let* ((xPoint (car rect))
                      (yPoint (cadr rect))
                      (x1 (car xPoint))
                      (y1 (cadr xPoint))
                      (x2 (car yPoint))
                      (y2 (cadr yPoint)))
                      (send f2 set-pen (*color*) (*width*) 'xor)      
                      (send f2 set-brush (*color*) 'transparent) 
                      (send f2 draw-rectangle (- x1 (*xdif*)) (- y1 (*ydif*)) (+ (* 2 (*xdif*)) (- x2 x1)) (+ (* 2 (*ydif*)) (- y2 y1)))))
             rectList)
        (send (send f2 get-bitmap) save-file output 'png)))))




(define (process-args args)
  (if (member "--version" args)
      (display-version)
      (if (null? args)
          (begin
            (display-version)
            (newline)
            (display-help))
          (parse-args args))))


(define (parse-args args)
  (command-line
   #:program "png-diff"
   #:argv args
   #:help-labels "PNG diff switch:"
   #:once-each
   
   ("--size" size 
             "Interval size for difference"
             (*size* (string->number size)))
   
   ("--xdif" xdif
             "Offset for x for rectangle"
             (*xdif* (string->number xdif)))
   
   ("--ydif" ydif 
             "Offset for y for rectangle"
             (*ydif* (string->number ydif)))
   
   ("--source" source 
               "1 or 2. Which file is source fo result. If differ from 1 or 2 then 1"
               (*source* (if (eq? "2" source) 2 1)))
   
   ("--color" color 
              "Color name for rectangle. see http://docs.racket-lang.org/draw/color-database___.html?q=color-database%3C%25%3E for color name"
              (*color* color))
   ("--width"  width 
               "Width for rectangle"
               (*width* (string->number width)))
   ("--time" "Print work time"
             (*time* #t))
   ;; expects one grammar path
   #:args (file1 file2 output)
   ;; set the grammar path when done
   ;(set! *grammar-path* grammar)
   (list file1 file2 output)))

(define (display-version)
  (printf "\\nPNG diff Generator v~a" *version*))

(define (display-help)
  (parse-args '("-help")))

(define (nth n lst)
  ;;; return the nth value in lst, #f if there is no nth value.
  (cond
    ((empty? lst) #f)
    ((= n 0)
     (car lst))
    (else
     (nth (- n 1) (cdr lst)))))

(define (main args)
  (define (file-ex name)
    (file-exists? (string->path name)))
  (let* [(filelist (process-args args))]
    (if (not (file-ex (nth 0 filelist)))
        (printf "File ~a don't exists" (nth 0 filelist))
        (if (not (file-ex (nth 1 filelist)))
            (printf "File ~a don't exists" (nth 1 filelist))
            (diff (nth 0 filelist) (nth 1 filelist) (nth 2 filelist))))))

(if (*time*)
    (time (main (vector->list (current-command-line-arguments))))
    (main (vector->list (current-command-line-arguments))))

