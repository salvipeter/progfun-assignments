;;; Assignment 1 in Common Lisp

(defun pascal-naive (c r)
  (if (or (= c 0) (= c r))
      1
      (+ (pascal-naive (1- c) (1- r))
         (pascal-naive c (1- r)))))

;;; David verziojanak parafrazisa
(defun pascal-david (c r)
  "Not really `functional', but CL is not a purely functional language."
  (let* ((n (- r c -1))
         (col (make-array n :initial-element 1)))
    (iter (repeat c)
          (iter (for i from 1 below n)
                (setf (elt col i) (+ (elt col (1- i)) (elt col i)))))
    (elt col (1- (length col)))))

;;; Gyors verzio
(defun pascal-iterative (c r)
  "Iterative version."
  (iter (with acc = 1)
        (for k from 1 to c)
        (setf acc (/ (* acc r) k))
        (decf r)
        (finally (return acc))))
(defun pascal (c r)
  "Recursive version."
  (labels ((rec (n k acc)
             (if (> k c)
                 acc
                 (rec (1- n) (1+ k) (/ (* acc n) k)))))
    (rec r 1 1)))

(defun main ()
  (format t "Pascal's Triangle~%")
  (dotimes (row 11)
    (dotimes (col (1+ row))
      (format t "~a " (pascal col row)))
    (terpri)))

(defun balance (str)
  (labels ((rec (lst open)
             (cond ((null lst) (= open 0))
                   ((char= (first lst) #\() (rec (rest lst) (1+ open)))
                   ((char= (first lst) #\)) (and (> open 0) (rec (rest lst) (1- open))))
                   (t (rec (rest lst) open)))))
    (rec (coerce str 'list) 0)))

(defun count-change (money coins)
  (cond ((zerop money) 1)
        ((null coins) 0)
        ((<= (first coins) money)
         (+ (count-change (- money (first coins)) coins)
            (count-change money (rest coins))))
        (t (count-change money (rest coins)))))
