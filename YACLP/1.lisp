;; 1
(defun singlep (ls)
  (eql (length ls) 1))
;
(defun singlep (ls)
  "引数がリストでcdrが空リストであれば要素は１つしか無いリストである"
  "lengthは不要"
  (and (consp ls) (null (cdr ls))))

;; 2
(defun doublep (ls)
  (eql (length ls) 2))
;
(defun doublep (ls)
  (and (consp ls) (singlep (cdr ls))))

;; 3
(defun longerp (ls1 ls2)
  (> (length ls1) (length ls2)))
;
(defun longerp (xs ys)
  "双方のリストについて１つずつ減らしていきysのほうが先になくなったらt"
  "lengthよりも効率的"
  (cond ((null xs) nil)
        ((null ys) t)
        (t (longerp (cdr xs) (cdr ys)))))

;; 4
(defun my-last (ls)
  (if (= (length ls) 1) ;上記通り要素数チェックはcdrが空リストかでチェックしたほうが効率的
      ls
      (my-last (cdr ls))))
;
(defun my-last (ls)
  (if (null (cdr ls))
      ls
      (my-last (cdr ls))))

(defun my-butlast (ls)
  (if (= (length ls) 1) ;上記通り要素数チェックはcdrが空リストかでチェックしたほうが効率的
      nil
      (cons (car ls) (my-butlast (cdr ls)))))

;; 5
(defun take (ls n)
  (if (eql n 0)
      '()
      (cons (car ls) (take (cdr ls) (1- n)))))
;
(defun take (ls n)
  "nの記述は上記で問題ないが，リストが先にnullになる可能性があるのでその条件も追加"
  (if (or (<= n 0) (null ls))
      nil
    (cons (car ls) (take (cdr ls) (1- n)))))

;; 6
(defun drop (ls n)
  (if (or (= n 0) (null ls))
      ls
      (drop (cdr ls) (1- n))))

;; 7
(defun my-subseq (ls s e)
  (if (= s 0)
      (take ls e)
      (my-subseq (cdr ls) (1- s) (1- e))))
;
(defun my-subseq (ls s e)
  (take (drop ls s) (- e s)))

;; 8
(defun my-butlast (ls &optional (n 1))
  (if (= n (length ls))
      nil
      (cons (car ls) (my-butlast (cdr ls) n))))
;
(defun my-butlast (ls &optional (n 1))
  (take ls (- (length ls) n)))

;; 9
(defun group (ls n)
  (if (null ls)
      nil
      (cons (take ls n) (group (drop ls n) n))))

;; 10
(defun my-position (x ls)
  (if (= x (car ls))
      break
      (1+ (my-position x (cdr ls)))))
;
(defun my-position (x ls)
  (labels ((position-sub (ls n)
             (cond ((null ls) nil)
                   ((eql x (car ls)) n)
                   (t (position-sub (cdr ls) (1+ n))))))
    (position-sub ls 0)))

;; 11
(defun my-count (x ls)
  (cond ((null ls) 0)
        ((eql x (car ls)) (1+ (my-count x (cdr ls))))
        (t (my-count x (cdr ls)))))

;; 12
(defun sum-list (ls)
  (labels ((sum-list-sub (ls s)
             (if (null ls)
                 s
                 (sum-list-sub (cdr ls) (+ (car ls) s)))))
    (sum-list-sub ls 0)))
;
(defun sum-list (ls)
  "もちろん最も簡単な方法はreduceを使うパターン"
  (reduce #'+ ls))

;; 13
(defun max-list (ls)
  (labels ((max-list-sub (ls m)
             (cond ((null ls) m)
                   ((< m (car ls)) (max-list-sub (cdr ls) (car ls)))
                   (t (max-list-sub (cdr ls) m)))))
    (max-list-sub (cdr ls) (car ls))))
;
(defun max-list (ls)
  "もちろん最も簡単な方法はreduceを使うパターン"
  (reduce #'max ls))
;
(defun max-list (ls)
  "dolistを使って少し手続きっぽく書くことも可能"
  (let ((m (car ls)))
    (dolist (x (cdr ls) m)
      (when (< m x) (setf m x)))))

(defun min-list (ls)
  (labels ((min-list-sub (ls m)
             (cond ((null ls) m)
                   ((> m (car ls)) (min-list-sub (cdr ls) (car ls)))
                   (t (min-list-sub (cdr ls) m)))))
    (min-list-sub (cdr ls) (car ls))))

;; 14
(defun adjacent (x y ls)
  (cond ((null (cdr ls)) nil)
        ((and (eql x (car ls)) (eql y (car (cdr ls)))) t)
        (t (adjacent x y (cdr ls)))))

;; 15
(defun before (x y ls)
  (cond ((null (cdr ls)) nil)
        ((and (eql x (car ls)) (member y (cdr ls))) t)
        (t (before x y (cdr ls)))))
;
(defun before (x y ls)
  "memberはtの場合その値を先頭要素とするリストを返す"
  (let ((xs (member x ls)))
    (and xs (member y (cdr xs)))))

;; 16
(defun iota (n m)
  (if (> n m)
      nil
    (cons n (iota (1+ n) m))))

;; 17
(defun set-of-list (ls)
  (cond ((null ls) nil)
        ((member (car ls) (cdr ls)) (set-of-list (cdr ls)))
        (t (cons (car ls) (set-of-list (cdr ls))))))
;
(defun set-of-list (ls)
  "reduceを使別解．aは保存される変数"
  (reduce #'(lambda (a x)
              (if (member x a)
                  a
                  (cons x a)))
          ls
          :initial-value nil))

;; 18
(defun my-union (ls1 ls2)
  (cond ((null ls1) ls2)
        ((member (car ls1) ls2) (my-union (cdr ls1) ls2))
        (t (cons (car ls1) (my-union (cdr ls1) ls2)))))

;; 19
(defun my-intersection (xs ys)
  (cond ((null xs) nil)
        ((member (car xs) ys) (cons (car xs) (my-intersection (cdr xs) ys)))
        (t (my-intersection (cdr xs) ys))))

;; 20
(defun difference (xs ys)
  (cond ((null xs) nil)
        ((member (car xs) ys) (difference (cdr xs) ys))
        (t (cons (car xs) (difference (cdr xs) ys)))))

;; 21
;
(defun merge-list (func xs ys)
  "それぞれのリストから１つずつ取り出し，条件に満たすものを採用していく"
  (cond ((null xs) ys)
        ((null ys) xs)
        ((funcall func (car xs) (car ys))
         (cons (car xs) (merge-list func (cdr xs) ys)))
        (t (cons (car ys) (merge-list func xs (cdr ys))))))

;; 22
;
(defun merge-sort (func n ls)
  "ソートは内部で同様の処理をするときに再帰的に呼び出すことがコツ"
  (if (= n 1)
      (list (car ls))
      (let ((m (floor n 2)))
         (merge-list func (merge-sort func m ls) (merge-sort func (- n m) (drop ls m))))))

;; 23
(defun prefix (ls ps)
  (cond ((null ps) t)
        ((eql (car ls) (car ps)) (prefix (cdr ls) (cdr ps)))
        (t nil)))

;; 24
(defun suffix (ls ss)
  (prefix (drop ls (- (length ls) (length ss))) ss))

;; 25
(defun sublistp (xs ls)
  (cond ((> (length xs) (length ls)) nil)
        ((prefix ls xs) t)
        (t (sublistp xs (cdr ls)))))
