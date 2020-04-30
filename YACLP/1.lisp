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

