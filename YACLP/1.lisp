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

(defun my-butlast (ls)
  (if (= (length ls) 1) ;上記通り要素数チェックはcdrが空リストかでチェックしたほうが効率的
      nil
      (cons (car ls) (my-butlast (cdr ls)))))
