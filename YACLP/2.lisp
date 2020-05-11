;; 26
;
(defun member-tree (x ls)
  (labels ((iter (ls)
             (cond ((consp ls)
                    (iter (car ls))
                    (iter (cdr ls)))
                   ((eql ls x)
                    (return-from member-tree t)) ;member-treeから抜け出してtを返す
                   (t nil))))
    (iter ls)))

;; 27
(defun count-leaf (ls)
  (labels ((count-leaf-sub (ls c)
             (cond ((consp ls)
                    (count-leaf-sub (car ls) (1+ c))
                    (count-leaf-sub (cdr ls) c))
                   (t c))))
    (count-leaf-sub ls 0)))
;
(defun count-leaf (ls)
  (cond ((null ls) 0)
        ((atom ls) 1)
        (t (+ (count-leaf (car ls)) (count-leaf (cdr ls))))))

;; 28
;
(defun my-subst (x y ls)
  (cond ((eql y ls) x)
        ((atom ls) ls)
        (t (cons (my-subst x y (car ls)) (my-subst x y (cdr ls))))))

;; 29
;
(defun flatmap (func ls)
  "mapcarでlsの各要素にfuncを適用した結果を平滑化する"
  (apply #'append (mapcar func ls)))

(defun permutation (n ls)
  (if (zerop n)
      (list nil)
      (flatmap #'(lambda (x)
                   (mapcar #'(lambda (y)
                               (cons x y)
                               ;(print (cons x y));printすると処理が追える
                               )
                           (permutation (1- n) (remove x ls))))
               ls)))
; flatmapしないバージョンで実行してみると上記printをさらに追いやすくなる
(defun permutation (n ls)
  (if (zerop n)
      (list nil)
      (mapcar #'(lambda (x)
                   (mapcar #'(lambda (y)
                               (cons x y)
                               )
                           (permutation (1- n) (remove x ls))))
               ls)))

;; 30
(defun repeat-perm (n ls)
  (if (zerop n)
      (list nil)
      (flatmap #'(lambda (x)
                   (mapcar #'(lambda (y)
                               (cons x y)
                               )
                           (repeat-perm (1- n) ls)))
               ls)))

;; 31
;
(defun comb-num (n r)
  "再帰を使うために下記漸化式を使う"
  "nＣ0 = nＣn = 1"
  "nＣr = nＣr-1 * (n - r + 1) / r"
  (if (or (= n r) (zerop r))
      1
      (/ (* (comb-num n (1- r)) (1+ (- n r))) r)))

;; 32
(defun combination (n ls)
  (if (zerop n)
      (list nil)
      (flatmap #'(lambda (x)
                   (mapcar #'(lambda (y)
                               (mapcar #'(lambda (z)
                                           (cons x y z)
                                           (print (cons x y z))
                                           )
                                       (combination (1- n) (remove y ls))))
                           (combination (1- n) (remove x ls))))
               ls)))
;
(defun combination (n ls)
  "再帰を使うために下記漸化式を使う"
  "nＣ0 = nＣn = 1"
  "nＣr = n-1Ｃr-1 + n-1Ｃr"
  (labels ((comb-sub (n ls)
             (cond ((zerop n)
                    (list nil))
                   ((= n (length ls))
                    (list ls))
                   (t (append (mapcar #'(lambda (x)
                                          (cons (car ls) x))
                                      (comb-sub (1- n) (cdr ls)))
                              (comb-sub n (cdr ls)))))))
    (if (< (length ls) n)
        nil
        (comb-sub n ls))))

;; 33
;
(defun singlep (ls)
  (null (cdr ls)))

(defun repeat-comb (n ls)
  (cond ((zerop n) (list nil))
        ((singlep ls) (list (make-list n :initial-element (car ls))))
        (t (append (mapcar #'(lambda (x)
                               (cons (car ls) x))
                           (repeat-comb (1- n) ls))
                   (repeat-comb n (cdr ls))))))

;; 34
(defun take (ls n)
  (if (or (= n 0) (null ls))
      nil
      (cons (car ls) (take (cdr ls) (1- n)))))
(defun drop (ls n)
  (if (or (= n 0) (null ls))
      ls
      (drop (cdr ls) (1- n))))
(defun split-nth (ls n)
  (values (take ls n) (drop ls n)))

;; 35
;
(defun partition (ls)
  (labels ((odd-part (ls xs ys)
             (if (null ls)
                 (values (nreverse xs) (nreverse ys))
                 (even-part (cdr ls) xs (cons (car ls) ys))))
           (even-part (ls xs ys)
             (if (null ls)
                 (values (nreverse xs) (nreverse ys))
                 (odd-part (cdr ls) (cons (car ls) xs) ys))))
    (even-part ls nil nil)))

;; 36
(defun split-find (x ls)
  (labels ((split-find-sub (x ls xs)
             (cond ((null ls) (nreverse xs))
                   ((eql (car ls) x) (values (nreverse xs) ls))
                   (t (split-find-sub x (cdr ls) (cons (car ls) xs))))))
    (split-find-sub x ls nil)))
;
(defun split-find (x ls)
  "doを使う別解"
  (do ((ls ls (cdr ls)) (xs nil));繰り返しに関係するlsをlsで初期化し毎ループは(cdr ls)し，他のレキシカル変数としてxsをnilで初期化する
      ((null ls) (values (nreverse xs) nil));ループ終了条件はlsがnullのときとして，返り値はxs
      (if (eql (car ls) x);毎ループで行う処理
          (return (values (nreverse xs) ls));対象の要素が見つかれば即返却
          (push (car ls) xs))));それ以外はxsに足していく

;; 37
(defun split-ge (x ls)
  (do ((ls ls (cdr ls)) (xs nil) (ys nil))
      ((null ls) (values (nreverse xs) (nreverse ys)))
      (if (<= (car ls) x)
          (push (car ls) xs)
          (push (car ls) ys))))

;; 38
;
(defun drop-same-code (x ls)
  "本関数は「連続した」記号であることに留意（あまり汎用性は無い？）"
  "「先頭から」連続している記号を取り除く"
  (if (or (null ls) (not (eql x (car ls))))
      (values nil ls)
      (multiple-value-bind (a b);続く関数が複数の値を返すときに，それら複数の返り値をbindするための関数
        (drop-same-code x (cdr ls));drop-same-codeの返り値はa, bにbindされる
        (values (cons x a) b))))
;
(defun drop-same-code (x ls)
  (do ((ls ls (cdr ls)) (xs nil))
      ((or (null ls) (not (eql x (car ls))))
       (values xs ls))
      (push x xs)))
;
(defun pack (ls)
  "返り値が複数の関数をうまく使って再帰する"
  (multiple-value-bind (xs ys)
    (drop-same-code (car ls) ls)
    (if (null ys)
        (list xs)
        (cons xs (pack ys)))))

;; 39
(defun pack-num-list (ls)
  (labels ((pack-num-list-sub (ls s e)
             (if (= (car ls) e)
                 (pack-num-list-sub (cdr ls) s (1+ e))
                 (cons (cons s (1- e)) (pack-num-list-sub (cdr ls) (cadr ls) (1+ (cadr ls)))))))
    (pack-num-list-sub (cdr ls) (car ls) (1+ (car ls)))))
;
(defun pack-num-list (ls)
  (labels ((push-num (s e a)
             (if (= s e)
                 (cons s a)
                 (cons (cons s e) a)))
           (iter (ls s e a)
             (cond ((null ls) (nreverse (push-num s e a)))
                   ((= (car ls) (1+ e)) (iter (cdr ls) s (car ls) a))
                   (t (iter (cdr ls) (car ls) (car ls) (push-num s e a))))))
    (iter (cdr ls) (car ls) (car ls) nil)))

;; 40
;
(defun iota (n m)
  (if (> n m)
      nil
      (cons n (iota (1+ n) m))))

(defun expand-num-list (ls)
  (cond ((null ls) nil)
        ((consp (car ls)) (append (iota (caar ls) (cdar ls)) (expand-num-list (cdr ls))))
        (t (cons (car ls) (expand-num-list (cdr ls))))))

;; 41
(defun encode (ls)
  (mapcar #'(lambda (ils)
              (cons (car ils) (length ils)))
          (pack ls)))

;; 42
(defun decode (ls)
  (labels ((num-list (x n)
              (if (= n 0)
                  nil
                  (cons x (num-list x (1- n))))))
     (apply #'append (mapcar #'(lambda (ils)
                         (num-list (car ils) (cdr ils)))
                         ls))))
;
(defun decode (ls)
  (apply #'append (mapcar #'(lambda (xs)
                              (make-list (cdr xs) :initial-element (car xs)))
                          ls)))

;; 43
(defun any (pred ls)
  (cond ((null ls) nil)
        ((funcall pred (car ls)) t)
        (t (any pred (cdr ls)))))
(defun my-every (pred ls)
  (cond ((null ls) t)
        ((not (funcall pred (car ls))) nil)
        (t (my-every pred (cdr ls)))))

;; 44
(defun my-maplist (func ls)
  (if (null ls)
      nil
      (cons (funcall func ls) (my-maplist func (cdr ls)))))

;; 45
(defun for-each-list (fn comb term xs)
  (if (null xs)
      term
      (funcall comb (funcall fn (car xs)) (for-each-list fn comb term (cdr xs)))))
