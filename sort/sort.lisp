; -------------------
; 挿入ソート
; -------------------
"数字を先頭から取り出し前後を逐次判断して挿入する"
"N^2"
(defun insert-element (x xs pred)
  "既存リストの数値と比較してpredを満たす場所に挿入する"
  (cond
    ((null xs) (list x))
    ((funcall pred x (car xs)) (cons x xs)) ;満たせば先頭に満たす
    (t (cons (car xs) ;満たさない場合は先頭をとっておき，次のリストで同様の処理をする
             (insert-element x (cdr xs) pred)))))

(defun insert-sort (xs pred)
  (if (null xs)
      nil
      (insert-element (car xs) (insert-sort (cdr xs) pred) pred)))

; -------------------
; クイックソート
; -------------------
"基準となる値を選んで（pivot）その値の大小で分割することを繰り返す"
"N*logN"
(defun partition (p xs pred)
  "リストの分割"
  (let (ys zs)
    (dolist (x xs (list ys zs))
      (if (funcall pred x p)
          (push x ys)
          (push x zs)))))
