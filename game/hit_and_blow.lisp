(defun make-answer ()
  "数字を４つ選ぶ"
  (do ((ans nil)
       (n (random 10) (random 10))); (random 10)で初期化し繰り返すたびに(random 10)を呼ぶ
      ((= (length ans) 4) ans)
      (unless (member n ans)
        (push n ans))))

(defun check-input-code (code)
  "入力コードのチェック"
  (and (consp code) ; codeがリストであるかチェック
       (= (length code) (length (remove-duplicates code)) 4)
       (every (lambda (x)
                (and (integerp x) (<= 0 x 9)))
              code)))

(defun input-code ()
  (loop
    (princ ">>>")
    ; ストリームはバッファを持っているので，上のプロンプトは一時的にバッファに格納される．バッファが満杯になったタイミングでバッファ内のすべてのデータを出力する（＝バッファリング）
    ; 標準出力の場合バッファが満杯になったタイミングだけでなく，改行文字が書き込まれたときにもバッファ内のデータが出力される（＝行バッファリング）finish-outputは強制的にバッファリングする
    (finish-output)
    (let ((code (read)))
      (if (check-input-code code)
          (return code))
      (format t "異なる４つの数字（０〜９）を入力してください~%"))))

(defun count-bulls (answer code)
  "bulls"
  (count t (mapcar #'= answer code)))

(defun count-cows (answer code)
  "cows"
  (let ((c 0))
    (dolist (x code c) ; リストcodeの要素を順番に取り出してxに入れる，終了したらcを返す
      (if (member x answer)
          (incf c)))))

(defun mastermind ()
  (let ((ans (make-answer)))
    (dotimes (x 10 (format t "残念！正解は~aでした" ans))
      (let* ((code (input-code))
             (bulls (count-bulls ans code))
             (cows (- (count-cows ans code) bulls)))
        (format t "~2d: ~a, bulls = ~d, cows = ~d~%" (1+ x) code bulls cows)
        (when (= bulls 4)
          (format t "おめでとう！！~%")
          (return))))))
