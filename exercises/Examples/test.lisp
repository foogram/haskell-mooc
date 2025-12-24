;; Golden ratio

(defun main ()
(let ((phi (/ (+ 1 (sqrt 5)) 2)))


; polynomial x = x^2 - x - 1

; f x = polynomial (polynomial x)

;;ploynomial
(defun polynomial (x)
    (- (* x x) x 1))



(defun f (x)
    (polynomial (polynomial x)))

(format t "Phi value:~,30f~%" phi)
(format t "ploynomial phi: ~,2f~%" (polynomial phi))
(format t "f phi: ~,2f~%" (f phi))
(format t "test: ~,2f~%" (polynomial 3 ))
)
)