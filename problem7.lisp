(defun primes ()
  (let ((primes-list '(2)))
    #'(lambda (n)
	(cond ((> n (length primes-list))
	       (setf primes-list (gen-primes n primes-list))
	       primes-list)
	      (t (subseq primes-list 0 n))))))

(defun gen-primes (num num-list)
  (cond ((= (length num-list) num) num-list)
	(t (gen-primes num
		       (append num-list
			       (list (gen-next-prime
				      num-list
				      (1+ (car (last num-list))))))))))

(defun gen-next-prime (primes biggest)
  (cond ((gen-next-prime2 primes biggest) biggest)
	(t (gen-next-prime primes (1+ biggest)))))

(defun gen-next-prime2 (primes biggest)
  (cond ((null primes))
	((> (car primes) (sqrt biggest)))
	((zerop (mod biggest (car primes))) nil)
	(t (gen-next-prime2 (cdr primes) biggest))))