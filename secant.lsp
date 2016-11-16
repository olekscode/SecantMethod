; Secant method of linear interpolation (a.k.a. Chord method)

; The following functions find a solution for
; the given equation f(x) = 0 on a specified range
; [a, b] with a given precision

; Recursive approach
(defun solve_rec(f a b prec)
	; These helper variables will make the following
	; definition of x more readable and reduce the
	; number of function calls
	(setq fa (funcall f a))
	(setq fb (funcall f b))

	(setq x (- a (* fa (/ (- a b) (- fa fb)))))

	; Return x if the desired precision is reached
	(if (< (abs (funcall f x)) prec)
		(return-from solve_rec x))
	
	(solve_rec f x b prec))

; Iterative approach
(defun solve_iter(f a b prec)
	(setq x a)

	(loop
		; These helper variables will make the update
		; of x more readable and reduce the number
		; of function calls
		(setq fx (funcall f x))
		(setq fb (funcall f b))

		; Return x if the desired precision is reached
		(if (< (abs fx) prec)
			(return-from solve_iter x))

		(setq x (- x (* fx (/ (- x b) (- fx fb)))))))


; Naming the lambda to increase the readability of code
(setq myfunc (lambda (x) (- (exp x) 3)))

; Recursive solution
(setq sol_rec
	(solve_rec myfunc 1 2 0.000001))

; Iterative solution
(setq sol_iter
	(solve_iter myfunc 1 2 0.000001))

; Formatted output. 7 digits after the decimal point of x
; and 15 - for the function value
(write-line "Recursive Function")
(format t "x = ~,7@f~%" sol_rec)
(format t "f(x) = ~2,15@f~%" (funcall myfunc sol_rec))

; This weird thing is a newline character
(format t "~%")

(write-line "Iterative Function")
(format t "x = ~10@f~%" sol_iter)
(format t "f(x) = ~2,15@f~%" (funcall myfunc sol_iter))