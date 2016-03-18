(define-class <Syntax> () (
    (name :accessor Syntax-name :init-keyword :name)
    (function :accessor Syntax-function :init-keyword :function)))

(define-class <Subroutine> () (
    (name :accessor Subroutine-name :init-keyword :name)
    (function :accessor Subroutine-function :init-keyword :function)))

(define-class <Closure> () (
    (name :accessor Closure-name :init-keyword :name :init-value #f)
    (arguments :accessor Closure-arguments :init-keyword :arguments)
    (environment :accessor Closure-environment :init-keyword :environment)
    (expression :accessor Closure-expression :init-keyword :expression)))

(define (syntax-define env tl)
    (let ((a (car tl)))
        (cond
            ((symbol? a)
                (begin
                    (set-car! env (insert (cons a (evaluate env (cadr tl))) (car env)))
                        (undefined))))))

(define (syntax-if env tl)
    (if (evaluate env (car tl)) (evaluate env (cadr tl)) (evaluate (caddr tl))))

(define (subroutine-plus l)
    (if (null? l) 0 (+ (car l) (sum (cdr l)))))

(define initial-environment
    (list
        (cons 'define (make <Syntax> :name 'define :function syntax-define))
        (cons 'if (make <Syntax> :name 'if :function syntax-if))
        (cons '+ (make <Subroutine> :name '+ :function subroutine-plus))))

(define (associate var env)
    (define (assoc l)
        (cond
            ((null? l) '(#f . #f))
            ((eq? (car (car l)) var) (cons #t (cdr (car l))))
            (else (assoc (cdr l)))))
    (if
        (null? env) '(#f . #f)
        (let ((p (assoc (car env))))
            (if (car p) p (assoc (cdr env))))))

(define (insert kv l)
    (cond
        ((null? l) (list kv))
        ((eq? (car kv) (car (car l))) (cons kv (cdr l)))
        (else (cons (car l) (insert kv (cdr l))))))

(define (main-loop env)
    (let* ((e (read)) (result (evaluate env e)))
        (begin
            (display result)
            (newline)
            (main-loop env))))

(define (evaluate env e)
    (cond
        ((number? e) e)
        ((boolean? e) e)
        ((list? e)
            (if (null? e)
                '()
                (let ((hd (evaluate env (car e))))
                    (cond
                        ((is-a? hd <Syntax>) ((Syntax-function hd) env (cdr e)))
                        ((is-a? hd <Subroutine>) ((Subroutine-function hd) (map (lambda (x) (evaluate env x)) (cdr e))))))))
        ((symbol? e)
            (let ((p (associate e env)))
                (if (car p) (cdr p) (undefined))))))

(define (sum l)
    (if (null? l) 0 (+ (car l) (sum (cdr l)))))

(define (main args)
    (main-loop (list initial-environment)))
