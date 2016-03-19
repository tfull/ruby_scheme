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
    (expressions :accessor Closure-expressions :init-keyword :expressions)))

(define id (lambda (x) x))

(define (syntax-lambda env tl)
    (make <Closure> :arguments (car tl) :environment env :expressions (cdr tl)))

(define (syntax-define env tl)
    (let ((a (car tl)))
        (cond
            ((symbol? a)
                (begin
                    (set-car! env (insert (cons a (evaluate env (cadr tl))) (car env)))
                    a))
            ((list? a)
                (let ((c (make <Closure> :arguments (cdr a) :environment env :expressions (cdr tl))))
                    (begin
                        (set-car! env (insert (cons (car a) c) (car env)))
                        (car a)))))))

(define (syntax-let env tl)
    (define (bind env kvs)
        (if (null? kvs)
            ()
            (cons (cons (car (car kvs)) (evaluate env (cadr (car kvs)))) (bind env (cdr kvs)))))
    (let ((frame (bind env (car tl))) (exp (cadr tl)))
        (evaluate (cons frame env) exp)))

(define (syntax-let* env tl)
    (define (bind env kvs)
        (if (null? kvs)
            env
            (bind (cons (list (cons (car (car kvs)) (evaluate env (cadr (car kvs))))) env) (cdr kvs))))
    (let ((newenv (bind env (car tl))) (exp (cadr tl)))
        (evaluate newenv exp)))

(define (syntax-if env tl)
    (if (evaluate env (car tl)) (evaluate env (cadr tl)) (evaluate env (caddr tl))))

(define (syntax-cond env tl)
    (if (null? tl)
        (undefined)
        (let ((body (car tl)))
            (if (or (eq? (car body) 'else) (evaluate env (car body)))
                (evaluate env (cadr body))
                (syntax-cond env (cdr tl))))))

(define (syntax-and env tl)
    (if (null? tl)
        #t
        (let ((v (evaluate env (car tl))))
            (if v
                (if (null? (cdr tl))
                    v
                    (syntax-and env (cdr tl)))
                v))))

(define (syntax-or env tl)
    (if (null? tl)
        #f
        (let ((v (evaluate env (car tl))))
            (if v
                v
                (if (null? (cdr tl))
                    v
                    (syntax-or env (cdr tl)))))))

(define (subroutine-plus l)
    (if (null? l) 0 (+ (car l) (subroutine-plus (cdr l)))))

(define (subroutine-minus l)
    (define (sub v l)
        (if (null? l)
            v
            (sub (- v (car l)) (cdr l))))
    (sub (car l) (cdr l)))

(define (subroutine-star l)
    (if (null? l) 1 (* (car l) (subroutine-star (cdr l)))))

(define (subroutine-slash l)
    (define (sub v l)
        (if (null? l)
            v
            (sub (/ v (car l)) (cdr l))))
    (sub (car l) (cdr l)))

(define (compare-function f l)
    (define (sub v l)
        (if (null? l)
            #t
            (if (f v (car l))
                (sub (car l) (cdr l))
                #f)))
    (if (f (car l) (cadr l))
        (sub (cadr l) (cddr l))
        #f))

(define (subroutine-less l) (compare-function < l))

(define (subroutine-greater l) (compare-function > l))

(define (subroutine-equal l) (compare-function = l))

(define (subroutine-cons l)
    (cons (car l) (cadr l)))

(define (subroutine-car l)
    (car (car l)))

(define (subroutine-cdr l)
    (cdr (car l)))

(define (subroutine-null? l)
    (null? (car l)))

(define (subroutine-eq? l)
    (eq? (car l) (cadr l)))

(define initial-environment
    (list
        (cons 'define (make <Syntax> :name 'define :function syntax-define))
        (cons 'if (make <Syntax> :name 'if :function syntax-if))
        (cons 'lambda (make <Syntax> :name 'lambda :function syntax-lambda))
        (cons 'let (make <Syntax> :name 'let :function syntax-let))
        (cons 'let* (make <Syntax> :name 'let* :function syntax-let*))
        (cons 'cond (make <Syntax> :name 'cond :function syntax-cond))
        (cons 'and (make <Syntax> :name 'and :function syntax-and))
        (cons 'or (make <Syntax> :name 'or :function syntax-or))
        (cons '+ (make <Subroutine> :name '+ :function subroutine-plus))
        (cons '- (make <Subroutine> :name '- :function subroutine-minus))
        (cons '* (make <Subroutine> :name '* :function subroutine-star))
        (cons '/ (make <Subroutine> :name '/ :function subroutine-slash))
        (cons '< (make <Subroutine> :name '< :function subroutine-less))
        (cons '> (make <Subroutine> :name '> :function subroutine-greater))
        (cons '= (make <Subroutine> :name '= :function subroutine-equal))
        (cons 'list (make <Subroutine> :name 'list :function id))
        (cons 'cons (make <Subroutine> :name 'cons :function subroutine-cons))
        (cons 'car (make <Subroutine> :name 'car :function subroutine-car))
        (cons 'cdr (make <Subroutine> :name 'cdr :function subroutine-cdr))
        (cons 'null? (make <Subroutine> :name 'null? :function subroutine-null?))
        (cons 'eq? (make <Subroutine> :name 'eq? :function subroutine-eq?))))

(define (apply cl l)
    (define (correspond args l)
        (cond
            ((null? args) '())
            ((symbol? args) (list (cons args l)))
            ((pair? args) (cons (cons (car args) (car l)) (correspond (cdr args) (cdr l))))))
    (define (execute env es)
        (cond
            ((null? es) (undefined))
            ((null? (cdr es))
                (evaluate env (car es)))
            (else
                (begin
                    (evaluate env (car es))
                    (execute env (cdr es))))))
    (let ((nenv (correspond (Closure-arguments cl) l)))
        (execute (cons nenv (Closure-environment cl)) (Closure-expressions cl))))

(define (associate var env)
    (define (assoc v l)
        (cond
            ((null? l) '(#f . #f))
            ((eq? (car (car l)) var) (cons #t (cdr (car l))))
            (else (assoc v (cdr l)))))
    (if (null? env)
        '(#f . #f)
        (let ((p (assoc var (car env))))
            (if (car p) p (associate var (cdr env))))))

(define (insert kv l)
    (cond
        ((null? l) (list kv))
        ((eq? (car kv) (car (car l))) (cons kv (cdr l)))
        (else (cons (car l) (insert kv (cdr l))))))

(define (main-loop env)
    (let ((e (begin (display "Scheme> ") (flush) (read))))
        (if (eof-object? e)
            (begin (newline) #t)
            (let ((result (evaluate env e)))
                (begin
                    (display result)
                    (newline)
                    (main-loop env))))))

(define (evaluate env e)
    (cond
        ((list? e)
            (cond
                ((null? e) '())
                ((eq? 'quote (car e)) (cadr e))
                (else
                    (let ((hd (evaluate env (car e))))
                        (cond
                            ((is-a? hd <Syntax>) ((Syntax-function hd) env (cdr e)))
                            ((is-a? hd <Subroutine>) ((Subroutine-function hd) (map (lambda (x) (evaluate env x)) (cdr e))))
                            ((is-a? hd <Closure>) (apply hd (map (lambda (x) (evaluate env x)) (cdr e))))
                            (else (display "not applicable")))))))
        ((symbol? e)
            (let ((p (associate e env)))
                (if (car p) (cdr p) (undefined))))
        (else e)))

(define (scheme)
    (main-loop (list initial-environment)))

(define (main args)
    (scheme))
