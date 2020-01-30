(define-module (schell)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 textual-ports)
  #:export (stdin
            stdout
            &
            wait
            $
            ||
            >>
            <<
            stdin->string))


(define stdin  (make-parameter (current-input-port)))
(define stdout (make-parameter (current-output-port)))

(define-record-type <proc>
  (make-proc pid)
  proc?
  (pid proc-pid))

(define (run-subproc command args)
  (define pid (primitive-fork))
  (when (= pid 0)
    (move->fdes (stdin)  0)
    (move->fdes (stdout) 1)
    (let ((cmd (if (symbol? command)
                   (symbol->string command)
                   command)))
      (apply execlp cmd cmd args)))
  (make-proc pid))

(define (& command . rest)
  (if (or (symbol? command)
          (string? command))
      (run-subproc command rest)
      (call-with-new-thread
       (lambda ()
         (apply command rest)))))

(define (wait pid)
  (cond
   ((proc? pid)
    (cdr (waitpid (proc-pid pid))))
   ((thread? pid)
    (join-thread pid))
   (else pid)))

(define ($ command . rest)
  (if (or (symbol? command)
          (string? command))
      (cdr (waitpid (proc-pid (apply & command rest))))
      (apply command rest)))

(define (pipe-thunks thunks nstdin)
  (if (null? (cdr thunks))
      (wait (parameterize ((stdin nstdin))
              ((car thunks))))
      (let* ((p (pipe))
             (in (car p))
             (out (cdr p)))
        (parameterize ((stdin nstdin)
                       (stdout out))
          ((car thunks)))
        (close out)
        (pipe-thunks (cdr thunks) in))))

(define-syntax-rule (|| command* ...)
  (pipe-thunks (list (lambda () command*) ...) (stdin)))

(define-syntax-rule (>> command file)
  (call-with-output-file file
    (lambda (port)
      (parameterize ((stdout port))
        command))))

(define-syntax-rule (<< command file)
  (call-with-input-file file
    (lambda (port)
      (parameterize ((stdin port))
        command))))

(define (stdin->string)
  (get-string-all (stdin)))
