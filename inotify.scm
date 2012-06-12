(define-module (linux inotify)
  :export (inotify-init
           inotify-init1
	   inotify-add-watch
	   inotify-remove-watch
	   inotify-read
	   inotify-close
	   inotify-show-constants))

(load-extension "guile-inotify-wrapper" "scm_init_wrapper_module")
(use-modules (linux inotify c-wrapper))

;; public part
(define (inotify-init)
  (inotify-init-wrap))

(define (inotify-init1 flags)
  (if (not (list? flags))
      (scm-error 'wrong-type-arg "inotify-init1"
		 "Argument must be a list containing flag keywords" '() '())
      (inotify-init1-wrap (resolve-flags "inotify-init1" flags))))

(define (inotify-add-watch fd filepath flags)
  (inotify-aw-wrap fd filepath (resolve-flags "inotify-add-watch" flags)))

(define (inotify-remove-watch fd wd)
  (inotify-rw-wrap fd wd))

(define (inotify-read fd)
  (let* ((r (inotify-read-wrap fd)) (rmask (cdr (assq 'mask r))))
    (let q3 ((test-events
	      (append inotify-events inotify-read-additional-masks))
	     (what-happened? '()))
      (if (null? test-events)
	  (acons 'what-happened? what-happened? r)
	  (let* ((flag (car test-events))
		 (sym (car flag)) (mask (cdr flag)))
	    (q3 (cdr test-events) (if (logtest mask rmask)
				      (cons sym what-happened?)
				      what-happened?)))))))

(define (inotify-close fd)
  (inotify-close-wrap fd))

(define (inotify-show-constants)
  (display "inotify-init1-available-flags: ")
  (display inotify-init1-available-flags)
  (newline)
  (display "inotify-add-watch-available-flags: ")
  (display inotify-add-watch-available-flags)
  (newline)
  (display "inotify-events: ")
  (display inotify-events)
  (newline))

;; private part

(define w-module (resolve-module '(linux inotify c-wrapper)))

(define (resolve-flags program list-of-flags)
  "A flag-integer generator for the c functions. Automatically maps
symbols to their c-wrapper symbols by prepending `in-' to the sym."
  (let* ((available-flags
	 (module-ref
	  w-module
	  (string->symbol (string-append program "-available-flags"))))
	 (aflags-keyset (map car available-flags)))
    (let ex ((r 0) (flags list-of-flags))
      (if (= (length flags) 0)
	  r
	  (let ((flagsym (string->symbol
			  (string-append
			   "in-" (symbol->string (car flags))))))
	    (if (memq flagsym aflags-keyset)
		(ex (logior r (cdr (assq flagsym available-flags))) (cdr flags))
		(scm-error 'invalid-argument #f
			   "Invalid argument: ~A" (list flagsym) '())))))))
