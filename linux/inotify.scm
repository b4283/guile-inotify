(define-module (linux inotify)
  :export (inotify-init
           inotify-init1
	   inotify-add-watch
	   inotify-remove-watch
	   inotify-read
	   inotify-close
	   inotify-show-constants))

(eval-when (compile load eval)
           (load-extension "guile-inotify-wrapper" "scm_init_linux_inotify_cwrapper_module")
           (use-modules (linux inotify cwrapper)))

;; public part
(define (inotify-init)
  "(inotify-init)

Returns a integer representing the file descriptor for reading inotify events."
  (inotify-init-wrap))

(define (inotify-init1 flags)
  "(inotify-init1 FLAGS)

Same as inotify-init, and FLAGS is a list containing the symbols for certain
behaviors. See (inotify-show-constants) for available symbols."
  (if (not (list? flags))
      (scm-error 'wrong-type-arg "inotify-init1"
		 "Argument must be a list containing flag keywords" '() '())
      (inotify-init1-wrap (resolve-flags "inotify-init1" flags))))

(define (inotify-add-watch fd filepath flags)
  "(inotify-add-watch FD FILEPATH EVENTS)

Add watches, returns a watch-descriptor (integer).
FD is the file descriptor returned by inotify-init functions.
FILEPATH is a string.
EVENTS is a list containing symbols, see (inotify-show-constants)."
  (inotify-aw-wrap fd filepath (resolve-flags "inotify-add-watch" flags)))

(define (inotify-remove-watch fd wd)
  "(inotify-remove-watch FD WD)

Removes the watch descriptor from file descriptor."
  (inotify-rw-wrap fd wd))

(define (inotify-read fd)
  "(inotify-read FD)

Read next event. This function will block until an events is available.
Returns a assoc-list resembling the original c-struct, see inotify(7)
for details.

This function translates the events into symbols and store them in the
what-happened? pair.

This function also returns multiple values of which the first value
is the struct, and the second is the size in octet returnd by read()."
  (let* ((rl (inotify-read-wrap fd))
         (r (car rl))
         (rmask (cdr (assq 'mask r))))
    (let q3 ((test-events
	      (append inotify-events inotify-read-additional-masks))
	     (what-happened? '()))
      (if (null? test-events)
	  (values (acons 'what-happened? what-happened? r) (cdr rl))
	  (let* ((flag (car test-events))
		 (sym (car flag)) (mask (cdr flag)))
	    (q3 (cdr test-events) (if (logtest mask rmask)
				      (cons sym what-happened?)
				      what-happened?)))))))

(define (inotify-close fd)
  "(inotify-close FD)

Close the file descriptor."
  (inotify-close-wrap fd))

(define (inotify-show-constants)
  "(inotify-show-constants)

Dumps the symbols available for other functions."
  (display "inotify-init1-available-flags: ")
  (display (map strip-symlist inotify-init1-available-flags))
  (newline)
  (display "inotify-add-watch-available-flags: ")
  (display (map strip-symlist inotify-add-watch-available-flags))
  (newline)
  (display "inotify-events: ")
  (display (map strip-symlist inotify-events))
  (newline))

;; private part

(define w-module (resolve-module '(linux inotify cwrapper)))

(define (strip-symlist l)
  (substring (symbol->string (car l)) 3))

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
