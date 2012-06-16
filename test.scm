(use-modules (linux inotify))

(define w (inotify-init))
(define file "/tmp/b")

(inotify-add-watch w file '(modify))

(let q1 ((times 5))
  (if (not (= times 0))
      (begin
        (display (string-append "write something to " file " now."))
        (newline)
        (call-with-values (lambda () (inotify-read w))
          (lambda (struct size)
            (display struct)
            (newline)
            (display size)
            (newline)))
	(q1 (- times 1)))))

(display (inotify-close w))
