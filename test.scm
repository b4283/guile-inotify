(use-modules (linux inotify))

(define w (inotify-init))

(inotify-add-watch w "/tmp/b" '(modify))

(let q1((times 5))
  (if (not (= times 0))
      (begin
	(display (inotify-read w))
	(q1 (- times 1)))))

(display (inotify-close w))
