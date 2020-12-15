(in-package :ccl)

(defun  %tcp-control (pb code &optional ignore-error-p ignore-timeout)
  (setf (rref pb tcpioPB.csCode) code
        (rref pb tcpioPB.ioCompletion) (%null-ptr))
  (let ((err nil))
    (loop
      (when (eql (setq err (#_control :async pb)) 0)
        (unless (eql code $TCPPassiveOpen)
          (let* ((*interrupt-level* 0))
            (while (> (setq err (rref pb tcpioPB.ioResult)) 0))))
        )
      (return))
    (unless (or ignore-error-p (eql err 0)
                (and ignore-timeout (eql err $TCPTimeout)))
      (%tcp-err-disp err))
    err)
  )
