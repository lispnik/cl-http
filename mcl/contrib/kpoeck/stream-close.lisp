(in-package :ccl)

(defmethod stream-close ((s modal-ascii-or-binary-tcp-stream)
                         &aux (conn (slot-value s 'conn)))
  (when conn
    (stream-clear-input s)
    ;; orginal code calls the function on stream rather than conn.
    (tcp-stream-force-output conn t)   ; Ok if fails (bogus)
    (let ((pb (conn-pb conn)))
      (setf (rref pb tcpioPB.close.validityFlags) 0)
      (%tcp-control pb $TCPClose T)     ; Ok if fails (bogus)
      ;;;;we should get the following sequence -- Karsten  2/16/1995.
      ;;; Can hang indefinitely so wait for :CLOSING-TIME-ACK -- Reti 2/17/1995.
      ;;  No -57 error with Netscape. MacWeb and Mosaic work fine.
      ;;;:FIN-WAIT-1 (10)
      ;;;:FIN-WAIT-2 (12)
      ;;;:CLOSING-TIME-ACK (20)
      ;;;:CLOSED (0)
      (let ((end-time (+ 60 (#_tickcount))))
        (loop for state = (tcp-connection-state s)
              until (or (zerop state)
                        (= state 20.)
                        (> (#_tickcount) end-time)
                        )
              finally (%tcp-release pb)))
      (#_DisposPtr pb)
      (setf (slot-value s 'conn) nil))
    (setq *open-tcp-streams* (delete s *open-tcp-streams* :test #'eq)))
  (call-next-method))
