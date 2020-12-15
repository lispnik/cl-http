(in-package :LIS)

(defPattern labortest-input
  ((name name)
   (obergruppe obergruppe)
   (einheit neueinheit)
   (goziffer goziffer)
   (bezeichnung bezeichnung))
  (let ((labortest nil))
    (declare (special *bereich*))
    (setf name (intern name (lis:bereich.package *bereich*)))
    (setf labortest
          (or (clos:find-instance 'lis::untersuchung
                                  :bereich *bereich*
                                  :name name
                                  :error-p nil)
              (make-instance 'lis::test :name name)))
    (setf obergruppe (intern (format nil "(~a)" obergruppe)
                             (lis:bereich.package *bereich*)))
    (setf obergruppe
          (or (clos:find-instance 'lis::untersuchung
                                  :bereich *bereich*
                                  :name obergruppe
                                  :error-p nil)
              (make-instance 'lis::untersuchungsreihe :name obergruppe)))
    (setf (lis:test.einheit labortest) (lis::einheit einheit))
    (setf (lis::test.bezeichnung labortest) bezeichnung)
    (unless (or (string-equal goziffer "NICHT")
                (string-equal goziffer ""))
      (setf (lis:untersuchung.go-kennzeichen labortest)
            (intern goziffer :keyword)))
    labortest))

(defParameter *test-path* (choose-file-dialog))

(with-open-file (stream *test-path* :direction :input
                        :element-type '(unsigned-byte 8))
  (let ((rs (make-instance 'clos::record-stream :stream stream)))
    (let ((record nil)
          (data nil)
          (wrapper nil))
      (unwind-protect
        (setf record (make-record :labortests))
        (loop  (when (stream-eofp rs) (return))
          (clos:read-object record rs)
          (setf wrapper (make-instance 'labortests :record record))
          (push (list (labortests.nr wrapper)
                      (labortests.bezeichnung wrapper))
                data)
          (setf (clos::xml-record-node.record wrapper) nil))
        (when record (dispose-record record)))
      (nreverse data))))


        
