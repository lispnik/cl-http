;;;-*- Mode: Lisp; Package: CCL -*-

;; Patches of MCL code for the SQL module for MCL, Version 0.85
;;
; Copyright 1989-1994 Apple Computer, Inc.
; Copyright 1995 Digitool, Inc.
;;

;; Simple patches for some fred functions to get selection, bracket blinking and indentation
;; right for [ and ] macro characters in sql package. meta-. shows you where they
;; originally come from. (They are taken from MCL 4.2.)
;; Has to be adapted to each new MCL version. Use at your own risk.

(in-package :ccl)

(eval-when (:compile-toplevel :execute)
  (defmacro %i-- (arg1 arg2 &rest args)
    (if args `(%i- ,arg1 (%i++ ,arg2 ,@args))
        `(%i- ,arg1 ,arg2)))
  
  (defmacro %i++ (a b &rest args)
    (if args `(%i+ ,a (%i++ ,b ,@args))
        `(%i+ ,a ,b))))

(let ((*warn-if-redefine-kernel* nil)
      (*warn-if-redefine* nil))
  ;; if you dare!
(defun buffer-select-blink-pos (w start end &aux pos ch temp-pos pos-1)
    "returns non-neg integer to blink or NIL if no char should be blinked."
    (declare (ignore start))
    (setq pos (buffer-position w))
    (setq end (buffer-size w))
    (cond ;check for bwd blink first
     ((and (> pos 1)
           (or (eq (setq ch (buffer-char w (setq pos-1 (- pos 1)))) #\) )
               (eq (setq ch (buffer-char w (setq pos-1 (- pos 1)))) #\] )
               (eq ch #\"))
           (not (buffer-lquoted-p w pos-1))
           (setq temp-pos (buffer-bwd-sexp w pos))
           (cond ((and (eq temp-pos pos-1) (neq ch #\")) temp-pos) ; we're unbalanced
                 ((setq temp-pos (buffer-forward-find-char
                                  w (cond ((eq ch #\") #\")
                                          ((eq ch #\]) #\[)
                                          (t #\())
                                  temp-pos pos-1))
                  (1- temp-pos)))))
     ((and (> pos 1)
           (eq ch #\#)
           (eq (buffer-char w (- pos 2)) #\|)) ;start comment
      (buffer-skip-bwd-#comment w 0 (- pos 2)))
     ;no bwd blink so check for fwd blink
     ((and (< pos end)
           (setq ch (buffer-char w pos))
           (cond ((or (eq ch #\() (eq ch #\[))
                  (setq temp-pos (buffer-fwd-up-sexp w (%i+ pos 1) end)))
                 ((or (eq ch #\")
                      (and (eq ch #\#)
                           (< (%i+ pos 1) end)
                           (eq (buffer-char w (%i+ pos 1)) #\|)))
                  (setq temp-pos (buffer-fwd-sexp w pos end ch))))
           (not (buffer-lquoted-p w  pos)))         
      ;fwd blink paren or double-quote. limit search to next cr-open-paren
      (- temp-pos 1))))

(defmethod ed-indent-for-lisp ((w fred-mixin) &optional b e &aux 
                                    (frec (frec w))
                                    (c (fred-buffer w)) fwd)
    (unless (and b e) 
      (multiple-value-setq (b e) (frec-get-sel frec))
      (when (neq b e) (if (eq e (buffer-position c))(setq fwd t))))
    (prog* ((pos (buffer-line-start c b))
            (two (or (eq (fred-last-command w) :append)
                     (and (eq b e)
                          (eq *last-command* :self-insert))))
            ; & remember listener oddness
            (cnt -1)
            defun ind)
      (set-mark c e)
      loop
      (when (<= cnt 0)
        (incf cnt)
        ;; changed pm. To do: make it work for listener
        (setq defun (let ((end (buffer-backward-search c "
\(" pos)))
                      (or (buffer-backward-search c "
\[" pos (or end 0)) end 0)))
        (when (typep w 'listener-fred-item)
          (let ((pos2 (buffer-backward-search c "
? (" pos)))
            (when pos2 (setq defun (max pos2 defun))))))
      (when (setq ind (lisp-indentation c defun pos))
        (if (align-pos&target w pos ind two) (setq two t)))
      (setq pos (buffer-line-start c pos 1))
      (when (%i< pos (buffer-position c)) (go loop))
      (when (and two (eq b e))(set-fred-last-command w :self-insert))
      (collapse-selection w fwd)))

;; from "l1-edbuf.lisp"

(defun buffer-bwd-up-sexp (buf &optional end count start
                                    &aux eol-count the-buf)
    ;Supposedly handles comments. hmm
    (when (null end)(setq end (buffer-position buf)))
    (when (null count) (setq count 1))
    (when (null start) (setq start 0))
    (setq the-buf (mark-buffer buf))
    (let* ((lastc nil)
           (skipping nil)
           zpos)
      (declare (fixnum start end)(optimize (speed 3)(safety 0)))
      (while (> end start)
        (multiple-value-bind (string eidx nchars)(%buffer-bwd-chunk-string the-buf start end)
          (declare (fixnum eidx nchars))
          (let ((base-p (simple-base-string-p string)))
            (dotimes (i nchars)
              (let* ((ch (if base-p 
                           (locally (declare (type (simple-array (unsigned-byte 8) (*)) string))
                             (aref string eidx))
                           (locally (declare (type (simple-array (unsigned-byte 16) (*)) string))
                             (aref string eidx)))))
                (cond (skipping
                       (when (and (eq ch skipping)(not (%buffer-lquoted-p the-buf (%i-- end i 1))))
                         (setq skipping nil lastc nil)))
                      ((and (eq ch (char-code #\\))
                            (%buffer-lquoted-p the-buf (%i- end i))
                            (neq lastc (char-code #\newline)))
                       (setq lastc nil))
                      (t (when (neq lastc (char-code #\space))
                           (case lastc
                             (#.(char-code #\newline)
                              (setq eol-count count)
                              (when zpos (return-from buffer-bwd-up-sexp zpos)))                            
                             ((#.(char-code #\))
                               #.(char-code #\])) ;; added pm 
                              (setq count (%i+ count 1)))
                             ((#.(char-code #\()
                               #.(char-code #\[)) ;; added pm
                              (setq count (%i- count 1))
                              (when (eq  count 0)
                                (when (not zpos)(setq zpos (%i- end i)))
                                (when (not eol-count)
                                  (return-from buffer-bwd-up-sexp zpos))))                            
                             ((#.(char-code #\") #.(char-code  #\|))
                              (if (and (eq ch lastc)(not (%buffer-lquoted-p the-buf (%i-- end i 1))))
                                (setq ch nil)
                                (setq skipping lastc)))
                             (#.(char-code #\;) (if eol-count
                                                  (setq count eol-count zpos nil)
                                                  (return-from buffer-bwd-up-sexp nil)))))
                         (setq lastc ch))))
              (setq eidx (1- eidx)))
            (setq  end (%i- end nchars)))))
      (if (and (eq count 1)
               (or (eq lastc (char-code #\())
                   (eq lastc (char-code #\[)))) start zpos)))

(defun buffer-fwd-up-sexp (mark &optional start end count)
    (multiple-value-setq (start end)(buffer-range mark (or end t) start))
    (when (not count) (setq count 1))
    (locally (declare (fixnum start end count)(optimize (speed 3)(safety 0)))
      (let* ((buf (mark.buffer mark))
             (gappos (bf.gappos buf))
             (chunkarr (bf.chunkarr buf))    
             (shft (bf.logsiz buf))
             (gaplen (%i- (bf.gapend buf)(bf.gapstart buf)))
             (chunksz (bf.chunksz buf))
             (chars-left (- end start))
             ;(bchar-p (eq (bf-chartype buf) 'base-character))
             (cpos start)
             (nchars 0)
             (sidx 0)
             (sharp-n 0)
             cross state skipping)
        (declare (fixnum gappos chunksz  chars-left cpos  nchars sidx sharp-n))
        (cond
         ((>= start gappos)
          (setq start (%i+ start gaplen)))
         ((> end gappos)
          (setq chars-left (%i- gappos start))
          (setq cross t)))
        (setq sidx (%iasr shft start))
        (setq start (%ilogand (%i- chunksz 1) start))
        (loop        
          (setq nchars (- chunksz start))
          (if (< chars-left nchars)(setq nchars chars-left))
          (let* ((bstr (svref chunkarr sidx))
                 (bchar-p (simple-base-string-p bstr))
                 (cend (+ start nchars)))
            (declare (fixnum cend))          
            (while (< start cend)
              (let ((code (if bchar-p 
                            (locally (declare (type (simple-array (unsigned-byte 8) (*)) bstr))
                              (aref bstr start))
                            (locally (declare (type (simple-array (unsigned-byte 16)(*)) bstr))
                              (aref bstr start)))))
                (tagbody
                  top
                  (case state
                    ((NIL)
                     (when (%i> code #x21) ; bypass spaces and tabs
                       (cond 
                        ((eq code (char-code #\|))(setq state :skipping skipping code))
                        ((%i< code 94 ; 60
                              )
                         (cond ((eq code (char-code #\#))(setq state :sharp))
                               ((eq code (char-code #\;))
                                (setq state :skipping skipping (char-code #\newline)))
                               ((eq code (char-code #\"))
                                (setq STATE :SKIPPING skipping code))
                               ((or (eq code (char-code #\()) (eq code (char-code #\[))) ;; pm
                                (setq count (%i+ count 1)))
                               ((or (eq code (char-code #\))) (eq code (char-code #\]))) 
                                (setq count (%i- count 1))
                                (when (eq count 0)
                                  (return-from buffer-fwd-up-sexp (%i++ 1 cpos nchars (%i- start cend)))))
                               )))))
                    (:skip-slash (setq state :skipping))
                    (:skip-1 (setq state nil))
                    (:skipping
                     (cond ((and (eq code (char-code #\\))
                                 (or (eq skipping (char-code #\"))
                                     (eq skipping (char-code #\|))))
                            (setq state :skip-slash))
                           ((eq code skipping)
                            (setq state nil))))
                    (:sharp
                     (cond ((eq code (char-code #\|))
                            (setq state :sharp-bar)
                            (setq sharp-n (1+ sharp-n)))
                           ((eq code (char-code #\\))
                            (setq state :skip-1))
                           (t (setq state nil)(go top))))                       
                    (:sharp-bar-bar ; inside a #|, seen a | need a #
                     (setq state :sharp-bar)
                     (when (eq code (char-code  #\#))
                       (setq sharp-n (1- sharp-n))
                       (when (eq sharp-n 0)(setq state nil))))
                    (:sharp-bar-sharp
                     (setq state :sharp-bar)
                     (when (eq code (char-code #\|))
                       (setq sharp-n (1+ sharp-n))))
                    (:sharp-bar
                     (case code
                       (#.(char-code #\|)(setq state :sharp-bar-bar))
                       (#.(char-code #\#)(setq state :sharp-bar-sharp)))))
                  (setq start (%i+  start 1))))))
          (setq chars-left (- chars-left nchars))
          (cond ((= chars-left 0)
                 (if cross
                   (let ((gapend (bf.gapend buf)))
                     (declare (fixnum gapend))
                     (setq start (%ilogand (%i- chunksz 1) gapend))
                     (setq sidx (%iasr shft (%i+ gappos gaplen)))
                     (setq chars-left (%i- end gappos))
                     (setq cross nil))
                   (return nil)))
                (t (setq start 0)
                   (setq sidx (1+ sidx))))
          (setq cpos (+ cpos nchars))))))

(defun buffer-fwd-sexp (buf &optional pos end ch ignore-#-comments)
    (when (null pos)(setq pos (buffer-position buf)))
    (when (null end) (setq end (buffer-size buf)))
    (when (and (null ch) (< pos end)) (setq ch (buffer-char buf pos)))
    (let ((agn nil))
      (prog ()
        top
        (return
         (cond ((>= pos end) nil)
               ((or (eq ch #\() (eq ch #\[)) (buffer-fwd-up-sexp buf (%i+ pos 1) end))
               ((or (eq ch #\)) (eq ch #\])) (%i+ pos 1))
               ((eq ch #\#)
                (setq pos (buffer-forward-find-not-char buf "0123456789" (%i+ pos 1) end))
                (cond ((null pos) end)
                      ((eq (setq ch (buffer-char buf (%i- pos 1))) #\#) pos)
                      ((%str-member ch wsp&cr) (%i- pos 1))
                      ((eq ch #\|)
                       (when (setq pos (buffer-skip-fwd-#comment buf pos end))
                         (if ignore-#-comments
                           (progn (setq agn nil) (go top))
                           pos)))
                      (t (when (%str-member ch "\\([\"") (setq pos (%i- pos 1)))
                         (buffer-fwd-sexp buf pos end))))
               ((eq ch #\") (buffer-fwd-skip-delimited buf "\"\\" (%i+ pos 1) end))
               ((not agn)
                (let ((newpos pos))
                  (while (and (setq newpos (buffer-forward-find-not-char buf prefix&wsp&cr newpos end))
                              (eq (buffer-char buf (%i- newpos 1)) #\;)
                              (setq newpos (buffer-forward-find-char buf #\Newline newpos end))))
                  (when newpos
                    (setq newpos (%i- newpos 1))
                    (if  (eq newpos pos)
                      (buffer-fwd-symbol buf pos end)
                      (progn (setq pos newpos)
                             (setq ch (buffer-char buf newpos))
                             (setq agn t)
                             (go top))))))
               (t (buffer-fwd-symbol buf pos end)))))))

(defun buffer-bwd-sexp (buf &optional pos over-sharps)
    "Returns POSITION that is the beginning of the sexp behind POS."
    (when (null pos)(setq pos (buffer-position buf)))
    (let ((pos (buffer-skip-bwd-wsp&comments buf 0 pos T))) ; dont skip #||#    
      (when (or (null pos) (zerop pos))
        (return-from buffer-bwd-sexp 0))
      (when (and (> pos 1) (buffer-substring-p buf "|#" (- pos 1)))
        (return-from buffer-bwd-sexp (buffer-skip-bwd-#comment buf 0 pos)))
      (let* ((ch (if (buffer-lquoted-p buf pos) #\A (buffer-char buf pos)))
             (pos (case ch
                    ((#\) #\])
                     (or (buffer-bwd-up-sexp buf pos)
                         pos))
                    ((#\" #\| )
                     (or (buffer-backward-search-unquoted buf ch pos)
                         pos))
                    (t
                     (let ((pos (buffer-backward-search-unquoted 
                                 buf symbol-specials pos)))
                       (if pos (1+ pos) 0))))))
        (loop
          (unless pos (return 0))
          (let ((old-pos pos))
            (when (and over-sharps
                       (>= pos 2)
                       (eq (buffer-char buf (- pos 2)) #\#))
              (setq pos (- pos 2)))
            (setq pos (buffer-backward-find-not-char buf ",@'#`" 0 pos))
            (if pos (incf pos))
            (when (eq pos old-pos) (return pos)))))))

  #+ignore 
  (defun defmethod-special-indent (buf pos)
  (let ((size (buffer-size buf)))
    (if (and
         (setq pos (buffer-fwd-sexp buf pos))   ; over "DEFMETHOD"
         (setq pos (buffer-fwd-sexp buf pos nil nil t)))  ; over function-name
      (let ((res 2))
        (while (and (setq pos (buffer-skip-fwd-wsp&comments buf pos size))
                    (< pos size)
                    (not (or (eql #\( (buffer-char buf pos))
                             (eql #\[ (buffer-char buf pos))))
                    (setq pos (buffer-fwd-sexp buf pos)))
          (incf res))
        res)
      2)))

  #+ignore 
  (defun lisp-indentation (buf start end)
    (or (flet-indentation buf start end)
        (prog* ((limit end)
                (scan-count 0)
                count inner scan-pos prev-end last-end last-start next line-start)
          top 
          (setq count scan-count)
          (multiple-value-bind (outer xstart xend) (buffer-scan-lists buf start limit)
            (when (and xstart (null xend))  ;Inside string or comment
              (return (and (eq (buffer-char buf xstart) #\")
                           (eq (buffer-char buf (%i- end 2)) #\~)
                           (%i+ xstart 1))))
            (when (and xend (null xstart))  ;Unmatched close
              (setq start xend)
              (go top))
            (when (null outer) (return 0))
            (setq inner (%i+ outer 1))
            (if xstart (setq scan-pos xstart last-end xend scan-count (%i+ scan-count 1))
                (setq scan-pos inner last-end inner))
            (setq prev-end nil)
            (while (and (setq next (buffer-fwd-sexp buf last-end limit nil t))
                        (not (= (the fixnum last-end)(the fixnum next))))
              (setq prev-end last-end last-end next scan-count (%i+ scan-count 1)))                  
            (when (eq last-end inner) (return inner))
            (setq last-start (if prev-end (real-sexp-start buf prev-end last-end)
                                 scan-pos))
            (setq line-start (buffer-line-start buf last-start))
            (when (%i<= line-start outer) ;All on one line
              (multiple-value-bind (spec pos) (buffer-special-indent-p buf inner)
                (when (eq spec 'defun) (return (%i+ outer *fred-body-indent*)))
                (when (and (fixnump spec) (%i>= spec 0))
                  (if (%i<= pos scan-pos)
                    (while (and (%i<= scan-count spec)
                                (setq pos (buffer-fwd-sexp buf pos scan-pos nil t)))
                      (setq scan-count (%i+ scan-count 1)))
                    (setq scan-count (%i- scan-count 1)))
                  (when (eq scan-count spec) (return (%i+ outer *fred-body-indent*)))
                  (when (%izerop scan-count)
                    (return (%i+ outer *fred-body-indent*)))))
              (let ((pos (real-sexp-start buf inner end)))
                (return
                 (backward-prefix-chars buf
                                        (if (or (%str-member (buffer-char buf pos) "[]()\"0123456789#")
                                                (and (not (%izerop outer))
                                                     (eq (buffer-char buf (%i- outer 1)) #\'))
                                                (null (setq next
                                                            (real-sexp-start
                                                             buf
                                                             (buffer-fwd-sexp buf pos end nil t) end))))
                                          pos next)))))
            (multiple-value-bind (l-outer l-start l-end)
                                 (buffer-scan-lists buf line-start last-start)
              (when (or (and l-start (null l-end))
                        (and l-end (null l-start))
                        l-outer)
                (setq limit last-start scan-count (%i+ count 1))
                (go top)))
            (multiple-value-bind (spec pos) (buffer-special-indent-p buf inner)
              (when (and (fixnump spec) (%i>= spec 0))
                (if (%i<= pos scan-pos)
                  (while (and (%i<= scan-count spec)
                              (setq pos (buffer-fwd-sexp buf pos scan-pos nil t)))
                    (setq scan-count (%i+ scan-count 1)))
                  (setq scan-count (%i- scan-count 1)))
                (when (eq scan-count spec) (return (%i+ outer *fred-body-indent*)))))
            (return (backward-prefix-chars buf (real-sexp-start buf line-start end)))))))

  #+ingnore
  (defun buffer-current-sexp-start (buf &optional pos &aux char bwd-char)
    (when (null pos)(setq pos (buffer-position buf)))
    (setq char
          (cond ((eq pos (buffer-size buf)) #\Newline)
                ((buffer-lquoted-p buf pos) #\A)
                (t (buffer-char buf pos))))
    (setq bwd-char
          (cond ((eq pos 0) #\Newline)
                ((buffer-lquoted-p buf (1- pos)) #\A)
                (t (buffer-char buf (1- pos)))))
    (cond ((or (eq bwd-char #\)) (eq bwd-char #\]) (eq bwd-char #\"))
           (values (buffer-bwd-sexp buf pos) pos))
          ((or (eq char #\() (eq char #\[) (eq char #\")) pos)
          ((and (not (whitespacep bwd-char))
                (not (or (eq bwd-char #\() (eq bwd-char #\[))))
           ;we're in the middle of or just behind a symbol
           (buffer-bwd-sexp buf pos))
          ((not (whitespacep char))
           ;we're just in front of a symbol
           pos)
          (t nil)))

(defun buffer-scan-lists (buf pos end)
    (unless (<= pos end) (error "~S > ~S" pos end))
    (prog ((last-start nil) (last-end nil) (open-stack ()) start p char)
      next
      (setq start pos)
      (setq pos (buffer-forward-find-char buf "[]();\"\\|" start end))
      (when (null pos)
        (return (values (%car open-stack) last-start last-end)))
      (setq char (buffer-char buf (%i- pos 1)))
      (when (or (eq char #\() (eq char #\[))
        (setq open-stack (push (%i- pos 1) open-stack))
        (setq last-start nil last-end nil)
        (go next))
      (when  (or (eq char #\)) (eq char #\]))
        (when (null open-stack) (return (values nil nil pos)))
        (setq last-start (pop open-stack))
        (setq last-end pos)
        (go next))
      (when (eq char #\")
        (unless (setq p (buffer-fwd-skip-delimited buf "\"\\" pos end))
          (return (values (%car open-stack) (%i- pos 1) nil)))
        (setq last-start (%i- pos 1) last-end (setq pos p))
        (go next))
      (when (eq char #\|)
        (if (and (%i<= start (%i- pos 2))
                 (eq (buffer-char buf (%i- pos 2)) #\#))
          (unless (setq p (buffer-skip-fwd-#comment buf pos end))
            (return (values (car open-stack) (%i- pos 2) nil)))
          (unless (setq p (buffer-fwd-skip-delimited buf "|\\" pos end))
            (return (values (car open-stack) (%i- pos 1) nil))))
        (setq pos p)
        (go next))
      (when (eq char #\\)
        (when (eq pos end) (return (values (%car open-stack) (%i- pos 1) nil)))
        (setq pos (%i+ pos 1))
        (go next))
      (unless (setq p (buffer-forward-find-char buf #\newline pos end))
        (return (values (%car open-stack) (%i- pos 1) nil)))
      (setq pos p)
      (go next))))

#+sql
((lambda (alist)
   (dolist (cell alist)
     (setf (assq (car cell) *fred-special-indent-alist*) (cdr cell))))
  '((sql::bind-execute-with-stream . 1)
    ;(odbc::with-error-handling . 1)
    ))
