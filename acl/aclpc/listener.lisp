;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: USER; Base: 10 -*-



;; Fill in this code and save to it some file.

(in-package :common-lisp-user)



(defvar *listener-history-size* 64)



(defun find-dialog-item (dialog item-name)

   (dolist (item (dialog-items dialog))

      (when (eq item-name (slot-value item 'cg::name))

         (return item))))



;; Set-value-fn function for "Run" button

(defun listener-input-set-value-fn (widget new-value old-value)

  (let* ((dialog (dialog-item-dialog widget))

         (text-field (find-dialog-item dialog :lisp-text-1))

	 (history (find-dialog-item dialog :single-item-list-1))

	 (eval (find-dialog-item dialog :eval-1))

	 value)

    (setq value (dialog-item-value text-field))

					;(print value)

    (when value

	  (setf (getf (dialog-item-plist eval) :value) value

		(getf (dialog-item-plist eval) :listen) t)

	  (let ((values (dialog-item-range history)))

	    (setq values (remove value values :test #'equal))

	    (push value values)

	    (let ((over (- (length values) *listener-history-size*)))

	      (if (plusp over)

		  (setq values (nbutlast values over))))

	    (set-dialog-item-range history values)

	    (set-dialog-item-value history value))))

  (values t ;; Accept the new value

	  nil))  ;; Don't exit the dialog (if a pop-up)



;;; Set-value-fn function for history pane

(defun listener-history-set-value-fn (widget new-value old-value)

  (let* ((dialog (dialog-item-dialog widget))

         (text-field (find-dialog-item dialog :lisp-text-1))

	 (history (find-dialog-item dialog :single-item-list-1))

	 value)

    (setq value (dialog-item-value history))

					;(print value)

    (set-dialog-item-range history (dialog-item-range history))

    (set-dialog-item-value text-field value))

  (values t ;; Accept the new value

	  nil))  ;; Don't exit the dialog (if a pop-up)



(defun listener-listen ()

  (let ((eval (find-dialog-item (mini-listener) :eval-1)))

    (and eval (getf (dialog-item-plist eval) :listen))))



(defun listener-read ()

  (let ((eval (find-dialog-item (mini-listener) :eval-1))

	value listen)

    (when eval

	  (setq value (getf (dialog-item-plist eval) :value)

		listen (getf (dialog-item-plist eval) :listen))

	  (setf (getf (dialog-item-plist eval) :listen) nil)

	  (if listen (values value listen)))))



(defun listener-write (value)

  (let ((edit-field (find-dialog-item (mini-listener) :lisp-text-1)))

    (set-dialog-item-value edit-field value)))



(defun listener-value ()

  (let ((edit-field (find-dialog-item (mini-listener) :lisp-text-1)))

    (dialog-item-value edit-field)))



(defun listener-clear ()

  (let ((edit-field (find-dialog-item (mini-listener) :lisp-text-1)))

    (set-dialog-item-value edit-field nil)))



(defun history-clear ()

  (let ((history (find-dialog-item (mini-listener) :single-item-list-1)))

    (set-dialog-item-range history nil)

    (set-dialog-item-value history nil)))



(defun flush-set-value-fn (widget new-value old-value)

  (let* ((dialog (dialog-item-dialog widget))

	 (history (find-dialog-item dialog :single-item-list-1)))

    (set-dialog-item-range history nil)

    (set-dialog-item-value history nil))

  (values t ;; Accept the new value

	  nil))  ;; Don't exit the dialog (if a pop-up)



(defun clear-set-value-fn (widget new-value old-value)

  (let* ((dialog (dialog-item-dialog widget))

         (text-field (find-dialog-item dialog :lisp-text-1))

         (history (find-dialog-item dialog :single-item-list-1)))

    (set-dialog-item-value history nil)

    (set-dialog-item-range text-field nil))

  (values t ;; Accept the new value

	  nil))  ;; Don't exit the dialog (if a pop-up)



(defun step-set-value-fn (widget new-value old-value)

  (listener-input-set-value-fn widget new-value old-value)

  (clear-set-value-fn widget new-value old-value))



(defun help-set-value-fn (widget new-value old-value)

  (let* ((dialog (dialog-item-dialog widget))

         (text-field (find-dialog-item dialog :lisp-text-1))

	 value)

    (setf (dialog-item-value text-field) :help)

    (listener-input-set-value-fn widget new-value old-value)))



(defun exit-set-value-fn (widget new-value old-value)

  (let* ((dialog (dialog-item-dialog widget))

         (text-field (find-dialog-item dialog :lisp-text-1))

	 value)

    (setf (dialog-item-value text-field) :exit)

    (listener-input-set-value-fn widget new-value old-value)))



(defun demo-set-value-fn (widget new-value old-value)

  (let* ((dialog (dialog-item-dialog widget))

         (text-field (find-dialog-item dialog :lisp-text-1))

	 value)

    (setf (dialog-item-value text-field) '(demo-systest))

    (listener-input-set-value-fn widget new-value old-value)

    (clear-set-value-fn widget new-value old-value)))

