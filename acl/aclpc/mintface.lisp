;; Define the dialog :Mini-Listener

 

(in-package :common-lisp-user)

 

(defvar *mini-listener* nil)

 

;; Return the window, creating it the first time or when it's closed.

;; Use only this function if you need only one instance.

(defun mini-listener ()

   (if (windowp *mini-listener*) *mini-listener* 

      (setq *mini-listener* (make-mini-listener))))

 

;; Create an instance of the window.

;; Use this if you need more than one instance.

(defun make-mini-listener ()

   (setq *loaded-but-uncreated-windows* 

      (delete 'mini-listener *loaded-but-uncreated-windows*))

   (let (window-0 window-1 window-2 window-3 window-4)

      (setq window-0 

         (open-dialog 

            (list 

               (make-dialog-item 

                  :widget 'multi-line-lisp-text 

                  :name :lisp-text-1 

                  :title "Multi Line Lisp Text 1" 

                  :box (make-box 7 186 333 264) 

                  :tabstop t 

                  :groupstart t 

                  :scrollable :vertical 

                  :font (make-font nil :courier 13 nil))

               (make-dialog-item 

                  :widget 'button 

                  :name :step 

                  :title "Step" 

                  :value t 

                  :box (make-box 69 145 115 183) 

                  :tabstop nil 

                  :groupstart nil 

                  :set-value-fn 'step-set-value-fn 

                  :font (make-font nil :arial 16 '(:bold)))

               (make-dialog-item 

                  :widget 'button 

                  :name :flush-history 

                  :title "Flush" 

                  :value t 

                  :box (make-box 208 167 269 183) 

                  :tabstop nil 

                  :groupstart nil 

                  :set-value-fn 'flush-set-value-fn 

                  :font (make-font nil :arial 16 '(:bold)))

               (make-dialog-item 

                  :widget 'button 

                  :name :clear 

                  :title "Clear" 

                  :value t 

                  :box (make-box 208 145 269 161) 

                  :tabstop nil 

                  :groupstart nil 

                  :set-value-fn 'clear-set-value-fn 

                  :font (make-font nil :arial 16 '(:bold)))

               (make-dialog-item 

                  :widget 'single-item-list 

                  :name :single-item-list-1 

                  :title "Single Item List 1" 

                  :box (make-box 7 3 333 142) 

                  :tabstop t 

                  :groupstart t 

                  :set-value-fn 'listener-history-set-value-fn 

                  :key 'capitalize-object 

                  :font (make-font nil :arial 16 nil))

               (make-dialog-item 

                  :widget 'button 

                  :name :eval-1 

                  :title "Run" 

                  :value t 

                  :box (make-box 7 145 53 183) 

                  :tabstop nil 

                  :groupstart nil 

                  :set-value-fn 'listener-input-set-value-fn 

                  :font (make-font nil :arial 16 '(:bold)) 

                  :value :exit 

                  :listen nil)

               (make-dialog-item 

                  :widget 'button 

                  :name :help 

                  :title "Help" 

                  :value t 

                  :box (make-box 131 145 190 161) 

                  :tabstop nil 

                  :groupstart nil 

                  :set-value-fn 'help-set-value-fn 

                  :font (make-font nil :arial 16 '(:bold)))

               (make-dialog-item 

                  :widget 'button 

                  :name :exit 

                  :title "Exit" 

                  :box (make-box 286 145 333 183) 

                  :tabstop nil 

                  :groupstart nil 

                  :set-value-fn 'exit-set-value-fn 

                  :font (make-font nil :arial 16 '(:bold)))

               (make-dialog-item 

                  :widget 'button 

                  :name :demo 

                  :title "Demo" 

                  :box (make-box 131 167 190 183) 

                  :tabstop nil 

                  :groupstart nil 

                  :set-value-fn 'demo-set-value-fn 

                  :font (make-font nil :arial 16 '(:bold))))

            'dialog *screen* 

            :name :mini-listener 

            :title "Mini Listener" 

            :font (make-font :swiss :system 16 '(:bold)) 

            :window-state :shrunk 

            :window-border :frame 

            :left-attachment nil 

            :top-attachment nil 

            :right-attachment nil 

            :bottom-attachment nil 

            :user-movable t 

            :user-resizable t 

            :user-closable t 

            :user-shrinkable t 

            :user-scrollable nil 

            :overlapped nil 

            :background-color (make-rgb :red 192 :green 192 :blue 192) 

            :pop-up-p nil 

            :window-interior (make-box 248 36 587 302)))

      (setf (window-editable-p window-0) t)

      (setf (getf (stream-plist window-0) :path) 

         (namestring (translate-logical-pathname "aclpc:mintface.lisp")))

      (setf (getf (stream-plist window-0) :logical-path) 

         "aclpc:mintface.lisp")

      (setf (getf (stream-plist window-0) :startup-state) nil)

      (setf (getf (stream-plist window-0) :top-level-p) t)

      (setf (help-string window-0) (delete #\Newline nil))

      (setf (getf (stream-plist window-0) :package) nil)

      nil

      (let* ((box (getf *window-exteriors* (object-name window-0))))

         (when box (reshape-window-exterior window-0 box)))

      (show-window window-0 nil)

      window-0))

(unless (windowp *mini-listener*) 

   (pushnew 'mini-listener *loaded-but-uncreated-windows*))

