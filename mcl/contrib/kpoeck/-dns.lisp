(in-package :ccl)

(defun tcp-host-cname (host-address)
  (if (integerp host-address)
    (setq host-address (tcp-host-address host-address)))
  (concatenate 'string (tcp-addr-to-str host-address) "."))

#|

(setq http::*standard-log-directory* (full-pathname "platte;"))

(setq http::*log* nil)
(http::ensure-current-log)
(tcp-host-cname (%tcp-getaddr))
|#
