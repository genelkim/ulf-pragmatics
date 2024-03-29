;; ASDF system definition for ULF Sanity Checker.
;; Original version of this file written for Epilog.
(require 'asdf)

;; Try to be quiet.
;(setf (sys:gsgc-switch :print) nil)
;(setf (sys:gsgc-switch :stats) nil)
;(setf (sys:gsgc-switch :verbose) nil)
(defvar *asdf-verbose* nil)

;; avoids saving compiled files in special local cache.
(if (fboundp (find-symbol "DISABLE-OUTPUT-TRANSLATIONS" 'asdf))
  (funcall (find-symbol "DISABLE-OUTPUT-TRANSLATIONS" 'asdf)))

;; from http://www.cliki.net/asdf
;;; If the fasl was stale, try to recompile and load (once). Since only SBCL
;;; has a separate condition for bogus fasls we retry on any old error
;;; on other lisps. Actually, Allegro has a similar condition, but it's
;;; unexported.  Works nicely for the ACL7 upgrade, though.
;;; CMUCL has an invalid-fasl condition as of 19c.
(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  (handler-case (call-next-method o c)
    (#+sbcl sb-ext:invalid-fasl
     #+allegro excl::file-incompatible-fasl-error
     #+lispworks conditions:fasl-error
     #+cmu ext:invalid-fasl
     #-(or sbcl allegro lispworks cmu) error ()
     (asdf:perform (make-instance 'asdf:compile-op) c)
     (call-next-method))))
(asdf:perform 'asdf:load-op 'asdf:cl-source-file)
;(asdf:initialize-source-registry
;  `(:source-registry
;      :ignore-inherited-configuration
;      (:tree ,(namestring (merge-pathnames "./")))))

;; Adds the directory containing load.lisp to the "PATH" of asdf, so
;; we can load *ulf-sanity-checker* from any directory.
(pushnew (make-pathname :directory (pathname-directory *load-truename*))
         asdf:*central-registry*)

;; Be quick.
(proclaim '(optimize (speed 1) (safety 1) (space 1) (debug 3)))

(locally
  (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
  (handler-bind
    (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
    ;; stuff that emits redefinition-warning's
;; Choose between the following two lines depending on
;; whether you want the files compiled into FASLs or not:
(asdf:operate 'asdf:load-op 'ulf-pragmatics) ;; Compile and load as necessary
;(asdf:operate 'asdf:load-source-op 'ulf-pragmatics) ;; Doesn't compile
))

(setf ulf-pragmatics::*debug-ulf-pragmatics* nil)

