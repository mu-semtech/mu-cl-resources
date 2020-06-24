(in-package :mu-cl-resources)

;;;; 
;; prefixes-parsing.lisp
;;
;; This file consumes functions from domain-parsing.lisp

(defun read-prefixes-file (relative-path)
  "Reads the JSON file from a relative path."
  (let ((type (pathname-type relative-path))
        (pathname (asdf:system-relative-pathname
                   :mu-cl-resources
                   (s+ "configuration/" relative-path))))
    (cond ((or (string= type "js")
               (string= type "json"))
           (read-prefixes-json-file-from-path pathname))
          (t
           (error "Could not find json prefixes file")))))

(defun read-prefixes-json-file-from-path (file)
  "Imports contents from the json file specified by
   file."
  (funcall (alexandria:compose
            #'read-prefixes-json-string
            #'alexandria:read-file-into-string)
           file))

(defun read-prefixes-json-string (string)
  "Imports the json string as a prefixes file"
  (funcall (alexandria:compose
            #'import-prefixes-from-jsown
            #'jsown:parse)
           string))

(defun import-prefixes-from-jsown (jsown-prefixes)
  "Imports the domain from the jsown file"
  (let ((version (jsown:val jsown-prefixes "version")))
    (cond
      ((string= version "0.1")
       (if (jsown:keyp jsown-prefixes "prefixes")
           (map-jsown-object
            (jsown:val jsown-prefixes "prefixes")
            (lambda (key value)
              (add-prefix key value)))
           (error "Did not find \"prefixes\" key in json prefixes file")))
      (t (error "Don't know version ~A of prefixes" version)))))
