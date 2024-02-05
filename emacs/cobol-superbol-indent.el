;;; cobol-superbol-indent.el --- automatic indentation with cobol-superbol-indent
;;
;; Copyright 2012-2013 OCamlPro

;; Keywords: ocaml languages
;; URL: http://www.typerex.org/cobol-superbol-indent.html

;; All rights reserved.This file is distributed under the terms of the
;; GNU Lesser General Public License version 2.1 with linking
;; exception.

;; TypeRex is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; Lesser GNU General Public License for more details.
;;

;;; Commentary:

;; Description:
;; cobol-superbol-indent is a simple tool and library to indent OCaml code.

;; Installation:
;; You need cobol-superbol-indent installed on you system to work.

;; Usage:
;; Eval this file to automatically use cobol-superbol-indent on caml/tuareg buffers.

;;; Code:

(require 'cl)

(defgroup cobol-superbol-indent nil
  "cobol-superbol-indent OCaml indenter binding configuration"
  :group 'languages)

(defcustom cobol-superbol-indent-path "superbol-free"
  "*Path to access the cobol-superbol-indent command"
  :group 'cobol-superbol-indent :type '(file))

(defcustom cobol-superbol-indent-untabify nil
  "Send the buffer `untabify'ed to cobol-superbol-indent. Allows partial
indent even with tabs present.

Tabs are not replaced in the buffer except on lines getting an
indentation change."
  :group 'cobol-superbol-indent
  :type '(bool))

(defun superbol-in-indentation-p ()
  "Tests whether all characters between beginning of line and point
are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun cobol-superbol-indent-args (start-line end-line)
  (list
   "indent"
   "file"
   "--numeric"
   "--lines" (format "%d-%d" start-line end-line)
   (if cobol-free-format-mode
       "--free"
     (if cobol-variable-format-mode
	 "--source-format=variable"
       "--source-format=fixed"))
   ))

(defun cobol-superbol-indent-file-to-string (file)
  (replace-regexp-in-string
   "\n$" ""
   (with-temp-buffer (insert-file-contents file)
                     (buffer-string))))

(defmacro cobol-superbol-indent--with-untabify (&rest body)
  "If there are tabs and `cobol-superbol-indent-untabify', create a
temporary buffer containing the current buffer's contents
untabified and evaluate BODY there like `progn'. See also
`with-temp-buffer'. Otherwise evaluate BODY in the current
buffer."
  (declare (indent 0) (debug t))
  (let ((buf (make-symbol "buf")))
    `(if (not (and cobol-superbol-indent-untabify
                   (save-excursion (goto-char (point-min)) (search-forward "\t" nil t))))
         (progn ,@body)
       (let ((,buf (generate-new-buffer " *cobol-superbol-indent*")))
         (unwind-protect
             (progn
               (copy-to-buffer ,buf (point-min) (point-max))
               (with-current-buffer ,buf
                 (untabify (point-min) (point-max))
                 (progn ,@body)))
           (and (buffer-name ,buf) (kill-buffer ,buf)))))))

(defun cobol-superbol-indent-region (start end)
  (interactive "r")
  (let*
      ((start-line (line-number-at-pos start))
       (end-line (line-number-at-pos end))
       (errfile (make-temp-name (concat temporary-file-directory "cobol-superbol-indent-error")))
       (indents-str
        (with-output-to-string
          (cobol-superbol-indent--with-untabify
            (if (/= 0
                    (apply 'call-process-region
                           (point-min) (point-max) cobol-superbol-indent-path nil
                           (list standard-output errfile) nil
                           (cobol-superbol-indent-args start-line end-line)))
                (error "Can't indent: %s returned failure" cobol-superbol-indent-path)))))
       (indents (mapcar 'string-to-number (split-string indents-str))))
    (when (file-exists-p errfile)
      (message (cobol-superbol-indent-file-to-string errfile))
      (delete-file errfile))
    (save-excursion
      (goto-char start)
      (mapcar
       #'(lambda (indent)
	   (when (>= indent 0)
	     (indent-line-to indent))
	   (forward-line))
       indents))
    (when (superbol-in-indentation-p) (back-to-indentation))))

(defun cobol-superbol-indent-line ()
  (interactive nil)
  (cobol-superbol-indent-region (point) (point)))

(defun cobol-superbol-indent-buffer ()
  (interactive nil)
  (cobol-superbol-indent-region 0 (buffer-size)))

;;;###autoload
(defun superbol-setup-indent ()
  (interactive nil)
  (let ((buffer-extension (and (buffer-file-name)
                               (file-name-extension (buffer-file-name)))))
    (set 'indent-tabs-mode nil)
    (set (make-local-variable 'tab-width) 8)
    (set (make-local-variable 'indent-line-function) #'cobol-superbol-indent-line)
    (set (make-local-variable 'indent-region-function) #'cobol-superbol-indent-region)))

(defun cobol-superbol-indent-setup ()
  (superbol-setup-indent)
  (local-unset-key "\t"))

(add-hook 'cobol-superbol-mode-hook 'superbol-setup-indent t)

(provide 'cobol-superbol-indent)

;;; cobol-superbol-indent.el ends here
