;;; lsp-superbol.el --- Eglot LSP client for Superbol COBOL -*- lexical-binding: t; -*-
;;
;;  Copyright (c) 2023 OCamlPro SAS
;;
;;  All rights reserved.
;;  This source code is licensed under the MIT license found in the
;;  LICENSE.md file in the root directory of this source tree.

;;; Commentary:

;; Eglot LSP client for Superbol COBOL

;;; Code:

(unless (fboundp 'eglot)
  (load "eglot-autoloads"))

(require 'eglot)
(require 'cobol-superbol-mode)

(defun eglot-superbol--start ()
  "Superbol LSP startup function for Eglot"

  ;; Actually start the LSP server
  (eglot-ensure)

  ;; Turn on fontification (even if minimal)
  (funcall font-lock-fontify-buffer-function))

(add-to-list 'eglot-server-programs
	     '(cobol-superbol-mode    . ("superbol-free" "lsp")))

;; Autostart the LSP when entering superbol-mode
(add-hook 'cobol-superbol-mode-hook #'eglot-superbol--start)

(provide 'eglot-superbol)

;;; eglot-superbol.el ends here
