;;; lsp-superbol.el --- lsp-mode LSP client for Superbol COBOL -*- lexical-binding: t; -*-
;;
;;  Copyright (c) 2023 OCamlPro SAS
;;
;;  All rights reserved.
;;  This source code is licensed under the MIT license found in the
;;  LICENSE.md file in the root directory of this source tree.

;;; Commentary:

;; lsp-mode.el LSP client for Superbol COBOL

;;; Code:

(unless (fboundp 'lsp-mode)
  (load "lsp-mode-autoloads"))

(require 'lsp-mode)
(require 'cobol-superbol-mode)

;; ---

(defgroup lsp-superbol nil
  "Settings for the Superbol Language Server for COBOL (lsp-mode)."
  :group 'lsp-mode
  :link '(url-link "https://github.com/OCamlPro/superbol-studio-oss")
  :package-version '(lsp-mode . "8.0.1"))

; This line should be conditioned to the existence of such a file
;(load (expand-file-name "lsp-superbol-customs.el"
;			(file-name-directory load-file-name)))

;; ---

(defun lsp-superbol--server-command ()
  "Startup command for the Superbol LSP language server."

  ;; instead of looking into a variable path, just expect the lsp server to
  ;; be in the PATH
  (list "superbol-free" "lsp"))

;; (lsp-dependency 'superbol-language-server
;; 		`(:system ,(executable-find (lsp-package-path 'superbol-language-server))))
;; '(:system "superbol-language-server"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-superbol--server-command)
  :priority 0
  :activation-fn (lsp-activate-on "superbol" "cobol" "COBOL")
  :server-id 'superbol-lsp
  ))

;; ---

(add-to-list 'lsp-language-id-configuration '(cobol-superbol-mode . "cobol"))

(defun lsp-superbol--start ()
  "Superbol LSP startup function for lsp-mode"

  ;; Enable semantic tokens
  (set (make-local-variable 'lsp-semantic-tokens-enable) t)

  ;; Actually start the LSP server
  (lsp)

  ;; Turn on fontification
  (funcall font-lock-fontify-buffer-function)
  )

(add-hook 'cobol-superbol-mode-hook #'lsp-superbol--start)

;; ---

(lsp-consistency-check lsp-superbol)

(provide 'lsp-superbol)

;;; lsp-superbol.el ends here
