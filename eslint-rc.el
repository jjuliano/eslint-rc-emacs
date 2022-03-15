;;; eslint-rc.el --- Minor mode for eslint to use local rc rules

;; Copyright (C) 2022-2023  Joel Bryan Juliano

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; Author: Joel Bryan Juliano <joelbryan dot juliano at gmail dot com>
;; Created: 15 March 2022
;; URL: https://github.com/jjuliano/eslint-rc-emacs
;; Package-Requires: ((emacs "24.3") (eslint-fix "0.1.0"))
;; Version: 0.1.0
;; Keywords: convenience edit js ts rc eslintrc eslint-rc eslint eslint-fix

;;; Commentary:

;; Formats your JavaScript & Typescript code using ESLint and defined rc rules.

;; Usage
;; -----
;;
;;     Running `eslint-rc` will look on the current project's folder for any
;;     defined `.eslintrc.*` and `.eslintignore` rules and automatically pass
;;     them to ESLint on the current buffer.
;;
;;       M-x eslint-rc
;;
;;     To automatically format after saving:
;;
;;       (add-hook 'typescript-mode-hook 'eslint-rc-mode)
;;       (add-hook 'js2-mode-hook 'eslint-rc-mode)
;;       (add-hook 'web-mode-hook 'eslint-rc-mode)

;;; Code:

(require 'eslint-fix)
(require 'cl-lib)

(defgroup eslint-rc nil
  "Minor mode to format JS code on file save using local rc rules"
  :group 'languages
  :prefix 'eslint-rc
  :link '(url-link :tag "Repository"
                   "https://github.com/jjuliano/eslint-rc-emacs"))

(defcustom eslint-rc-use-package-json nil
  "If non-nil, `eslint-rc' will use `package.json' file."
  :type 'boolean
  :group 'eslint-rc)

(defcustom eslint-rc-use-eslintignore t
  "If non-nil, `eslint-rc' will use `.eslintignore' file."
  :type 'boolean
  :group 'eslint-rc)

(defcustom eslint-rc-use-node-modules-bin t
  "If non-nil, `eslint-rc' will search `node_modules' for `eslint' bin."
  :type 'boolean
  :group 'eslint-rc)

(defun eslint-rc ()
  "Format the current buffer using `eslint-rc' using the defined rc rules."
  (interactive)

  (let (args)
    (cl-letf (((symbol-function 'eslint-rc--add-file)
               (lambda (file) ;; Builds and store the local rc FILE list if found.
                 (list :file (concat (locate-dominating-file default-directory file)
                                     file))))
              ((symbol-function 'eslint-rc--find-file)
               (lambda (file) ;; Search the local base directory for local FILE and store to list.
                 (if (bound-and-true-p file)
                     (if (locate-dominating-file default-directory file)
                         (append (eslint-rc--add-file file))))))
              ((symbol-function 'eslint-rc--build-config)
               (lambda (file) ;; Build the config arguments
                 (if (eslint-rc--find-file file)
                     (cond ((string= file ".eslintignore") ;; check if `.eslintignore' will be skipped
                            (if (bound-and-true-p eslint-rc-use-eslintignore)
                                (push (concat "--ignore-path " (concat (locate-dominating-file
                                                                        default-directory file) file))
                                      args)))
                           ;; check if `package.json' will be skipped
                           ((string= file "package.json")
                            (if (bound-and-true-p eslint-rc-use-package-json)
                                (push (concat "--config " (concat (locate-dominating-file
                                                                   default-directory file) file))
                                      args)))
                           ;; append the rc file to the list when found
                           (t (push (concat "--config " (concat (locate-dominating-file
                                                                 default-directory file) file))
                                    args)))))))

      (mapc (lambda (rc)
                (eslint-rc--build-config rc))
              (list ".eslintrc.js"
                    ".eslintrc.cjs"
                    ".eslintrc.yaml"
                    ".eslintrc.yml"
                    ".eslintrc.json"
                    "package.json"
                    ".eslintignore")))

    ;; only specify eslint-fix-options-options if files are found
    (if (bound-and-true-p args)
        (setq eslint-fix-options (remove nil
                                         `(,(unless (bound-and-true-p eslint-rc-use-eslintignore)
                                              "--no-ignore")
                                           ,(mapconcat #'identity args " "))))
      ;; cleanup args
      (setq eslint-fix-options '())))

  ;; check if prefer to use local eslint via `npm'
  (progn
    (if eslint-rc-use-node-modules-bin
        (let* ((file-name (or (buffer-file-name) default-directory))
               (root (locate-dominating-file file-name "node_modules"))
               (eslint (and root
                            (expand-file-name "node_modules/.bin/eslint" root))))
          (if (and eslint (file-executable-p eslint))
              (setq eslint-fix-executable eslint)))))

  ;; finally call eslint --fix
  (eslint-fix)
  (message "Applied `%s' --fix with args `%s'" eslint-fix-executable
           (if (bound-and-true-p eslint-fix-options)
               eslint-fix-options
             "none")))

;;;###autoload
(define-minor-mode eslint-rc-mode
  "Runs eslint on file save using local rc rules when this mode is turned on"
  :lighter " Eslint-RC"
  :global nil
  ;; Toggle eslint-rc-mode
  (if eslint-rc-mode
      (add-hook 'after-save-hook #'eslint-rc nil t)
    (remove-hook 'after-save-hook #'eslint-rc t)))

(declare-function eslint-rc--build-config "eslint-rc" (file))
(declare-function eslint-rc--add-file "eslint-rc" (file))
(declare-function eslint-rc--find-config "eslint-rc" (file))

(provide 'eslint-rc)
;;; eslint-rc.el ends here
