;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Customizations.

;;; Code:
(setq custom-file "~/.emacs.d/lisp/init-custom.el")

;; Fonts
(when (display-graphic-p)
  ;; Set default font
  (catch 'loop
    (dolist (font '("SF Mono" "Hack" "Source Code Pro" "Fira Code"
                    "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas"))
      (when (member font (font-family-list))
        (set-face-attribute 'default nil :font font :height 100))
      (throw 'loop t))))

;; Specify font for all unicode characters
(catch 'loop
  (dolist (font '("Symbola" "Apple Symbols" "Symbol"))
    (when (member font (font-family-list))
      (set-fontset-font t 'unicode font nil 'prepend)
      (throw 'loop t))))

;; Specify font for Chinese characters
(catch 'loop
  (dolist (font '("wqy-microhei" "Microsoft Yahei"))
    (when (member font (font-family-list))
      (set-fontset-font t '(#x4e00 . #x9fff) font)
      (throw 'loop t))))

;; Mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587))



;;; Code:

(eval-when-compile
  (require 'init-const))

;;(require 'snails)

;;(use-package snails
;;  :ensure nil
;;  :hook (after-init .)
;;  :bind (("<f4>" . snails)))


;;(defcustom centaur-dashboard t
;;  "Use dashboard at startup or not.
;;If Non-nil, use dashboard, otherwise will restore previous session."
;;  :group 'centaur
;;  :type 'boolean)
;;
;;(defcustom centaur-lsp 'lsp-mode
;;  "Set language server."
;;  :group 'centaur
;;  :type '(choice
;;          (const :tag "LSP Mode" 'lsp-mode)
;;          (const :tag "eglot" 'eglot)
;;          nil))
;;
;;(defcustom centaur-chinese-calendar t
;;  "Use Chinese calendar or not."
;;  :group 'centaur
;;  :type 'boolean)
;;
;;(defcustom centaur-benchmark nil
;;  "Enable the init benchmark or not."
;;  :group 'centaur
;;  :type 'boolean)
;;
;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
;;(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;(let ((custom-template-file
;;       (expand-file-name "custom-template.el" user-emacs-directory)))
;;  (if (and (file-exists-p custom-template-file)
;;           (not (file-exists-p custom-file)))
;;      (copy-file custom-template-file custom-file)))

;;(if (file-exists-p custom-file)
;;    (load custom-file))

;; Load `custom-post.el'
;; Put personal configurations to override defaults here.
;;(add-hook 'after-init-hook
;;          (lambda ()
;;            (let ((file
;;                   (expand-file-name "custom-post.el" user-emacs-directory)))
;;              (if (file-exists-p file)
;;                  (load file)))))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
