;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Customizations.

;;; Code:

;; (setq centaur-mail-address "user@email.com")   ; Email address
;; (setq centaur-proxy "127.0.0.1:1080")          ; Network proxy
;; (setq package-archives 'emacs-china)   ; Package repo: melpa, melpa-mirror, emacs-china, netease, tencent or tuna
;; (setq centaur-dashboard nil)                   ; Use dashboard at startup or not: t or nil
;; (setq centaur-lsp 'eglot)                      ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-chinese-calendar nil)            ; Use Chinese calendar or not: t or nil
;; (setq centaur-benchmark t)                     ; Enable initialization benchmark or not: t or nil

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

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

  ;;(defcustom centaur-package-archives 'Emacs-China
  ;;  "Set package archives from which to fetch."
  ;;  :type '(choice
  ;;          (const :tag "Melpa" melpa)
  ;;          (const :tag "Melpa Mirror" melpa-mirror)
  ;;          (const :tag "Emacs-China" emacs-china)
  ;;          (const :tag "Netease" netease)
  ;;          (const :tag "Tencent" tencent)
  ;;          (const :tag "Tuna" tuna)))

;;;;(defcustom centaur-dashboard t
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
