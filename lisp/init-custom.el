;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Customizations.

;;; Code:
(setq custom-file "~/.emacs.d/lisp/init-custom.el")

;; Fonts
;;(require 'cnfonts)
;; 让 cnfonts 随着 Emacs 自动生效。
;;(cnfonts-enable)
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
;; Mail(Wanderlust)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))



;;; Code:

(eval-when-compile
  (require 'init-const))

;;(require 'snails)

;;(use-package snails
;;  :ensure nil
;;  :hook (after-init .)
;;  :bind (("<f4>" . snails)))

  ;;(defcustom centaur-chinese-calendar t
  ;;  "Use Chinese calendar or not."
  ;;  :type 'boolean)
  ;;
  ;;(defcustom centaur-benchmark nil
  ;;  "Enable the init benchmark or not."
  ;;  :type 'boolean)
  ;;
  ;; Load `custom-file'
  ;; If it doesn't exist, copy from the template, then load it.
  ;;(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


  (provide 'init-custom)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "ab9456aaeab81ba46a815c00930345ada223e1e7c7ab839659b382b52437b9ea" default)))
 '(org-pomodoro-format "%s" t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-dired-dir-face ((t (:foreground nil))))
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
 '(diff-hl-change ((t (:foreground "#fabd2f"))))
 '(git-timemachine-minibuffer-author-face ((t (:inherit success))))
 '(git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
 '(macrostep-expansion-highlight-face ((t (:background "#2d2e2e"))))
 '(mode-line ((t (:foreground "#ebdbb2"))))
 '(org-ellipsis ((t (:foreground nil))))
 '(symbol-overlay-default-face ((t (:inherit (quote region)))))
 '(symbol-overlay-face-1 ((t (:inherit (quote highlight)))))
 '(symbol-overlay-face-2 ((t (:inherit (quote font-lock-builtin-face) :inverse-video t))))
 '(symbol-overlay-face-3 ((t (:inherit (quote warning) :inverse-video t))))
 '(symbol-overlay-face-4 ((t (:inherit (quote font-lock-constant-face) :inverse-video t))))
 '(symbol-overlay-face-5 ((t (:inherit (quote error) :inverse-video t))))
 '(symbol-overlay-face-6 ((t (:inherit (quote dired-mark) :inverse-video t :bold nil))))
 '(symbol-overlay-face-7 ((t (:inherit (quote success) :inverse-video t))))
 '(symbol-overlay-face-8 ((t (:inherit (quote dired-symlink) :inverse-video t :bold nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
