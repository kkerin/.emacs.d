;; init-shell.el --- Initialize shell configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Shell configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package shell
  :ensure nil
  :commands comint-send-string comint-simple-send comint-strip-ctrl-m
  :preface
  (defun n-shell-simple-send (proc command)
    "Various PROC COMMANDs pre-processing before sending to shell."
    (cond
     ;; Checking for clear command and execute it.
     ((string-match "^[ \t]*clear[ \t]*$" command)
      (comint-send-string proc "\n")
      (erase-buffer))
     ;; Checking for man command and execute it.
     ((string-match "^[ \t]*man[ \t]*" command)
      (comint-send-string proc "\n")
      (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
      (setq command (replace-regexp-in-string "[ \t]+$" "" command))
      ;;(message (format "command %s command" command))
      (funcall 'man command))
     ;; Send other commands to the default handler.
     (t (comint-simple-send proc command))))
  (defun n-shell-mode-hook ()
    "Shell mode customizations."
    (local-set-key '[up] 'comint-previous-input)
    (local-set-key '[down] 'comint-next-input)
    (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
    (setq comint-input-sender 'n-shell-simple-send))
  :hook ((shell-mode . ansi-color-for-comint-mode-on)
         (shell-mode . n-shell-mode-hook))
  :config
  (setq system-uses-terminfo nil)       ; don't use system term info

  (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)

  ;; ANSI & XTERM 256 color support
  (use-package xterm-color
    :init
    (setenv "TERM" "xterm-256color")
    (setq comint-output-filter-functions
          (remove 'ansi-color-process-output comint-output-filter-functions))

    (add-hook 'shell-mode-hook
              (lambda ()
                ;; Disable font-locking in this buffer to improve performance
                (font-lock-mode -1)
                ;; Prevent font-locking from being re-enabled in this buffer
                (make-local-variable 'font-lock-function)
                (setq font-lock-function (lambda (_) nil))
                (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))))

;; Better term
;; @see https://github.com/akermu/emacs-libvterm#installation
(when (and (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
    :init (defalias #'term #'vterm)))

;; Shell Pop
;;(use-package shell-pop
;;  :bind ([f9] . shell-pop)
;;  :init
;;  (setq shell-pop-shell-type (cond
;;                              (sys/win32p
;;                               '("eshell" "*eshell*" (lambda () (eshell))))
;;                              ((fboundp 'vterm)
;;                               '("vterm" "*vterm*" (lambda () (vterm))))
;;                              (t
;;                               '("ansi-term" "*ansi-term*"
;;                                 (lambda () (ansi-term shell-pop-term-shell)))))))

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
