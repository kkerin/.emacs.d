;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Some usefule Utilities.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))
;;  (require 'init-custom))

;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :hook (after-init . which-key-mode))

;; Youdao Dictionary

;;
;; Search tools
;;

;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

;; `ripgrep'
(when (executable-find "rg")
  (use-package rg
    :defines projectile-command-map
    :hook (after-init . rg-enable-default-bindings)
    :config
    (setq rg-group-result t
          rg-show-columns t)

    (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

    (with-eval-after-load 'projectile
      (defalias 'projectile-ripgrep 'rg-project)
      (bind-key "s R" #'rg-project projectile-command-map))

    (with-eval-after-load 'counsel
      (bind-keys
       :map rg-global-map
       ("c r" . counsel-rg)
       ("c s" . counsel-ag)
       ("c p" . counsel-pt)
       ("c f" . counsel-fzf)))))

;; Docker
(use-package docker
  :bind ("C-c d" . docker)
  :init (setq docker-image-run-arguments '("-i" "-t" "--rm")
              docker-container-shell-file-name "/bin/bash"))

;; Tramp


;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

;; Persistent the scratch buffer
(use-package persistent-scratch
  :preface
  (defun my-save-buffer ()
    "Save scratch and other buffer."
    (interactive)
    (let ((scratch-name "*scratch*"))
      (if (string-equal (buffer-name) scratch-name)
          (progn
            (message "Saving %s..." scratch-name)
            (persistent-scratch-save)
            (message "Wrote %s" scratch-name))
        (save-buffer))))
  :hook (after-init . persistent-scratch-setup-default)
  :bind (:map lisp-interaction-mode-map
         ("C-x C-s" . my-save-buffer)))

;; PDF reader
(when (display-graphic-p)
  (use-package pdf-tools
    :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
    :defines pdf-annot-activate-created-annotations
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :bind (:map pdf-view-mode-map
           ("C-s" . isearch-forward))
    :init
    (setq pdf-view-midnight-colors '("#ededed" . "#21242b")
          pdf-annot-activate-created-annotations t)
    :config
    ;; WORKAROUND: Fix compilation errors on macOS.
    ;; @see https://github.com/politza/pdf-tools/issues/480
    (when sys/macp
      (setenv "PKG_CONFIG_PATH"
              "/usr/local/lib/pkgconfig:/usr/local/opt/libffi/lib/pkgconfig"))
    (pdf-tools-install t nil t t)

    ;; Recover last viewed position
    (when emacs/>=26p
      (use-package pdf-view-restore
        :hook (pdf-view-mode . pdf-view-restore-mode)
        :init (setq pdf-view-restore-filename
                    (locate-user-emacs-file ".pdf-view-restore"))))))

;; Epub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :preface
  (defun my-nov-setup ()
    (visual-line-mode 1)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5)
    (if (fboundp 'olivetti-mode) (olivetti-mode 1)))
  :hook (nov-mode . my-nov-setup))


;; Music player

;; Misc
(use-package snails
  :load-path "~/.emacs.d/site-lisp/snails")

(use-package daemons)                   ; system services/daemons
(use-package esup)                      ; Emacs startup profiler
(use-package focus)                     ; Focus on the current region
(use-package list-environment)
(use-package memory-usage)

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
