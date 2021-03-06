;; init-base.el --- Better default configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Better defaults.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))
;;  (require 'init-custom))

(global-unset-key (kbd "S-c"))
(global-set-key (kbd "S-c") 'ignore/nil)

;; Personal information
;; (setq user-full-name centaur-full-name)
;; (setq user-mail-address centaur-mail-address)


;; Environment
(when sys/linux-x-p
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil
          exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH")
          exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; Start server
(use-package server
  :ensure nil
  :defer t
  :hook (after-init . server-mode)
  :config
  (autoload 'server-running-p "server")
  (unless (server-running-p) (server-start)))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))


(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 200
              recentf-exclude '((expand-file-name package-user-dir)
                                ".cache"
                                ".cask"
                                ".elfeed"
                                "bookmarks"
                                "cache"
                                "ido.*"
                                "persp-confs"
                                "recentf"
                                "undo-tree-hist"
                                "url"
                                "COMMIT_EDITMSG\\'")))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))

(use-package simple
  :ensure nil
  :hook (window-setup . size-indication-mode)
  :init (setq column-number-mode t
              line-number-mode t
              kill-whole-line t               ; Kill line including '\n'
              line-move-visual nil
              track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
              set-mark-command-repeat-pop t)) ; Repeating C-SPC after popping mark pops it again

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

;; Misc
(setq-default fill-column 100)
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell t
      inhibit-compacting-font-caches t) ; Don’t compact font caches during GC.

(provide 'init-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-base.el ends here
