;; init-buffer.el --- Initialize ibuffer configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; IBuffer configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package ibuffer
  :ensure nil
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-auto-mode-match?
              all-the-icons-faicon)
  :commands ibuffer-find-file
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))

  ;; Display buffer icons on GUI
  (when (display-graphic-p)
    ;; For alignment, the size of the name field should be the width of an icon
    (define-ibuffer-column icon (:name "  ")
      (let ((icon (if (and (buffer-file-name)
                           (all-the-icons-auto-mode-match?))
                      (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                    (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
        (if (symbolp icon)
            (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
          icon)))

    (setq ibuffer-formats `((mark modified read-only ,(if emacs/>=26p 'locked "")
                                  ;; Here you may adjust by replacing :right with :center or :left
                                  ;; According to taste, if you want the icon further from the name
                                  " " (icon 2 2 :left :elide)
                                  ,(propertize " " 'display `(space :align-to 8))
                                  (name 18 18 :left :elide)
                                  " " (size 9 -1 :right)
                                  " " (mode 16 16 :left :elide) " " filename-and-process)
                            (mark " " (name 16 -1) " " filename))))

  (with-eval-after-load 'counsel
    (defun my-ibuffer-find-file ()
      (interactive)
      (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                 (if (buffer-live-p buf)
                                     (with-current-buffer buf
                                       default-directory)
                                   default-directory))))
        (counsel-find-file default-directory)))
    (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))

  ;; Group ibuffer's list by project root
  (use-package ibuffer-projectile
    :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
    :hook ((ibuffer . (lambda ()
                        (ibuffer-projectile-set-filter-groups)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic)))))
    :config
    (setq ibuffer-projectile-prefix
          (if (display-graphic-p)
              (concat
               (all-the-icons-octicon "file-directory"
                                      :face ibuffer-filter-group-name-face
                                      :v-adjust -0.05
                                      :height 1.25)
               " ")
            "Project: "))))

(provide 'init-ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
