;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package org
  :ensure nil
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :preface
  (defun hot-expand (str &optional mod)
    "Expand org template."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (org-try-structure-completion)
      (when mod (insert mod) (forward-line))
      (when text (insert text))))
  ;;换行
  (add-hook 'org-mode-hook
            (lambda()
              (setq truncate-lines nil)))

  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org")
    :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("e" (hot-expand "<e") "example")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c c" . org-capture)
         ;;         ("C-c !" . org-time-stamp-inactive)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))
  :hook((org-indent-mode . (lambda()
                             (diminish 'org-indent-mode)
                             ;; WORKAROUND: Prevent text moving around while using brackets
                             ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                             (make-variable-buffer-local 'show-paren-mode)
                             (setq show-paren-mode nil))))
  :config
  ;;org-capture(Stage 1 : Collecting)
  ;;  (setq org-capture-templates
  ;;        `(("i" "inbox" entry (file ,(concat org-agenda-directory "inbox.org"))
  ;;           "* TODO %?")
  ;;          ("e" "email" entry (file+headline ,(concat org-agenda-directory "emails.org") "Emails")
  ;;           "* TODO [#A] Reply: %a :@home:@school:" :immediate-finish t)
  ;;          ("l" "link" entry (file ,(concat org-agenda-directory "inbox.org"))
  ;;           "* TODO %(org-cliplink-capture)" :immediate-finish t)
  ;;          ("z" "elfeed-link" entry (file ,(concat org-agenda-directory "inbox.org"))
  ;;           "* TODO %a\n" :immediate-finish t)
  ;;          ("w" "Weekly Review" entry (file+olp+datetree ,(concat org-agenda-directory "reviews.org"))
  ;;           (file ,(concat org-agenda-directory "templates/weekly_review.org")))))
  (setq org-capture-templates nil)
  (add-to-list 'org-capture-templates '("i" "Inbox"))
  (add-to-list 'org-capture-templates
               '("ii" "Inbox" entry (file+headline "~/Nutstore/org/abc/inbox.org" "Task")
                 "* %U - %^{heading} %^g\n %?\n"))
  (add-to-list 'org-capture-templates
               '("il" "Link" entry (file+headline "~/Nutstore/org/abc/inbox.org" "Link")
                 "* TODO %(org-cliplink-capture)" :immediate-finish t))
  (add-to-list 'org-capture-templates
               '("r" "Reading" entry
                 (file+olp "~/Nutstore/org/read.org" "Reading" "Book")
                 "* TODO %^{书名}\n%u\n" :clock-in t))
  (add-to-list 'org-capture-templates
               '("n" "Notes" entry (file "~/Nutstore/org/notes.org")
                 "* %^{heading} %t %^g\n  %?\n"))
  (add-to-list 'org-capture-templates
               '("j" "Journal" plain
                 (file+datetree "~/Nutstore/org/journal.org")" %?"))


  ;; org-agenda(Stage 2 : Processing)
  ;; org-agenda Reading Review
  (require 'find-lisp)
  (setq org-agenda-directory "~/Nutstore/org/abc/")
  (setq org-agenda-files
        (find-lisp-find-files org-agenda-directory "\.org$"))
  (setq org-agenda-reading-view
        `("r" "Reading" todo ""
          ((org-agenda-files '(,(concat org-agenda-directory "read.org"))))))
                                        ;  (add-to-list 'org-agenda-custom-commands `,org-agenda-reading-view)

                                        ;  (setq org-agenda-custom-commands
                                        ;        '(("r" todo "Reading"
                                        ;           ))

  ;; org TODO Keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d)" "CANCELLED(c@/!)")))

  (setq org-log-done 'time
        org-log-into-drawer t
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-pretty-entities nil
        org-hide-emphasis-markers t
        org-log-state-notes-insert-after-drawers nil)

  ;;tags(The Processing Step 1 : Clarifying)
  (setq org-tag-alist (quote (("@errand" . ?e)
                              ("@office" . ?o)
                              ("@home" . ?h)
                              ("@school" . ?s)
                              (:newline)
                              ("WAITING" . ?w)
                              ("HOLD" . ?H)
                              ("CANCELLED" . ?c))))

  (setq org-fast-tag-selection-single-key nil)

  ;;refile(The Processing Step 2 : Organizing)
  ;; https://github.com/syl20bnr/spacemacs/issues/3094
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '(("task.org" :level . 0)
                             ("trash.org" :level . 1)
                             ("projects.org" :maxlevel . 1)))


  (defun org-process-inbox ()
    "Called in org-agenda-mode, processes all inbox items."
    (interactive)
    (org-agenda-bulk-mark-regexp "inbox:")
    (bulk-process-entries))

  (defun my-org-agenda-set-effort (effort)
    "Set the effort property for the current headline."
    (interactive
     (list (read-string (format "Effort [%s]: " org-current-effort) nil nil org-current-effort)))
    (setq org-current-effort effort)
    (org-agenda-check-no-diary)
    (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                         (org-agenda-error)))
           (buffer (marker-buffer hdmarker))
           (pos (marker-position hdmarker))
           (inhibit-read-only t)
           newhead)
      (org-with-remote-undo buffer
        (with-current-buffer buffer
          (widen)
          (goto-char pos)
          (org-show-context 'agenda)
          (funcall-interactively 'org-set-effort nil org-current-effort)
          (end-of-line 1)
          (setq newhead (org-get-heading)))
        (org-agenda-change-all-lines newhead hdmarker))))

  (defun org-agenda-process-inbox-item ()
    "Process a single item in the org-agenda."
    (org-with-wide-buffer
     (org-agenda-set-tags)
     (org-agenda-priority)
     (my-org-agenda-set-effort)
     (org-agenda-refile nil nil t)))

  (defun org-inbox-capture ()
    (interactive)
    "Capture a task in agenda mode."
    (org-capture nil "i"))

  (require 'org-agenda)
  (define-key org-agenda-mode-map "i" 'org-agenda-clock-in)
  (define-key org-agenda-mode-map "r" 'org-process-inbox)
  (define-key org-agenda-mode-map "R" 'org-agenda-refile)
  (define-key org-agenda-mode-map "c" 'org-inbox-capture)

  ;;Clocking in
  (defun set-todo-state-next ()
    "Visit each parent task and change NEXT states to TODO"
    (org-todo "NEXT"))

  (add-hook 'org-clock-in-hook 'set-todo-state-next 'append)

  (use-package org-clock-convenience
    :bind (:map org-agenda-mode-map
           ("<S-up>" . org-clock-convenience-timestamp-up)
           ("<S-down>" . org-clock-convenience-timestamp-down)
           ("o" . org-clock-convenience-fill-gap)
           ("e" . org-clock-convenience-fill-gap-both)))

  ;;Custom agenda Commands(Stage 3 : Reviewing)
  (setq org-agenda-block-separator nil)
  (setq org-agenda-start-with-log-mode t)

  (setq org-agenda-todo-view
        `(" " "Agenda"
          ((agenda ""
                   ((org-agenda-span 'day)
                    (org-deadline-warning-days 365)))
           (todo "TODO"
                 ((org-agenda-overriding-header "To Refile")
                  (org-agenda-files '(,(concat org-agenda-directory "inbox.org")))))
           (todo "TODO"
                 ((org-agenda-overriding-header "Emails")
                  (org-agenda-files '(,(concat org-agenda-directory "emails.org")))))
           (todo "NEXT"
                 ((org-agenda-overriding-header "In Progress")
                  (org-agenda-files '(,(concat org-agenda-directory "someday.org")
                                      ,(concat org-agenda-directory "projects.org")
                                      ,(concat org-agenda-directory "next.org")))
                  ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
                  ))
           (todo "TODO"
                 ((org-agenda-overriding-header "Projects")
                  (org-agenda-files '(,(concat org-agenda-directory "projects.org")))
                  ;; (org-agenda-skip-function #'org-agenda-skip-all-siblings-but-first)
                  ))
           (todo "TODO"
                 ((org-agenda-overriding-header "One-off Tasks")
                  (org-agenda-files '(,(concat org-agenda-directory "next.org")))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
           nil)))

  (add-to-list 'org-agenda-custom-commands `,org-agenda-todo-view)

  (defun org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (or (org-current-is-todo)
                  (not (org-get-scheduled-time (point))))
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max))))))

  (defun org-current-is-todo ()
    (string= "TODO" (org-get-todo-state)))

  (defun switch-to-agenda ()
    (interactive)
    (org-agenda nil " ")
    (delete-other-windows))

  (bind-key "<f1>" 'switch-to-agenda)
  ;;Column View
  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

  ;;Org-pomodoro(Stage 4: Doing)
  (use-package org-pomodoro
    :after org
    :bind
    (:map org-agenda-mode-map
     (("I" . org-pomodoro)))
    :custom
    (org-pomodoro-format "%s"))

  ;; Enable markdown backend
  (add-to-list 'org-export-backends 'md)

  ;; Prettify UI
  (use-package org-bullets
    :if (char-displayable-p ?◉)
    :hook (org-mode . org-bullets-mode))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  (if emacs/>=26p
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  ;;  (use-package ob-rust
  ;;    :init (cl-pushnew '(rust . t) load-language-list))

  (use-package ob-ipython
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
           ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Preview
  (use-package org-preview-html
    :diminish org-preview-html-mode)

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
           ("C-<f7>" . org-tree-slide-mode)
           :map org-tree-slide-mode-map
           ("<left>" . org-tree-slide-move-previous-tree)
           ("<right>" . org-tree-slide-move-next-tree)
           ("S-SPC" . org-tree-slide-move-previous-tree)
           ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :config
    (org-tree-slide-simple-profile)
    (setq org-tree-slide-skip-outline-level 2))

  ;; Visually summarize progress
  (use-package org-dashboard))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
