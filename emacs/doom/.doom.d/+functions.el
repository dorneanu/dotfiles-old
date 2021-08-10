;;; +functions.el -*- lexical-binding: t; -*-

(defun dorneanu/hugo-add-slug ()
 (interactive)
 (org-set-property "EXPORT_FILE_NAME"
 (org-hugo-slug (org-get-heading :no-tags :no-todo))))

(defun dorneanu/org-link-make-string (link &optional description)
  "Make a bracket link, consisting of LINK and DESCRIPTION.
LINK is escaped with backslashes for inclusion in buffer."
  (let* ((zero-width-space (string ?\x200B))
         (description
          (and (org-string-nw-p description)
               ;; Description cannot contain two consecutive square
               ;; brackets, or end with a square bracket.  To prevent
               ;; this, insert a zero width space character between
               ;; the brackets, or at the end of the description.
               (replace-regexp-in-string
                "\\(]\\)\\(]\\)"
                (concat "\\1" zero-width-space "\\2")
                (replace-regexp-in-string "]\\'"
                                          (concat "\\&" zero-width-space)
                                          (org-trim description))))))
    (if (not (org-string-nw-p link)) description
      (format "[[%s|%s]]"
              (org-link-escape link)
              (if description (format "%s" description) "")))))

(defun dorneanu/org-web-tools--org-link-for-url (url)
  "Return Org link to URL using title of HTML page at URL.
If URL is not given, look for first URL in `kill-ring'.  If page
at URL has no title, return URL."
  (let* ((html (org-web-tools--get-url url))
         (title (org-web-tools--html-title html)))
    (if title
        (dorneanu/org-make-link-string url title)
      (message "HTML page at URL has no title")
      url)))

;; https://blog.lazkani.io/posts/text-editors/bookmark-with-org-capture/
(defun org-web-tools-insert-link-for-clipboard-url ()
  "Extend =org-web-tools-inster-link-for-url= to take URL from clipboard or kill-ring"
  (interactive)
  (org-web-tools--org-link-for-url (org-web-tools--get-first-url)))


;; Setup markdown preview impatient mode
;; from: https://blog.bitsandbobs.net/blog/emacs-markdown-live-preview/
(defun my-markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

(defun my-markdown-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'my-markdown-filter)
  (imp-visit-buffer))

(defun +remap-faces-at-start-present ()
  (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
                                     (org-verbatim (:height 1.75) org-verbatim)
                                     (org-block (:height 1.25) org-block)))
  (hide-mode-line-mode 1)
  (centaur-tabs-mode 0)
  )

(defun +remap-faces-at-start-present-term ()
  (interactive)
  (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
                                     (org-verbatim (:height 1.75) org-verbatim)
                                     (org-block (:height 1.25) org-block)))
  )

(defun +remap-faces-at-stop-present ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (hide-mode-line-mode 0)
  (centaur-tabs-mode 1)
  )

;; custom functions for getpockt
(defun dorneanu/pocket-2share ()
  "Show getpocket items to be shared"
  (interactive)
  (pocket-reader-search ":all t:2share" :add nil))

(defun dorneanu/pocket-2read ()
  "Show getpocket items to be shared"
  (interactive)
  (pocket-reader-search "t:2read" :add nil))


;; From https://github.com/svetlyak40wt/dot-emacs/blob/master/.emacs.d/lib/org-auto-clock.el
;; Auto clock-in when task is marked STARTED
(eval-after-load 'org
  '(progn
     (defun wicked/org-clock-in-if-starting ()
       "Clock in when the task is marked STARTED."
       (when (and (string= org-state "STARTED")
                  (not (string= org-last-state org-state)))
         (org-clock-in)))

     (add-hook 'org-after-todo-state-change-hook
               'wicked/org-clock-in-if-starting)

     (defadvice org-clock-in (after wicked activate)
       "Set this task's status to 'STARTED'."
       (org-todo "STARTED"))


     (defun wicked/org-clock-out-if-waiting ()
       "Clock out when the task is marked WAITING or WIP (Work in Progress)."
       (when (and (or (string= org-state "WAITING")
                      (string= org-state "WIP"))
                  (equal (marker-buffer org-clock-marker) (current-buffer))
                  (< (point) org-clock-marker)
                  (> (save-excursion (outline-next-heading) (point))
                     org-clock-marker)
                  (not (string= org-last-state org-state)))
         (org-clock-out)))

     (add-hook 'org-after-todo-state-change-hook
               'wicked/org-clock-out-if-waiting)))

;; show pomodoro also on CLI
;; from https://colekillian.com/posts/org-pomodoro-and-polybar/
(defun ruborcalor/org-pomodoro-time ()
  "Return the remaining pomodoro time"
  (if (org-pomodoro-active-p)
      (cl-case org-pomodoro-state
        (:pomodoro
           (format "Pomo: %d minutes - %s" (/ (org-pomodoro-remaining-seconds) 60) org-clock-heading))
        (:short-break
         (format "Short break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:long-break
         (format "Long break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:overtime
         (format "Overtime! %d minutes" (/ (org-pomodoro-remaining-seconds) 60))))
    "No active pomo"))
