;;; +bindings.el -*- lexical-binding: t; -*-

;; https://github.com/hlissner/doom-emacs/issues/906
(map! (:when IS-MAC
  (:map general-override-mode-map
    :gi "M-v" #'yank)))

(map!
 ;; Ensure there are no conflicts
 :nmvo doom-leader-key nil
 :nmvo doom-localleader-key nil

 ;; Window Movements
 "C-h"    #'evil-window-left
 "C-j"    #'evil-window-down
 "C-k"    #'evil-window-up
 "C-l"    #'evil-window-right
)
