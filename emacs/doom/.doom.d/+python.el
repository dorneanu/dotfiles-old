;;; +python.el -*- lexical-binding: t; -*-

(use-package pyvenv
  :demand t
  :config
  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals
