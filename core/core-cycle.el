;;; core-cycle.el --- Spacemacs Core File
;;
;; Copyright (c) 2021 Rostislav Svoboda
;;
;; Author: Rostislav Svoboda <Rostislav.Svoboda@gmail.com>
;; URL: https://github.com/Bost/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'core-funcs)

(defvar spacemacs-cycles '()
  "List of all declared cycles. The structure of an element is a
property list (name :func FUNCTION :doc STRING :key STRING).")

(defmacro spacemacs|add-cycle (name cycling-list &rest props)
  "Add a cycle-NAME symbol. In contrast to toggle which cycles around two
elements, the length of the CYCLING-LIST is unlimited.

Usage:
  (defun a () (message \"a\") \"a\")
  (defun b () (message \"b\") \"b\")
  (defun c () (message \"c\") \"c\")
  (defun d () (message \"d\") \"d\")

  (spacemacs|add-cycle foo-bar \\='(a b c) :start-func \\='b)
  (spacemacs|add-cycle foo-bar \\='(a b c) :start-func \\='d)

Try: M-x spacemacs/cycle-foo-bar or:
  (message \"1. %s, 2. %s 3. %s; func-last: %s\"
           (spacemacs/cycle-foo-bar)
           (spacemacs/cycle-foo-bar)
           (spacemacs/cycle-foo-bar)
           (spacemacs/cycle-foo-bar)
           spacemacs/cycle-foo-bar-func-last)

Note: function advising can be also used. E.g.:
 (advice-add \\='spacemacs/cycle-foo-bar :after (lambda () (recenter)))
 (advice-remove \\='spacemacs/cycle-foo-bar (lambda () (recenter)))

Available PROPS:

`:start-func FUNC'
    FUNC function initially called. May be or may not be in present in the
    CYCLING-LIST. If it is then the cycling starts from this function. Defaults
    to nil.

`:doc STRING'
    STRING describes what the cycle-NAME does.

`:prefix SYMBOL'
    SYMBOL is bound to the raw value of prefix-arg (same as calling
    (interactive \"P\")) in the wrapper function.

All properties supported by `spacemacs//create-key-binding-form' can be
used."
  (declare (indent 1))
  (let* (
         (wrapper-func (intern (format "spacemacs/cycle-%s"
                                       (symbol-name name))))
         ;; TODO
         ;; (wrapper-func-forward (intern (format "spacemacs/cycle-%s-forward" (symbol-name name))))
         ;; (wrapper-func-backward (intern (format "spacemacs/cycle-%s-backward" (symbol-name name))))
         (wrapper-func-last (intern (format "%s-func-last" wrapper-func)))
         (start-func (plist-get props :start-func))
         (doc (plist-get props :documentation))
         (prefix-arg-var (plist-get props :prefix))
         (evil-leader-for-mode (spacemacs/mplist-get-values props :evil-leader-for-mode))
         (bindkeys (spacemacs//create-key-binding-form props wrapper-func)))
    `(progn
       (let ((properties (append '(:function ,wrapper-func :predicate ,wrapper-func-last)
                                 ',props))
             (cell (assq ',name spacemacs-cycles)))
         (if cell
             (setcdr cell properties)
           (push (cons ',name properties) spacemacs-cycles)))
       (setq ,wrapper-func-last ,start-func)
       (if ,wrapper-func-last
           (funcall ,wrapper-func-last))
       (defun ,wrapper-func ,(if prefix-arg-var (list prefix-arg-var) ())
         ,(format "%s"
                  (if doc (concat "\n\n" doc) ""))
         ,(if prefix-arg-var '(interactive "P") '(interactive))
         (setq ,wrapper-func-last
               (car (or (cdr (memq ,wrapper-func-last ,cycling-list))
                        ;; if ,wrapper-func-last isn't in cycleable, start over
                        ,cycling-list)))
         (funcall ,wrapper-func-last))
       ,@bindkeys)))

(provide 'core-cycle)
