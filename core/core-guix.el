(require 'core-spacemacs-buffer) ;; for spacemacs-buffer/message

;;; (defun partition (n list)
;;;   "Partition LIST into sublists of length N. Unfortunately it leads to
;;;     error: Lisp nesting exceeds ‘max-lisp-eval-depth’
;;; E.g.:
;;; (partition 3 '(1 2 3 4 5 6 7 8 9))
;;; ;; => ((1 2 3) (4 5 6) (7 8 9))"
;;;   (if (<= (length list) n)
;;;       (list list)
;;;     (cons (cl-subseq list 0 n)
;;;           (partition n (nthcdr n list)))))

(defun partition (n list)
  "Partition LIST into sublists of length N.
E.g.:
(partition 3 '(1 2 3 4 5 6 7 8 9))
;; => ((1 2 3) (4 5 6) (7 8 9))"
  (let ((result '())
        (current-list list))
    (while current-list
      (let ((sublist (cl-subseq current-list 0 (min n (length current-list)))))
        (setq result (append result (list sublist)))
        (setq current-list (nthcdr n current-list))))
    result))

(defun guix-delete-orphan-packages (orphans-orig)
  (let* ((orphans
          (cl-remove-if-not #'configuration-layer//system-package-p
                            orphans-orig))
         (orphans-count (length orphans)))
    (when orphans
      (spacemacs-buffer/set-mode-line "Uninstalling unused packages..." t)
      (spacemacs-buffer/append
       (format "Found %s orphan package(s) to delete...\n"
               orphans-count))
      (setq deleted-count 0)
      (dolist (orphan orphans)
        (setq deleted-count (1+ deleted-count))
        (spacemacs-buffer/replace-last-line
         (format "--> deleting %s... [%s/%s]"
                 orphan
                 deleted-count
                 orphans-count) t)
        (configuration-layer//package-delete orphan)
        (spacemacs//redisplay))
      (spacemacs-buffer/append "\n"))))

(defun guix-get-installed-emacs-packages (&optional profile-path)
  "Returns a list emacs-related packages explicitly installed by
Guix in the PROFILE-PATH or in the default profile if the
PROFILE-PATH is not specified."
  (let ((system-command
         (concat
          "guix package --profile="
          (or profile-path "$HOME/.guix-profile")
          " '--list-installed=^emacs-|^spacemacs-rolling-release$'"
          ;; " '--list-installed=^emacs-'"
          " | awk '{print $1, $4}'"
          )))
    (spacemacs-buffer/message
     "[guix-get-installed-emacs-packages] cmd:\n%s\n" system-command)
    ;; The `system-command' returns a string containing pairs e.g.:
    ;;   "emacs-keycast /gnu/store/...-emacs-keycast-1.4.2
    ;;    emacs-crdt /gnu/store/...-emacs-crdt-0.3.5
    ;;    ..."
    ;; which needs to be split into a list of substrings along the whitespace
    ;; chars, from which a list of pairs e.g.:
    ;;   (("emacs-keycast" "/gnu/store/...-emacs-keycast-1.4.2")
    ;;    ("emacs-crdt" "/gnu/store/...-emacs-crdt-0.3.5")
    ;;    ...)
    ;; The partitioning of here to prevent:
    ;;   Lisp nesting exceeds 'max-lisp-eval-depth': 1601
    (let ((lst
           (partition
            2
            (split-string
             (with-temp-buffer
               ;; Process files synchronously in a separate process.
               ;; Similar to ‘call-process-shell-command’, but calls
               ;; ‘process-file’.
               (process-file-shell-command system-command nil t)
               (sort-lines nil (point-min) (point-max))
               (buffer-string))))))
      (spacemacs-buffer/message
       "Found %s Emacs packages installed by Guix" (length lst))
      (mapcar #'car lst))))

;; Pre-calculated list of "emacs-<package-name>" packages
(setq
 guix-installed-emacs-packages
 ;; TODO substitute the following line in the emacs-spacemacs package
 (guix-get-installed-emacs-packages)
 )

;; Pre-calculated list of "emacs-<package-name>" packages w/o the "emacs-" prefix
(setq guix-system-packages
      (mapcar
       (lambda (guix-pkg) (substring guix-pkg (length "emacs-")))
       guix-installed-emacs-packages))

(defun guix-system-package-p (pkg-symbol)
  "Examples:
;; (and (guix-system-package-p \\='treemacs-magit) t) ; => nil
;; (and (guix-system-package-p \\='git-commit) t)     ; => nil
;; (and (guix-system-package-p \\='magit) t)          ; => t
;; (and (guix-system-package-p \\='magit-section) t)  ; => t
"
  (or
   (member (symbol-name pkg-symbol) guix-system-packages)
   (if-let ((pkg-lst (alist-get pkg-symbol package-alist)))
       (string-prefix-p "/gnu" (package-desc-dir (car pkg-lst))))))

(defun guix-package-installed-p (package &optional min-version)
  (or (package-installed-p package min-version)
      (guix-system-package-p package)))

(provide 'core-guix)
