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

(defun guix-system--emacs-package-p (pkg-symbol)
  "Is PKG-SYMBOL an Emacs package installed by Guix?
Examples:
;; (and (guix-system--emacs-package-p \\='treemacs-magit) t) ; => nil
;; (and (guix-system--emacs-package-p \\='git-commit) t)     ; => nil
;; (and (guix-system--emacs-package-p \\='magit) t)          ; => t
;; (and (guix-system--emacs-package-p \\='magit-section) t)  ; => t
"
  (or
   (member (symbol-name pkg-symbol) guix-system-packages)
   (if-let ((pkg-lst (alist-get pkg-symbol package-alist)))
       (string-prefix-p "/gnu" (package-desc-dir (car pkg-lst))))))

(defun guix-package-installed-p (package &optional min-version)
  (or (package-installed-p package min-version)
      (guix-system--emacs-package-p package)))

(defun guix-package-import-keyring (&optional file)
  "Import keys from FILE."
  (interactive "fFile: ")
  (setq file (expand-file-name file))
  ;; (spacemacs-buffer/message ";;;; file : %s" file)
  (let ((context (epg-make-context 'OpenPGP)))
    (let (
          (guix-package-gnupghome-dir (concat (getenv "XDG_DATA_HOME") "/spacemacs/spguix/elpa"))
          )
      ;; (spacemacs-buffer/message ";;;; guix-package-gnupghome-dir : %s" guix-package-gnupghome-dir)
      (when guix-package-gnupghome-dir
        (with-file-modes 448
          (make-directory guix-package-gnupghome-dir t))
        (setf (epg-context-home-directory context) guix-package-gnupghome-dir)))
    (message "Importing %s..." (file-name-nondirectory file))
    (epg-import-keys-from-file context file)
    (message "Importing %s...done" (file-name-nondirectory file))))

(defun guix-package--download-one-archive (archive file &optional async)
  "Retrieve an archive file FILE from ARCHIVE, and cache it.
ARCHIVE should be a cons cell of the form (NAME . LOCATION),
similar to an entry in `package-alist'.  Save the cached copy to
\"archives/NAME/FILE\" in `package-user-dir'."
  ;; The downloaded archive contents will be read as part of
  ;; `package--update-downloads-in-progress'.
  (when async
    (cl-pushnew (cons archive file) package--downloads-in-progress
                :test #'equal))
  (package--with-response-buffer (cdr archive) :file file
    :async async
    :error-form (package--update-downloads-in-progress (cons archive file))
    (let* ((location (cdr archive))
           (name (car archive))
           (content (buffer-string))
           (dir (expand-file-name (concat "archives/" name) package-user-dir))
           (local-file (expand-file-name file dir)))
      (when (listp (read content))
        (let ((f ";;;; [guix-package--download-one-archive]"))
          (spacemacs-buffer/message "%s %s" f dir)
          (make-directory dir t)
          (if (or (not (package-check-signature))
                  (member name package-unsigned-archives))
              ;; If we don't care about the signature, save the file and
              ;; we're done.
              (progn
                (spacemacs-buffer/message "%s we don't care about the signature" f)
                (cl-assert (not enable-multibyte-characters))
                (let ((coding-system-for-write 'binary))
                  (write-region content nil local-file nil 'silent))
                (package--update-downloads-in-progress (cons archive file)))
            ;; If we care, check it (perhaps async) and *then* write the file.
            (progn
              (spacemacs-buffer/message "%s (package--check-signature ...)" f)
              (package--check-signature
               location file content async
               ;; This function will be called after signature checking.
               (lambda (&optional good-sigs)
                 (spacemacs-buffer/message "%s good-sigs : %s" f good-sigs)
                 (cl-assert (not enable-multibyte-characters))
                 (let ((coding-system-for-write 'binary))
                   (write-region content nil local-file nil 'silent))
                 ;; Write out good signatures into archive-contents.signed file.
                 (when good-sigs
                   (write-region (mapconcat #'epg-signature-to-string good-sigs "\n")
                                 nil (concat local-file ".signed") nil 'silent)))
               (lambda ()
                 (spacemacs-buffer/message "%s (package--update-downloads-in-progress ...) : %s" f)
                 (package--update-downloads-in-progress (cons archive file)))))))))))

(defun guix-package--download-and-read-archives (&optional async)
  "Download descriptions of all `package-archives' and read them.
Populate `package-archive-contents' with the result.

If optional argument ASYNC is non-nil, perform the downloads
asynchronously."
  (dolist (archive package-archives)
    (condition-case-unless-debug nil
        (guix-package--download-one-archive archive "archive-contents" async)
      (error (message "Failed to download `%s' archive."
                      (car archive))))))

;; List of functions to call to refresh the package archive. Each function may
;; take an optional argument indicating that the operation ought to be executed
;; asynchronously.
(setq package-refresh-contents-hook (list #'guix-package--download-and-read-archives))

(defun guix-package-refresh-contents (&optional async)
  "Download descriptions of all configured ELPA packages.
For each archive configured in the variable `package-archives',
inform Emacs about the latest versions of all packages it offers,
and make them available for download.
Optional argument ASYNC specifies whether to perform the
downloads in the background."
  (interactive)
  (let ((f ";;; [guix-package-refresh-contents]"))
    (spacemacs-buffer/message "%s (file-exists-p package-user-dir) : %s; package-user-dir : %s"
                              f (file-exists-p package-user-dir) package-user-dir)
    (unless (file-exists-p package-user-dir)
      (make-directory package-user-dir t))
    (spacemacs-buffer/message "%s data-directory : %s" f data-directory)
    (let ((default-keyring (expand-file-name "package-keyring.gpg"
                                             data-directory))
          (inhibit-message (or inhibit-message async)))
      (when (and (package-check-signature) (file-exists-p default-keyring))
        (condition-case-unless-debug error
            (progn
              ;; (spacemacs-buffer/message ";;;; default-keyring : %s" default-keyring)
              (guix-package-import-keyring default-keyring))
          (error (message "Cannot import default keyring: %S" (cdr error))))))
    (spacemacs-buffer/message "%s %s" f 'package-refresh-contents-hook)
    (run-hook-with-args 'package-refresh-contents-hook async)))

(provide 'core-guix)
