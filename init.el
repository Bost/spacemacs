;;; init.el --- Spacemacs Initialization File -*- no-byte-compile: t -*-
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Avoid garbage collection during startup.
;; see `SPC h . dotspacemacs-gc-cons' for more info

(defun my=dbg=tstp ()
  0
  ;; (if (functionp #'dbg=tstp) (dbg=tstp) (car (time-convert nil t)))
  )
(setq my=dbg=init-time
      0
      ;; (if (boundp #'dbg=init-time) dbg=init-time (my=dbg=tstp))
      )
(setq my=dbg=fmt (if (boundp #'dbg=fmt) dbg=fmt "%012d"))

(defun my=log (fun-point)
  (format "%s %s [%%s] (length load-path) %s"
          (format my=dbg=fmt (- (my=dbg=tstp) my=dbg=init-time))
          (cond ((eq fun-point #'beg) "{{{{{{{{")
                ((eq fun-point #'end) "}}}}}}}}")
                (t "________"))
          (length load-path)))

(defun my=msg (some-str)
  (message (my=log nil) some-str)
  )

(defun my=beg (f)
  "TODO my=beg could / should be done using (advice-add :before ...)"
  (message (my=log #'beg) f)
  )
(defun my=end (f)
  "TODO my=end could / should be done using (advice-add :after ...)"
  (message (my=log #'end) f)
  )

;; (my=msg "001")
(defconst emacs-start-time (current-time))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
;; (my=msg "002")

;; (message "(concat (file-name-directory load-file-name) \"core/core-load-paths\"):\n%s"
;;          (concat (file-name-directory load-file-name) "core/core-load-paths"))

;; (message "002 load-path:\n%s" load-path)
(load (concat (file-name-directory load-file-name) "core/core-load-paths")
      nil (not init-file-debug))

;; (message "003 load-path:\n%s" load-path)
;; (load "/home/bost/dev/.spguimacs.d/core/core-load-paths" nil (not init-file-debug))

;; (my=msg "003")
(load (concat spacemacs-core-directory "core-versions")
      nil (not init-file-debug))
;; (my=msg "004")
(load (concat spacemacs-core-directory "core-dumper")
      nil (not init-file-debug))

;; (my=msg "005")
;; Remove compiled core files if they become stale or Emacs version has changed.
(load (concat spacemacs-core-directory "core-compilation")
      nil (not init-file-debug))
;; (my=msg "006")
(load spacemacs--last-emacs-version-file t (not init-file-debug))
;; (my=msg "007")
(when (or (not (string= spacemacs--last-emacs-version emacs-version))
          (> 0 (spacemacs//dir-byte-compile-state
                (concat spacemacs-core-directory "libs/"))))
  (spacemacs//remove-byte-compiled-files-in-dir spacemacs-core-directory))
;; (my=msg "008")
;; Update saved Emacs version.
(unless (string= spacemacs--last-emacs-version emacs-version)
  (spacemacs//update-last-emacs-version))
;; (my=msg "009")

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  ;; Disabling file-name-handlers for a speed boost during init might seem like
  ;; a good idea but it causes issues like
  ;; https://github.com/syl20bnr/spacemacs/issues/11585 "Symbol's value as
  ;; variable is void: \213" when emacs is not built having:
  ;; `--without-compress-install`
  (let ((please-do-not-disable-file-name-handler-alist nil))
    (require 'core-spacemacs)
    ;; (my=msg "01")
    (spacemacs/dump-restore-load-path)
    ;; (my=msg "02")
    (configuration-layer/load-lock-file)
    ;; (my=msg "03")
    (spacemacs/init)
    ;; (my=msg "04")
    (configuration-layer/stable-elpa-init)
    ;; (my=msg "05")
    (configuration-layer/load)
    ;; (my=msg "06")
    (spacemacs-buffer/display-startup-note)
    ;; (my=msg "07")
    (spacemacs/setup-startup-hook)
    ;; (my=msg "08")
    (spacemacs/dump-eval-delayed-functions)
    ;; (my=msg "09")
    (when (and dotspacemacs-enable-server (not (spacemacs-is-dumping-p)))
      (require 'server)
      (when dotspacemacs-server-socket-dir
        (setq server-socket-dir dotspacemacs-server-socket-dir))
      (unless (server-running-p)
        (message "Starting a server...")
        (server-start)))
    ;; (my=msg "10")
    ))
