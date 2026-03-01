;;; core-versions.el --- Spacemacs Core File  -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
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

;;; Commentary:
;;
;; This file defines the current Spacemacs version and the minimum supported
;; Emacs version. It also provides a macro for conditional evaluation based on
;; the minimum Emacs version required by Spacemacs.

;;; Code:

;;;; Version Constants

;; The current version of Spacemacs.
;; This string should be updated whenever a new release is made.
(defconst spacemacs-version
  "0.999.0"
  "Spacemacs version. This string identifies the current release of Spacemacs.")

;;;; Provide Feature

;; Make this feature available for require statements in other files.
(provide 'core-versions)

;;; core-versions.el ends here
