;;; all-the-icons-completion.el --- Add icons to completion candidates -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Itai Y. Efrat
;;
;; Author: Itai Y. Efrat <https://github.com/iyefrat>
;; Maintainer: Itai Y. Efrat <itai3397@gmail.com>
;; Created: June 06, 2021
;; Modified: June 06, 2021
;; Version: 0.0.1
;; Keywords: convenient, lisp
;; Homepage: https://github.com/iyefrat/all-the-icons-completion
;; Package-Requires: ((emacs "26.1") (all-the-icons "5.0"))
;;
;; This file is not part of GNU Emacs.
;;
;; Licence:
;;
;; This program is free software: you can redistribute it and/or modify
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
;;
;;; Commentary:
;;
;;  Add icons to completion candidates.
;;
;;; Code:

(require 'all-the-icons)

(defgroup all-the-icons-completion nil
  "Add icons to completion candidates."
  :version "26.1"
  :group 'appearance
  :group 'convenience
  :prefix "all-the-icons-completion")

(defun all-the-icons-completion-get-icon (cand cat)
  "Return the icon for the candidate CAND of completion category CAT."
  (cl-case cat
    (file (all-the-icons-completion-get-file-icon cand))
    (project-file (all-the-icons-completion-get-file-icon cand))
    (buffer (all-the-icons-completion-get-buffer-icon cand))
    (theme (all-the-icons-completion-get-theme-icon))
    (imenu (all-the-icons-completion-get-imenu-icon cand))
    (t "")))

(defun all-the-icons-completion-get-imenu-icon (cand)
  "Return the icon for the candidate CAND of completion category imenu."
  (concat
   (let ((case-fold-search nil))
     (cond
      ((string-match-p "Type Parameters?[:)]" cand)
       (all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
      ((string-match-p "\\(Variables?\\)\\|\\(Fields?\\)\\|\\(Parameters?\\)[:)]" cand)
       (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue))
      ((string-match-p "Constants?[:)]" cand)
       (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.15))
      ((string-match-p "Enum\\(erations?\\)?[:)]" cand)
       (all-the-icons-material "storage" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-orange))
      ((string-match-p "References?[:)]" cand)
       (all-the-icons-material "collections_bookmark" :height 0.95 :v-adjust -0.2))
      ((string-match-p "\\(Types?\\)\\|\\(Property\\)[:)]" cand)
       (all-the-icons-faicon "wrench" :height 0.9 :v-adjust -0.05))
      ((string-match-p "\\(Functions?\\)\\|\\(Methods?\\)\\|\\(Constructors?\\)[:)]" cand)
       (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
      ((string-match-p "\\(Class\\)\\|\\(Structs?\\)[:)]" cand)
       (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
      ((string-match-p "Interfaces?[:)]" cand)
       (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
      ((string-match-p "Modules?[:)]" cand)
       (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
      ((string-match-p "Packages?[:)]" cand)
       (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver))
      (t (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-blue))))
   " "))

(defun all-the-icons-completion-get-theme-icon ()
  "Return the icon for completion category theme."
  (concat (all-the-icons-material "palette" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-lcyan) " "))

(defun all-the-icons-completion-get-file-icon (cand)
  "Return the icon for the candidate CAND of completion category file."
  (cond ((string-match-p "\\/$" cand) (concat (all-the-icons-icon-for-dir cand) " "))
        (t (concat (all-the-icons-icon-for-file cand) " "))))

(defun all-the-icons-completion-get-buffer-icon (cand)
  "Return the icon for the candidate CAND of completion category buffer."
  (let* ((mode (buffer-local-value 'major-mode (get-buffer cand)))
         (icon (all-the-icons-icon-for-mode mode))
         (parent-icon (all-the-icons-icon-for-mode
                       (get mode 'derived-mode-parent))))
    (concat
     (if (symbolp icon)
         (if (symbolp parent-icon)
             (all-the-icons-faicon "sticky-note-o")
           parent-icon)
       icon)
     " ")))

(defun all-the-icons-completion-completion-metadata-get (orig metadata prop)
  "Meant as :around advice for `completion-metadata-get', Add icons as prefix.
ORIG should be `completion-metadata-get'
METADATA is the metadata.
PROP is the property which is looked up."
  (if (eq prop 'affixation-function)
      (let ((cat (funcall orig metadata 'category))
            (aff (or (funcall orig metadata 'affixation-function)
                     (when-let ((ann (funcall orig metadata 'annotation-function)))
                       (lambda (cands)
                         (mapcar (lambda (x) (list x "" (funcall ann x))) cands))))))
        (cond
         ((and (eq cat 'multi-category) aff)
          (lambda (cands)
            (mapcar (lambda (x)
                      (pcase-exhaustive x
                        (`(,cand ,prefix ,suffix)
                         (let ((orig (get-text-property 0 'multi-category cand)))
                           (list cand
                                 (concat (all-the-icons-completion-get-icon (cdr orig) (car orig))
                                         prefix)
                                 suffix)))))
                    (funcall aff cands))))
         ((and cat aff)
          (lambda (cands)
            (mapcar (lambda (x)
                      (pcase-exhaustive x
                        (`(,cand ,prefix ,suffix)
                         (let ((orig (get-text-property 0 'multi-category cand)))
                           (list cand
                                 (concat (all-the-icons-completion-get-icon cand cat)
                                         prefix)
                                 suffix)))))
                    (funcall aff cands))))
         ((eq cat 'multi-category)
          (lambda (cands)
            (mapcar (lambda (x)
                      (let ((orig (get-text-property 0 'multi-category x)))
                        (list x (all-the-icons-completion-get-icon (cdr orig) (car orig)) "")))
                    cands)))
         (cat
          (lambda (cands)
            (mapcar (lambda (x)
                      (list x (all-the-icons-completion-get-icon x cat) ""))
                    cands)))
         (aff)))
    (funcall orig metadata prop)))

;; For the byte compiler
(defvar marginalia-mode)
;;;###autoload
(defun all-the-icons-completion-marginalia-setup ()
  "Hook to `marginalia-mode-hook' to bind `all-the-icons-completion-mode' to it."
  (all-the-icons-completion-mode (if marginalia-mode 1 -1)))

;;;###autoload
(define-minor-mode all-the-icons-completion-mode
  "Add icons to completion candidates."
  :global t
  (if all-the-icons-completion-mode
      (advice-add #'completion-metadata-get :around #'all-the-icons-completion-completion-metadata-get)
    (advice-remove #'completion-metadata-get #'all-the-icons-completion-completion-metadata-get)))

(provide 'all-the-icons-completion)
;;; all-the-icons-completion.el ends here
