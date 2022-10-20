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

(defun all-the-icons-completion--display-icons ()
  (display-graphic-p))

(defun all-the-icons-completion--format-icon (icon)
  (let* ((props (get-text-property 0 'face icon))
         (family (plist-get props :family))
         (face (plist-get props :inherit))
         (new-face
		  `(:inherit ,face
					 :family ,family
					 :height 1.0)))
    (put-text-property 0 (length icon) 'face new-face icon)
	(format " %s\t" icon)))

;; Icon generator functions,
;; stolen from all-the-icons-ivy-rich
;; modified to use EIEIO

(cl-defgeneric all-the-icons-completion-get-icon (cand cat)
  "Return an appropriate icon for candidate CAND in category CAT.")

(cl-defmethod all-the-icons-completion-get-icon ((cand string) (cat (eql 'buffer)))
  (when (all-the-icons-completion--display-icons)
	(let ((icon (with-current-buffer (get-buffer cand)
                  (if (eq major-mode 'dired-mode)
                      (all-the-icons-icon-for-dir cand)
					(all-the-icons-icon-for-buffer)))))
      (all-the-icons-completion--format-icon
	   (if (or (null icon) (symbolp icon))
		   (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust 0.0)
         (propertize icon 'display '(raise 0.0)))))))

(cl-defmethod all-the-icons-completion-get-icon ((cand string) (cat (eql 'file)))
  "Display file icon for CAND all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (let ((icon (cond
                 ((directory-name-p cand)
                  (all-the-icons-icon-for-dir cand))
                 ((not (string-empty-p cand))
                  (all-the-icons-icon-for-file (file-name-nondirectory cand) :height 0.9 :v-adjust 0.0)))))
      (all-the-icons-completion--format-icon
       (if (or (null icon) (symbolp icon))
           (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust 0.0)
         (propertize icon 'display '(raise 0.0)))))))

(cl-defmethod all-the-icons-completion-get-icon ((cand string) (cat (eql 'project-file)))
  (all-the-icons-completion-get-icon cand 'file))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'dir)))
  "Display project icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01 :face 'all-the-icons-silver))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'project)))
  "Display project icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-octicon "repo" :height 1.0 :v-adjust 0.01 :face 'all-the-icons-silver))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'mode)))
  "Display mode icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-blue))))

(cl-defmethod all-the-icons-completion-get-icon ((cand string) (cat (eql 'function)))
  "Display function icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (if (commandp (intern cand))
         (all-the-icons-faicon "cog" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-blue)
       (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple)))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'command)))
  "Display command icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-faicon "cog" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-blue))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'history)))
  "Display command icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-material "history" :height 1.1 :v-adjust -0.1 :face 'all-the-icons-lblue))))

(cl-defmethod all-the-icons-completion-get-icon ((cand string) (cat (eql 'variable)))
  "Display the variable icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (if (custom-variable-p (intern cand))
         (all-the-icons-faicon "tag" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)
       (all-the-icons-octicon "tag" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-lblue)))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'face)))
  "Display face icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-material "palette" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-blue))))

(defun all-the-icons-completion--counsel-imenu-symbol (cand)
  "Return imenu symbol from CAND."
  (let ((str (split-string cand ": ")))
    (or (cadr str) (car str))))

(cl-defmethod all-the-icons-completion-get-icon ((cand string) (cat (eql 'symbol)))
  "Display the symbol icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (let ((sym (intern (all-the-icons-completion--counsel-imenu-symbol cand))))
	  (cond
	   ((string-match-p "Packages?[:)]" cand)
        (all-the-icons-completion--format-icon
         (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver)))
	   ((or (functionp sym) (macrop sym))
        (all-the-icons-completion-get-icon cand 'function))
	   ((facep sym)
        (all-the-icons-completion-get-icon cand 'face))
	   ((symbolp sym)
        (all-the-icons-completion-get-icon cand 'variable))
	   (t (all-the-icons-completion--format-icon
		   (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver)))))))

(cl-defmethod all-the-icons-completion-get-icon ((cand string) (cat (eql 'company)))
  "Display the symbol icon of company all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (if (fboundp 'company-box--get-icon)
         (company-box--get-icon cand)
       (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver)))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'theme)))
  "Display the theme icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-material "palette" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-lcyan))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'keybinding)))
  "Display the keybindings icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-faicon "keyboard-o" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lsilver))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'library)))
  "Display the library icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-lblue))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'package)))
  "Display the package icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'font)))
  "Display the font icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-faicon "font" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'world-clock)))
  "Display the world clock icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-faicon "globe" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'tramp)))
  "Display the tramp icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-octicon "radio-tower" :height 0.8 :v-adjust 0.01))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'git-branch)))
  "Display the git branch icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-octicon "git-branch" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-green))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'process)))
  "Display the process icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-faicon "bolt" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-lblue))))

(cl-defmethod all-the-icons-completion-get-icon ((cand string) (cat (eql 'imenu)))
  "Display the imenu icon for CAND all-the-icons-completion."
  (if (derived-mode-p 'emacs-lisp-mode)
      (all-the-icons-completion-get-icon cand 'symbol)
    (when (all-the-icons-completion--display-icons)
      (all-the-icons-completion--format-icon
       (let ((case-fold-search nil))
         (cond
          ((string-match-p "Type Parameters?[:)]" cand)
           (all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
          ((string-match-p "\\(Variables?\\)\\|\\(Fields?\\)\\|\\(Parameters?\\)[:)]" cand)
           (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue))
          ((string-match-p "Constants?[:)]" cand)
           (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.05))
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
          (t (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-blue))))))))

(cl-defmethod all-the-icons-completion-get-icon ((cand string) (cat (eql 'bookmark)))
  "Return bookmark type for CAND."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (let ((file (bookmark-get-filename cand)))
       (cond
        ((null file)
         (all-the-icons-material "block" :height 1.0 :v-adjust -0.2))  ; fixed #38
        ((file-remote-p file)
         (all-the-icons-octicon "radio-tower" :height 0.8 :v-adjust 0.01))
        ((not (file-exists-p file))
         (all-the-icons-material "block" :height 1.0 :v-adjust -0.2))
        ((file-directory-p file)
         (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.01))
        (t (all-the-icons-icon-for-file (file-name-nondirectory file) :height 0.9 :v-adjust 0.0)))))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'settings)))
  "Display settings icon for CAND all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-octicon "settings" :height 0.9 :v-adjust -0.01 :face 'all-the-icons-lblue))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'charset)))
  "Display charset icon for CAND all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-faicon "table" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'coding-system)))
  "Display coding system icon for CAND all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-faicon "table" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'lang)))
  "Display language icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-faicon "language" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'input-method)))
  "Display input method icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-faicon "keyboard-o" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue))))

(cl-defmethod all-the-icons-completion-get-icon ((cand string) (cat (eql 'grep-file)))
  "Display file icon for CAND all-the-icons-completion.
Support`counsel-ack', `counsel-ag', `counsel-pt' and `counsel-rg', etc."
  (when (or (string-match "\\(.+\\):\\([0-9]+\\):\\(.+\\)" cand)
            (string-match "\\(.+\\):\\(.+\\)(\\(.+\\))" cand))
    (all-the-icons-completion-get-icon (match-string 1 cand) 'file)))

(cl-defmethod all-the-icons-completion-get-icon ((cand string) (cat (eql 'link)))
  "Display link icon all-the-icons-completion."
  (if (string-prefix-p "#" cand)
      (all-the-icons-faicon "anchor" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-green)
    (all-the-icons-material "link" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-blue)))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat (eql 'key)))
  "Display key icon all-the-icons-completion."
  (when (all-the-icons-completion--display-icons)
    (all-the-icons-completion--format-icon
     (all-the-icons-octicon "key" :height 0.8 :v-adjust -0.05))))

(cl-defmethod all-the-icons-completion-get-icon ((_cand string) (cat symbol))
  "Default method for not returning any icon."
  "")




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
                         (list cand
                               (concat (all-the-icons-completion-get-icon cand cat)
                                       prefix)
                               suffix))))
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

(defun all-the-icons-completion-align-icons ()
  (setq-local tab-width 1))

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
      (progn
		(advice-add #'completion-metadata-get :around #'all-the-icons-completion-completion-metadata-get)
		(add-hook 'minibuffer-setup-hook #'all-the-icons-completion-align-icons))
    (advice-remove #'completion-metadata-get #'all-the-icons-completion-completion-metadata-get)
	(remove-hook 'minibuffer-setup-hook #'all-the-icons-completion-align-icons)))

(provide 'all-the-icons-completion)
;;; all-the-icons-completion.el ends here
