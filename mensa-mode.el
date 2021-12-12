;;; mensa-mode.el --- Access the Mensa menu from Emacs -*- lexical-binding: t; -*-

;; Author: Illia Ostapyshyn <ilya.ostapyshyn@gmail.com>
;; Created: 2021-12-12
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, food
;; URL: https://github.com/iostapyshyn/mensa-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'dom)
(require 'hi-lock)

(defvar mensa-nutrition-facts
  '((calories     "Brennwert: \\([0-9.]*\\).kcal"  "Calories"      "kcal")
    (fat          "Fett: \\([0-9,]*\\).g"          "Fat" 	   "g")
    (carbohydrate "Kohlenhydrate: \\([0-9,]*\\).g" "Carbohydrates" "g")
    (sugar        "Zucker: \\([0-9,]*\\).g"        "Sugar"         "g")
    (protein      "Eiweiß: \\([0-9,]*\\).g"        "Protein"       "g")))

(defvar mensa-category-faces
  '(("GESUND & MUNTER" . hi-green)
    ("PASTA & FRIENDS" . hi-yellow)
    ("FLEISCH & MEER"  . hi-salmon)
    ("VEGGIE & VEGAN"  . hi-green)
    ("QUEERBEET"       . hi-blue)
    ("EVERGREENS"      . hi-salmon)
    ("SÜSSE ECKE"      . hi-pink)))

(defvar mensa-url "https://www.stwh-portal.de/mensa/webapp/?price=1&pay=1&mensa=1")

(defvar mensa--highlight-overlays nil)

(defun mensa--parse-food (food)
  (let* ((price-text (dom-text (dom-by-class food "\\`food_price\\'")))
         (price (replace-regexp-in-string "," "." (progn
                                                    (string-match "[0-9,]+" price-text)
                                                    (match-string 0 price-text))))
         (details-text (dom-texts (dom-by-class food "\\`composition_details_right\\'")))
         (food-alist `((name . ,(dom-text (dom-by-class food "\\`food_name\\'")))
                       (price . ,(string-to-number price)))))
    (pcase-dolist (`(,keyword ,regexp) mensa-nutrition-facts)
      (let ((value (progn (string-match regexp details-text)
                          (match-string 1 details-text))))
        (push (cons keyword (string-to-number (replace-regexp-in-string "," "." value)))
              food-alist)))
    food-alist))

(defun mensa--insert-meal (open meal)
  (let* ((name (alist-get 'name meal))
         (price (alist-get 'price meal))
         (ol-beg (point)))
    (insert (format "%5.2f €  %s\n" price name))
    (when open
      (pcase-dolist (`(,keyword ,_ ,fact ,unit) mensa-nutrition-facts)
        (insert (format "         %s: %g %s\n" fact (alist-get keyword meal) unit))))
    (let ((ol (make-overlay ol-beg (point) nil t)))
      (overlay-put ol 'evaporate t)
      (overlay-put ol 'mensa--state (cons open meal)))))

(defun mensa--insert-menu (meals)
  (dolist (category meals)
    (let* ((category-name (car category))
           (padding (max 0 (- (window-width) (length category-name) 9))))
      (insert (propertize
               (concat "------ " category-name " " (make-string padding ?-))
               'face (alist-get category-name mensa-category-faces nil nil 'equal)))
      (insert "\n"))
    (dolist (meal (cdr category))
      ;; (pp meal (current-buffer))
      (mensa--insert-meal nil meal))
    (insert "\n")))

(defun mensa--state-overlay-at (point)
  (let ((ols (overlays-at point)))
    (seq-find (lambda (e) (overlay-get e 'mensa--state)) ols)))

(defun mensa-retrieve-menu (_status)
  (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
         (lis (dom-children (dom-by-class dom "\\<list\\>")))
         (meals (mapcar (lambda (li)
                          (when-let ((category (dom-by-class li "\\<category\\>")))
                            (cons (dom-text category)
                                  (mapcar #'mensa--parse-food
                                          (dom-by-class li "\\`food\\'")))))
                        lis))
         (meals (remq nil meals)))
    (let ((buf (get-buffer-create "*Mensa*")))
      (with-current-buffer buf
        (let ((buffer-read-only nil))
          (erase-buffer)
          (when-let ((h1-strings (dom-strings (dom-by-tag dom 'h1)))
                     (mensa (car h1-strings))
                     (date (nth 2 h1-strings)))
            (setq header-line-format (concat " " mensa " - " date)))
          (mensa--insert-menu meals)
          (goto-char (point-min)))))))

(defun mensa-toggle-meal ()
  (interactive)
  (when-let* ((ol (mensa--state-overlay-at (point)))
              (ol-beg (overlay-start ol))
              (ol-end (overlay-end ol))
              (state (overlay-get ol 'mensa--state)))
    (let ((buffer-read-only nil))
      (save-excursion
        (delete-region ol-beg ol-end)
        (goto-char ol-beg)
        (mensa--insert-meal (not (car state)) (cdr state))))))

(defun mensa-highlight-meal ()
  (mapc #'delete-overlay mensa--highlight-overlays)
  (when-let ((ol (mensa--state-overlay-at (point))))
    (save-excursion
      (goto-char (overlay-start ol))
      (let ((hl-ol (make-overlay (point-at-bol) (point-at-eol))))
        (overlay-put hl-ol 'evaporate t)
        (overlay-put hl-ol 'face 'bold)
        (push hl-ol mensa--highlight-overlays)))))

(defun mensa-next-meal ()
  (interactive)
  (when-let ((point (next-single-char-property-change (point) 'mensa--state)))
    (if (get-char-property point 'mensa--state)
        (goto-char point)
      (when-let ((point (next-single-char-property-change point 'mensa--state))
                 ((get-char-property point 'mensa--state)))
        (goto-char point)))))

(defun mensa-previous-meal ()
  (interactive)
  (when-let ((point (previous-single-char-property-change (point) 'mensa--state)))
    (if (get-char-property point 'mensa--state)
        (goto-char point)
      (when-let ((point (previous-single-char-property-change point 'mensa--state))
                 ((get-char-property point 'mensa--state)))
        (goto-char point)))))

(defun mensa-revert-buffer (&optional _ignore-auto _noconfirm)
  (url-retrieve mensa-url #'mensa-retrieve-menu))

(defvar mensa-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") 'mensa-toggle-meal)
    (define-key map (kbd "RET") 'mensa-toggle-meal)
    (define-key map (kbd "n") 'mensa-next-meal)
    (define-key map (kbd "p") 'mensa-previous-meal)
    map))

(define-derived-mode mensa-mode special-mode "Mensa"
  "Major mode for browsing a list of available meals in the Mensa."
  (setq-local revert-buffer-function #'mensa-revert-buffer)
  (setq-local truncate-lines t)
  (add-hook 'post-command-hook #'mensa-highlight-meal))

(defun mensa ()
  "Display the list of meals on offer today in the Mensa canteen."
  (interactive)
  (let ((buf (get-buffer-create "*Mensa*")))
    (with-current-buffer buf
      (mensa-mode)
      (mensa-revert-buffer))
    (pop-to-buffer buf)))

(provide 'mensa-mode)
;;; mensa-mode.el ends here
