;;;; org-query.el --- Generate alternative views of your org-mode headlines

;; Copyright (C) 2014-2015  Matthew Duck

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Require

(eval-when-compile
  (require 'cl)) ;; Think it's correct to use this, TODO

(require 'org)
(require 'org-clock)

;;;; General utils
(defun org-query-util-word-wrap (len prepend s)
  (with-temp-buffer
    (insert s)
    (let ((fill-column len)
          (remaining-lines nil))
      (fill-region 1 (point-max)))
    (goto-char 0)
    (setq remaining-lines (count-lines 1 (point-max)))
    (while (< 0 remaining-lines)
      (forward-line)
      (insert prepend)
      (setq remaining-lines (1- remaining-lines)))
    (buffer-substring 1 (point-max))
    ))

(defun org-query-util-ljust (str len)
  (if (< (length str) len)
      ;; 32 is space char
      (concat str (make-string (- len (length str)) 32))
    str
    )
  )

(defun org-query-util-rstrip (str)
  (if (string-match "[ \t]*$" str)
      (concat (replace-match "" nil nil str))
    str)
  )

(defun org-query-util-get-hash-keys (hashtable)
  (let (allkeys)
    (maphash (lambda (k v) (setq allkeys (cons k allkeys))) hashtable)
    allkeys
    )
  )
(defun org-query-util-get-hash-values (hashtable)
  (let (vals allvals)
    (maphash (lambda (k v) (setq vals (cons v allvals))) hashtable)
    allvals
    )
  )

;;;; Utils specific to org query

(defun org-query-get-buffer ()
  (save-excursion
    (get-buffer-create org-query-buffer-name)
    ))

(defun org-query-get-temp-file-buffer (filepath)
  ;; Create buffer if doesn't exist, and save it in hash table.
  (save-excursion

    (if (not (gethash filepath org-query--temp-buffers))
        (progn
          (set-buffer (get-buffer-create
                       (format " *org-query-temp-%s*" filepath)))
          (insert-file-contents filepath)
          (org-mode)
          (org-clock-sum) ; This makes :org-clock-minutes property available
          (puthash filepath (current-buffer) org-query--temp-buffers)))

    (set-buffer (gethash filepath org-query--temp-buffers))
    (current-buffer)
    )
  )
(defun org-query-clear-buffer ()
  (save-excursion
    (set-buffer (org-query-get-buffer))
    (erase-buffer)
    )
  )


(defun org-query-kill-temp-buffers ()
  (interactive)
  (maphash (lambda (filename buffer) (kill-buffer buffer)) org-query--temp-buffers)
  (clrhash org-query--temp-buffers)
  )
(defun org-query-clear-cache ()
  (clrhash org-query--cache-up-heading-safe)
  (clrhash org-query--cache-deadline-time)
  (clrhash org-query--cache-scheduled-time)
  (clrhash org-query--cache-tags-at)
  (clrhash org-query--cache-priority)
  (clrhash org-query--cache-cmp)
  (clrhash org-query--cache-todo-state)
  (clrhash org-query--cache-entry-get)
  (clrhash org-query--cache-heading)
  (clrhash org-query--cache-goto-sibling)
  (clrhash org-query--cache-outline-level)
  (clrhash org-query--cache-goto-first-child)
  (clrhash org-query--cache-tags-string)
  (clrhash org-query--cache-entry-properties)
  (clrhash org-query--cache-filter-down)
  (clrhash org-query--cache-filter-up)
  (clrhash org-query--cache-outline-next-heading)
  (clrhash org-query--cache-thing-at-point)
  )

(defun org-query-get-files ()
  (org-agenda-files)
  )

(defun org-query-util-goto-location (file-name pt)
  (pop-to-buffer (get-buffer-create (find-file file-name)))

  ;; If the item is visible, assume that the buffer is already displaying
  ;; the context that the user wants. If it's invisible, show the
  ;; context of the parent and toggle the parent to display all
  ;; sibling headlines.
  (goto-char pt)
  (if (org-invisible-p2)
      (progn
        (org-up-heading-safe) (org-show-context) (org-cycle)))

                                        ; The cursor should be at the point requested
  (goto-char pt)
  (beginning-of-line)
  )

(defun org-query-get-priority-face ()
  (save-excursion
    (if (string-match org-priority-regexp (org-get-heading))
        (let* (
               (this-heading (org-get-heading))
               (priority-match (string-match org-priority-regexp (org-get-heading)))
               (priority-cookie (match-string 1 this-heading))
               (char-point 2) ;; [#A] - A is starting point + 2.
               (priority-char-num (get-byte char-point priority-cookie))
               (priority-face (assoc priority-char-num org-priority-faces)) 
               )
          ;;(propertize (priority-cookie 'face priority-face))
          priority-face
          )
      )))

(defun org-query-get-level-face (level-n)
  "Get the right face for headline level N. Uses the selection logic
from org-get-level-face (but doesn't rely on previous searches - I not
completely certain how that function works).
"
  (if org-cycle-level-faces
      (nth (% (1- level-n) org-n-level-faces) org-level-faces)
    (nth (1- (min level-n org-n-level-faces)) org-level-faces)))

(defun org-query-get-deadline-time (POM)
  (org-get-deadline-time POM org-query-inherit-dates))

(defun org-query-get-scheduled-time (POM)
  (org-get-scheduled-time POM org-query-inherit-dates))

(defun org-query-get-priority (POM)
  (if (not org-query-inherit-priority)
      (org-get-priority (thing-at-point 'line))
    (save-excursion
      (let ((keep-searching t))
        (while keep-searching
          (if (string-match org-priority-regexp (thing-at-point 'line))
              (setq keep-searching nil)
            (setq keep-searching (org-up-heading-safe))))
        (org-get-priority (thing-at-point 'line))))))

;;;; Structs

(defstruct org-query-item
  "org-query representation of a single headline item."
  (filename nil)
  (buffer nil)
  (point nil)
  (children nil)
  (repr nil))

(defstruct org-query-group
  (name nil)
  (items nil))

(defstruct org-query-config
  ;; Will need to add default arguments later
  (name nil)
  (description "(no description)")
  (files (org-query-get-files))
  (parse-fn 'org-query-parse-items-horizontal-half)
  (filter nil)
  (group-fn nil)
  (sort-criteria nil))

;;;; Variables

(defvar org-query-inherit-dates t)
(defvar org-query-inherit-priority t)

(defvar org-query-buffer-name "*org-query*")

(defvar org-query-time-format "%a %d %b %Y")

(defvar org-query-list '(org-query-fn-context)
  )

(defvar org-query--temp-buffers
  (make-hash-table :size 40 :test 'equal)) ; {filename: temp-buffer}

(defvar org-query-review-property "LAST_REVIEWED")
(defvar org-query--cache-cmp
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-tags-at
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-deadline-time
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-scheduled-time
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-up-heading-safe
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-priority
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-todo-state
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-entry-get
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-heading
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-goto-sibling
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-outline-level
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-goto-first-child
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-tags-string
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-entry-properties
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-filter-down
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-filter-up
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-outline-next-heading
  (make-hash-table :size 100000 :test 'equal))
(defvar org-query--cache-thing-at-point
  (make-hash-table :size 100000 :test 'equal))
;;;; Advice

(defadvice org-up-heading-safe (around org-query-adv first)
  "Cache org-up-heading-safe result for this buffer/point."
  (let* ((key `(,(current-buffer) ,(point)))
         (cached-val (gethash key org-query--cache-up-heading-safe)))
    (if cached-val
        (progn
          (set-buffer (car cached-val))
          (goto-char (nth 1 cached-val))
          (setq ad-return-value (nth 2 cached-val)))
      ad-do-it
      (puthash key `(,(current-buffer) ,(point) ,(symbol-value 'ad-return-value))
               org-query--cache-up-heading-safe))))

(defadvice outline-next-heading (around org-query-adv first)
  "Cache org-up-heading-safe result for this buffer/point."
  (let* ((key `(,(current-buffer) ,(point)))
         (cached-val (gethash key org-query--cache-outline-next-heading)))
    (if cached-val
        (progn
          (set-buffer (car cached-val))
          (goto-char (nth 1 cached-val))
          (setq ad-return-value (nth 2 cached-val)))
      ad-do-it
      (puthash key `(,(current-buffer) ,(point) ,(symbol-value 'ad-return-value))
               org-query--cache-outline-next-heading))))


(defadvice org-get-priority (around org-query-adv first)
  (let* ((s (ad-get-arg 0))
         (cached-val (gethash s org-query--cache-priority "notfound")))
    (if (not (equal cached-val "notfound"))
        (setq ad-return-value cached-val)
      ad-do-it
      (puthash s ad-return-value org-query--cache-priority))))

(defadvice org-get-deadline-time (around org-query-adv first)
  ;; POM is required as arg 0, so no need to save point separately.
  (let* ((key `(,(current-buffer) ,(ad-get-args 0)))
         (cached-val (gethash key org-query--cache-deadline-time "notfound")))
    (if (not (equal cached-val "notfound"))
        (setq ad-return-value cached-val)
      ad-do-it
      (puthash key ad-return-value org-query--cache-deadline-time))))

(defadvice org-get-scheduled-time (around org-query-adv first)
  ;; POM is required as arg 0, so no need to save point separately.
  (let* ((key `(,(current-buffer) ,(ad-get-args 0)))
         (cached-val (gethash key org-query--cache-scheduled-time "notfound")))
    (if (not (equal cached-val "notfound"))
        (setq ad-return-value cached-val)
      ad-do-it
      (puthash key ad-return-value org-query--cache-scheduled-time))))

(defadvice org-get-tags-at (around org-query-adv first)
  ;; POM is an optional arg, but this makes it easier to generate the key
  (if (not (ad-get-arg 0))
      (ad-set-arg 0 (point)))
  (let* ((key `(,(current-buffer) ,(ad-get-args 0)))
         (cached-val (gethash key org-query--cache-tags-at "notfound")))
    (if (not (equal cached-val "notfound"))
        (setq ad-return-value cached-val)
      ad-do-it
      (puthash key ad-return-value org-query--cache-tags-at))))


(defadvice org-get-todo-state (around org-query-adv first)
  (let* ((key `(,(current-buffer) ,(point)))
         (cached-val (gethash key org-query--cache-todo-state "notfound")))
    (if (not (equal cached-val "notfound"))
        (setq ad-return-value cached-val)
      ad-do-it
      (puthash key ad-return-value org-query--cache-todo-state))))

(defadvice org-entry-get (around org-query-adv first)
  ;; Arg 0 is POM, if nil current point is used by org-with-point-at
  (let* ((key (if (ad-get-arg 0)
                  `(,(current-buffer) ,(ad-get-args 0))
                `(,(current-buffer) ,(point) ,(ad-get-args 0))))
         (cached-val (gethash key org-query--cache-entry-get "notfound")))
    (if (not (equal cached-val "notfound"))
        (setq ad-return-value cached-val)
      ad-do-it
      (puthash key ad-return-value org-query--cache-entry-get))))

(defadvice org-entry-properties (around org-query-adv first)
  ;; POM is an optional arg, but this makes it easier to generate the key
  (if (not (ad-get-arg 0))
      (ad-set-arg 0 (point)))
  (let* ((key `(,(current-buffer) ,(ad-get-args 0)))
         (cached-val (gethash key org-query--cache-entry-properties "notfound")))
    (if (not (equal cached-val "notfound"))
        (setq ad-return-value cached-val)
      ad-do-it
      (puthash key ad-return-value org-query--cache-entry-properties))))

(defadvice org-get-heading (around org-query-adv first)
  (let* ((key `(,(current-buffer) ,(point) ,(ad-get-args 0)))
         (cached-val (gethash key org-query--cache-heading "notfound")))
    (if (not (equal cached-val "notfound"))
        (setq ad-return-value cached-val)
      ad-do-it
      (puthash key ad-return-value org-query--cache-heading))))

(defadvice org-get-tags-string (around org-query-adv first)
  (let* ((key `(,(current-buffer) ,(point) ,(ad-get-args 0)))
         (cached-val (gethash key org-query--cache-tags-string "notfound")))
    (if (not (equal cached-val "notfound"))
        (setq ad-return-value cached-val)
      ad-do-it
      (puthash key ad-return-value org-query--cache-tags-string))))


(defadvice org-goto-sibling (around org-query-adv first)
  (let* ((key `(,(current-buffer) ,(point) ,(ad-get-args 0)))
         (cached-val (gethash key org-query--cache-goto-sibling)))
    (if cached-val
        (progn
          (set-buffer (car cached-val))
          (goto-char (nth 1 cached-val))
          (setq ad-return-value (nth 2 cached-val)))
      ad-do-it
      (puthash key `(,(current-buffer) ,(point) ,(symbol-value 'ad-return-value))
               org-query--cache-goto-sibling))))

(defadvice org-goto-first-child (around org-query-adv first)
  (let* ((key `(,(current-buffer) ,(point)))
         (cached-val (gethash key org-query--cache-goto-first-child)))
    (if cached-val
        (progn
          (set-buffer (car cached-val))
          (goto-char (nth 1 cached-val))
          (setq ad-return-value (nth 2 cached-val)))
      ad-do-it
      (puthash key `(,(current-buffer) ,(point) ,(symbol-value 'ad-return-value))
               org-query--cache-goto-first-child))))

(defadvice org-outline-level (around org-query-adv first)
  (let* ((key `(,(current-buffer) ,(point)))
         (cached-val (gethash key org-query--cache-outline-level "notfound")))
    (if (not (equal cached-val "notfound"))
        (setq ad-return-value cached-val)
      ad-do-it
      (puthash key ad-return-value org-query--cache-outline-level))))

(defadvice thing-at-point (around org-query-adv first)
  (let* ((key `(,(current-buffer) ,(point) ,(ad-get-args 0)))
         (cached-val (gethash key org-query--cache-thing-at-point "!!!OPnotfoundfp$6")))
    (if (not (equal cached-val "!!!OPnotfoundfp$6"))
        (setq ad-return-value cached-val)
      ad-do-it
      (puthash key ad-return-value org-query--cache-thing-at-point))))


(defun org-query-advise-activate ()
  (interactive)
  (org-query-advise-deactivate)
  (ad-activate-regexp "^org-query-adv"))
  

(defun org-query-advise-deactivate ()
  (interactive)
  (ad-deactivate-regexp "^org-query-adv"))
(defun org-query-recover () ;; Perhaps we don't need this
  (interactive)
  (ad-recover 'org-up-heading-safe)
  (ad-recover 'org-get-priority)
  (ad-recover 'org-get-deadline-time)
  (ad-recover 'org-get-scheduled-time)
  (ad-recover 'org-get-tags-at)
  (ad-recover 'org-get-todo-state)
  (ad-recover 'org-entry-get)
  (ad-recover 'org-entry-properties)
  (ad-recover 'org-get-heading)
  (ad-recover 'org-get-tags-string)
  (ad-recover 'org-goto-sibling)
  (ad-recover 'org-goto-first-child)
  (ad-recover 'org-outline-level)
  (ad-recover 'outline-next-heading)
  (ad-recover 'outline-thing-at-point)
  )

;;;; Dispatcher

(defun org-query ()
  (interactive)

  (org-query-advise-deactivate)

  (pop-to-buffer (set-buffer (org-query-get-buffer)))
  (setq buffer-read-only nil)

  (org-query-clear-buffer)

  (let (
        (query)
        (btn-action (lambda (btn)
                      (setq buffer-read-only nil)
                      (org-query-fn (button-get btn 'btn-query))))
        )

    (insert "* Org-query: dispatcher\n\n")
    (insert "  - Each text button generates a query.\n")
    (insert "  - This buffer accepts the standard org-mode cycling commands.\n")
    (insert "    Show all with `org-shifttab to see query descriptions.\n\n")
    (dolist (query org-query-list)
      (insert (format "* %s\n"
                      (with-temp-buffer
                        
                        (insert-text-button
                         (format "[%s]" (org-query-config-name query))
                         'btn-query query
                         'action btn-action
                         )
                        (buffer-substring (point-min) (point-max))
                        )
                      ))

      (insert (format "  - %s\n\n"
                      (org-query-util-word-wrap
                       60
                       (make-string 4 32)
                       (org-query-config-description query))))
      )
    )

  (org-query-mode)
  (setq buffer-read-only t)
  (org-shifttab 1)
  (goto-char (point-min))
  (org-query-cycle)

  )

;;;; Query functions

(defun org-query-fn (config)

  (org-query-advise-deactivate)
  (org-query-advise-activate)
  (org-query-clear-cache)
  
  ;; Swap to query buffer
  (pop-to-buffer (set-buffer (org-query-get-buffer)))
  (setq buffer-read-only nil)

  ;; Clear any existing data
  (org-query-kill-temp-buffers)
  (org-query-clear-buffer)

  ;; Insert title etc
  (goto-char (point-min))
  (insert (format "* Org-query: %s\n\n" (org-query-config-name config)))
  
  (let (
        (dispatcher (lambda (btn) (org-query)))
        (reload (lambda (btn) (org-query-fn (button-get btn 'btn-config))))
        )

    (insert (format "  - %s\n\n"
                    (org-query-util-word-wrap
                     60
                     (make-string 4 32)
                     (org-query-config-description config))))

    (insert (format "  - %s\n"
                    (with-temp-buffer
                      (insert-text-button "[Reload]"
                                          'action reload
                                          'btn-config config)
                      (buffer-substring (point-min) (point-max))
                      )))

    (insert (format "  - %s\n"
                    (with-temp-buffer
                      (insert-text-button "[Back to dispatcher]" 'action dispatcher)
                      (buffer-substring (point-min) (point-max))
                      )))
    )
  (insert "\n\n")

  (let (
        this-file
        this-item-list
        this-group
        (items '())
        (groups '())
        )


    ;; Parse all files to build list of item objects
    (dolist (this-file (org-query-config-files config))
      (save-excursion
        (set-buffer (org-query-get-temp-file-buffer this-file))
        (goto-char (point-min)) ; as insurance
        (org-first-headline-recenter) ; goto first headline
        (setq this-item-list (funcall (org-query-config-parse-fn config)
                                      (org-query-config-filter config)
                                      this-file
                                      (point)
                                      ))
        (setq items (append items this-item-list))
        ))

    ;; Arrange the items into list of group objects
    (setq groups 
          (if (org-query-config-group-fn config)
              (funcall (org-query-config-group-fn config) items)
            ;; The user isn't forced to specify a grouping function
            `(,(make-org-query-group
                :name "Items (no grouping)"
                :items items))))

    ;; Sort and insert each group
    (save-excursion
      (dolist (this-group groups)
        (setf (org-query-group-items this-group)
              (org-query-sort-items (org-query-group-items this-group)
                                           (org-query-config-sort-criteria config)))

        (org-query-insert-group this-group)
        )
      )

    ;; Tidy up
    (org-query-kill-temp-buffers)
    (org-query-mode) ; enable minor mode
    (goto-char (point-min))
    )

  ;; Force user to save file if they want to edit the buffer. Otherwise edits
  ;; might be overwritten when running another org-query command.
  (setq buffer-read-only nil)
  (org-query-advise-deactivate)
  )

;;;; Parsing

(defun org-query-parse-items-vertical (filter filename point)
  " Parse all headlines into nested list of item structs, maintaining the
natural order of subtrees. Subtrees are parsed until the point where an item
fails to pass the filter.
"
  (save-excursion
    (goto-char point)
    (let
        ((item-list '())
         (elements-remaining t)
         (item-table nil)
         (child-point nil)
         (children nil)
         (next-sibling-point nil))
      (while elements-remaining
        (if (save-excursion (eval filter))
            (progn
              (setq child-point
                    (save-excursion
                      (if (org-goto-first-child) (point) nil)))
              (setq children
                    (if child-point
                        (org-query-parse-items-vertical
                         filter filename child-point)
                      nil))
              (setq item-list 
                    (append item-list
                            `(,(make-org-query-item
                                :filename filename
                                :buffer (current-buffer)
                                :point (point)
                                :children children
                                :repr (org-get-heading)))))))
        (setq next-sibling-point (org-get-next-sibling)) ;; This moves point location
        (if (not next-sibling-point) (setq elements-remaining nil)))
      item-list)))

(defun org-query-parse-items-horizontal-half (filter filename point)
  "Parse all headlines into a flat list, regardless of their natural depth.
If an item in a subtree fails the filter, don't parse the children of that item.
"
  (save-excursion
    (goto-char point)
    (let
        ((item-list '())
         (elements-remaining t)
         item
         child-point
         next-sibling-point)

      (while elements-remaining
        (if (save-excursion (eval filter))
            (progn
              (setq item (make-org-query-item
                          :filename filename
                          :buffer (current-buffer)
                          :point (point)
                          :children nil
                          :repr (org-get-heading)))
              (setq item-list (append item-list `(,item)))
              
              (setq child-point (save-excursion (if
                                                    (org-goto-first-child)
                                                    (point)
                                                  nil)))
              (if child-point
                  (setq item-list
                        (append item-list (org-query-parse-items-horizontal-half
                                           filter filename child-point))))
              )
          )
        (setq next-sibling-point (org-get-next-sibling)) ;; This moves point location
        (if (not next-sibling-point) (setq elements-remaining nil))
        )
      ;; Return the items
      item-list
      )
    )
  )

(defun org-query-parse-items-horizontal-full (filter filename point)
  "Parse all headlines into a flat list, regardless of their natural depth.
Continue parsing child items even if a parent in the subtree fails to pass the filter.
"
  (save-excursion
    (goto-char point)
    (let
        ((item-list '())
         (elements-remaining t)
         item
         child-point
         next-sibling-point)

      (while elements-remaining
        (if (save-excursion (eval filter))
            (progn
              (setq item (make-org-query-item
                          :filename filename
                          :buffer (current-buffer)
                          :point (point)
                          :children nil
                          :repr (org-get-heading)))
              (setq item-list (append item-list `(,item)))))

        (setq child-point (save-excursion (if
                                              (org-goto-first-child)
                                              (point)
                                            nil)))
        (if child-point
            (setq item-list
                  (append item-list (org-query-parse-items-horizontal-full
                                     filter filename child-point))))
        (setq next-sibling-point (org-get-next-sibling)) ;; This moves point location
        (if (not next-sibling-point) (setq elements-remaining nil)))

      ;; Return the items
      item-list)))

;;;; Filtering

(defun* org-query-filter-apply (match-exp &key (self t) (parents nil) (children nil))
  (save-excursion
    (let ((match-found-p nil))
      (if self
          (setq match-found-p (eval match-exp)))
      (save-excursion
        (if (and parents (not match-found-p) (org-up-heading-safe))
            (setq match-found-p (org-query--filter-parents match-exp))))
      (save-excursion
        (if (and children (not match-found-p) (org-goto-first-child))
            (setq match-found-p (org-query--filter-children match-exp))))
      match-found-p)))

(defun org-query--filter-parents (match-exp)
  (save-excursion
    (let ((match-found-p (eval match-exp)))
      (if (and (not match-found-p) (org-up-heading-safe))
          (org-query--filter-parents match-exp)
        match-found-p))))
  
(defun org-query--filter-children (match-exp)
  (save-excursion
    ;; First check current headline
    (let* ((match-found-p (eval match-exp)))
      ;; Then descend through children
      (save-excursion 
        (if (and (not match-found-p) (org-goto-first-child))
            (setq match-found-p (org-query--filter-children match-exp))))
      ;; Then through siblings
      (save-excursion
        (if (and (not match-found-p) (org-goto-sibling))
            (setq match-found-p (org-query--filter-children match-exp))))
      match-found-p)))

(defun org-query-filter-keyword-p (user-fn)
  (funcall user-fn (org-get-todo-state)))

(defun org-query-filter-keyword-matches-p (keyword-list)
  ;; For convenience, instead of using filter-keyword-p
  (let ((kwd (org-get-todo-state)))
    ;; Allow user to match against nil or "" to represent a missing keyword.
    (if (not kwd) 
        (setq kwd '(nil "")))
    (member kwd keyword-list)))

(defun org-query-filter-tags-p (user-fn)
  ;; Get only local tags. Can use :up to get inherited.
  (funcall user-fn (org-get-tags-at (point) t)))

(defun org-query-filter-tag-matches-p (user-tag-list)
  ;; For convenience, instead of using filter-tags-p
  (let ((matches-p nil)
        (this-tag nil)
        (tags (org-get-tags-at (point) t)))
    ;; Allow user to match against nil or "" (as this isn't a valid org
    ;; tag) to represent a missing tag.
    (if (not tags) 
        (setq tags '(nil "")))
    (dolist (this-tag tags)
      (if (member this-tag user-tag-list)
          (setq matches-p t)))
    matches-p))

(defun org-query-filter-priority-level-p (user-fn)
  (funcall user-fn (org-query-get-priority (point))))

(defun org-query-filter-outline-level-p (user-fn)
  (funcall user-fn (org-outline-level)))

(defun org-query-filter-archived-p ()
  ;; There's probably a better way to determine this
  (member "ARCHIVE" (org-get-tags-at (point))))

(defun org-query-filter-deadline-in-days-p (user-fn)
  (let ((deadline-time (org-query-get-deadline-time (point)))
        (days nil))
    (setq days ; Number of days between now and deadline
          (if deadline-time
              (time-to-number-of-days
               (time-subtract deadline-time (org-current-effective-time)))
            nil))
    (funcall user-fn days)))

(defun org-query-filter-scheduled-in-days-p (user-fn)
  (let ((scheduled-time (org-query-get-scheduled-time (point)))
        (days nil))
    (setq days ; Number of days between now and scheduled
          (if scheduled-time
              (time-to-number-of-days
               (time-subtract scheduled-time (org-current-effective-time)))
            nil))
    (funcall user-fn days)))

(defun org-query-filter-clocked-mins-p (user-fn)
  (let ((mins (org-clock-sum-current-item)))
    ;; Just in case, but org-clock-sum-current-item should return 0 if
    ;; nothing clocked
    (if (not mins)
        (setq mins 0))
    (funcall user-fn mins)))

(defun org-query-filter-property-exists-p (property)
  (let ((val (org-entry-get (point) property)))
    (if (eq val nil) nil t)))

(defun org-query-filter-has-children-p ()
  (save-excursion (org-goto-first-child)))


;;;; Grouping

(defun org-query-group-tag-local (items)
  (org-query--group-tag items t)
  )
(defun org-query-group-tag-inherit (items)
  (org-query--group-tag items nil)
  )
(defun org-query--group-tag (items tag-local-p)
  (save-excursion
    (let (
          (single-tag-groups (make-hash-table :size 20 :test 'equal)) ; {tag: item-list}
          (multi-tag-groups (make-hash-table :size 100 :test 'equal)) ; {tag: item-list}
          (no-tag-group '())
          this-item
          this-tag-list
          this-tag
          this-multi-tag
          this-group
          this-group-name
          group-list
          )

      (dolist (this-item items)
        (set-buffer (org-query-item-buffer this-item))
        (goto-char (org-query-item-point this-item))
        (setq this-tag-list
              (sort (org-get-tags-at (point) tag-local-p) 'string<))

        (cond
         ;; No tags
         ((= 0 (length this-tag-list))
          (add-to-list 'no-tag-group this-item))

         ;; 1 tag
         ((= 1 (length this-tag-list))
          (setq this-tag (format ":%s:" (car this-tag-list)))
          (setq this-group (gethash this-tag single-tag-groups))
          (puthash this-tag (add-to-list 'this-group this-item) single-tag-groups))

         ;; Multiple tags
         ((> (length this-tag-list) 1)

          ;; Multi tag groups
          (setq this-multi-tag
                (let (loop-tag loop-result)
                  (format "%s:" (dolist (loop-tag
                                         (sort this-tag-list 'string<)
                                         loop-result)
                                  (setq loop-result
                                        (concat loop-result ":" loop-tag))))))
          (setq this-group (gethash this-multi-tag multi-tag-groups))
          (puthash this-multi-tag (add-to-list 'this-group this-item) multi-tag-groups)

          ;; Single tag groups
          (dolist (this-tag this-tag-list)
            (setq this-tag (format ":%s:" this-tag))
            (setq this-group (gethash this-tag single-tag-groups))
            (puthash this-tag (add-to-list 'this-group this-item) single-tag-groups)))

         )) ; End item loop

      ;; No-tag group
      (if (> (length no-tag-group) 0)
          (progn
            (setq this-group (make-org-query-group
                              :name "NO TAGS"
                              :items (reverse no-tag-group)))
            (add-to-list 'group-list this-group)))


      ;; Multi-tag groups
      (dolist (this-group-name (reverse (sort
                                         (org-query-util-get-hash-keys multi-tag-groups)
                                         'string<)))
        (setq this-group (make-org-query-group
                          :name (propertize this-group-name 'face (org-get-tag-face this-group-name))
                          :items (reverse (gethash this-group-name multi-tag-groups))
                          ))
        (add-to-list 'group-list this-group)
        )

      ;; Add single-tag groups to beginning of result
      (dolist (this-group-name (reverse (sort
                                         (org-query-util-get-hash-keys single-tag-groups)
                                         'string<)))
        (setq this-group (make-org-query-group
                          :name (propertize this-group-name 'face (org-get-tag-face this-group-name))
                          :items (reverse (gethash this-group-name single-tag-groups))
                          ))
        (add-to-list 'group-list this-group)
        )



      ;; Return group list
      group-list
      )
    )
  )

(defun org-query-group-file (items)
  (save-excursion
    (let ((file-groups (make-hash-table :size 100 :test 'equal))
          (this-item nil)
          (this-filename nil)
          (this-group nil)
          (this-group-name nil)
          (group-list nil))
      ;; Populate hash table with {groupname: group items}.
      (dolist (this-item items)
        (setq this-filename (org-query-item-filename this-item))
        (setq this-group (gethash this-filename file-groups))
        (puthash this-filename (add-to-list 'this-group this-item) file-groups))
      ;; Make the group objects
      (dolist (this-group-name (reverse (sort (org-query-util-get-hash-keys
                                               file-groups) 'string<)))
        (setq this-group (make-org-query-group
                          :name this-group-name
                          :items (reverse (gethash this-group-name file-groups))))
        (add-to-list 'group-list this-group))
      group-list)))

(defun org-query-group-keyword (items)
  (save-excursion
    (let ((items-table (make-hash-table :size 50 :test 'equal))
          (this-item nil)
          (this-group nil)
          (this-group-key nil)
          (this-group-name nil)
          (no-keyword-group nil)
          (group-list nil))
      (dolist (this-item items)
        (set-buffer (org-query-item-buffer this-item))
        (goto-char (org-query-item-point this-item))
        (setq this-group-key (org-get-todo-state))
        ;; Put the no-keyword group in a separate variable so it can be placed
        ;; at the end of the list of groups.
        (if (not this-group-key)
            (add-to-list 'no-keyword-group this-item)
          (setq this-group (gethash this-group-key items-table))
          (puthash this-group-key (add-to-list 'this-group this-item) items-table)))
      (if (> (length no-keyword-group) 0)
          (add-to-list 'group-list 
                       (make-org-query-group
                        :name "No keyword"
                        :items (reverse no-keyword-group))))
      
      
      (dolist (this-group-name (reverse (sort (org-query-util-get-hash-keys
                                               items-table) 'string<)))
        (add-to-list 'group-list
                     (make-org-query-group
                      :name (propertize this-group-name 'face
                                        (org-get-todo-face this-group-name))
                      :items (reverse (gethash this-group-name items-table)))))
      group-list)))




(defun org-query-group-priority (items)
                                        ; One group for each level
  (save-excursion
    (let ((items-table (make-hash-table :size 50 :test 'equal))
          (this-item nil)
          (this-group nil)
          (this-group-key nil)
          (this-group-name nil)
          (group-list nil))
      (dolist (this-item items)
        (set-buffer (org-query-item-buffer this-item))
        (goto-char (org-query-item-point this-item))
        (setq this-group-key (org-query-get-priority (point)))
        (setq this-group (gethash this-group-key items-table))
        (puthash this-group-key (add-to-list 'this-group this-item) items-table))
      (dolist (this-group-name (sort (org-query-util-get-hash-keys
                                      items-table) '<))
        (add-to-list 'group-list
                     (make-org-query-group
                      :name (format "Priority level: %d" this-group-name)
                      :items (reverse (gethash this-group-name items-table)))))
      group-list)))

(defun org-query-group-review (items)
  (save-excursion
    (let ((group-data
           (list
            ;; List of group names and functions that test group membership.
            ;; d = time-to-number-of-days(current-time - LAST_REVIEWED time)
            '("Reviewed in the future?!"
              (lambda (d) (> d 0)))
            '("Reviewed more than 1 year ago"
              (lambda (d) (< d -365)))
            '("Reviewed up to 1 year ago"
              (lambda (d) (and (>= d -365) (< d -180))))
            '("Reviewed up to 6 months ago"
              (lambda (d) (and (>= d -180) (< d -90))))
            '("Reviewed up to 3 months ago"
              (lambda (d) (and (>= d -90) (< d -42))))
            '("Reviewed up to 6 weeks ago"
              (lambda (d) (and (>= d -42) (< d -21))))
            '("Reviewed up to 3 weeks ago"
              (lambda (d) (and (>= d -21) (< d -14))))
            '("Reviewed up to 2 weeks ago"
              (lambda (d) (and (>= d -14) (< d -7))))
            '("Reviewed up to 1 week ago"
              (lambda (d) (and (>= d -7) (< d -1))))
            '("Reviewed today"
              (lambda (d) (and (>= d -1) (<= d 0))))))
          (items-table (make-hash-table :size 20 :test 'equal))
          (no-date-group nil)
          (this-days nil)
          (this-review nil)
          (group-i nil)
          (this-item nil)
          (this-group nil)
          (this-group-key nil)
          (this-group-data nil)
          (this-group-items nil)
          (group-list nil))
      (dolist (this-item items)
        (set-buffer (org-query-item-buffer this-item))
        (goto-char (org-query-item-point this-item))
        (setq this-review (org-entry-get (point) org-query-review-property t))
        (if this-review
            (progn
              (setq this-days
                    (time-to-number-of-days
                     (time-subtract
                      (apply 'encode-time (org-parse-time-string this-review))
                      (org-current-effective-time))))
              (setq group-i 0)
              (while (< group-i (length group-data))
                (if (funcall (nth 1 (nth group-i group-data)) this-days)
                    (progn
                      (setq this-group-key (car (nth group-i group-data)))
                      (setq this-group (gethash this-group-key items-table))
                      (puthash this-group-key (add-to-list 'this-group this-item) items-table)
                      (setq group-i (length group-data)))
                  (setq group-i (+ group-i 1)))))
          (add-to-list 'no-date-group this-item)))
      (dolist (this-group-data (reverse group-data))
        (setq this-group-items (gethash (car this-group-data) items-table))
        (if this-group-items
            (add-to-list 'group-list
                         (make-org-query-group
                          :name (car this-group-data)
                          :items this-group-items))))
      (if no-date-group
          (add-to-list 'group-list
                       (make-org-query-group
                        :name "No review property set"
                        :items no-date-group)))
      group-list)))



(defun org-query-group-deadline (items)
  (save-excursion
    (let ((group-data
           (list
            '("Deadline was due more than 1 year ago"
              (lambda (d) (< d -365)))
            '("Deadline was due up to 1 year ago"
              (lambda (d) (and (>= d -365) (< d -180))))
            '("Deadline was due up to 6 months ago"
              (lambda (d) (and (>= d -180) (< d -90))))
            '("Deadline was due up to 3 months ago"
              (lambda (d) (and (>= d -90) (< d -42))))
            '("Deadline was due up to 6 weeks ago"
              (lambda (d) (and (>= d -42) (< d -21))))
            '("Deadline was due up to 3 weeks ago"
              (lambda (d) (and (>= d -21) (< d -7))))
            '("Deadline was due up to 1 week ago"
              (lambda (d) (and (>= d -7) (< d 0))))
            '("Deadline is due in up to 1 week (including today)"
              (lambda (d) (and (>= d 0) (<= d 7))))
            '("Deadline is due in up to 3 weeks"
              (lambda (d) (and (> d 7) (<= d 21))))
            '("Deadline is due in up to 6 weeks"
              (lambda (d) (and (> d 21) (<= d 42))))
            '("Deadline is due in up to 3 months"
              (lambda (d) (and (> d 42) (<= d 90))))
            '("Deadline is due in up to 6 months"
              (lambda (d) (and (> d 90) (<= d 180))))
            '("Deadline is due in up to 1 year"
              (lambda (d) (and (> d 180) (<= d 365))))
            '("Deadline is due in more than 1 year"
              (lambda (d) (> d 365)))))
          (items-table (make-hash-table :size 20 :test 'equal))
          (no-date-group nil)
          (this-days nil)
          (this-deadline nil)
          (group-i nil)
          (this-item nil)
          (this-group nil)
          (this-group-key nil)
          (this-group-data nil)
          (this-group-items nil)
          (group-list nil))
      (dolist (this-item items)
        (set-buffer (org-query-item-buffer this-item))
        (goto-char (org-query-item-point this-item))
        (setq this-deadline (org-query-get-deadline-time (point)))
        (if this-deadline
            (progn
              (setq this-days
                    (time-to-number-of-days
                     (time-subtract this-deadline (org-current-effective-time))))
              (setq group-i 0)
              (while (< group-i (length group-data))
                (if (funcall (nth 1 (nth group-i group-data)) this-days)
                    (progn
                      (setq this-group-key (car (nth group-i group-data)))
                      (setq this-group (gethash this-group-key items-table))
                      (puthash this-group-key (add-to-list 'this-group this-item) items-table)
                      (setq group-i (length group-data)))
                  (setq group-i (+ group-i 1)))))
          (add-to-list 'no-date-group this-item)))
      (if no-date-group
          (add-to-list 'group-list
                       (make-org-query-group
                        :name "No deadline"
                        :items no-date-group)))
      (dolist (this-group-data (reverse group-data))
        (setq this-group-items (gethash (car this-group-data) items-table))
        (if this-group-items
            (add-to-list 'group-list
                         (make-org-query-group
                          :name (car this-group-data)
                          :items this-group-items))))
      group-list)))

(defun org-query-group-scheduled (items)
  (save-excursion
    (let ((group-data
           (list
            '("Scheduled in more than 1 year"
              (lambda (d) (> d 365)))
            '("Scheduled in up to 1 year"
              (lambda (d) (and (> d 180) (<= d 365))))
            '("Scheduled in up to 6 months"
              (lambda (d) (and (> d 90) (<= d 180))))
            '("Scheduled in up to 3 months"
              (lambda (d) (and (> d 42) (<= d 90))))
            '("Scheduled in up to 6 weeks"
              (lambda (d) (and (> d 21) (<= d 42))))
            '("Scheduled in up to 3 weeks"
              (lambda (d) (and (> d 7) (<= d 21))))
            '("Scheduled in up to 1 week (including today)"
              (lambda (d) (and (>= d 0) (<= d 7))))
            '("Was scheduled up to 1 week ago"
              (lambda (d) (and (>= d -7) (< d 0))))
            '("Was scheduled up to 3 weeks ago"
              (lambda (d) (and (>= d -21) (< d -7))))
            '("Was scheduled up to 6 weeks ago"
              (lambda (d) (and (>= d -42) (< d -21))))
            '("Was scheduled up to 3 months ago"
              (lambda (d) (and (>= d -90) (< d -42))))
            '("Was scheduled up to 6 months ago"
              (lambda (d) (and (>= d -180) (< d -90))))
            '("Was scheduled up to 1 year ago"
              (lambda (d) (and (>= d -365) (< d -180))))
            '("Was scheduled more than 1 year ago"
              (lambda (d) (< d -365)))))
          (items-table (make-hash-table :size 20 :test 'equal))
          (no-date-group nil)
          (this-days nil)
          (this-scheduled nil)
          (group-i nil)
          (this-item nil)
          (this-group nil)
          (this-group-key nil)
          (this-group-data nil)
          (this-group-items nil)
          (group-list nil))
      (dolist (this-item items)
        (set-buffer (org-query-item-buffer this-item))
        (goto-char (org-query-item-point this-item))
        (setq this-scheduled (org-query-get-scheduled-time (point)))
        (if this-scheduled
            (progn
              (setq this-days
                    (time-to-number-of-days
                     (time-subtract this-scheduled (org-current-effective-time))))
              (setq group-i 0)
              (while (< group-i (length group-data))
                (if (funcall (nth 1 (nth group-i group-data)) this-days)
                    (progn
                      (setq this-group-key (car (nth group-i group-data)))
                      (setq this-group (gethash this-group-key items-table))
                      (puthash this-group-key (add-to-list 'this-group this-item) items-table)
                      (setq group-i (length group-data)))
                  (setq group-i (+ group-i 1)))))
          (add-to-list 'no-date-group this-item)))
      (if no-date-group
          (add-to-list 'group-list
                       (make-org-query-group
                        :name "No scheduled time"
                        :items no-date-group)))
      (dolist (this-group-data (reverse group-data))
        (setq this-group-items (gethash (car this-group-data) items-table))
        (if this-group-items
            (add-to-list 'group-list
                         (make-org-query-group
                          :name (car this-group-data)
                          :items this-group-items))))
      group-list)))



(defun org-query-group-closed (items)
  (save-excursion
    (let ((group-data
           (list
            '("Was closed in the future?!"
              (lambda (d) (> d 0)))
            '("Was closed today"
              (lambda (d) (and (>= d -1) (<= d 0))))
            '("Was closed up to 1 week ago"
              (lambda (d) (and (>= d -7) (< d -1))))
            '("Was closed up to 3 weeks ago"
              (lambda (d) (and (>= d -21) (< d -7))))
            '("Was closed up to 6 weeks ago"
              (lambda (d) (and (>= d -42) (< d -21))))
            '("Was closed up to 3 months ago"
              (lambda (d) (and (>= d -90) (< d -42))))
            '("Was closed up to 6 months ago"
              (lambda (d) (and (>= d -180) (< d -90))))
            '("Was closed up to 1 year ago"
              (lambda (d) (and (>= d -365) (< d -180))))
            '("Was closed up to 3 years ago"
              (lambda (d) (and (>= d -1095) (< d -365))))
            '("Was closed more than 3 years ago"
              (lambda (d) (< d -1095)))))
          (items-table (make-hash-table :size 20 :test 'equal))
          (no-date-group nil)
          (this-days nil)
          (this-closed nil)
          (group-i nil)
          (this-item nil)
          (this-group nil)
          (this-group-key nil)
          (this-group-data nil)
          (this-group-items nil)
          (group-list nil))
      (dolist (this-item items)
        (set-buffer (org-query-item-buffer this-item))
        (goto-char (org-query-item-point this-item))
        (setq this-closed (org-entry-get (point) "CLOSED" t))
        (if this-closed
            (progn
              (setq this-days
                    (time-to-number-of-days
                     (time-subtract
                      (apply 'encode-time (org-parse-time-string this-closed))
                      (org-current-effective-time))))
              (setq group-i 0)
              (while (< group-i (length group-data))
                (if (funcall (nth 1 (nth group-i group-data)) this-days)
                    (progn
                      (setq this-group-key (car (nth group-i group-data)))
                      (setq this-group (gethash this-group-key items-table))
                      (puthash this-group-key (add-to-list 'this-group this-item) items-table)
                      (setq group-i (length group-data)))
                  (setq group-i (+ group-i 1)))))
          (add-to-list 'no-date-group this-item)))
      (if no-date-group
          (add-to-list 'group-list
                       (make-org-query-group
                        :name "Not closed"
                        :items no-date-group)))
      (dolist (this-group-data (reverse group-data))
        (setq this-group-items (gethash (car this-group-data) items-table))
        (if this-group-items
            (add-to-list 'group-list
                         (make-org-query-group
                          :name (car this-group-data)
                          :items this-group-items))))
      group-list)))




;;;; Sorting

(defun org-query--cmp-cached-funcall (fn< a b)
  ;; This caches results where the same item is sorted in multiple groups.
  ;; So it only brings benefit in huge groups with overlapping data.
  (save-excursion
    (let* ((cache-key `(,fn< ,a ,b))
           (val nil)
           (cached-val (gethash cache-key org-query--cache-cmp "notfound")))
      (if (not (equal cached-val "notfound"))
          cached-val
        (setq val (funcall fn< a b))
        (puthash cache-key val org-query--cache-cmp)
        val))))

(defun org-query-sort-items (item-list sort-criteria)
  (sort item-list
        (lambda (a b)
          (save-excursion
            (let ((stop nil)
                  (i 0)
                  (res t)
                  (fn< nil)
                  (sort-vals nil)
                  (LT nil)
                  (EQ nil))
              (while (and (not stop) (< i (length sort-criteria)))
                (setq fn< (nth i sort-criteria))
                (setq sort-vals (org-query--cmp-cached-funcall fn< a b))
                (setq LT (car sort-vals))
                (setq EQ (nth 1 sort-vals))
                (if (not EQ)
                    (setq res LT
                          stop t))
                (setq i (1+ i)))
              res)))))


(defun org-query-cmp-headline (a b)
  (save-excursion
    (let* ((headline nil)
           (get-fn
            (lambda (item)
              (set-buffer (org-query-item-buffer item))
              (goto-char (org-query-item-point item))
              (setq headline (org-get-heading t t))
              (if headline headline "")))
           (val-a (downcase (funcall get-fn a)))
           (val-b (downcase (funcall get-fn b))))
      (list
       (string< val-a val-b)
       (string= val-a val-b)))))
;;(string< (downcase (funcall get-fn a))
;;(downcase (funcall get-fn b))))))

(defun org-query-cmp-keyword (a b)
  (save-excursion
    (let* ((kwd nil)
           (get-fn
            (lambda (item)
              (set-buffer (org-query-item-buffer item))
              (goto-char (org-query-item-point item))
              (setq kwd (org-get-todo-state))
              (if kwd kwd "")))
           (val-a (downcase (funcall get-fn a)))
           (val-b (downcase (funcall get-fn b))))
      (list
       (string< val-a val-b)
       (string= val-a val-b)))))


(defun org-query-cmp-tag (a b)
  (save-excursion
    (let* ((get-fn
            (lambda (item)
              (set-buffer (org-query-item-buffer item))
              (goto-char (org-query-item-point item))
              ;; It's far too slow to compare on org-get-tags-at
              (org-get-tags-string)))
           (val-a (downcase (funcall get-fn a)))
           (val-b (downcase (funcall get-fn b))))
      (list
       (string< val-a val-b)
       (string= val-a val-b)))))

(defun org-query-cmp-filename (a b)
  (save-excursion
    (let* ((get-fn
            (lambda (item)
              (org-query-item-filename item)))
           (val-a (downcase (funcall get-fn a)))
           (val-b (downcase (funcall get-fn b))))
      (list
       (string< val-a val-b)
       (string= val-a val-b)))))

(defun org-query-cmp-deadline (a b)
  (save-excursion
    (let* ((deadline-time nil)
           (get-fn
            (lambda (item)
              (set-buffer (org-query-item-buffer item))
              (setq deadline-time (org-query-get-deadline-time
                                   (org-query-item-point item)))
              (if deadline-time
                  (org-float-time deadline-time)
                0)))
           (val-a (funcall get-fn a))
           (val-b (funcall get-fn b)))
      (list
       (> val-a val-b)
       (= val-a val-b)))))


(defun org-query-cmp-scheduled (a b)
  (save-excursion
    (let* ((scheduled-time nil)
           (get-fn
            (lambda (item)
              (set-buffer (org-query-item-buffer item))
              (setq scheduled-time (org-query-get-scheduled-time
                                    (org-query-item-point item)))
              (if scheduled-time
                  (org-float-time scheduled-time)
                0)))
           (val-a (funcall get-fn a))
           (val-b (funcall get-fn b)))
      (list
       (> val-a val-b)
       (= val-a val-b)))))

(defun org-query-cmp-closed (a b)
  ;; Just compares the time of the CLOSED property, if it exists.
  (save-excursion
    (let* ((closed nil)
           (get-fn
            (lambda (item)
              (set-buffer (org-query-item-buffer item))
              (goto-char (org-query-item-point item))
              (setq closed (org-entry-get (point) "CLOSED" t))
              (if closed
                  (org-float-time (apply 'encode-time (org-parse-time-string closed)))
                0)))
           (val-a (funcall get-fn a))
           (val-b (funcall get-fn b)))
      (list
       (< val-a val-b)
       (= val-a val-b)))))

(defun org-query-cmp-review (a b)
  (save-excursion
    (let* ((review nil)
           (get-fn
            (lambda (item)
              (set-buffer (org-query-item-buffer item))
              (goto-char (org-query-item-point item))
              (setq review (org-entry-get (point) org-query-review-property t))
              (if review
                  (org-float-time (apply 'encode-time (org-parse-time-string review)))
                0)))
           (val-a (funcall get-fn a))
           (val-b (funcall get-fn b)))
      (list
       (< val-a val-b)
       (= val-a val-b)))))

(defun org-query-cmp-priority (a b)
  (save-excursion
    (let* ((get-fn
            (lambda (item)
              (set-buffer (org-query-item-buffer item))
              (goto-char (org-query-item-point item))
              (org-query-get-priority (point))))
           (val-a (funcall get-fn a))
           (val-b (funcall get-fn b)))
      (list
       (< val-a val-b)
       (= val-a val-b)))))

(defun org-query-cmp-time-spent (a b)
  (save-excursion
    (let* ((mins nil)
           (get-fn
            (lambda (item)
              (set-buffer (org-query-item-buffer item))
              (goto-char (org-query-item-point item))
              (org-clock-sum-current-item)))
           (val-a (funcall get-fn a))
           (val-b (funcall get-fn b)))
      (list
       (< val-a val-b)
       (= val-a val-b)))))

;;;; Displaying

(defun org-query-insert-group (group)
  ;; This moves point
  (let (
        (group-name (org-query-group-name group))
        (headline-level 1)
        this-item
        )

    (set-buffer (org-query-get-buffer))
    (insert (format "%s %s %s\n\n\n"
                    (propertize "*" 'face (org-query-get-level-face 1))
                    group-name
                    (propertize (make-string (- 80 (length group-name)) ?=)
                                'face (org-query-get-level-face 1))
                    ))
    (dolist (this-item (org-query-group-items group))
      (org-query-insert-item this-item headline-level))
    (insert "\n\n")
    ))

(defun org-query-insert-item (this-item headline-level)
  ;; This moves point. TODO should it return point? probably
  (set-buffer (org-query-item-buffer this-item))
  (goto-char (org-query-item-point this-item))

  (let* (
         (ljust-width 14)
         (item-heading (org-get-heading))
         (content (org-get-heading t t))
         (tags-string-res (org-get-tags-string))
         (tags-local
          (propertize tags-string-res 'face (org-get-tag-face tags-string-res)))

         this-tag
         tags-inherited-str
         (tags-res-local (org-get-tags-at (point) t))
         (tags-res-inherit (org-get-tags-at (point) nil))
         (tags-inherited
          (let (
                (temp-result (propertize
                              (format "%s:"
                                      (dolist (this-tag tags-res-inherit tags-inherited-str) 
                                        ; Get tags that aren't defined locally
                                        (if (not (member this-tag tags-res-local))
                                            (setq tags-inherited-str
                                                  (concat tags-inherited-str ":" this-tag)))))
                              'face (org-get-tag-face tags-string-res)))
                )
            (if (not (string= "nil:" temp-result))
                temp-result
              ""
              )))


         ;; I think org-mode bug makes this treat eg. the lowercase "todo" as a
         ;; keyword.
         (keyword
          (if (org-get-todo-state)
              (concat
               (propertize (org-get-todo-state) 'face
                           (org-get-todo-face (org-get-todo-state)))
               " ")
            ""
            ))

         (time-clocked
          (let (
                (clocked (get-text-property (point) :org-clock-minutes)))
            (if clocked
                (format "%s" (org-minutes-to-clocksum-string clocked))
              ""
              )))


         (scheduled-date
          (let ((time-tuple
                 (org-query-get-scheduled-time (org-query-item-point this-item))))
            (if time-tuple (format-time-string org-query-time-format time-tuple)
              ""
              )))

         (due-date
          (let ((time-tuple
                 (org-query-get-deadline-time (point))))
            (if time-tuple (format-time-string org-query-time-format time-tuple)
              ""
              )))

         (priority
          (let ((priority-match (string-match org-priority-regexp item-heading)))
            (if priority-match
                (concat
                 (propertize (org-query-util-rstrip
                              (match-string 1 item-heading))
                             'face (org-query-get-priority-face))
                 " ")
              "")))

         (item-link
          (with-temp-buffer
            (let (
                  (action (lambda (btn)
                            (org-query-util-goto-location
                             (button-get btn 'btn-filename)
                             (button-get btn 'btn-point))))
                  )
              (insert-text-button "[Goto item]"
                                  'btn-filename (org-query-item-filename this-item)
                                  'btn-point (org-query-item-point this-item)
                                  'action action
                                  'mouse-action action)
              )
            (buffer-substring (point-min) (point-max))))

         (path-seg-i 0)
         path-seg-heading
         path-seg-result
         path-full-result
         (path
          (concat
           (format "%s: " (org-query-item-filename this-item))
           (dolist (path-seg-heading (org-get-outline-path) path-full-result)
             (progn
               (setq path-seg-i (1+ path-seg-i))
               (setq path-seg-result (propertize path-seg-heading 'face
                                                 (org-query-get-level-face path-seg-i)))
               (if (= path-seg-i 1) ; Don't have slash on the first item
                   (setq path-full-result (concat path-full-result path-seg-result))
                 (setq path-full-result (concat path-full-result " / " path-seg-result)))
               ))))

         ) ; let definitions

    ;; Insert the data
    (set-buffer (org-query-get-buffer))
    (insert (format "%s %s%s%s \n"
                    (propertize (make-string (+ 1 headline-level) ?*) 'face
                                (org-query-get-level-face (+ 1 headline-level)))
                    keyword
                    priority
                    content))

    (insert (format "  - %s %s\n"
                    (org-query-util-ljust "Link ::" ljust-width)
                    item-link
                    ))

    (if (not (string= "" tags-local))
        (insert (format "  - %s %s\n"
                        (org-query-util-ljust "Local ::" ljust-width)
                        tags-local
                        )))

    (if (not (string= "" tags-inherited))
        (insert (format "  - %s %s\n"
                        (org-query-util-ljust "Inherited ::" ljust-width)
                        tags-inherited
                        )))

    (if (not (string= "" due-date))
        (insert (format "  - %s %s\n"
                        (org-query-util-ljust "Due ::" ljust-width)
                        due-date
                        )))

    (if (not (string= "" scheduled-date))
        (insert (format "  - %s %s\n"
                        (org-query-util-ljust "Scheduled ::" ljust-width)
                        scheduled-date
                        )))

    (if (not (string= "" time-clocked))
        (insert (format "  - %s %s\n"
                        (org-query-util-ljust "Clocked ::" ljust-width)
                        time-clocked
                        )))

    (insert (format "  - %s %s\n"
                    (org-query-util-ljust "Path ::" ljust-width)
                    (org-query-util-word-wrap 60 (make-string (+ 5 ljust-width) 32) path)))


    (insert "\n")

    ;; Insert child items
    (let ((children (org-query-item-children this-item))
          (this-child nil))
      (if children
          (dolist (this-child children)
            (org-query-insert-item this-child (+ 1 headline-level)))))

    ) ; let
  )


;;;; Org-query minor mode

(defun org-query-cycle ()
  ;; Outside of org-mode, it seems org-cycle works nicely for the headings,
  ;; but doesn't work on the "- foo" items - it folds them but doesn't show them again.
  ;; We don't need that functionality as our headline bodies are minimal,
  ;; so only cycle if we're on an actual heading.
  (interactive)
  (let ((org-cycle-open-archived-trees t))
    (if (string-match org-outline-regexp-bol (thing-at-point 'line))
        (org-cycle))))

(defvar org-query-keymap
  (progn (let ((keymap (make-sparse-keymap)))
           (define-key keymap (kbd "<tab>") 'org-query-cycle)
           (define-key keymap (kbd "<S-tab>") 'org-shifttab)
           (define-key keymap (kbd "<C-tab>") 'org-force-cycle-archived)
           keymap)))

(define-minor-mode org-query-mode
  "Foo"
  :lighter " Org-query"
  :keymap org-query-keymap 
  )

(provide 'org-query)
;;;; Debug / Profiling

(if (not (boundp 'org-query--dir))
    (setq org-query--dir (file-name-directory load-file-name)))
(defun org-query--profile (msg)
  "Generate a query and dump profile information into
  './profile/$date-$commit.txt'."
  (interactive "MMessage: ")
  (org-query-advise-deactivate)
  ;;(org-query-advise-activate) ;; this was the offending line, broke the profiling
  (save-excursion
    (let* ((this-filter-fn
            (lambda ()
              (and ;; A bunch of filters that will mostly all pass
               (not (org-query-filter-keyword-matches-p
                     '("BLAH" "FOO" "STH" "RUBBISH") :up t :down t))
               (not (org-query-filter-tag-matches-p
                     '("BLAH" "FOO" "STH" "RUBBISH")))
               (or t (org-query-filter-archived-p))
               (or t (org-query-filter-deadline-in-days-p
                      (lambda (d) d > 0)))
               (not (org-query-filter-clocked-mins-p
                     (lambda (h) (eq h 123456))))
               (not (org-query-filter-property-exists-p "NONSENSE"
                                                               :up t :down t))
               (not (org-query-filter-property-exists-p "SCHEDULED"
                                                               :up t :down t)))))
           (this-sort-criteria
            '(org-query-cmp-deadline
              org-query-cmp-priority
              org-query-cmp-keyword
              org-query-cmp-tag
              org-query-cmp-filename
              org-query-cmp-headline
              org-query-cmp-time-spent))
           (config (make-org-query-config
                    :name "Performance test"
                    :parse-fn 'org-query-parse-items-horizontal-full
                    :filter-fn this-filter-fn
                    :group-fn 'org-query-group-tag-inherit
                    :sort-criteria this-sort-criteria))
           (filepath
            (replace-regexp-in-string
             "\n" ""
             (concat 
              org-query--dir
              "profile/"
              (shell-command-to-string "date +\"%Y-%m-%d-%H-%M-%S-\"")
              (shell-command-to-string
               (format "cd '%s'; git rev-parse --short HEAD" org-query--dir))))))

      ;;(elp-instrument-function 'gethash)
      ;;(elp-instrument-function 'puthash)
      ;;(elp-instrument-function 'insert)
      ;;(elp-instrument-function 'propertize)
      ;;(elp-instrument-function 'thing-at-point)
      ;;(elp-instrument-function 'buffer-substring)
      ;;(elp-instrument-package "outline")
      (elp-instrument-function 'format)
      (elp-instrument-package "org")
      (org-query-fn config)
      (elp-results) ; Also sets elp to current buffer
      (setq buffer-read-only nil)
      (goto-char 0)
      (insert (format "%s\n" msg))
      (write-file filepath)
      (elp-restore-all)

      ;;(profiler-start 'cpu)
      ;;(with-demoted-errors (org-query-fn config)) ; So cleanup still runs
      ;;(org-query-fn config) ; So cleanup still runs
      ;;(profiler-report)
      ;;(profiler-stop)

      (org-query-advise-deactivate)
      )))

