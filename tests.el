;;;; This file is part of org-query.el.

;; Org-query is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Org-query is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Test filter functions
(ert-deftest filter-property-exists-true ()
  (should 
   (with-temp-buffer 
     (org-mode)
     (insert 
      (mapconcat 'identity 
                 '("* property exists"
                   ":PROPERTIES:"
                   ":FOO: a real value"
                   ":END:"
                   ) "\n"))
     (goto-line 1)
     (org-query-filter-property-exists-p "FOO"))))

(ert-deftest filter-property-exists-false ()
  (should-not 
   (with-temp-buffer 
     (org-mode)
     (insert
      (mapconcat 'identity 
                 '("* property doesn't exist"
                   ) "\n"))
     (goto-line 1)
     (org-query-filter-property-exists-p "FOO"))))


(ert-deftest filter-keyword-true ()
  (should
   (with-temp-buffer
     (org-mode)
     (insert
      (mapconcat 'identity 
                 '("#+TODO: TODO DONE" ; Define buffer keywords
                   "* DONE foo"
                   ) "\n"))
     (goto-line 2)
     (org-query-filter-keyword-p (lambda (kwd) (string= kwd "DONE"))))))

(ert-deftest filter-keyword-false ()
  (should-not
   (with-temp-buffer
     (org-mode)
     (insert
      (mapconcat 'identity 
                 '("#+TODO: TODO DONE" ; Define buffer keywords
                   "* NOTDONE foo"
                   ) "\n"))
     (goto-line 2)
     (org-query-filter-keyword-p (lambda (kwd) (string= kwd "DONE"))))))

(ert-deftest filter-keyword-matches-true ()
  (should
   (with-temp-buffer
     (org-mode)
     (insert
      (mapconcat 'identity 
                 '("#+TODO: TODO DONE" ; Define buffer keywords
                   "* DONE foo"
                   ) "\n"))
     (goto-line 2)
     (org-query-filter-keyword-matches-p '("DONE" "BAR")))))

(ert-deftest filter-keyword-matches-false ()
  (should-not
   (with-temp-buffer
     (org-mode)
     (insert
      (mapconcat 'identity 
                 '("#+TODO: TODO DONE" ; Define buffer keywords
                   "* NOTDONE foo"
                   ) "\n"))
     (goto-line 2)
     (org-query-filter-keyword-matches-p '("DONE" "BAR")))))

(ert-deftest filter-tags-true ()
  (should
   (with-temp-buffer
     (org-mode)
     (insert "* Headline with tag :foo:")
     (goto-line 1)
     (org-query-filter-tags-p (lambda (tag) tag)))))

(ert-deftest filter-tags-false ()
  (should-not
   (with-temp-buffer
     (org-mode)
     (insert "* Headline without tag")
     (goto-line 1)
     (org-query-filter-tags-p (lambda (tag) tag)))))

(ert-deftest filter-tag-matches-true ()
  (should
   (with-temp-buffer
     (org-mode)
     (insert "* Headline with tag :foo:")
     (goto-line 1)
     (org-query-filter-tag-matches-p '("foo")))))

(ert-deftest filter-tag-matches-false ()
  (should-not
   (with-temp-buffer
     (org-mode)
     (insert "* Headline with uppercase tag :FOO:")
     (goto-line 1)
     ;; The tag filter should be case sensitive
     (org-query-filter-tag-matches-p '("foo")))))

(ert-deftest filter-priority-level-high ()
  (should
   (with-temp-buffer
     (org-mode)
     (insert "* [#A] Headline with high priority")
     (goto-line 1)
     ;; 1000 is default priority
     (org-query-filter-priority-level-p (lambda (pri) (> pri 1000))))))

(ert-deftest filter-priority-level-default ()
  (should
   (with-temp-buffer
     (org-mode)
     (insert "* Headline with default priority")
     (goto-line 1)
     (org-query-filter-priority-level-p (lambda (pri) (= pri 1000))))))

(ert-deftest filter-archived-true ()
  (should
   (with-temp-buffer
     (org-mode)
     (insert
      (mapconcat 'identity 
                 '("* ARCHIVE :ARCHIVE:"
                   "** Archived headline"
                   ) "\n"))
     (goto-line 2)
     (org-query-filter-archived-p))))

(ert-deftest filter-archived-false ()
  (should-not
   (with-temp-buffer
     (org-mode)
     (insert
      (mapconcat 'identity 
                 '("* Arbitrary parent"
                   "** Non-archived headline"
                   ) "\n"))
     (goto-line 2)
     (org-query-filter-archived-p))))

(ert-deftest filter-deadline-in-days-past ()
  (should
   (with-temp-buffer
     (org-mode)
     (insert
      (mapconcat 'identity 
                 '("* Item with deadline in past"
                   " DEADLINE: <2015-01-01>"
                   ) "\n"))
     (goto-line 2)
     ;; Deadline should be in the past, so negative number of days
     (org-query-filter-deadline-in-days-p (lambda (d) (< d 0))))))

;; Test is good for ~1000 years
(ert-deftest filter-deadline-in-days-future ()
  (should
   (with-temp-buffer
     (org-mode)
     (insert
      (mapconcat 'identity 
                 '("* Item with deadline in future"
                   " DEADLINE: <3015-01-01>"
                   ) "\n"))
     (goto-line 2)
     (org-query-filter-deadline-in-days-p (lambda (d) (> d 100))))))

(ert-deftest filter-deadline-not-exist ()
  (should
   (with-temp-buffer
     (org-mode)
     (insert "* Item without deadline")
     (goto-line 1)
     (org-query-filter-deadline-in-days-p (lambda (d) (eq d nil))))))

(ert-deftest filter-deadline-inherited ()
   (with-temp-buffer
     (org-mode)
     (insert
      (mapconcat 'identity 
                 '("* Item"
                   " DEADLINE: <2015-01-01>"
                   "** Child item"
                   "* Item without deadline"
                   ) "\n"))
     (goto-line 3)
     (should-not (org-query-filter-deadline-in-days-p (lambda (d) (eq d nil))))
     (goto-line 4)
     (should (org-query-filter-deadline-in-days-p (lambda (d) (eq d nil))))))

(ert-deftest filter-scheduled-in-days-past ()
  (should
   (with-temp-buffer
     (org-mode)
     (insert
      (mapconcat 'identity 
                 '("* Item scheduled in past"
                   " SCHEDULED: <2015-01-01>"
                   ) "\n"))
     (goto-line 2)
     (org-query-filter-scheduled-in-days-p (lambda (d) (< d 0))))))

;; Test is good for ~1000 years
(ert-deftest filter-scheduled-in-days-future ()
  (should
   (with-temp-buffer
     (org-mode)
     (insert
      (mapconcat 'identity 
                 '("* Item scheduled in future"
                   " SCHEDULED: <3015-01-01>"
                   ) "\n"))
     (goto-line 2)
     (org-query-filter-scheduled-in-days-p (lambda (d) (> d 100))))))

(ert-deftest filter-scheduled-not-exist ()
  (should
   (with-temp-buffer
     (org-mode)
     (insert "* Item without schedule")
     (goto-line 1)
     (org-query-filter-scheduled-in-days-p (lambda (d) (eq d nil))))))

(ert-deftest filter-clocked-mins-exists ()
  (should
   (with-temp-buffer
     (insert
      (mapconcat 'identity 
                 '("* Clocked item"
                   "    CLOCK: [2014-10-26 Sun 12:29]--[2014-10-26 Sun 12:59] =>  0:30"
                   "    CLOCK: [2014-10-26 Sun 12:59]--[2014-10-26 Sun 13:30] =>  0:31"
                   ) "\n"))
     (org-mode)
     (goto-line 1)
     (org-query-filter-clocked-mins-p (lambda (mins) (eq mins 61))))))

(ert-deftest filter-clocked-mins-not-exists ()
  (should
   (with-temp-buffer
     (insert "* Not clocked item")
     (org-mode)
     (goto-line 1)
     (org-query-filter-clocked-mins-p (lambda (mins) (eq mins 0))))))

(ert-deftest filter-has-children-true ()
  (should
   (with-temp-buffer
     (insert
      (mapconcat 'identity 
                 '("* Parent"
                   "** Child"
                   ) "\n"))
     (org-mode)
     (goto-line 1)
     (org-query-filter-has-children-p))))

(ert-deftest filter-has-children-false ()
  (should-not
   (with-temp-buffer
     (insert
      (mapconcat 'identity 
                 '("* Parent"
                   "** Child"
                   ) "\n"))
     (org-mode)
     (goto-line 2)
     (org-query-filter-has-children-p))))



;;;; Test org-query-filter-apply
(defun example-filter ()
  (let ((kwd (org-get-todo-state)))
    (equal kwd "TRUE")))

(ert-deftest filter-apply-self-returns-t ()
  (with-temp-buffer
    (insert 
      (mapconcat 'identity 
                 '("#+TODO: TRUE FALSE" ; Define buffer keywords
                   "* TRUE headline"
                   ) "\n"))
    (org-mode)
    (goto-line 2)
    (should (org-query-filter-apply '(example-filter)))))

(ert-deftest filter-apply-self-returns-nil ()
  (with-temp-buffer
    (insert 
      (mapconcat 'identity 
                 '("#+TODO: TRUE FALSE" ; Define buffer keywords
                   "* FALSE headline"
                   ) "\n"))
    (org-mode)
    (goto-line 2)
    (should-not (org-query-filter-apply '(example-filter)))))

(ert-deftest filter-apply-parents-returns-t ()
  (with-temp-buffer
    (insert 
      (mapconcat 'identity 
                 '("#+TODO: TRUE FALSE" ; Define buffer keywords
                   "* TRUE headline"
                   "** FALSE headline under test"
                   ) "\n"))
    (org-mode)
    (goto-line 3)
    (should (org-query-filter-apply '(example-filter) :self nil :parents t))))

(ert-deftest filter-apply-parents-returns-nil ()
  (with-temp-buffer
    (insert 
      (mapconcat 'identity 
                 '("#+TODO: TRUE FALSE" ; Define buffer keywords
                   "* FALSE headline"
                   "** TRUE headline under test"
                   ) "\n"))
    (org-mode)
    (goto-line 3)
    (should-not (org-query-filter-apply '(example-filter) :self nil :parents t))))

(ert-deftest filter-apply-children-returns-t-for-first-child ()
  (with-temp-buffer
    (insert 
      (mapconcat 'identity 
                 '("#+TODO: TRUE FALSE" ; Define buffer keywords
                   "* FALSE headline"
                   "** FALSE headline under test"
                   "*** TRUE headline"
                   ) "\n"))
    (org-mode)
    (goto-line 3)
    (should (org-query-filter-apply '(example-filter) :self nil :children t))))

(ert-deftest filter-apply-children-returns-t-for-subchild ()
  (with-temp-buffer
    (insert 
      (mapconcat 'identity 
                 '("#+TODO: TRUE FALSE" ; Define buffer keywords
                   "* FALSE headline"
                   "** FALSE headline under test"
                   "*** FALSE headline"
                   "**** TRUE headline"
                   "***** FALSE headline"
                   ) "\n"))
    (org-mode)
    (goto-line 3)
    (should (org-query-filter-apply '(example-filter) :self nil :children t))))

(ert-deftest filter-apply-children-returns-t-for-first-child-sibling ()
  (with-temp-buffer
    (insert 
      (mapconcat 'identity 
                 '("#+TODO: TRUE FALSE" ; Define buffer keywords
                   "* FALSE headline"
                   "** FALSE headline under test"
                   "*** FALSE headline"
                   "**** FALSE headline"
                   "*** FALSE headline"
                   "*** TRUE headline"
                   "*** FALSE headline"
                   ) "\n"))
    (org-mode)
    (goto-line 3)
    (should (org-query-filter-apply '(example-filter) :self nil :children t))))

(ert-deftest filter-apply-children-returns-t-for-first-child-sibling-subchild ()
  (with-temp-buffer
    (insert 
      (mapconcat 'identity 
                 '("#+TODO: TRUE FALSE" ; Define buffer keywords
                   "* FALSE headline"
                   "** FALSE headline under test"
                   "*** FALSE headline"
                   "**** FALSE headline"
                   "*** FALSE headline"
                   "**** FALSE headline"
                   "*** FALSE headline"
                   "**** TRUE headline"
                   "*** FALSE headline"
                   ) "\n"))
    (org-mode)
    (goto-line 3)
    (should (org-query-filter-apply '(example-filter) :self nil :children t))))

(ert-deftest filter-apply-children-returns-nil ()
  (with-temp-buffer
    (insert 
      (mapconcat 'identity 
                 '("#+TODO: TRUE FALSE" ; Define buffer keywords
                   "* TRUE headline"
                   "** TRUE headline under test"
                   "*** FALSE headline"
                   ) "\n"))
    (org-mode)
    (goto-line 3)
    (should-not (org-query-filter-apply '(example-filter) :self nil :children t))))



;;;; Test sorting
(ert-deftest cmp-headline-lt ()
  (should (equal '(t nil) ; t = LT, nil = EQ
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* AAA item"
                     "* ZZZ item"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 2)
                                            (point))))
       (org-query-cmp-headline item-a item-b))))))
       

(ert-deftest cmp-headline-eq ()
  (should (equal '(nil t)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Same item"
                     "* Same item"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 2)
                                            (point))))
       (org-query-cmp-headline item-a item-b))))))
                    

       

(ert-deftest cmp-headline-gt ()
  (should (equal '(nil nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* ZZZ item"
                     "* AAA item"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 2)
                                            (point))))
       (org-query-cmp-headline item-a item-b))))))
                    

(ert-deftest cmp-keyword-lt ()
  (should (equal '(t nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("#+TODO: AAA BBB"
                     "* AAA item" ; This is alphabetical comparison
                     "* BBB item"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 2)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 3)
                                            (point))))
       (org-query-cmp-keyword item-a item-b))))))

(ert-deftest cmp-keyword-eq ()
  (should (equal '(nil t)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("#+TODO: SAME"
                     "* SAME item" ; This is alphabetical comparison
                     "* SAME item"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 2)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 3)
                                            (point))))
       (org-query-cmp-keyword item-a item-b))))))

(ert-deftest cmp-keyword-gt ()
  (should (equal '(nil nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("#+TODO: KEYWORD"
                     "* KEYWORD item"
                     "* item without keyword" ; This is alphabetical comparison
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 2)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 3)
                                            (point))))
       (org-query-cmp-keyword item-a item-b))))))

(ert-deftest cmp-tag-lt ()
  (should (equal '(t nil) ; t = LT, nil = EQ
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item :aaaa:"
                     "* Item :bbbb:"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 2)
                                            (point))))
       (org-query-cmp-tag item-a item-b))))))
       

(ert-deftest cmp-tag-eq ()
  (should (equal '(nil t)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item :Sametag:"
                     "* Item :Sametag:"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 2)
                                            (point))))
       (org-query-cmp-tag item-a item-b))))))
                    

       

(ert-deftest cmp-tag-gt ()
  (should (equal '(nil nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item :zzzz:"
                     "* Item without tag"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 2)
                                            (point))))
       (org-query-cmp-tag item-a item-b))))))

(ert-deftest cmp-filename-lt ()
  (should (equal '(t nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (org-mode)
       (setq item-a (make-org-query-item
                     :filename "AAAA.org"
                     :buffer (current-buffer)
                     :point nil))
       (setq item-b (make-org-query-item
                     :filename "BBBB.org"
                     :buffer (current-buffer)
                     :point nil))
       (org-query-cmp-filename item-a item-b))))))
       


(ert-deftest cmp-filename-eq ()
  (should (equal '(nil t)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (org-mode)
       (setq item-a (make-org-query-item
                     :filename "EQUAL.org"
                     :buffer (current-buffer)
                     :point nil))
       (setq item-b (make-org-query-item
                     :filename "EQUAL.org"
                     :buffer (current-buffer)
                     :point nil))
       (org-query-cmp-filename item-a item-b))))))

(ert-deftest cmp-filename-gt ()
  (should (equal '(nil nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (org-mode)
       (setq item-a (make-org-query-item
                     :filename "ZZZZ.org"
                     :buffer (current-buffer)
                     :point nil))
       (setq item-b (make-org-query-item
                     :filename "AAAA.org"
                     :buffer (current-buffer)
                     :point nil))
       (org-query-cmp-filename item-a item-b))))))

(ert-deftest cmp-deadline-lt ()
  (should (equal '(t nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Older item" 
                    "   DEADLINE: <2025-01-16 Thu>"
                     "* Younger item"
                    "   DEADLINE: <2011-01-16 Sun>"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 3)
                                            (point))))
       (org-query-cmp-deadline item-a item-b))))))
       

(ert-deftest cmp-deadline-eq ()
  (should (equal '(nil t)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item" 
                    "   DEADLINE: <2011-01-16 Thu>" ; This day is deliberately wrong
                     "* Item"
                    "   DEADLINE: <2011-01-16 Sun>"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 3)
                                            (point))))
       (org-query-cmp-deadline item-a item-b))))))
       

(ert-deftest cmp-deadline-gt ()
  (should (equal '(nil nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item without deadline" 
                     "* Item"
                    "   DEADLINE: <2011-01-16 Sun>"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 2)
                                            (point))))
       (org-query-cmp-deadline item-a item-b))))))
       

(ert-deftest cmp-scheduled-lt ()
  (should (equal '(t nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Older item" 
                    "   SCHEDULED: <2025-01-16 Thu>"
                     "* Younger item"
                    "   SCHEDULED: <2011-01-16 Sun>"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 3)
                                            (point))))
       (org-query-cmp-scheduled item-a item-b))))))
       

(ert-deftest cmp-scheduled-eq ()
  (should (equal '(nil t)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item" 
                    "   SCHEDULED: <2011-01-16 Thu>" ; This day is deliberately wrong
                     "* Item"
                    "   SCHEDULED: <2011-01-16 Sun>"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 3)
                                            (point))))
       (org-query-cmp-scheduled item-a item-b))))))
       

(ert-deftest cmp-scheduled-gt ()
  (should (equal '(nil nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item without scheduled" 
                     "* Item"
                    "   SCHEDULED: <2011-01-16 Sun>"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 2)
                                            (point))))
       (org-query-cmp-scheduled item-a item-b))))))
       
(ert-deftest cmp-closed-lt ()
  (should (equal '(t nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item" 
                     "  CLOSED: [2015-01-13 Tue 15:00]"
                     "* Item"
                     "  CLOSED: [2015-01-16 Fri 15:10]"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 3)
                                            (point))))
       (org-query-cmp-closed item-a item-b))))))
       

(ert-deftest cmp-closed-eq ()
  (should (equal '(nil t) 
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item" 
                     "  CLOSED: [2015-01-13 Tue 15:00]"
                     "* Item"
                     "  CLOSED: [2015-01-13 Tue 15:00]"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 3)
                                            (point))))
       (org-query-cmp-closed item-a item-b))))))
       

(ert-deftest cmp-closed-gt ()
  (should (equal '(nil nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item" 
                     "  CLOSED: [2015-01-13 Tue 15:01]"
                     "* Item"
                     "  CLOSED: [2015-01-13 Tue 15:00]"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 3)
                                            (point))))
       (org-query-cmp-closed item-a item-b))))))
       
(ert-deftest cmp-review-lt ()
  (should (equal '(t nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item" 
                     "  :PROPERTIES:"
                     "  :LAST_REVIEWED:   [2013-04-20 Sat]"
                     "  :END:"
                     "* Item"
                     "  :PROPERTIES:"
                     "  :LAST_REVIEWED:   [2015-01-16 Fri]"
                     "  :END:"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 5)
                                            (point))))
       (org-query-cmp-review item-a item-b))))))
       

(ert-deftest cmp-review-eq ()
  (should (equal '(nil t)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item" 
                     "  :PROPERTIES:"
                     "  :LAST_REVIEWED:   [2015-01-16 Fri]"
                     "  :END:"
                     "* Item"
                     "  :PROPERTIES:"
                     "  :LAST_REVIEWED:   [2015-01-16 Fri]"
                     "  :END:"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 5)
                                            (point))))
       (org-query-cmp-review item-a item-b))))))
       

(ert-deftest cmp-review-gt ()
  (should (equal '(nil nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item" 
                     "  :PROPERTIES:"
                     "  :LAST_REVIEWED:   [2015-01-16 Fri]"
                     "  :END:"
                     "* Item"
                     "  :PROPERTIES:"
                     "  :END:"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 5)
                                            (point))))
       (org-query-cmp-review item-a item-b))))))

(ert-deftest cmp-priority-lt ()
  (should (equal '(t nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item regular priority" 
                     "* [#A] Item high priority"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 2)
                                            (point))))
       (org-query-cmp-priority item-a item-b))))))
       

(ert-deftest cmp-priority-eq ()
  (should (equal '(nil t)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* [#A] Item" 
                     "* [#A] Item"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 2)
                                            (point))))
       (org-query-cmp-priority item-a item-b))))))
       

(ert-deftest cmp-priority-gt ()
  (should (equal '(nil nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* [#A] Item"
                     "* [#B] Item" 
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 3)
                                            (point))))
       (org-query-cmp-priority item-a item-b))))))
       

(ert-deftest cmp-time-spent-lt ()
  (should (equal '(t nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item" 
                     "  CLOCK: [2014-11-16 Sun 14:50]--[2014-11-16 Sun 15:08] =>  0:18"
                     "* Item"
                     "  CLOCK: [2014-12-02 Tue 21:47]--[2014-12-02 Tue 22:54] =>  1:07"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 3)
                                            (point))))
       (org-query-cmp-time-spent item-a item-b))))))
       

(ert-deftest cmp-time-spent-eq ()
  (should (equal '(nil t)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item" 
                     "  CLOCK: [2014-12-02 Tue 21:47]--[2014-12-02 Tue 22:54] =>  1:07"
                     "* Item"
                     "  CLOCK: [2014-12-02 Tue 21:47]--[2014-12-02 Tue 22:54] =>  1:07"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 3)
                                            (point))))
       (org-query-cmp-time-spent item-a item-b))))))
       

(ert-deftest cmp-time-spent-gt ()
  (should (equal '(nil nil)
   (with-temp-buffer
     (let ((item-a nil) (item-b nil))
       (insert
        (mapconcat 'identity 
                   '("* Item" 
                     "  CLOCK: [2014-12-02 Tue 21:47]--[2014-12-02 Tue 22:54] =>  1:07"
                     "* Item"
                     ) "\n"))
       (org-mode)
       (setq item-a (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 1)
                                            (point))))
       (setq item-b (make-org-query-item
                     :buffer (current-buffer)
                     :point (save-excursion (goto-line 3)
                                            (point))))
       (org-query-cmp-time-spent item-a item-b))))))

;;;; Test grouping
(ert-deftest group-tag-local ()
  (with-temp-buffer 
    (let ((item-a nil) (item-b nil) (item-c nil) (item-d) (results nil))
      (insert 
       (mapconcat 'identity 
                  '("* Item A :groupFOO:"
                    "* Item B :groupBAR:"
                    "* Item C :groupFOO:"
                    "* Item without tag"
                    ) "\n"))
      (org-mode)
      (goto-line 1)
      (setq item-a (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 1)
                                           (point))))
      (setq item-b (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 2)
                                           (point))))
      (setq item-c (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 3)
                                           (point))))
      (setq item-d (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 4)
                                           (point))))
      (setq results (org-query-group-tag-local
                     (list item-a item-b item-c item-d)))

      (should (equal 3 (length results)))

      (should (equal ":groupBAR:" 
                     (substring-no-properties
                      (org-query-group-name (nth 0 results)))))
      (should (equal `(,item-b)
                     (org-query-group-items (nth 0 results))))

      (should (equal ":groupFOO:" 
                     (substring-no-properties
                      (org-query-group-name (nth 1 results)))))
      (should (equal `(,item-a ,item-c)
                     (org-query-group-items (nth 1 results))))

      ;; Don't test the group name, as that's liable to change.
      (should (equal `(,item-d)
                     (org-query-group-items (nth 2 results)))))))

(ert-deftest group-tag-inherit ()
  (with-temp-buffer 
    (let ((item-a nil) (item-a-inherit) (item-b nil) (results nil))
      (insert 
       (mapconcat 'identity 
                  '("* Item A :groupFOO:"
                    "** Item A-inherit"
                    "* Item B :groupBAR:"
                    ) "\n"))
      (org-mode)
      (goto-line 1)
      (setq item-a (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 1)
                                           (point))))
      (setq item-a-inherit (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 2)
                                           (point))))
      (setq item-b (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 3)
                                           (point))))

      (setq results (org-query-group-tag-inherit
                     (list item-a item-a-inherit item-b)))

      (should (equal 2 (length results)))
      (should (equal `(,item-a ,item-a-inherit)
                     (org-query-group-items (nth 1 results)))))))

(ert-deftest group-file ()
  (with-temp-buffer 
    (let ((item-a nil) (item-b nil) (item-c nil) (results nil))
      (org-mode)
      (setq item-a (make-org-query-item
                    :repr "value to differentiate this from item-b"
                    :filename "/foo/A.org"
                    :buffer (current-buffer)))
      (setq item-b (make-org-query-item
                    :repr "value to differentiate this from item-a"
                    :filename "/foo/A.org"
                    :buffer (current-buffer)))
      (setq item-c (make-org-query-item
                    :filename "/bar/B.org"
                    :buffer (current-buffer)))

      (setq results (org-query-group-file
                     (list item-a item-b item-c)))

      (should (equal 2 (length results)))

      (should (equal "/bar/B.org" 
                     (substring-no-properties
                      (org-query-group-name (nth 0 results)))))
      (should (equal `(,item-c)
                     (org-query-group-items (nth 0 results))))

      (should (equal "/foo/A.org" 
                     (substring-no-properties
                      (org-query-group-name (nth 1 results)))))
      (should (equal `(,item-a ,item-b)
                     (org-query-group-items (nth 1 results)))))))
(ert-deftest group-keyword ()
  (with-temp-buffer 
    (let ((item-a nil) (item-b nil) (item-c nil) (item-d) (results nil))
      (insert 
       (mapconcat 'identity 
                  '("#+TODO: KWD_FOO KWD_BAR"
                    "* KWD_FOO Item A"
                    "* KWD_FOO Item B"
                    "* Item C without keyword"
                    "* KWD_BAR Item D"
                    ) "\n"))
      (org-mode)
      (goto-line 1)
      (setq item-a (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 2)
                                           (point))))
      (setq item-b (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 3)
                                           (point))))
      (setq item-c (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 4)
                                           (point))))
      (setq item-d (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 5)
                                           (point))))
      (setq results (org-query-group-keyword
                     (list item-a item-b item-c item-d)))

      (should (equal 3 (length results))) ; 3 groups

      (should (equal "KWD_BAR" 
                     (substring-no-properties
                      (org-query-group-name (nth 0 results)))))
      (should (equal `(,item-d)
                     (org-query-group-items (nth 0 results))))

      (should (equal "KWD_FOO" 
                     (substring-no-properties
                      (org-query-group-name (nth 1 results)))))
      (should (equal `(,item-a ,item-b)
                     (org-query-group-items (nth 1 results))))

      ;; Don't test the group name, as that's liable to change.
      (should (equal `(,item-c)
                     (org-query-group-items (nth 2 results)))))))


(ert-deftest group-priority ()
  (with-temp-buffer 
    (let ((item-a nil) (item-b nil) (item-c nil) (item-d) (results nil))
      (insert 
       (mapconcat 'identity 
                  '("* [#A] Item A" ; 2000
                    "* [#A] Item B" ; 2000
                    "* Item C without priority" ; 1000
                    "* [#C] Item D" ; 0
                    ) "\n"))
      (org-mode)
      (goto-line 1)
      (setq item-a (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 1)
                                           (point))))
      (setq item-b (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 2)
                                           (point))))
      (setq item-c (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 3)
                                           (point))))
      (setq item-d (make-org-query-item
                    :buffer (current-buffer)
                    :point (save-excursion (goto-line 4)
                                           (point))))
      (setq results (org-query-group-priority
                     (list item-a item-b item-c item-d)))
      
      (should (equal 3 (length results))) ; 3 groups

      (should (equal "Priority level: 2000" 
                     (substring-no-properties
                      (org-query-group-name (nth 0 results)))))
      (should (equal `(,item-a ,item-b)
                     (org-query-group-items (nth 0 results))))

      (should (equal "Priority level: 1000" 
                     (substring-no-properties
                      (org-query-group-name (nth 1 results)))))
      (should (equal `(,item-c)
                     (org-query-group-items (nth 1 results))))

      (should (equal "Priority level: 0" 
                     (substring-no-properties
                      (org-query-group-name (nth 2 results)))))
      (should (equal `(,item-d)
                     (org-query-group-items (nth 2 results)))))))


(ert-deftest group-review ()
  (flet
      ;; This grouping is detemined relative to the current time,
      ;; so need to return a static value to stop tests going out of date.
      ((org-current-effective-time () (encode-time 0 0 0 16 01 2015)))

    (with-temp-buffer 
      (let ((item-a nil) (item-b nil) (item-c nil) (item-d) (item-e) (results nil))
        (insert 
         (mapconcat 'identity 
                    '("* Item A last reviewed today"
                      ":PROPERTIES:"
                      ":LAST_REVIEWED:   [2015-01-16]"
                      ":END:"
                      "* Item B last reviewed 7 days ago"
                      ":PROPERTIES:"
                      ":LAST_REVIEWED:   [2015-01-09]"
                      ":END:"
                      "* Item C last reviewed 30 days in the future"
                      ":PROPERTIES:"
                      ":LAST_REVIEWED:   [2015-02-15]"
                      ":END:"
                      "* Item D no review date" 
                      "** Item E last reviewed 170 days ago"
                      ":PROPERTIES:"
                      ":LAST_REVIEWED:   [2014-07-30]"
                      ":END:"
                      ) "\n"))
        (org-mode)
        (setq item-a (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 1)
                                             (point))))
        (setq item-b (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 5)
                                             (point))))
        (setq item-c (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 9)
                                             (point))))
        (setq item-d (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 13)
                                             (point))))
        (setq item-e (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 14)
                                             (point))))
        (setq results (org-query-group-review
                       (list item-a item-b item-c item-d item-e)))

        ;; No review property should be first group
        (should (equal `(,item-d)
                       (org-query-group-items (nth 0 results))))

        ;; Future-dates should be second group, as it would normally represent a
        ;; user error.
        (should (equal `(,item-c)
                       (org-query-group-items (nth 1 results))))

        (should (equal "Reviewed up to 6 months ago" 
                       (substring-no-properties
                        (org-query-group-name (nth 2 results)))))
        (should (equal `(,item-e)
                       (org-query-group-items (nth 2 results))))

        (should (equal "Reviewed up to 1 week ago" 
                       (substring-no-properties
                        (org-query-group-name (nth 3 results)))))
        (should (equal `(,item-b)
                       (org-query-group-items (nth 3 results))))

        (should (equal "Reviewed today" 
                       (substring-no-properties
                        (org-query-group-name (nth 4 results)))))
        (should (equal `(,item-a)
                       (org-query-group-items (nth 4 results))))))))

(ert-deftest group-deadline ()
  (flet
      ;; This grouping is detemined relative to the current time,
      ;; so need to return a static value to stop tests going out of date.
      ((org-current-effective-time () (encode-time 0 0 0 01 01 2015)))

    (with-temp-buffer 
      (let ((item-a nil) (item-b nil) (item-c nil) (item-d) (item-e) (results nil))
        (insert 
         (mapconcat 'identity 
                    '("* Item A deadline today"
                      ":PROPERTIES:"
                      ":DEADLINE: <2015-01-01>"
                      ":END:"
                      "* Item B deadline 7 days ago"
                      ":PROPERTIES:"
                      ":DEADLINE:   <2014-12-25>"
                      ":END:"
                      "* Item C deadline in 45 days"
                      ":PROPERTIES:"
                      ":DEADLINE:   <2015-02-15>"
                      ":END:"
                      "* Item D no deadline" 
                      "** Item E deadline 200 days ago"
                      "   DEADLINE:   <2014-06-15>"
                      ) "\n"))
        (org-mode)
        (setq item-a (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 1)
                                             (point))))
        (setq item-b (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 5)
                                             (point))))
        (setq item-c (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 9)
                                             (point))))
        (setq item-d (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 13)
                                             (point))))
        (setq item-e (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 14)
                                             (point))))
        (setq results (org-query-group-deadline
                       (list item-a item-b item-c item-d item-e)))
        
        (should (equal "Deadline was due up to 1 year ago" 
                       (substring-no-properties
                        (org-query-group-name (nth 0 results)))))
        (should (equal `(,item-e)
                       (org-query-group-items (nth 0 results))))

        (should (equal "Deadline was due up to 1 week ago" 
                       (substring-no-properties
                        (org-query-group-name (nth 1 results)))))
        (should (equal `(,item-b)
                       (org-query-group-items (nth 1 results))))

        (should (equal "Deadline is due in up to 1 week (including today)" 
                       (substring-no-properties
                        (org-query-group-name (nth 2 results)))))
        (should (equal `(,item-a)
                       (org-query-group-items (nth 2 results))))

        (should (equal "Deadline is due in up to 3 months" 
                       (substring-no-properties
                        (org-query-group-name (nth 3 results)))))
        (should (equal `(,item-c)
                       (org-query-group-items (nth 3 results))))

        ;; No deadline group
        (should (equal `(,item-d)
                       (org-query-group-items (nth 4 results))))))))

(ert-deftest group-scheduled ()
  (flet
      ;; This grouping is detemined relative to the current time,
      ;; so need to return a static value to stop tests going out of date.
      ((org-current-effective-time () (encode-time 0 0 0 01 01 2015)))

    (with-temp-buffer 
      (let ((item-a nil) (item-b nil) (item-c nil) (item-d) (item-e) (results nil))
        (insert 
         (mapconcat 'identity 
                    '("* Item A scheduled today"
                      ":PROPERTIES:"
                      ":SCHEDULED:   <2015-01-01>"
                      ":END:"
                      "* Item B scheduled 7 days ago"
                      ":PROPERTIES:"
                      ":SCHEDULED:   <2014-12-25>"
                      ":END:"
                      "* Item C scheduled in 45 days"
                      ":PROPERTIES:"
                      ":SCHEDULED:   <2015-02-15>"
                      ":END:"
                      "* Item D no schedule" 
                      "** Item E scheduled 200 days ago"
                      "   SCHEDULED: <2014-06-15>"
                      ) "\n"))
        (org-mode)
        (setq item-a (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 1)
                                             (point))))
        (setq item-b (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 5)
                                             (point))))
        (setq item-c (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 9)
                                             (point))))
        (setq item-d (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 13)
                                             (point))))
        (setq item-e (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 14)
                                             (point))))
        (setq results (org-query-group-scheduled
                       (list item-a item-b item-c item-d item-e)))
        
        (should (equal "Scheduled in up to 3 months" 
                       (substring-no-properties
                        (org-query-group-name (nth 0 results)))))
        (should (equal `(,item-c)
                       (org-query-group-items (nth 0 results))))

        (should (equal "Scheduled in up to 1 week (including today)" 
                       (substring-no-properties
                        (org-query-group-name (nth 1 results)))))
        (should (equal `(,item-a)
                       (org-query-group-items (nth 1 results))))

        (should (equal "Was scheduled up to 1 week ago" 
                       (substring-no-properties
                        (org-query-group-name (nth 2 results)))))
        (should (equal `(,item-b)
                       (org-query-group-items (nth 2 results))))

        (should (equal "Was scheduled up to 1 year ago" 
                       (substring-no-properties
                        (org-query-group-name (nth 3 results)))))
        (should (equal `(,item-e)
                       (org-query-group-items (nth 3 results))))

        ;; No schedule group
        (should (equal `(,item-d)
                       (org-query-group-items (nth 4 results))))))))
(ert-deftest group-closed ()
  (flet
      ;; This grouping is detemined relative to the current time,
      ;; so need to return a static value to stop tests going out of date.
      ((org-current-effective-time () (encode-time 0 0 0 01 01 2015)))

    (with-temp-buffer 
      (let ((item-a nil) (item-b nil) (item-c nil) (item-d) (item-e) (results nil))
        (insert 
         (mapconcat 'identity 
                    '("* Item A was closed today"
                      "CLOSED:   [2015-01-01]"
                      "* Item B closed in 20 days"
                      "CLOSED:   [2015-01-21]"
                      "* Item C was closed 8 days ago"
                      "CLOSED:   [2014-12-24]"
                      "* Item D was closed 1000 days ago"
                      "CLOSED:   [2012-04-06]"
                      "* Item E not closed"
                      ) "\n"))
        (org-mode)
        (setq item-a (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 1)
                                             (point))))
        (setq item-b (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 3)
                                             (point))))
        (setq item-c (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 5)
                                             (point))))
        (setq item-d (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 7)
                                             (point))))
        (setq item-e (make-org-query-item
                      :buffer (current-buffer)
                      :point (save-excursion (goto-line 9)
                                             (point))))
        (setq results (org-query-group-closed
                       (list item-a item-b item-c item-d item-e)))
        
        ;; Future group - this likely user error
        (should (equal `(,item-b)
                       (org-query-group-items (nth 0 results))))
        
        (should (equal "Was closed today" 
                       (substring-no-properties
                        (org-query-group-name (nth 1 results)))))
        (should (equal `(,item-a)
                       (org-query-group-items (nth 1 results))))

        (should (equal "Was closed up to 3 weeks ago" 
                       (substring-no-properties
                        (org-query-group-name (nth 2 results)))))
        (should (equal `(,item-c)
                       (org-query-group-items (nth 2 results))))

        (should (equal "Was closed up to 3 years ago" 
                       (substring-no-properties
                        (org-query-group-name (nth 3 results)))))
        (should (equal `(,item-d)
                       (org-query-group-items (nth 3 results))))

        ;; "Not closed" group
        (should (equal `(,item-e)
                       (org-query-group-items (nth 4 results))))))))
;;;; Test parsing
(ert-deftest parse-items-horizontal-full ()
  (with-temp-buffer 
    (let ((results nil)
          (filename "/foo/bar.org") ; Doesn't matter for test
          (filter '(org-query-filter-tag-matches-p '("FOO"))))
      (insert 
       (mapconcat 'identity 
                  '("* Item A :FOO:"
                    "* Item B :FOO: "
                    "** Item B-A :FOO:"
                    "* Item C"
                    "** Item C-A"
                    "* Item D"
                    "** Item D-A"
                    "*** Item D-A-A :FOO:"
                    ) "\n"))
      (org-mode)
      (setq results (org-query-parse-items-horizontal-full 
                     filter filename (point-min)))
      
      (should 
       (equal
        (save-excursion (goto-line 1) (point))
        (org-query-item-point (nth 0 results))))

      (should 
       (equal
        (save-excursion (goto-line 2) (point))
        (org-query-item-point (nth 1 results))))

      (should 
       (equal
        (save-excursion (goto-line 3) (point))
        (org-query-item-point (nth 2 results))))

      (should 
       (equal
        (save-excursion (goto-line 8) (point))
        (org-query-item-point (nth 3 results)))))))

(ert-deftest parse-items-horizontal-half ()
  (with-temp-buffer 
    (let ((results nil)
          (filename "/foo/bar.org") ; Doesn't matter for test
          (filter '(org-query-filter-keyword-matches-p '("TODO"))))
      (insert 
       (mapconcat 'identity 
                  '("* Item A"
                    "* TODO Item B this will be included"
                    "** TODO Item B-A this will be included"
                    "* Item C"
                    "** TODO Item C-A this won't be included due to parent"
                    "* TODO Item D this will be included"
                    "** Item D-A this won't be included"
                    "*** TODO Item D-A-A this won't be included due to parent"
                    ) "\n"))
      (org-mode)
      (setq results (org-query-parse-items-horizontal-half 
                     filter filename (point-min)))
      
      (should 
       (equal
        (save-excursion (goto-line 2) (point))
        (org-query-item-point (nth 0 results))))

      (should 
       (equal
        (save-excursion (goto-line 3) (point))
        (org-query-item-point (nth 1 results))))

      (should 
       (equal
        (save-excursion (goto-line 6) (point))
        (org-query-item-point (nth 2 results)))))))
               

(ert-deftest parse-items-vertical ()
  (with-temp-buffer 
    (let ((results nil)
          (filename "/foo/bar.org") ; Doesn't matter for test
          (filter '(org-query-filter-keyword-matches-p '("TODO"))))
      (insert 
       (mapconcat 'identity 
                  '("* Item A"
                    "* TODO Item B this will be included"
                    "** TODO Item B-A this will be included"
                    "** TODO Item B-B this will be included"
                    "* Item C"
                    "** TODO Item C-A this won't be included due to parent"
                    "* TODO Item D this will be included"
                    "** Item D-A this won't be included"
                    "*** TODO Item D-A-A this won't be included due to parent"
                    "** TODO Item D-B this will be included"
                    ) "\n"))
      (org-mode)
      (setq results (org-query-parse-items-vertical
                     filter filename (point-min)))
      
      (should 
       (equal
        (save-excursion (goto-line 2) (point))
        (org-query-item-point (nth 0 results))))

      (should 
       (equal
        (save-excursion (goto-line 3) (point))
        (org-query-item-point 
         (nth 0 (org-query-item-children (nth 0 results))))))

      (should 
       (equal
        (save-excursion (goto-line 4) (point))
        (org-query-item-point 
         (nth 1 (org-query-item-children (nth 0 results))))))

      (should 
       (equal
        (save-excursion (goto-line 7) (point))
        (org-query-item-point (nth 1 results))))

      (should 
       (equal
        (save-excursion (goto-line 10) (point))
        (org-query-item-point 
         (nth 0 (org-query-item-children (nth 1 results)))))))))

;;;; Test full query config
(ert-deftest query-fn-basic ()
  (let ((results nil)
        (test-buffer (generate-new-buffer "org-query-test"))
        (config (make-org-query-config
                 :name "Test query"
                 :description "Test description"
                 :files '("foo/bar.org") ; Doesn't matter as we're returning 
                                        ; mock file contents
                 :parse-fn 'org-query-parse-items-horizontal-full
                 :filter '(org-query-filter-keyword-matches-p '("FOO"))
                 :group-fn nil
                 :sort-criteria nil)))
    (flet
        ((org-query-get-temp-file-buffer (file) test-buffer))
      (set-buffer test-buffer)
      (insert 
       (mapconcat 'identity 
                  '("#+TODO: FOO"
                    "* Item A"
                    "* FOO Item B"
                    "* Item C"
                    "* FOO Item D"
                    ) "\n"))
      (org-mode)
      
      ;; Display results in org-query buffer
      (org-query-fn config)
      (set-buffer org-query-buffer-name)
      (goto-char (point-min))
      
      ;; Query name
      (should (save-excursion (search-forward "Test query")))

      ;; Filename
      (should (save-excursion (search-forward "foo/bar.org")))

      (should-not (save-excursion (search-forward "Item A" nil t)))
      (should (save-excursion (search-forward "Item B" nil t)))
      (should-not (save-excursion (search-forward "Item C" nil t)))
      (should (save-excursion (search-forward "Item D" nil t))))))

(ert-deftest query-fn-with-group-and-sort ()
  (let ((results nil)
        (test-buffer (generate-new-buffer "org-query-test"))
        (config (make-org-query-config
                 :name "Test query"
                 :description "Test description"
                 :files '("foo/bar.org") ; Doesn't matter as we're returning 
                                        ; mock file contents
                 :parse-fn 'org-query-parse-items-horizontal-full
                 :filter '(org-query-filter-tag-matches-p '(nil "FOO" "BAR" "BAZ"))
                 :group-fn 'org-query-group-tag-local
                 :sort-criteria '(org-query-cmp-priority))))
    (flet
        ((org-query-get-temp-file-buffer (file) test-buffer))
      (set-buffer test-buffer)
      (insert 
       (mapconcat 'identity 
                  '("* Item A :FOO:"
                    "* Item B :NOT_INCLUDED:"
                    "* Item C :FOO:BAR:"
                    "* Item D :BAZ:"
                    "* Item E :INVALID_TAG:"
                    "* [#A] Item F :BAZ:"
                    "* Item G"
                    ) "\n"))
      (org-mode)
      
      ;; Display results in org-query buffer
      (org-query-fn config)
      (set-buffer org-query-buffer-name)
      (goto-char (point-min))
      
      (should (save-excursion (search-forward "Item A" nil t)))
      (should-not (save-excursion (search-forward "Item B" nil t)))
      (save-excursion
        ;; Item C should be there twice due to tags
        (should (search-forward "Item C" nil t))
        (should (search-forward "Item C" nil t)))
      (should (save-excursion (search-forward "Item D" nil t)))
      (should-not (save-excursion (search-forward "Item E" nil t)))
      (should (save-excursion (search-forward "[#A] Item F" nil t)))
      (should (save-excursion (search-forward "Item G" nil t))))))

(ert-deftest query-fn-vertical-make-sure-hit-every-headline ()
  (let ((results nil)
        (test-buffer (generate-new-buffer "org-query-test"))
        (config (make-org-query-config
                 :name "Test query"
                 :description "Test description"
                 :files '("foo/bar.org") ; Doesn't matter as we're returning 
                                        ; mock file contents
                 :parse-fn 'org-query-parse-items-vertical
                 :filter 't ; Include every headline
                 :group-fn nil
                 :sort-criteria nil)))
    (flet
        ((org-query-get-temp-file-buffer (file) test-buffer))
      (set-buffer test-buffer)
      (insert 
       (mapconcat 'identity 
                  '("* Item A"
                    "** Item B"
                    "*** Item C"
                    "*** Item D"
                    "**** Item E"
                    "** Item F"
                    "*** Item G"
                    "**** Item H"
                    "***** Item I"
                    "     DEADLINE: <2015-01-01>"
                    "* Item J"
                    "* Item K"
                    "** Item L"
                    ) "\n"))
      (org-mode)
      
      ;; Display results in org-query buffer
      (org-query-fn config)
      (set-buffer org-query-buffer-name)
      (goto-char (point-min))
      
      (should (save-excursion (search-forward "Item A" nil t)))
      (should (save-excursion (search-forward "Item B" nil t)))
      (should (save-excursion (search-forward "Item C" nil t)))
      (should (save-excursion (search-forward "Item D" nil t)))
      (should (save-excursion (search-forward "Item E" nil t)))
      (should (save-excursion (search-forward "Item F" nil t)))
      (should (save-excursion (search-forward "Item G" nil t)))
      (should (save-excursion (search-forward "Item H" nil t)))
      (should (save-excursion (search-forward "Item I" nil t)))
      (should (save-excursion (search-forward "Item J" nil t)))
      (should (save-excursion (search-forward "Item K" nil t)))
      (should (save-excursion (search-forward "Item L" nil t)))
      (should (save-excursion (search-forward "Due" nil t)))))) ; Make sure deadline displayed

(ert-deftest query-fn-vertical-filter-regression-test ()
  ;; Test query config that wasn't returning expected results
  (let ((test-buffer (generate-new-buffer "org-query-test"))
        (config (make-org-query-config
                 :name "Test query"
                 :description "Test description"
                 :files '("foo/bar.org") ; Doesn't matter as we're returning 
                                        ; mock file contents
                 :parse-fn 'org-query-parse-items-vertical
                 :group-fn nil
                 :sort-criteria nil
                 :filter
                 '(org-query-filter-apply
                   '(and
                     (not (org-query-filter-archived-p))
                     (or
                      (org-query-filter-priority-level-p (lambda (p) (> p 1000)))
                      (org-query-filter-deadline-in-days-p (lambda (d) (if (eq d nil) nil (<= d 100))))))
                   :self t :parents t :children t))))
    (flet
        ((org-query-get-temp-file-buffer (file) test-buffer))
      (set-buffer test-buffer)
      (insert 
       (mapconcat 'identity 
                  '("* Item A"
                    "** Item B"
                    "*** [#A] Item C :ARCHIVE:"
                    "*** Item D"
                    "**** Item E"
                    "* Item F"
                    "** Item G"
                    "*** Item H"
                    "**** Item I"
                    "     DEADLINE: <2015-01-01>"
                    "* Item J"
                    "* [#A] Item K"
                    "** Item L"
                    ) "\n"))
      (org-mode)
      
      ;; Display results in org-query buffer
      (org-query-fn config)
      (set-buffer org-query-buffer-name)
      (goto-char (point-min))
      
      (should-not (save-excursion (search-forward "Item A" nil t)))
      (should-not (save-excursion (search-forward "Item B" nil t)))
      (should-not (save-excursion (search-forward "Item C" nil t)))
      (should-not (save-excursion (search-forward "Item D" nil t)))
      (should-not (save-excursion (search-forward "Item E" nil t)))
      (should (save-excursion (search-forward "Item F" nil t)))
      (should (save-excursion (search-forward "Item G" nil t)))
      (should (save-excursion (search-forward "Item H" nil t)))
      (should (save-excursion (search-forward "Item I" nil t)))
      (should-not (save-excursion (search-forward "Item J" nil t)))
      (should (save-excursion (search-forward "Item K" nil t)))
      (should (save-excursion (search-forward "Item L" nil t))))))


;;;; Test inheritance
(ert-deftest inherit-deadline-t ()
  (with-temp-buffer
    (insert
     (mapconcat 'identity 
                '("* Parent"
                  "  DEADLINE: <2015-01-01>"
                  "** Child"
                  "*** Subchild"
                  ) "\n"))
    (org-mode)
    (goto-line 4)
    (should (org-query-get-deadline-time (point)))))

(ert-deftest inherit-deadline-nil ()
  (with-temp-buffer
    (insert
     (mapconcat 'identity 
                '("* Parent"
                  "  DEADLINE: <2015-01-01>"
                  "** Child"
                  "*** Subchild"
                  "* Another parent"
                  ) "\n"))
    (org-mode)
    (goto-line 5)
    (should-not (org-query-get-deadline-time (point)))))


(ert-deftest inherit-priority-t ()
  (with-temp-buffer
    (insert
     (mapconcat 'identity 
                '("* [#A] Parent"
                  "** Child"
                  "*** Subchild"
                  "* Another parent"
                  ) "\n"))
    (org-mode)
    (goto-line 3)
    (should (eq 2000 (org-query-get-priority (point))))))

(ert-deftest inherit-priority-nil ()
  (with-temp-buffer
    (insert
     (mapconcat 'identity 
                '("* [#A] Parent"
                  "** Child"
                  "*** Subchild"
                  "* Another parent"
                  ) "\n"))
    (org-mode)
    (goto-line 4) 
    (should (eq 1000 (org-query-get-priority (point))))))
