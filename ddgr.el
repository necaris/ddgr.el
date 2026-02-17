;;; ddgr.el --- DuckDuckGo search  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Rami Chowdhury
;; Author: Rami Chowdhury <rami.chowdhury@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))

;; Keywords: comm, duckduckgo, search, web
;; URL: https://github.com/necaris/ddgr.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides an interface to DuckDuckGo search using the `ddgr'
;; command line tool.

(require 'button)

;;; Code:

(defgroup ddgr nil
  "DuckDuckGo search via ddgr."
  :group 'external
  :prefix "ddgr-")

(defcustom ddgr-max-results 6
  "Maximum number of search results to display."
  :type 'integer
  :group 'ddgr)

(defvar ddgr-results-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "g")   #'ddgr-results-refresh)
    (define-key km (kbd "s")   #'ddgr-results-new-search)
    (define-key km (kbd "q")   #'quit-window)
    km)
  "Keymap for `ddgr-results-mode'.")

(define-derived-mode ddgr-results-mode special-mode "ddgr"
  "Major mode for displaying DuckDuckGo search results from ddgr."
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defvar-local ddgr--current-query nil)

(defun ddgr--read-query ()
  "Read search query from user."
  (read-string "Search DuckDuckGo: "))

(defun ddgr--search-api (query)
  "Call ddgr CLI and return parsed JSON results for QUERY."
  (unless (executable-find "ddgr")
    (user-error "Executable not found: ddgr!"))
  (let ((results
         (with-temp-buffer
           (let ((exit-code (call-process "ddgr" nil t nil
                                          "--unsafe" "--np" "--json"
                                          "-n" (number-to-string ddgr-max-results)
                                          query)))
             (if (/= exit-code 0)
                 (error "Failed call to ddgr: %s" (buffer-string))
               (goto-char (point-min))
               (json-parse-buffer :object-type 'alist))))))
    results))

(defun ddgr-results-open-url (button)
  "Open the URL associated with BUTTON."
  (browse-url (button-get button 'ddgr-url)))

(defun ddgr--render-results (results)
  "Render RESULTS in current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (format "Search Results for: %s\n\n" ddgr--current-query)
                        'face 'bold-italic))
    (cl-loop for result across results do
             (let ((title (alist-get 'title result))
                   (url (alist-get 'url result))
                   (abstract (alist-get 'abstract result)))
               (insert-text-button title
                                   'action #'ddgr-results-open-url
                                   'ddgr-url url
                                   'help-echo url
                                   'follow-link t)
               (insert "\n")
               (when (and abstract (not (string-empty-p abstract)))
                 (insert (propertize abstract 'face 'font-lock-comment-face) "\n"))
               (insert "\n")))
    (goto-char (point-min))))

(defun ddgr-search (query)
  "Search DuckDuckGo for QUERY."
  (interactive (list (ddgr--read-query)))
  (let ((results (ddgr--search-api query))
        (buf (get-buffer-create "*ddgr-results*")))
    (with-current-buffer buf
      (ddgr-results-mode)
      (setq ddgr--current-query query)
      (ddgr--render-results results))
    (pop-to-buffer buf)))

(defun ddgr-results-refresh ()
  "Refresh current search."
  (interactive)
  (ddgr-search ddgr--current-query))

(defun ddgr-results-new-search ()
  "Start a fresh search."
  (interactive)
  (call-interactively #'ddgr-search))

(provide 'ddgr)

;;; ddgr.el ends here
