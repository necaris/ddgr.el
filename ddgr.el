;; Web search with ddgr
;;; ### [ddgr-results-mode] ###
(defvar ddgr-results-mode-map (make-sparse-keymap)
  "Keymap for `ddgr-results-mode'.")

;; Standard Emacs bindings
(define-key ddgr-results-mode-map (kbd "RET") #'ddgr-results-open)
(define-key ddgr-results-mode-map (kbd "e")   #'ddgr-results-open-eww)
(define-key ddgr-results-mode-map (kbd "o")   #'ddgr-results-open-browser)
(define-key ddgr-results-mode-map (kbd "n")   #'ddgr-results-next-result)
(define-key ddgr-results-mode-map (kbd "p")   #'ddgr-results-prev-result)
(define-key ddgr-results-mode-map (kbd "g")   #'ddgr-results-refresh)
(define-key ddgr-results-mode-map (kbd "s")   #'ddgr-results-new-search)
(define-key ddgr-results-mode-map (kbd "q")   #'quit-window)

;; Evil-mode bindings (for Doom Emacs users)
(when (fboundp 'evil-define-key)
  (evil-define-key 'motion ddgr-results-mode-map
    (kbd "RET") #'ddgr-results-open
    "e" #'ddgr-results-open-eww
    "o" #'ddgr-results-open-browser
    "j" #'ddgr-results-next-result
    "k" #'ddgr-results-prev-result
    "n" #'ddgr-results-next-result
    "p" #'ddgr-results-prev-result
    "g" #'ddgr-results-refresh
    "s" #'ddgr-results-new-search
    "q" #'quit-window))

(define-derived-mode ddgr-results-mode special-mode "ddgr"
  "Major mode for displaying DuckDuckGo search results from ddgr."
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defvar-local ddgr--current-query nil)

(defun ddgr--search-api (query)
  "Call ddgr CLI and return parsed JSON results for QUERY."
  (let ((results
         (with-temp-buffer
           (let ((exit-code (call-process "ddgr" nil t nil
                                          "--unsafe" "--np" "--json"
                                          query)))
             (if (/= exit-code 0)
                 (error "ddgr failed: %s" (buffer-string))
               (goto-char (point-min))
               (json-parse-string (buffer-substring-no-properties (point-min) (point-max))
                                   :object-type 'alist))))))
    results))

(defun ddgr--render-results (results)
  "Render RESULTS in current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (format "Search Results for: %s\n\n" ddgr--current-query)
                        'face 'bold-italic))
    (seq-doseq (result results)
      (let ((title (alist-get 'title result))
            (url (alist-get 'url result))
            (abstract (alist-get 'abstract result)))
        (insert (propertize title 'face 'link 'ddgr-url url 'help-echo url))
        (insert "\n")
        (when (and abstract (not (string-empty-p abstract)))
          (insert (propertize abstract 'face 'font-lock-comment-face) "\n"))
        (insert "\n")))
    (goto-char (point-min))))

(defun ddgr/search (query)
  "Search DuckDuckGo for QUERY."
  (interactive (list (read-string "Search DuckDuckGo: ")))
  (let ((results (ddgr--search-api query))
        (buf (get-buffer-create "*ddgr-results*")))
    (with-current-buffer buf
      (ddgr-results-mode)
      (setq ddgr--current-query query)
      (ddgr--render-results results))
    (pop-to-buffer buf)))

;;; ### [Commands] ###

(defun ddgr-results-open ()
  "Open the result at point."
  (interactive)
  (if-let ((url (get-text-property (point) 'ddgr-url)))
      (browse-url url)
    (message "No result at point")))

(defun ddgr-results-open-eww ()
  "Open the result at point in eww."
  (interactive)
  (if-let ((url (get-text-property (point) 'ddgr-url)))
      (eww url)
    (message "No result at point")))

(defun ddgr-results-open-browser ()
  "Open the result at point in the default browser."
  (interactive)
  (ddgr-results-open))

(defun ddgr-results-next-result ()
  "Move to the next result."
  (interactive)
  (let ((pos (next-single-property-change (point) 'ddgr-url)))
    (if pos
        (progn
          (goto-char pos)
          ;; If we landed on the end of a property, we might need to find the start of the next one
          (unless (get-text-property (point) 'ddgr-url)
            (ddgr-results-next-result)))
      (message "No more results"))))

(defun ddgr-results-prev-result ()
  "Move to the previous result."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'ddgr-url)))
    (if pos
        (progn
          (goto-char pos)
          ;; If we landed on a property, but it's not the start, move to the start
          (let ((start (previous-single-property-change (1+ (point)) 'ddgr-url)))
            (when start (goto-char (1+ start)))))
      (message "No previous results"))))

(defun ddgr-results-refresh ()
  "Refresh current search."
  (interactive)
  (ddgr/search ddgr--current-query))

(defun ddgr-results-new-search ()
  "Start a fresh search."
  (interactive)
  (call-interactively #'ddgr/search))

(provide 'ddgr)
