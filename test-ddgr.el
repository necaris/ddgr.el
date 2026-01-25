;;; test-ddgr.el --- Tests for ddgr.el  -*- lexical-binding: t; -*-

;; Mock the ddgr CLI call for testing
(require 'buttercup)
(require 'ddgr)
(require 'seq)
(require 'cl-lib)

(defvar ddgr-test-mock-results nil)
(defvar ddgr-test-mock-exit-code 0)

;; Override the search API function for testing
(defun ddgr--search-api (query)
  "Mock version of ddgr--search-api for testing."
  (if (/= ddgr-test-mock-exit-code 0)
      (error "ddgr failed: mock error")
    (if (and (boundp 'ddgr-max-results) ddgr-max-results)
        (seq-take ddgr-test-mock-results ddgr-max-results)
      ddgr-test-mock-results)))

;; Test suite for ddgr functionality
(describe "ddgr.el functionality"
  
  ;; Test setup and teardown
  (before-each
   (setq ddgr-test-mock-exit-code 0)
   (setq ddgr-test-mock-results '(
                                  ((title . "Test Result 1")
                                   (url . "https://example.com/1")
                                   (abstract . "This is a test abstract 1"))
                                  ((title . "Test Result 2")
                                   (url . "https://example.com/2")
                                   (abstract . "This is a test abstract 2")))))
  
  (after-each
   (when (get-buffer "*ddgr-results*")
     (kill-buffer "*ddgr-results*")))
  
  ;; Test the search API function
  (describe "ddgr--search-api"
    (it "returns parsed results for successful queries"
      (let ((results (ddgr--search-api "test query")))
        (expect results :to-equal ddgr-test-mock-results)))
    
    (it "signals error when ddgr fails"
      (setq ddgr-test-mock-exit-code 1)
      (expect (ddgr--search-api "fail query") :to-throw 'error)))
  
  ;; Test the render results function
  (describe "ddgr--render-results"
    (it "renders results in buffer"
      (let ((buf (get-buffer-create "*test-render*")))
        (with-current-buffer buf
          (ddgr--render-results ddgr-test-mock-results)
          (expect (buffer-substring-no-properties (point-min) (point-max)) :to-match "Search Results for: ")
          (expect (buffer-substring-no-properties (point-min) (point-max)) :to-match "Test Result 1")
          (expect (buffer-substring-no-properties (point-min) (point-max)) :to-match "This is a test abstract 1"))))
    
    (it "handles empty results"
      (let ((buf (get-buffer-create "*test-empty*")))
        (with-current-buffer buf
          (ddgr--render-results '())
          (expect (buffer-substring-no-properties (point-min) (point-max)) :to-match "Search Results for: ")
          (expect (buffer-substring-no-properties (point-min) (point-max)) :not :to-match "Test Result"))))
    
    (it "handles results without abstracts"
      (let ((results '(((title . "No Abstract") (url . "https://example.com"))))
            (buf (get-buffer-create "*test-no-abstract*")))
        (with-current-buffer buf
          (ddgr--render-results results)
          (expect (buffer-substring-no-properties (point-min) (point-max)) :to-match "No Abstract")))))
  
  ;; Test the main search function
  (describe "ddgr/search"
    (it "creates results buffer and displays search results"
      (let ((buf (ddgr/search "test query")))
        (expect (get-buffer "*ddgr-results*") :to-be-truthy)
        (with-current-buffer "*ddgr-results*"
          (expect major-mode :to-equal 'ddgr-results-mode)
          (expect (buffer-substring-no-properties (point-min) (point-max)) :to-match "Search Results for: test query")
          (expect (buffer-substring-no-properties (point-min) (point-max)) :to-match "Test Result 1"))))
    
    (it "handles search failures gracefully"
      (setq ddgr-test-mock-exit-code 1)
      (expect (ddgr/search "fail query") :to-throw 'error)))
  
  ;; Test results navigation functions
  (describe "results navigation"
    (before-each
     (ddgr/search "test query")
     (switch-to-buffer "*ddgr-results*"))
    
    (it "can move to next result"
      (goto-char (point-min))
      (ddgr-results-next-result)
      (expect (point) :to-be-greater-than (point-min)))
    
    (it "can move to previous result"
      (goto-char (point-max))
      (ddgr-results-prev-result)
      (expect (point) :to-be-less-than (point-max)))
    
    (it "handles no more results"
      (goto-char (point-max))
      (spy-on 'message)
      (ddgr-results-next-result)
      (expect 'message :to-have-been-called-with "No more results"))
    
    (it "handles no previous results"
      (goto-char (point-min))
      (spy-on 'message)
      (ddgr-results-prev-result)
      (expect 'message :to-have-been-called-with "No previous results")))
  
  ;; Test result opening functions
  (describe "result opening"
    (before-each
     (ddgr/search "test query")
     (switch-to-buffer "*ddgr-results*")
     (goto-char (point-min))
     (ddgr-results-next-result)) ;; Move to first result
    
    (it "can get URL from result at point"
      (expect (get-text-property (point) 'ddgr-url) :to-equal "https://example.com/1"))
    
    (it "can open result with browse-url"
      (let ((browse-url-called nil)
            (url nil))
        (fset 'browse-url (lambda (u) (setq browse-url-called t) (setq url u)))
        (ddgr-results-open)
        (expect browse-url-called :to-be-truthy)
        (expect url :to-equal "https://example.com/1")))
    
    (it "can open result with eww"
      (let ((eww-called nil)
            (url nil))
        (fset 'eww (lambda (u) (setq eww-called t) (setq url u)))
        (ddgr-results-open-eww)
        (expect eww-called :to-be-truthy)
        (expect url :to-equal "https://example.com/1")))
    
    (it "shows message when no result at point"
      (goto-char (point-max))
      (spy-on 'message)
      (ddgr-results-open)
      (expect 'message :to-have-been-called-with "No result at point")))
  
  ;; Test refresh and new search
  (describe "search management"
    (it "can refresh current search"
      (ddgr/search "initial query")
      (let ((initial-buffer (current-buffer)))
        (with-current-buffer "*ddgr-results*"
          (setq ddgr--current-query "initial query")
          (ddgr-results-refresh)
          (expect (buffer-substring-no-properties (point-min) (point-max)) :to-match "Search Results for: initial query"))))
    
    (it "can start new search"
      (ddgr/search "first query")
      (let ((message-log-max nil)
            (message ""))
        (with-current-buffer "*ddgr-results*"
          (spy-on 'read-string :and-return-value "test query")
          (call-interactively #'ddgr-results-new-search)
          ;; This should call ddgr/search interactively
          (expect 'read-string :to-have-been-called-with "Search DuckDuckGo: ")))))
  
  ;; Test mode setup
  (describe "ddgr-results-mode"
    (it "sets up correct buffer properties"
      (let ((buf (get-buffer-create "*test-mode*")))
        (with-current-buffer buf
          (ddgr-results-mode)
          (expect buffer-read-only :to-be-truthy)
          (expect truncate-lines :to-be-truthy)
          (expect major-mode :to-equal 'ddgr-results-mode))))
    
    (it "has correct key bindings"
      (let ((buf (get-buffer-create "*test-bindings*")))
        (with-current-buffer buf
          (ddgr-results-mode)
          (expect (lookup-key ddgr-results-mode-map (kbd "RET")) :to-equal 'ddgr-results-open)
          (expect (lookup-key ddgr-results-mode-map (kbd "n")) :to-equal 'ddgr-results-next-result)
          (expect (lookup-key ddgr-results-mode-map (kbd "p")) :to-equal 'ddgr-results-prev-result)
          (expect (lookup-key ddgr-results-mode-map (kbd "q")) :to-equal 'quit-window)))))
  
  ;; Test customization
  (describe "customization"
    (it "respects ddgr-max-results setting"
      (let ((ddgr-max-results 3)
            (mock-results '(((title . "title 1") (url . "1") (abstract . "1"))
                            ((title . "title 2") (url . "2") (abstract . "2"))
                            ((title . "title 3") (url . "3") (abstract . "3"))
                            ((title . "title 4") (url . "4") (abstract . "4")))))
        (setq ddgr-test-mock-results mock-results)
        (ddgr/search "test")
        (with-current-buffer "*ddgr-results*"
          ;; Should only show 3 results due to ddgr-max-results setting
          (expect (count-matches "title") :to-equal 3)))))
  
  ;; Test evil mode bindings if available
  (when (fboundp 'evil-define-key)
    (describe "evil mode bindings"
      (it "sets up evil mode key bindings when available"
        (let ((buf (get-buffer-create "*test-evil*")))
          (with-current-buffer buf
            (ddgr-results-mode)
            (expect (lookup-key ddgr-results-mode-map (kbd "j")) :to-equal 'ddgr-results-next-result)
            (expect (lookup-key ddgr-results-mode-map (kbd "k")) :to-equal 'ddgr-results-prev-result))))))

  ;; Edge case and error handling tests
  (describe "edge cases and error handling"
    (it "handles single result"
      (setq ddgr-test-mock-results '(((title . "Single Result") (url . "https://example.com"))))
      (ddgr/search "single")
      (with-current-buffer "*ddgr-results*"
        (expect (count-matches "Single Result") :to-equal 1)))
    
    (it "handles very long URLs"
      (setq ddgr-test-mock-results '(((title . "Long URL") 
                                       (url . "https://example.com/very/long/path/with/many/segments/to/test/buffer/handling"))))
      (ddgr/search "long url")
      (with-current-buffer "*ddgr-results*"
        (expect (buffer-substring-no-properties (point-min) (point-max)) :to-match "Long URL")))
    
    (it "handles special characters in search query"
      (ddgr/search "test query with spaces & special chars!")
      (with-current-buffer "*ddgr-results*"
        (expect (buffer-substring-no-properties (point-min) (point-max)) :to-match "test query with spaces & special chars!")))
    
    (it "handles empty abstracts"
      (setq ddgr-test-mock-results '(((title . "No Abstract") (url . "https://example.com") (abstract . ""))))
      (ddgr/search "empty abstract")
      (with-current-buffer "*ddgr-results*"
        (expect (buffer-substring-no-properties (point-min) (point-max)) :to-match "No Abstract")
        (expect (buffer-substring-no-properties (point-min) (point-max)) :not :to-match "This is a test abstract")))
    
    (it "handles results with only title and URL"
      (setq ddgr-test-mock-results '(((title . "Minimal Result") (url . "https://minimal.com"))))
      (ddgr/search "minimal")
      (with-current-buffer "*ddgr-results*"
        (expect (buffer-substring-no-properties (point-min) (point-max)) :to-match "Minimal Result")))
    
    (it "handles buffer switching and cleanup"
      (ddgr/search "test 1")
      (expect (get-buffer "*ddgr-results*") :to-be-truthy)
      (ddgr/search "test 2")
      (expect (get-buffer "*ddgr-results*") :to-be-truthy)
      ;; Should reuse the same buffer
      (expect (length (buffer-list)) :to-be-less-than 20))))

