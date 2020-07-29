;; -*- lexical-binding: t -*-
;; TODO: allow browsing files graphically?
;; TODO: check for post success/failure and handle failure appropriately

(require 'recaptcha-v2)

;; request.el is required just to make POST requests because the builtin url.el
;; doesn't like POST requests with the data in the request body and decides to
;; try to do something else with the request for some reason.
(require 'request)

(defcustom chan-recaptcha-site-key "6Ldp2bsSAAAAAAJ5uyx_lx34lJeEpTLVkP5k04qc"
  "The public site key for 4chan's reCAPTCHA.")

(defvar chan-post-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-return>") 'chan-post-submit)
    (define-key map (kbd "<C-c C-o>") 'chan-post-attach-file)
    map))
(setq chan-post-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-return>") 'chan-post-submit)
    (define-key map (kbd "C-c C-o") 'chan-post-attach-file)
    map))

(defvar chan-post-single-line-field-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'chan-post-next-field)
    map)
  "Keymap for fields that should only be a single line. Return key is disabled,
and instead moves to the next field. Note that it is still
possible to insert newlines, but that will break posting.")

(defvar chan-post-filename nil
  "Name of file to attach to post.")

(defvar chan-post-url "https://sys.4channel.org/g/post"
  "URL to send post forms to.")
(setq chan-post-url "https://sys.4channel.org/g/post")
;(setq chan-post-url "http://localhost:8000")

(defvar chan-post-pwd "_fSeQUtwMMdlpYEwhvOSDi0RRVzXvYVRB"
  "\"Password\" for posting on 4chan that seems to be the same
everywhere. Probably meant to make it easy to break custom
clients trying to post.")

(defvar chan-post--thread-id nil)
(defvar chan-post--max-file-size nil)
(defvar chan-post--referer nil
  "URL to use for referrer headers (origin only).")

(defconst chan-post--single-line-field-text-props
  `(read-only t
	      front-sticky t
	      rear-nonsticky (read-only)
	      keymap ,chan-post-single-line-field-map))

(defun chan-post--post-form (url inputs referer)
  "Submits an HTML form using the POST method with encoding type
of \"multipart/form-data\" given an alist of (NAME . VALUE) pairs
in INPUTS, where NAME is the HTML `name` attribute of an input
and VALUE is the corresponding `value` attribute. URL is the URL
to send the POST request to. Returns the buffer returned by
url.el."
  (let* ((boundary (concat "---------------------------"
			   (number-to-string (abs (random)))))
	 (rbody (concat (seq-mapcat (lambda (input)
				      (concat "--" boundary
					      "\r\nContent-Disposition: form-data; name=\""
					      (car input)
					      "\"\r\n\r\n"
					      (cdr input)
					      "\r\n"))
				    inputs)
			"--" boundary
			;"\r\nContent-Disposition: form-data; name=\"upfile\"; filename=\"\"\r\nContent-Type: application/octet-stream\r\n\r\n\r\n"
			"\r\nContent-Disposition: form-data; name=\"upfile\"; filename=\""
			(if chan-post-filename
			    (file-name-nondirectory chan-post-filename)
			  "")
			"\"\r\nContent-Type: application/octet-stream\r\n\r\n"
			(if chan-post-filename
			    (let ((filename chan-post-filename))
			      (with-temp-buffer
				(insert-file-contents filename)
				(buffer-string))))
			"\r\n"
			"--" boundary
			"--")))
    (request
     url
     :type "POST"
     :headers `(("Content-Type" . ,(concat "multipart/form-data; boundary=" boundary))
		("Content-Length" . ,(number-to-string (length rbody)))
		("Referer" . ,referer))
     :data rbody
     :sync t
     :parser (lambda () (buffer-string))
     :success (cl-function
	       (lambda (&key data &key response &allow-other-keys)
		 (print (request-response-status-code response))
		 )))))

;; IIRC I couldn't the builtin url package to make POST requests even though it
;; should be possible, so I just switched to request.el The commented out
;; function below is the url.el version.
; (defun chan-post--post-form (url inputs referer)
;   "Submits an HTML form using the POST method with encoding type
; of \"multipart/form-data\" given an alist of (NAME . VALUE) pairs
; in INPUTS, where NAME is the HTML `name` attribute of an input
; and VALUE is the corresponding `value` attribute. URL is the URL
; to send the POST request to. Returns the buffer returned by
; url.el."
;   (let ((inputs '(("a" . "b"))))
;   (let* ((url-request-method "POST")
; 	 (boundary (concat "-----------------------------"
; 			   (number-to-string (abs (random)))))
; 	 (url-request-body
; 	  ;; create request body, according to
; 	  ;; https://tools.ietf.org/html/rfc1867#section-6
; 	  (concat (seq-mapcat (lambda (input)
; 				(concat "--" boundary
; 					"\r\nContent-Disposition: form-data; name=\""
; 					(car input)
; 					"\"\r\n\r\n"
; 					(cdr input)
; 					"\r\n"))
; 			      inputs)
; 		  "--" boundary
; 		  ;"\r\nContent-Disposition: form-data; name=\"upfile\"; filename=\"\"\r\nContent-Type: application/octet-stream\r\n\r\n\r\n"
; 		  "\r\nContent-Disposition: form-data; name=\"upfile\"; filename=\""
; 		  (if chan-post-filename
; 		      (file-name-nondirectory chan-post-filename)
; 		    "")
; 		  "\"\r\nContent-Type: application/octet-stream\r\n\r\n"
; 		  (if chan-post-filename
; 		      (let ((filename chan-post-filename))
; 			(with-temp-buffer
; 			  (insert-file-contents filename)
; 			  (buffer-string))))
; 		  "\r\n"
; 		  "--" boundary
; 		  "--"))
; 	 (url-request-extra-headers
; 	  `(("Content-Type" . ,(concat "multipart/form-data; boundary=" boundary))
; 	    ("Content-Length" . ,(number-to-string (length url-request-body))))))
;     ; (when referer
;     ;   (push `("Referer" . ,referer) url-request-extra-headers))
;     (switch-to-buffer (url-retrieve-synchronously url)))))

(define-derived-mode chan-post-mode text-mode "chan-post-mode"
  "Mode for composing a 4chan post."
  (make-local-variable 'chan-post-filename)
  (setq chan-post-filename nil)
  (make-local-variable 'chan-post--referer)
  (make-local-variable 'chan-post--max-file-size))

(defun chan-post-next-field ()
  (interactive)
  ;; TODO implement
  )

(defun chan-post-submit-callback (verification-token)
  "Callback called by recaptcha-v2.el. This function submits a
completed post form."
  (let ((form))
    (goto-char (point-min))
    ;; get "name" field
    (forward-line)
    (setq form (list (cons "name" (buffer-substring (point) (line-end-position)))))
    ;; get "options"/"email" field
    (forward-line 2)
    (setq form (cons (cons "email" (buffer-substring (point)
						     (line-end-position)))
		     form))
    ;; skip past "file" field as its value is stored in a buffer-local variable
    (forward-line)
    ;; get "comment" field
    (forward-line 2)
    (setq form (cons (cons "com" (buffer-substring (point) (point-max)))
		     form))
    (setq form `(("MAX_FILE_SIZE" . ,chan-post--max-file-size)
		 ("mode" . "regist")
		 ("pwd" . ,chan-post-pwd)
		 ("resto" . ,chan-post--thread-id)
		 ("g-recaptcha-response" . ,verification-token)
		 . ,form))
    ;; referer-policy is "origin"
    (chan-post--post-form chan-post-url form chan-post--referer)
    (kill-buffer)))

(defun chan-post-submit ()
  (interactive)
  (recaptcha-v2 chan-recaptcha-site-key
		chan-post--referer
		(lambda (token)
		  (setq debug-on-error t)
		  (setq url-debug t)
		  (chan-post-submit-callback token)
		  (setq debug-on-error nil)
		  (setq url-debug nil))))

(defun chan-post-attach-file ()
  (interactive)
  (setq chan-post-filename (read-file-name "Attach file: "))
  (goto-char (point-min))
  (forward-line 4)
  (forward-char 5)
  (setq inhibit-read-only t)
  (delete-region (point) (point-at-eol))
  (insert (apply 'propertize (concat " "
				     chan-post-filename)
		 chan-post--single-line-field-text-props))
  (setq inhibit-read-only nil))

(defun chan-post (referer max-file-size &optional thread-id)
  "MAX-FILE-SIZE is required field and depends on the
board. THREAD-ID is nil if creating a new thread."
  (get-buffer-create "chan-post")
  (switch-to-buffer "chan-post")
  (chan-post-mode)
  (setq chan-post--referer referer)
  (setq inhibit-read-only t)
  (erase-buffer)

  (setq chan-post--max-file-size max-file-size
	chan-post--thread-id thread-id)

  (let ((single-line-field-props
	 chan-post--single-line-field-text-props)
	(comment-field-props
	 `(read-only t
		     front-sticky t
		     rear-nonsticky (read-only))))
    (insert (apply 'propertize "Name:\n" single-line-field-props)
	    (propertize "\n" 'read-only t)
	    (apply 'propertize "Options:\n" single-line-field-props)
	    (propertize "\n" 'read-only t)
	    (apply 'propertize "File: \n" single-line-field-props)
	    (apply 'propertize "Comment:\n" comment-field-props)))

  (setq inhibit-read-only nil))

(provide 'chan-post)

; (setq debug-on-error t)
; (let ((url-cache-expire-time 0)) (url-cache-prune-cache))
; (kill-matching-buffers " \*http.*" t t)

; (chan-post "https://boards.4channel.org/" "4194304" "73141305")

; (setq debug-on-error nil)
