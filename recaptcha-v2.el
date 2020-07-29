;; -*- lexical-binding: t -*-

;; reCAPTCHA v2 challenges in Emacs
;;
;; Google provides a fallback reCAPTCHA v2 for browsers with javascript disabled
;; in the form of an HTML page located at www.google.com/recaptcha/api/fallback,
;; which is normally simply embedded in an iframe. This Emacs Lisp code pulls
;; out the parts of the needed with elquery.el (challenge instructions,
;; challenge image, and hidden input value), displays the challenge in an Emacs
;; buffer, implements interaction with the challenge (selecting images and
;; submitting), and finally constructs and submits an HTML <form> to Google's
;; reCAPTCHA service, returning a verification token that is valid for two
;; minutes.

(require 'dash)
(require 'elquery)
(require 'url)

(defvar recaptcha-v2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "u" 'recaptcha-v2-toggle-0)
    (define-key map "i" 'recaptcha-v2-toggle-1)
    (define-key map "o" 'recaptcha-v2-toggle-2)
    (define-key map "j" 'recaptcha-v2-toggle-3)
    (define-key map "k" 'recaptcha-v2-toggle-4)
    (define-key map "l" 'recaptcha-v2-toggle-5)
    (define-key map "m" 'recaptcha-v2-toggle-6)
    (define-key map "," 'recaptcha-v2-toggle-7)
    (define-key map "." 'recaptcha-v2-toggle-8)
    (define-key map "\r" 'recaptcha-v2-submit)
    (define-key map "q" 'recaptcha-v2-quit) ;; TODO: implement this
    map))

;; variables that persist between reCAPTCHAs for the same site

(defvar recaptcha-v2-url nil
  "Stores the URL (including site key) that the reCAPTCHA was
retrieved from.")
(make-variable-buffer-local 'recaptcha-v2-url)

(defvar recaptcha-v2-callback nil
  "Function to call when a CAPTCHA is successfully solved. This function should
take the verification code (a string) its sole argument.")
(make-variable-buffer-local 'recaptcha-v2-callback)

(defvar recaptcha-v2-callback-buffer nil
  "Buffer from which recaptcha-v2 was started called from and
should return to when a captcha is completed correctly.")
(make-variable-buffer-local 'recaptcha-v2-callback-buffer)

;; variables that are updated for each reCAPTCHA

(defvar hidden-input-imageselect nil)
(make-variable-buffer-local 'hidden-input-imageselect)

(defvar recaptcha-v2-selection nil
  "Keeps track of which images are selected.")
(make-variable-buffer-local 'recaptcha-v2-selection)

(defvar recaptcha-v2-imageselect-pos 0
  "Buffer position that the imageselect starts at.")
(make-variable-buffer-local 'recaptcha-v2-imageselect-pos)

(defvar recaptcha-v2-image-size nil
  "The size, in character units, of the image as a pair (WIDTH . HEIGHT)")
(make-variable-buffer-local 'recaptcha-v2-image-size)

(defun recaptcha-v2--post-form (url inputs &optional referer)
  "Submits an HTML form using the POST method given an alist
of (NAME . VALUE) pairs in INPUTS, where NAME is the HTML `name`
attribute of an input and VALUE is the corresponding `value`
attribute. URL is the URL to send the POST request to. Returns
the buffer returned by url.el."
  (let* ((url-request-method "POST")
	 (url-request-data
	  ;; create request body, a string of the form
	  ;; name=value[&name=value[&name=value...]]
	  (seq-mapcat (lambda (input)
			(concat (if (eq input (car inputs))
				    nil
				  "&")
				(car input)
				"="
				(cdr input)))
		      inputs))
	(url-request-extra-headers
	 `(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Content-Length" . ,(number-to-string (length url-request-data))))))
    (when referer
      (push `("Referer" . ,referer) url-request-extra-headers))
    (url-retrieve-synchronously url)))

(defun recaptcha-v2-toggle-image (num)
  "Toggle image/checkbox number NUM."
  ;; update internal selection
  (setcar (nthcdr num recaptcha-v2-selection)
	  (not (nth num recaptcha-v2-selection)))
  ;; update selection indicators
  (let* ((total-width (floor (car recaptcha-v2-image-size)))
	 (img-col (mod num 3)) ;; column number from 0-3
	 (col (round (* total-width (/ img-col 3.0)))) ;; buffer column
	 (n-chars-to-write (- (round (* total-width (/ (1+ img-col) 3.0)))
			      col)))
    (goto-char (+ recaptcha-v2-imageselect-pos
		  ;; move past rows
		  (* (+ 3 total-width) (/ num 3)) ; integer division; 3 from image+newline+newline
		  ;; move past line containing image row into line containing
		  ;; selection indicators
		  2
		  ;; move to column that starts the indicator for image NUM
		  col))
    (setq inhibit-read-only t)
    (delete-char n-chars-to-write)
    (insert-char (if (nth num recaptcha-v2-selection) ?X ?\s)
		 n-chars-to-write)
    (setq inhibit-read-only nil)))

(defun recaptcha-v2-toggle-0 ()
  (interactive)
  (recaptcha-v2-toggle-image 0))
(defun recaptcha-v2-toggle-1 ()
  (interactive)
  (recaptcha-v2-toggle-image 1))
(defun recaptcha-v2-toggle-2 ()
  (interactive)
  (recaptcha-v2-toggle-image 2))
(defun recaptcha-v2-toggle-3 ()
  (interactive)
  (recaptcha-v2-toggle-image 3))
(defun recaptcha-v2-toggle-4 ()
  (interactive)
  (recaptcha-v2-toggle-image 4))
(defun recaptcha-v2-toggle-5 ()
  (interactive)
  (recaptcha-v2-toggle-image 5))
(defun recaptcha-v2-toggle-6 ()
  (interactive)
  (recaptcha-v2-toggle-image 6))
(defun recaptcha-v2-toggle-7 ()
  (interactive)
  (recaptcha-v2-toggle-image 7))
(defun recaptcha-v2-toggle-8 ()
  (interactive)
  (recaptcha-v2-toggle-image 8))

(defun recaptcha-v2-submit ()
  "When submitting, Google will either give another CAPTCHA or it
will give a verification code. If Google gives another CAPTCHA,
present that CAPTCHA and let the user solve it again. If Google
gives a verification code, extract it and call the callback
stored in RECAPTCHA-V2-CALLBACK."
  (interactive)
  (setq debug-on-error t)
  (let ((response
	 (recaptcha-v2--post-form
	  recaptcha-v2-url
	  (cons hidden-input-imageselect
		(seq-filter
		 (lambda (e) e)
		 (seq-map-indexed (lambda (e i)
				    (if e
					(cons "response"
					      (number-to-string i))
				      nil))
				  recaptcha-v2-selection)))
	  recaptcha-v2-url)))
    (let* ((dom
	    (with-current-buffer response
	      (goto-char (point-min))
	      (search-forward "\n\n")
	      (elquery-read-string (buffer-substring (point) (point-max)))))
	   (verification-token-div (elquery-$ ".fbc-verification-token" dom)))
      (if verification-token-div
	  (let ((callback recaptcha-v2-callback))
	    (kill-buffer)
	    (message "KILL BUFFER")
	    (switch-to-buffer recaptcha-v2-callback-buffer)
	    (funcall callback
		     (elquery-text
		      (car (elquery-$ "textarea"
				      (car verification-token-div))))))
	(recaptcha-v2-present-captcha dom)
	(message "reCAPTCHA: Please try again.")))))

(define-derived-mode recaptcha-v2-mode special-mode "reCAPTCHA-v2"
  "Mode for solving and sumbitting a reCAPTCHA v2.")

(defun recaptcha-v2-insert-challenge-instructions (dom)
  "Inserts the challenge instructions in the buffer to present to
the user from the DOM as represented by elquery.el"
  ;; FIXME: elquery is stripping spaces so we add them back, and for some reason
  ;; elquery seems to be appending a text element containing a '.' when there is
  ;; no '.'  in the original HTML. Needs investigation.
  (insert (apply 'concat (--map (concat (elquery-text it) " ")
				(elquery-children dom))))
  (insert "\n\n"))

(defun recaptcha-v2-insert-imageselect (image)
  "Inserts the challenge imageselect in the buffer to present to
the user, given the Emacs image descriptor IMAGE."
  (setq recaptcha-v2-selection (copy-tree '(nil nil nil nil nil nil nil nil nil))
	recaptcha-v2-image-size (image-size image)
	recaptcha-v2-imageselect-pos (point))
  (insert-sliced-image image nil nil 3)
  (goto-char recaptcha-v2-imageselect-pos)
  (dotimes (_ 3)
    (forward-char)
    (insert "\n")
    (insert-char ?\s (floor (car recaptcha-v2-image-size)))
    (forward-char)))

(defun recaptcha-v2-present-captcha (dom)
  "Given the DOM of the reCAPTCHA, display it in the current
buffer. This function assumes that the buffer is already in
recaptcha-v2-mode, and has RECAPTCHA-V2-URL set. This function
will set up everything else."
  (setq inhibit-read-only t)
  (erase-buffer)

  ;; get the hidden HTML input required to submit a completed captcha. Store it
  ;; as a pair of (NAME . VALUE)
  (setq hidden-input-imageselect
	(let* ((challenge-dom
		(car (elquery-$ ".fbc-imageselect-challenge"
				dom)))
	       (hidden-input-dom
		(car (elquery-$ "[type=hidden]" challenge-dom))))
	  (cons (elquery-prop hidden-input-dom "name")
		(elquery-prop hidden-input-dom "value"))))

  ;; insert the challenge instructions
  (recaptcha-v2-insert-challenge-instructions
   (car (or (elquery-$ ".rc-imageselect-desc-no-canonical" dom)
	    (elquery-$ ".rc-imageselect-desc" dom))))

  ;; insert the imageselect (image+room for selection indicators
  (let ((img-buffer (url-retrieve-synchronously ;; TODO: cleanup img-buffer on error
		     (concat
		      "https://www.google.com"
		      (elquery-prop
		       (car (elquery-$ ".fbc-imageselect-payload" dom))
		       "src")))))
    (recaptcha-v2-insert-imageselect
     `(image :type jpeg
	     :data ,(with-current-buffer img-buffer
		      (goto-char (point-min))
		      (search-forward "\n\n")
		      (buffer-substring (point) (point-max)))))
    (kill-buffer img-buffer))

  (setq inhibit-read-only nil))

(defun recaptcha-v2 (site-key referer callback)
  "To retrieve a reCAPTCHA, the public site key for
reCAPTACHA (SITE-KEY) and the domain for the site embedding the
reCAPTCHA (REFERER) need to be sent to Google."
  ;; get our buffer to display the reCAPTCHA
  (setq recaptcha-v2-callback-buffer (current-buffer))
  (get-buffer-create "reCAPTCHA")
  (switch-to-buffer "reCAPTCHA")

  (recaptcha-v2-mode)
  (setq recaptcha-v2-url 
	(concat "https://www.google.com/recaptcha/api/fallback?k="
		site-key))
  (setq recaptcha-v2-callback callback)
  (let* (
	 ;; this variable configures url.el to send these headers
	 (url-request-extra-headers `(("Referer" . ,referer)))
	 ;; get and parse the recaptcha html into a dom tree with elquery
	 (dom
	  (let ((url-buffer (url-retrieve-synchronously recaptcha-v2-url)))
	    (prog1
		(with-current-buffer url-buffer
		  (goto-char (point-min))
		    (search-forward "\n\n")
		    (elquery-read-string (buffer-substring (point)
							   (point-max))))
	      (kill-buffer url-buffer)))))
    (recaptcha-v2-present-captcha dom)))

(provide 'recaptcha-v2)
