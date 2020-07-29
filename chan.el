;; -*- lexical-binding: t -*-

;; TODO: proper cleanup of temporary buffers when errors occur

;; TODO: Image loading needs lots of work. Currently all images are retrieved up
;; front, one request at a time, one after the other. This is because just
;; opening a bunch of connections at once to try to retrieve all the images led
;; to most of the requests failing. Going one at a time is also extremely slow,
;; however. The first improvement should be to only load the images that are
;; visible, and wait to load off screen images, just like a web browser. The
;; second, and much harder, improvement is to create a new HTTP library for
;; emacs that supports a modern HTTP version and can make use of pipelining on a
;; single connection. As a quick hack, images on the catalog are currently
;; disabled as it can take multiple minutes to load them all.

(require 'json)

(require 'chan-post)

(require 'url-http)
(defun url-http--user-agent-default-string ()
  "Compute a default User-Agent string based on `url-privacy-level'.

This version mimicks the User-Agent strings that Firefox with
fingerprinting protection on gives, providing decent anonymity."
  "Mozilla/5.0 (Windows NT 10.0; rv:68.0) Gecko/20100101 Firefox/68.0")

;; User facing variables

(defface chan-subject-face
  '((t :foreground "green" :weight bold))
  "Font lock mode face for highlighting thread subject lines."
  :group 'font-lock-faces)

(defface chan-name-face
  '((t :foreground "green" :weight bold))
  "Font lock mode face for highlighting thread subject lines."
  :group 'font-lock-faces)

(defvar chan-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'chan-next-post)
    (define-key map "p" 'chan-prev-post)
    (define-key map "g" 'chan-refresh)
    (define-key map "l" 'recenter-top-bottom)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'end-of-buffer)
    (define-key map "," 'chan-history-pop-mark)
    (define-key map "\r" 'chan-visit-current-thread)
    (define-key map " " 'scroll-up-command)
    (define-key map "\d" 'scroll-down-command)
    (define-key map "r" 'chan-write-post)
    map))
(setq chan-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map "n" 'chan-next-post)
	(define-key map "p" 'chan-prev-post)
	(define-key map "g" 'chan-refresh)
	(define-key map "l" 'recenter-top-bottom)
	(define-key map "<" 'beginning-of-buffer)
	(define-key map ">" 'end-of-buffer)
	(define-key map "," 'chan-history-pop-mark)
	(define-key map "\r" 'chan-visit-current-thread)
	(define-key map " " 'scroll-up-command)
	(define-key map "\d" 'scroll-down-command)
	(define-key map "r" 'chan-write-post)
	map))

(defvar chan-post-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'chan-follow-post-link)
    map))
    
;; Internal buffer state

(defvar chan-refresh-function nil)
(make-variable-buffer-local 'chan-refresh-function)

(defvar chan-thread-number nil
  "Number of the thread being displayed in the current buffer.")
(make-variable-buffer-local 'chan-thread-number)

(defvar chan--listings-beg 1
  "Point at which the rendered listings start in the display
buffer")
(make-variable-buffer-local 'chan--listings-beg)

;; Thread/Catalog forward/backward navigation
;;
;; Interface:
;;   chan-post-list-append
;;   chan-next-post
;;   chan-prev-post
;;   chan-current-post
;;   chan-find-post-by-number
;;
;; A simple list of posts/threads and corresponding buffer positions, ordered by
;; buffer position, is kept for each thread/catalog buffer. Storing buffer
;; positions means that the buffer cannot be changed aside from appending
;; posts/threads to the end, or else the list will need to be corrected.

(defvar chan-post-list '())
(make-variable-buffer-local 'chan-post-list)

(defvar chan-navigation-history '()
  "Keeps a history of where the mark was when a post link was
  followed.")

(defvar chan-navigation-history-max-length 16)

(defun chan--post-list-set-entry-number (entry num)
  (setcar entry num))

(defun chan-post-list-entry-number (entry)
  "Returns the post number for ENTRY."
  (car entry))

(defun chan--post-list-set-entry-pos (entry pos)
  (setcdr entry pos))

(defun chan-post-list-entry-pos (entry)
  "Returns the buffer position for the post described by ENTRY."
  (cdr entry))

(defun chan-post-list-iter (table f)
  "Intended only to be called from within this file.

F should return nil to indicate that it should keep iterating. If
F returns non-nil, iteration stops and this function returns what
F returned. If the end of the table is reached, nil is returned."
  (defun safe-cadr (table)
    (if (cdr table)
	(cadr table)
      nil))

  (catch 'exit
    (while table
      (let ((r (funcall f (car table) (safe-cadr table))))
	(if r
	    (throw 'exit r)
	  (setq table (cdr table)))))
    nil))

(defun chan-post-list-append (number pos-in-buffer)
  "Appends a post with number NUMBER and buffer position
POS-IN-BUFFER to the post list."
  (push (cons number pos-in-buffer) chan-post-list))

(defun chan-prev-post ()
  "Moves the cursor to the start of the previous post."
  (interactive)
  (chan-post-list-iter
   chan-post-list
   (lambda (this next)
     (if next
	 (if (>= (chan-post-list-entry-pos next) (point))
	     nil
	   (goto-char (chan-post-list-entry-pos next))
	   t)
       ;; we've reached the end of the list, meaning we are already at the first
       ;; post
       (signal 'end-of-buffer nil)))))

(defun chan-next-post ()
  "Moves the cursor to the start of the next post."
  (interactive)
  (chan-post-list-iter
   chan-post-list
   ;; look for the first post thats before our cursor and go to the post after
   ;; it
   (lambda (this next)
     (if (> (chan-post-list-entry-pos next) (point))
	 nil
       (goto-char (chan-post-list-entry-pos this))
       t))))

(defun chan-current-post ()
  "Returns the table entry for the current post.

Use the functions CHAN-POST-TABLE-ENTRY-NUMBER and
CHAN-POST-TABLE-ENTRY-POS to get the post number and buffer
position of the post returned by this function."
  (chan-post-list-iter
   chan-post-list
   ;; look for the first post thats at or before our cursor and grab it
   (lambda (this next)
     (if (> (chan-post-list-entry-pos this) (point))
	 nil
       this))))

(defun chan-find-post-by-number (number)
  (catch 'found
    (let ((rest chan-post-list))
      (while rest
	(if (string= (chan-post-list-entry-number (car rest)) number)
	    (throw 'found (car rest))
	  (setq rest (cdr rest))))
      (signal 'chan-post-not-found '("CHAN-FIND-POST-BY-NUMBER" number)))))

(defun chan-history-push-mark ()
  "Pushes the current mark onto chan-modes's mark history."
  (if (= (length chan-navigation-history) chan-navigation-history-max-length)
      (setq chan-navigation-history (nbutlast chan-navigation-history)))
  (setq chan-navigation-history (cons (point) chan-navigation-history)))

(defun chan-history-pop-mark ()
  (interactive)
  (if chan-navigation-history
      (progn (goto-char (car chan-navigation-history))
	     (setq chan-navigation-history (cdr chan-navigation-history)))
    (signal 'chan-history-empty (list "chan-history-pop-mark"))))

(define-error 'chan-post-not-found
  "chan-mode: Post not found.")
(define-error 'chan-history-empty
  "chan-mode: Navigation history is empty")

;; JSON parsing

;; json.el, built into Emacs, provides methods for reading JSON into Lisp
;; representations, but none for just skipping over a complete form in JSON
;; without parsing it into a Lisp representation. Below, some more generic
;; parsing functions are defined as well as a family of `json-bound-*` functions
;; that move over a JSON form with as little processing of it as possible.
;;
;; TODO: Good practice and conventions would dictate that these are named
;; `chan-json-*` instead of polluting the `json-*` """namespace"""

(define-error 'json-unexpected-syntax-array
  "JSON unexpexted syntax: expected array '['"
  'json-error)
(define-error 'json-unexpected-syntax-object
  "JSON unexpexted syntax: expected object '{'"
  'json-error)
(define-error 'json-unexpected-syntax-colon
  "JSON unexpexted syntax: expected colon ':'"
  'json-error)

(defun json-bound-string ()
  "Return (BEG END) where BEG is the position of the beginning
of the string and END is the position of the end of the string
excluding the surrouding '\"'s.

Moves point to end of string, past the final \"."
  ;; Skip the first quote
  (json-advance)
  (let ((beg (point)))
    (search-forward-regexp "[^\\]\"")
    (list beg (- (point) 1))))

(defun json-bound-constant ()
  "Moves point to the end of an occurence of a literal \"true\", \"false\",
\"null\", or a number literal. Returns (BEG END)."
  (let ((beg (point)))
    (search-forward-regexp "true\\|false\\|null\\|[0-9]*")
    (list beg (point))))

(defun json-read-constant-as-string ()
  (let ((peek (json-peek)))
    (cond
     ((= peek ?t) "true")
     ((= peek ?f) "false")
     ((= peek ?n) "null")
     (t ;; assume we are at an integer literal
      (let ((beg (point)))
	(re-search-forward "[0-9]*")
	(buffer-substring beg (point)))))))

(defun json-parse-array (f)
  "Parse the array at point, for each element calling F with the
point at the start of the element. F should move the point to the
end of the element."

  ;; Verify that the point is at the start of an array
  (unless (= (json-peek) ?\[)
    (signal 'json-unexpected-syntax-array (point)))

  ;; Skip over the '['
  (json-advance)
  (json-skip-whitespace)
  ;; read objects until we arrive at the end of the array
  (while (not (= (json-peek) ?\]))
    (funcall f)
    (if (= (json-peek) ?,) (json-advance))
    (json-skip-whitespace))
  ;; skip over final ']'
  (json-advance))

(defun json-parse-object (f)
  "Parse the object at point, for each key-value pair calling F
with the point at the start of the value and with the key as a
string as an argument to F."
  ;; Verify that the point is at the start of an object
  (unless (= (json-peek) ?{)
    (signal 'json-unexpected-syntax-object (list "found:" (json-peek) (point))))

  ;; Skip over the '{'
  (json-advance)
  (json-skip-whitespace)
  ;; read key-value pairs until we arrive at the end of the object
  (while (not (= (json-peek) ?}))
    (let ((key (json-bound-string)))
      ;; Verify that there is a colon after the key
      (unless (= (json-peek) ?:)
	(signal 'json-unexpected-syntax-colon (point)))
      ;; skip over the colon
      (json-advance)
      (json-skip-whitespace)
      ;; process the value
      (apply f key)
      ;; skip over any trailing commas
      (if (= (json-peek) ?,) (json-advance))
      (json-skip-whitespace)))
  ;; skip over final '}'
  (json-advance))

(defun json-bound (&optional _1 _2)
  "Move past whatever JSON form is at the point and return a
list (BEG END). Arguments are ignored and are only to allow this
function to be passed to json-parse-*."
  (let ((peek (json-peek)))
    (cond
     ((= peek ?{) (json-parse-object 'json-bound))
     ((= peek ?\[) (json-parse-array 'json-bound))
     ((= peek ?\") (json-bound-string))
     (t (let ((beg (point)))
	  ;(search-forward-regexp
	  ;"\"\\(\\.\\|.\\)*\"\\|true\\|false\\|null\\|[0-9]*")
	  ; this commented out version is correct but causes stack overflow
	  ; sometimes. pulling out the string matching and handling it
	  ; separately is working.
	  (search-forward-regexp "true\\|false\\|null\\|[0-9]*")
	  (list beg (point)))))))

(defmacro json-parse-object-by-key (&rest body)
  "BODY should be a list of elements of the form (KEY BODY) where
BODY are the forms to be executed when KEY is encountered in the
json object. When BODY is executed, the point will be at the
start of the value associated with KEY in the json buffer, and
BODY should move the point to the end of the value in the json
buffer (before any trailing commas)."
  `(json-parse-object
    (lambda (beg end)
      ;; look for key we care about
      (pcase (buffer-substring beg end)
	,@body
	(key (json-bound))))))

;; HTML rendering functions
;;
;; These functions are plugged into shr.el via shr-external-rendering-functions

(defun chan-render-a-tag (dom)
  "Custom HTML tag renderer for shr.el. Properly renders post links so that
chan-mode is used to follow them instead of the default behavior."
  (let* ((url (dom-attr dom 'href))
	 (start (point))
	 (match-result (string-match "#[0-9]*" url)))
    (if (and match-result (= match-result 0))
	(progn
	  (shr-generic dom)
	  (add-face-text-property start (point) 'shr-link t)
	  (add-text-properties
	   start
	   (point)
	   `(mouse-face highlight keymap ,chan-post-link-keymap
			;; MAGIC NUMBER: for links to posts within the thread,
			;; the format of the href element is #pXXXXXX, so we
			;; take a substring starting at the second character to
			;; get the post number
			post-number ,(substring url 2))))
      (shr-tag-a dom))))

;; Creating parts of pages

(defun chan-strip-headers ()
  "url.el includes HTTP headers in the buffers it returns. We
don't care about those, so this method will strip them out for
us. Point will end at the beginning of the buffer."
  (goto-char (point-min))
  (search-forward "\n\n")
  (delete-region 1 (point)))

(defun chan-fix-trailing-newlines (n-desired beg end)
  "Ensure that there are exactly N-DESIRED trailing newlines in
the region BEG END. Point should be at END.

When rendering html with shr-render-region, preceding newlines
are important. Always ensuring that there is at least one blank
like before the region being rendered seems to provide consistent
results. If there isn't a blank line before, then
shr-render-region will sometimes add newlines depending on the
tags (if any) surrounding the region. CHAN-FIX-TRAILING-NEWLINES
helps us with this."
  (if (re-search-backward "[^\n]" beg t)
      (let ((n (- end (point))))
	(forward-char 1)
	(delete-char (1- n))
	(insert-char ?\n n-desired))
    ;; if re-search-backward returned nil, then the region consists only of
    ;; newlines
    (progn
      (delete-backward-char (- end beg))
      (insert-char ?\n n-desired))))

;; Lots of Emacs's useful text manipulation functions work on buffers only, so I
;; tried storing as little state outside of the buffer text as possible, but
;; when inserting thumbnails asynchronously it does not suffice to use only the
;; buffer and keep track of positions where posts begins. Either I start adding
;; text properties and searching back and forth through the buffer, or I simply
;; store a list of rendered post bodies that can be concatenated together at the
;; end. The simpler (and hopefully faster) method wins.
(defvar chan--listings nil
  "Stores a list of listings to concatenate together in the display buffer.")
(make-variable-buffer-local 'chan--listings)

(defvar chan--n-threads-running 0
  "Simple counter used to synchronize multiple threads by
counting the number of threads still running.")
(make-variable-buffer-local 'chan--n-threads-running)

(defun chan--listings-put (n text thumb)
  "Put a listing in `chan--listings'. N is the listing
number. Either one of TEXT or THUMB should be set; the other
should be nil.

TEXT should simply be a string. THUMB should be a buffer
containing the sliced image. This is so no new buffers have to be
created when inserting the thumbnail with the text.

For each entry in `chan-listings', the main thread writes some
text and possibly the asynchronous callback from loading the
thumbnail writes the thumbnail. Because these processes proceed
in parallel, either one can be the first to write to
`chan--listings'."
  (if (<= (length chan--listings) n)
      ;; chan--listings isn't long enough, so extend it and put whatever we have
      ;; in it
      (setq chan--listings
	    (append chan--listings
		    (make-list (- n (length chan--listings)) nil)
		    (list (or text thumb))))
    (let ((listing (nthcdr n chan--listings)))
      (if (car listing)
	  ;; There is already an entry
	  (if text
	      ;; insert text w/ existing thumb
	      (let ((thumb-buffer (car listing)))
		(with-current-buffer thumb-buffer
		  (let ((sliced-thumb (buffer-string)))
		    (goto-char (point-min))
		    (insert text)
		    (let ((end (point)))
		      (goto-char (point-min))
		      (forward-line 2) ; move past subject line
		      (chan--insert-thumb-from-sliced-image sliced-thumb
							    5;TODO:magic
							    (point)
							    end))
		    (setcar listing (buffer-substring (point-min) (point)))))
		(kill-buffer thumb-buffer))
	    ;; insert thumb w/ existing text
	    (with-current-buffer thumb
	      (let ((sliced-thumb (buffer-string)))
		(goto-char (point-min))
		(insert (car listing))
		(let ((end (point)))
		  (goto-char (point-min))
		  (forward-line 2) ; move past subject line
		  (chan--insert-thumb-from-sliced-image sliced-thumb
							5;TODO:magic
							(point)
							end))
		(setcar listing (buffer-substring (point-min) (point)))))
	    (kill-buffer thumb))
	;; entry is empty, put whatever we have in it
	(setcar listing (or text thumb))))))

(defun chan--insert-listings (display-buffer)
  "Rewrites all the listings to the display buffer and also
updates the positions stored in `chan-post-list'."
    (with-current-buffer display-buffer	;TODO: with-current-buffer shound'nt be
					;needed here. this should only be called
					;from display buffer.
      (save-position
       (when (= 0 chan--n-threads-running)
	 (message "ALL DONE!")
	 ;; rewrite display buffer
	 (setq inhibit-read-only t)
	 (delete-region chan--listings-beg (point-max))
	 (goto-char (point-max))
	 (insert (apply 'concat chan--listings))
	 (setq inhibit-read-only nil)
	 ;; update navigation positions
	 (let ((accum 0)
	       (lengths nil)
	       ; (debug-on-error t)
	       )
	   (dolist (listing chan--listings)
	     (push (1+ accum) lengths)
	     (setq accum (+ accum (length listing))))
	   (mapcar (lambda (x)
		     (chan--post-list-set-entry-pos (car x) (cdr x)))
		   (-zip-pair chan-post-list lengths)))))))

(defun chan--insert-thumb-from-sliced-image (sliced-image height com-beg com-end)
  "Inserts a thumbnail image inline in the start of a
post. COM-BEG should be the buffer position of the first line of
the post body/comment. IMG-BUFFER should be the buffer containing
the image data as returned by url.el (its should still contain
headers or else this function will fail when trying to remove the
headers).

Point will end at the new end of the post (it can change if lines
must be added to accomodate the thumbnail)."
  (let* ((n-comment-lines (- (line-number-at-pos com-end)
			     (line-number-at-pos com-beg)))
	 (n-slices-to-inline (min height n-comment-lines)))

    ;; copy the parts of the image into the display buffer
    (goto-char com-beg)
    (dotimes (i n-slices-to-inline)
      (insert (substring sliced-image (* 2 i) (1+ (* 2 i)))
	      "  ")
      (search-forward "\n"))
    (if (= height n-slices-to-inline)
	;; all slices were inlined. move to new end of comment.
	(goto-char (+ com-end (* height 3))) ; slice + 2 spaces inserted
      ;; comment not long enough to inline all slices. add the remaining slices
      ;; to the end.
      (insert (substring sliced-image
			 (* 2 n-slices-to-inline))
	      "\n"))))

(defun chan--sliced-image-from-url-buffer (img-buffer height)
  "Returns nil on failure."
  (catch 'early-exit
    (progn
      ;; create split image in the buffer containing the raw image data from
      ;; url.el We buffer-swap-text only in this block
      (buffer-swap-text img-buffer)
      (let ((image
	     `(image :type imagemagick
		     :data ,(let ((data))
			      (condition-case nil
				  (chan-strip-headers)
				(error
				 (message
				  "Error when stripping headers from thumbnail. Buffer name: %s"
				  (buffer-name img-buffer))
				 (buffer-swap-text img-buffer)
				 (throw 'early-exit nil)))
			      (setq data (buffer-string))
			      data)
		     :height ,(* height (line-pixel-height))
		     :ascent center
		     ))
	    (img-beg (point)))
	(insert-sliced-image image nil nil height)
	(prog1 (buffer-substring img-beg (point))
	  (buffer-swap-text img-buffer))))))

(defvar chan--images-to-retrieve nil
  "List of (URL . LISTING-NUMBER) pairs representing images that
are queued up to be retrieved and then rendered into the listing
LISTING-NUMBER.")

(defun chan--queue-image (url listing-number)
  "Adds an image to the queue of images to retrieve.

When the current image being retrieved finishes, it will start
the retrieval of the next image. If no image is in the process of
being retrieved, this function will start the retrieval of the
image it is given."
  ;; chan--images-to-retrieve will be nil if there is no process currently
  ;; retrieving images. Images aren't pulled off the front until after they are
  ;; processed, not when processing starts.
  (unless chan--images-to-retrieve
    ;; start process
    (url-retrieve url 'chan--retrieve-image-callback (list listing-number
							   ;; Current buffer
							   ;; should be the
							   ;; display buffer
							   (current-buffer))))
  ;; add to queue
  (setq chan--images-to-retrieve (append chan--images-to-retrieve (list (cons url listing-number)))))

(defun chan--retrieve-image-callback (status listing-number display-buffer)
  "Callback called by url-retrieve."
  ;; pop from queue
  (setq chan--images-to-retrieve (cdr chan--images-to-retrieve))
  ;; retrieve next image if there are more to retrieve
  (when chan--images-to-retrieve
    (let ((url (car (car chan--images-to-retrieve)))
	  (listing-number (cdr (car chan--images-to-retrieve))))
    (url-retrieve url 'chan--retrieve-image-callback (list listing-number
							   display-buffer))))
  ;; process this image
  (chan--insert-thumb-callback nil listing-number display-buffer)
  )

(defun chan--insert-thumb-callback (status listing-number display-buffer)
  "Callback, called by url-retrieve."
  (message "CALLBACK %s" (buffer-name))
  (if (seq-find (lambda (x) (eq x :error)) status)
      (progn
        (message "THUMBNAIL RETRIEVAL FAILED. STATUS FOLLOWS:")
	(print status))
    (let* (; (debug-on-error t)
	   (height 5)
	   (sliced-thumb (chan--sliced-image-from-url-buffer (current-buffer)
							     height))
	   (thumb-buffer (current-buffer)))

      (when sliced-thumb
	(erase-buffer)
	(insert sliced-thumb)

	;; We have to watch which buffer we are in in these callbacks. All our
	;; buffer-local variables are in the display buffer
	(with-current-buffer display-buffer
	  (chan--listings-put listing-number nil thumb-buffer)))))
  (with-current-buffer display-buffer
    (message "DECREMENT %i" chan--n-threads-running)
    (setq chan--n-threads-running (1- chan--n-threads-running))
    (chan--insert-listings display-buffer)))

(defun chan-insert-thumb (url comment-beg)
  "Inserts a thumbnail taken from URL."
  (chan--queue-image url (length chan--listings))
  (message "INCREMENT %i\nURL: %s" chan--n-threads-running url)
  (setq chan--n-threads-running (1+ chan--n-threads-running)))

(defun chan-insert-subject-line (name time number)
  "Inserts the \"subject line\" for a post. Does not move point to a new line,
so that other info can be added to this line by the caller."
  (insert (propertize name 'face 'chan-name-face)
	  " "
	  time)
  (when number (insert " " number)))

(defun chan-insert-post-body (comment img-time)
  "Inserts a post body, which consists of a comment (if any) and a
thumbnail (if any)"
  (let ((beg (point)))
    ;; write comment
    (when comment
      (insert comment)
      (shr-render-region beg (point))
      (chan-fix-trailing-newlines 1 beg (point)))

    ;; write thumbnail
    (when img-time
      (chan-insert-thumb (concat
			  "https://i.4cdn.org/g/"
			  img-time
			  "s.jpg")
			 beg))

    (insert "\n")))

;; Parsing and rendering listings from JSON

(defmacro define-chan-listing-parser (name parameters &rest body)
  "Defines a function named NAME that renders a listing in the
display buffer using parameters parsed from JSON.

PARAMETERS is a list of (PARAM-NAME JSON-KEY JSON-PARSE-FUNCTION)
elements, where PARAM-NAME is the symbol name that the parameter
values are bound to for access from within BODY, JSON-KEY is the
key to extract the parameter value from, and JSON-PARSE-FUNCTION
is the function to use to parse the value from the JSON.

The resulting function has one required parameter JSON-BUFFER,
which actually refers to the buffer containing the display buffer
text, when called, but is swapped back to containing the json
text before BODY is evaluated. So in BODY it can be treated as
simply the buffer containing the JSON.

It also has an optional parameter, LISTING-NUMBER, which is the
number of the next listing to be written. If omitted, it defaults
to 0.

The function will return LISTING-NUMBER + the number of listings
it wrote."
  (declare (indent 2))
  (let ((param-list (seq-map (lambda (x) (car x)) parameters)))
    `(defun ,name (json-buffer &optional listing-number) ; json-buffer actually
       ; contains the display
       ; buffer text when it
       ; is passed in here
       "A function for parsing and rendering a single listing
from its JSON source into an Emacs buffer. This function is not
defined by hand. Intead, the macro `define-chan-listing-parser'
is used to generate this function."
       (let ((listing-number (or listing-number 0)))
	 (json-parse-array
	  (lambda ()
	    (let ,param-list
	      (json-parse-object-by-key
	       ,@(seq-map (lambda (x)
			    (list (cadr x) `(setq ,(car x) (,(caddr x)))))
			  parameters))
	      (buffer-swap-text json-buffer) ; swap to display buffer text
	      (let ((beg (point)))
		,@body
		(chan--listings-put listing-number (buffer-substring beg (point))
				    nil)
		(setq listing-number (1+ listing-number)))
	      (buffer-swap-text json-buffer) ; swap back to json buffer text
	      )))
	 listing-number))))

(define-chan-listing-parser chan-post-parse-function
    ((number "no" json-read-constant-as-string)
     (name "name" json-read-string)
     (comment "com" json-read-string)
     (img-ext "ext" json-read-string)
     (img-time "tim" json-read-constant-as-string)
     (time "now" json-read-string))

  (chan-post-list-append number (point))
  (chan-insert-subject-line name time number)
  (insert "\n\n")
  (chan-insert-post-body comment img-time))

(define-chan-listing-parser chan-catalog-listing-parse-function
    ((number "no" json-read-constant-as-string)
     (name "name" json-read-string)
     (subject "sub" json-read-string)
     (comment "com" json-read-string)
     (replies "replies" json-bound-constant)
     (images "images" json-bound-constant)
     (img-ext "ext" json-read-string)
     (img-time "tim" json-read-constant-as-string)
     (time "now" json-read-string))

  (chan-post-list-append number (point))

  (let ((beg (point)))
    (if subject
	(progn
	  ;; using <div> tags seems to stop shr from adding newlines
	  (insert "<div>" subject "</div>")
	  (shr-render-region beg (point))
	  ;; get rid of any trailing newlines shr might have added
	  (chan-fix-trailing-newlines 0 beg (point)))
      (insert "~~~~~"))
    (add-face-text-property beg (point) 'chan-subject-face)
    (insert " "))
  (chan-insert-subject-line name time number)
  (insert " R: ")
  (apply 'insert-buffer-substring json-buffer replies)
  (insert " I: ")
  (apply 'insert-buffer-substring json-buffer images)
  (insert "\n\n")

  ;; (chan-insert-post-body comment img-time)
  (chan-insert-post-body comment nil) ; skip thumbnails in the catalog because
				      ; it takes too long to retrieve them right
				      ; now
  )

;; Creating pages

(define-derived-mode chan-mode text-mode "chan-mode"
  ""
  ;; mark this mode as "special", meaning that it only is applicable to special
  ;; buffers produced by lisp code
  (put 'chan-mode 'mode-class 'special)

  ;; configure libraries we use
  (setq url-automatic-caching t)

  ; (buffer-face-mode 1)
  ; (setq buffer-face-mode-face 'chan-default-face)
  (variable-pitch-mode)

  (setq shr-use-fonts nil)
  ; (make-local-variable 'shr-external-rendering-functions) ;; TODO: is this safe
  ; 							  ;; and proper?
  ; (setq shr-external-rendering-functions
  ; 	'((a . chan-render-a-tag)))
  (setq-local shr-external-rendering-functions
	      '((a . chan-render-a-tag)))

  (read-only-mode)

  (setq chan--n-threads-running 0))

(defmacro save-position (body)
  "Like `save-excursion' except that only the value of point is
saved."
  `(let ((pos (point)))
     ,body
     (goto-char pos)))

(defun chan-update-display-buffer (bufname url f)
  "Handles some boilerplate that needs to be taken care of when
creating a new display buffer, retriving JSON from a URL, and
preparing to parse the JSON. BUFNAME is the buffer name to create
or update, URL is the URL to pull the JSON from, and F is the
function that will parse and render the JSON. F is called with
one argument, which is the display buffer to render to."
  (let ((display-buffer (get-buffer-create (concat "4chan g/" bufname))))
    (let ((json-buffer (url-retrieve-synchronously
			(concat "https://a.4cdn.org/g/" url ".json") nil t)))
      ;; switch to display buffer
      ;; with current buffer doesn't work because we rely on
      ;; (line-pixel-height), which doesn't return the correct value unless we
      ;; perform a buffer switch
      (switch-to-buffer display-buffer)
      (setq inhibit-read-only t)
      (erase-buffer)			; TODO: append to buffer instead of
					; rewriting?
      (chan-mode)
      (setq chan--listings nil)
      (message "INCREMENT MAIN %i" chan--n-threads-running)
      (setq chan--n-threads-running 1)

      ;; use buffer-swap-text rather than with-current-buffer so that all our
      ;; buffer local variable are in our display buffer rather than the
      ;; temporary json buffer
      (buffer-swap-text json-buffer)
      (chan-strip-headers)
      (funcall f json-buffer)
      ;; swap back to display buffer text
      (buffer-swap-text json-buffer)

      (setq inhibit-read-only nil)
      (goto-char (point-min))		; move to top of buffer
      (kill-buffer json-buffer))
    (switch-to-buffer display-buffer);TODO: redundant?
    (message "DECREMENT MAIN %i" chan--n-threads-running)
    (setq chan--n-threads-running (1- chan--n-threads-running))
    (chan--insert-listings display-buffer)))

(defun chan-thread (number)
  "Loads and displays the thread with number NUMBER."
  (chan-update-display-buffer
   number
   (concat "thread/" number)
   (lambda (display-text-buffer)

     (json-parse-object-by-key
      ("posts"
       (chan-post-parse-function display-text-buffer)))

     (setq chan-thread-number number)
     (setq chan-refresh-function (lambda ()
				   (save-position
				    (chan-thread
				     chan-thread-number)))))))

(defun chan-catalog ()
  "Loads and displays the thread catalog."
  (interactive)
  (chan-update-display-buffer
   "catalog"
   "catalog"
   (lambda (display-text-buffer)

     (let ((listing-number 0))
       (json-parse-array ;; threads are separated into an array of pages
	(lambda ()
	  (json-parse-object-by-key
	   ("threads"
	    (setq listing-number
		  (chan-catalog-listing-parse-function display-text-buffer
						       listing-number)))))))

     (setq chan-refresh-function (lambda () (save-position (chan-catalog)))))))

;; Some mode commands

(defun chan-refresh ()
  (interactive)
  (funcall chan-refresh-function))

(defun chan-visit-current-thread ()
  (interactive)
  (chan-thread (chan-post-list-entry-number (chan-current-post))))

(defun chan-follow-post-link (&optional mouse-event)
  "Jump to the post linked by the link at point.

Optional argument MOUSE-EVENT describes the mouse click event."
  (interactive (list last-nonmenu-event))
  (mouse-set-point mouse-event)
  (chan-history-push-mark)
  (let ((post-number (get-text-property (point) 'post-number)))
    (goto-char
     (chan-post-list-entry-pos
      (chan-find-post-by-number post-number)))))
  
(defun chan-write-post ()
  (interactive)
  (chan-post "https://boards.4channel.org/" "4194304" chan-thread-number))

(provide 'chan)

;(kill-matching-buffers " \*http.*" t t)
;(setq url-debug t)
;(setq debug-on-error t)
;(chan-thread "51971506")
;(chan-thread "73764040")
;(debug-on-entry 'chan--listings-put)

