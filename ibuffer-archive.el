;; GPLv3+

(require 'ibuffer)
(require 'ibuf-ext)
(require 'arc-mode)
(require 'tar-mode)

(define-ibuffer-filter archive-root
    "Limit current view to buffers corresponding to files within
a specific archive (zip, tar etc.) QUALIFIER"
  (:description "archive file"
		:reader
		(read-from-minibuffer
		 "Filter by archive file name (cons cell): "
		 nil nil t))
  (ibuffer-awhen (ibuffer-archive-root buf)
    (equal qualifier it)))


(defun ibuffer-archive-root (buf)
  "Return a cons cell (archive-type . archive-file-name), for the
_topmost_ archive, to which BUF belongs. If the file is not part
of an archive, return nil. Tarballs are considered archives."
  (with-current-buffer buf
    (cond
     (archive-subfile-mode
      (ibuffer-archive-root archive-superior-buffer))
     (tar-subfile-mode
      (ibuffer-archive-root tar-superior-buffer))
     ((equal major-mode 'archive-mode)
      (cons archive-subtype
	    (file-truename buffer-file-name)))
     ((equal major-mode 'tar-mode)
      (cons 'tar
	    (file-truename buffer-file-name))))))

;; TODO
;; Add checks:
;; 1. Was the containing archive buffer deleted?
;; 2. Is the buffer corresponding to an actual buffer or is somebody using the mode incorrectly?

;; emacs does not seem to deal with 7z files
;; ditto for rar files

;;;###autoload
(defun ibuffer-archive-generate-filter-groups-by-archive-root ()
  "Generate filter groups based on the archives corresponding to
the current buffers."
  (let (filter-groups)
    (dolist (buffer (buffer-list) filter-groups)
      (let ((filter-spec (ibuffer-archive-root buffer)))
	(if filter-spec
	    (let* ((archive-type (car filter-spec))
		   (archive-file-name (cdr filter-spec))
		   (filter-group-name
		    (concat
		     (capitalize (format "%s" archive-type))
		     ":"
		     (format "%s" archive-file-name)))
		   (filter-group
		    `(,filter-group-name
		      (archive-root . ,filter-spec))))
	      (if (not (member filter-group filter-groups))
		  (setq filter-groups
			(cons filter-group filter-groups)))))))))


(provide 'ibuffer-archive)