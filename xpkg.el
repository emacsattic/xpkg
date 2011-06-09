;;; xpkg.el --- extract information from Emacs Lisp packages

;; Copyright (C) 2010-2011  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20101001
;; Updated: 20110609
;; Version: 0.2.1
;; Homepage: https://github.com/tarsius/xpkg
;; Keywords: git packages

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extract information from Emacs Lisp packages stored in git repositores.

;;; Code:

(require 'cl)
(require 'magit)
(require 'elx)
(require 'elm)

(defun xpkg-metadata (ref config)
  "Return the metadata of REF in the current git repository."
  (let* ((name (plist-get config :name))
	 (fetcher (plist-get config :fetcher))
	 (features (xpkg-features ref config))
	 (hard-deps (nth 1 features))
	 (soft-deps (nth 2 features)))
    (xpkg-with-file ref (xpkg-mainfile ref config)
      (let ((wikipage (or (let ((page (plist-get config :wikipage)))
			    (when page
			      (concat "http://www.emacswiki.org/" page)))
			  (elx-wikipage nil name nil t))))
	(list :summary (elx-summary nil t)
	      :repository (when (memq fetcher '(bzr cvs darcs git hg svn))
			    (cons fetcher (plist-get config :url)))
	      :created (elx-created)
	      :updated (elx-updated)
	      :license (elx-license)
	      :authors (elx-authors nil t)
	      :maintainer (elx-maintainer nil t)
	      :adapted-by (elx-adapted-by nil t)
	      :provided (car features)
	      :required (when (or hard-deps soft-deps)
			  (if soft-deps
			      (list hard-deps soft-deps)
			    (list hard-deps)))
	      :keywords (elx-keywords nil t)
	      :homepage (or (elx-homepage)
			    (plist-get config :homepage)
			    (when (eq 'wiki (plist-get config :fetcher))
			      wikipage))
	      :wikipage wikipage
	      :commentary (unless (plist-get config :bad-encoding)
			    (elx-commentary)))))))


;;; Files.

(defun xpkg-libraries (rev config)
  (let ((exclude (plist-get config :exclude-path)))
    (mapcan (lambda (file)
	      (when (and (string-match "\\.el\\(\\.in\\)?$" file)
			 (not (string-match "\\(\\`\\|/\\)\\." file))
			 (not (and exclude (string-match exclude file))))
		(list file)))
	    (magit-git-lines "ls-tree" "-r" "--name-only" rev))))

(defun xpkg-mainfile (ref config)
  "Return the mainfile of REF in the current git repository.

The returned path is relative to the repository of nil if the mainfile
can't be determined.  If REF contains only one file return that.
Otherwise return the file whose basename matches NAME or NAME with
\"-mode\" added to or removed from the end, whatever makes sense; case
is ignored.  If there is still no match try to extract the value of
`:mainfile' from the plist CONFIG."
  (let ((name (plist-get config :name))
	(explicit (plist-get config :mainfile))
	(files (xpkg-libraries ref config)))
    (if (= 1 (length files))
	(car files)
      (flet ((match (feature)
		    (car (member* (format "^\\(.+?/\\)?%s\\.el\\(\\.in\\)?$"
					  (regexp-quote feature))
				  files :test 'string-match))))
	(or (match name)
	    (match (if (string-match "-mode$" name)
		       (substring name 0 -5)
		     (concat name "-mode")))
	    (when (or (not files) (member explicit files))
	      explicit))))))

(defmacro xpkg-with-file (rev file &rest body)
  (declare (indent 2) (debug t))
  (let ((fsymb (gensym "file"))
	(rsymb (gensym "rev")))
    `(let ((,fsymb ,file)
	   (,rsymb ,rev))
       (with-temp-buffer
	 (magit-git-insert (list "show" (format "%s:%s" ,rsymb ,fsymb)))
	 (goto-char (point-min))
	 (archive-set-buffer-as-visiting-file ,fsymb)
	 (setq buffer-file-name ,fsymb)
	 (with-syntax-table emacs-lisp-mode-syntax-table
	   ,@body)))))


;;; Features.

(defvar xpkg-feature-alist nil
  "Alist of known features and the providing packages.

Each element is a cons cell whose car is a feature symbol and whose cdr is
the providing package, a string.  This variable has to be set when using
function `xpkg-features' with the DEPENDENCIES argument; this can
be done by first calling this function for all known packages.")

(defun xpkg-features (ref config &optional associate)
  "Process the features of REF in the current git repository.

If optional ASSOCIATE is non-nil associate the provided features with the
current package (extracted from property `:name' in CONFIG) and return a
list of the provided features.

Otherwise not only determine provided features but also the features and
packages required by the current package and return a list of the form:

  ((PROVIDED-FEATURE...)
   ((PROVIDING-PACKAGE HARD-REQUIRED-FEATURE...)...)
   ((PROVIDING-PACKAGE SOFT-REQUIRED-FEATURE...)...)
   (BUNDLED-FEATURE...))

Also see the source comments of this function for more information."
  (let ((name (plist-get config :name))
	;; When a package bundles libraries also distributed seperately
	;; the features these libraries provide have to be excluded from
	;; the list of features provided by the package.
	;;
	;; Likewise some features that appear to be required by a package
	;; sometimes are not, because they are only needed for testing or
	;; by some library belonging to the package which is normally not
	;; loaded.
	(exclude-required (plist-get config :exclude-required))
	(exclude-provided (plist-get config :exclude-provided))
	provided required bundled)
    (dolist (file (xpkg-libraries ref config))
      (xpkg-with-file ref file
	(dolist (prov (xpkg--provided))
	  ;; If a package bundles required dependencies we do not want
	  ;; these features to appear in the list of provided features so
	  ;; that other packages do not pull in these packages instead of
	  ;; the real upstream package.
	  ;;
	  ;; If a library is bundled and the actual upstream package is
	  ;; *not* mirrored then the features it provides do not appear
	  ;; in the list returned by this function.
	  ;;
	  ;; If a library is bundled and the actual upstream package *is*
	  ;; mirrored then that package along with the features provided
	  ;; by the bundled library appears in the list of required
	  ;; packages.
	  (if (member prov exclude-provided)
	      (add-to-list 'bundled prov)
	    (when prov
	      (add-to-list 'provided prov))
	    (unless associate
	      ;; If some but not all of the features provided by a file
	      ;; are excluded do not exclude the additional features.
	      ;;
	      ;; We do this because the file might be legitimately belong
	      ;; to the package but might never-the-less illegitimately
	      ;; provide a foreign feature to indicate that is a drop-in
	      ;; replacement.
	      (push (xpkg--required) required))))))
    (setq provided (xpkg--sanitize-provided provided))
    (if associate
	;; If and only if optional argument ASSOCIATE is non-nil add
	;; associations for the provided features to the value of variable
	;; `xpkg-feature-alist' unless another package is already
	;; associated with the feature and the current package does not win
	;; based on a comparison of it's package name with the feature name.
	;; In case of conflict and regardless which package wins a warning
	;; is shown.
	;;
	;; The value of `xpkg-feature-alist' is not always updated when this
	;; function is called because it is called for all versions as well
	;; as the tips of all vendor branches and these different refs
	;; might differ in what features they provide.  If the caller of this
	;; function could not control whether associations are updated or not
	;; this could result in seemingly random change depending on what
	;; ref was last processed.
	;;
	;; Since Emacs provides no way to specify what version of a package
	;; another package depends on callers of this function have to ensure
	;; that they are consistent about what ref is used when using
	;; ASSOCIATE, or the features provided by a package might randomly
	;; change depending on what ref was last used as argument to this
	;; function.
	(progn (dolist (prov provided)
		 (let ((elt (assoc prov xpkg-feature-alist)))
		   (if elt
		       (unless (equal (cdr elt) name)
			 (xpkg-log "Feature %s provided by %s and %s"
				   prov (cdr elt) name)
			 (when (eq (intern name) prov)
			   (aput 'xpkg-feature-alist prov name)))
		     (aput 'xpkg-feature-alist prov name))))
	       (setq xpkg-feature-alist
		     (sort* xpkg-feature-alist 'string< :key 'car)))
      (setq required (xpkg--sanitize-required required provided))
      (list provided
	    (xpkg--lookup-required (nth 0 required) exclude-required name 'hard)
	    (xpkg--lookup-required (nth 1 required) exclude-required name 'soft)
	    (sort bundled 'string<)))))

(defun xpkg--lookup-required (required exclude name type)
  (let (packages)
    (dolist (feature required)
      (let ((package (xpkg--lookup-required-1 feature exclude)))
	(case package
	  (:excluded)
	  (nil
	   (xpkg-log "%s: %s required %s not available" name type feature))
	  (t
	   (let ((entry (assoc package packages)))
	     (if entry
		 (unless (memq feature (cdr entry))
		   (setcdr entry (sort (cons feature (cdr entry)) 'string<)))
	       (push (list package feature) packages)))))))
    (sort* packages
	   (lambda (a b)
	     (cond ((null a) nil)
		   ((null b) t)
		   (t (string< a b))))
	   :key 'car)))

(defun xpkg--lookup-required-1 (feature exclude)
  "Return a string representing the package that provides FEATURE."
  (cond ((member feature exclude) :excluded)
	((not (string-match "-autoloads?$" (symbol-name feature)))
	 (cdr (assoc feature xpkg-feature-alist)))
	(t
	 (xpkg--lookup-required-1
	  (intern (substring (symbol-name feature) 0 (match-beginning 0)))
	  exclude))))

(defconst xpkg-provided-regexp "\
\(\\(?:cc-\\|silentcomp-\\)?provide[\s\t\n]+'\
\\([^(),\s\t\n]+\\)\\(?:[\s\t\n]+'\
\(\\([^(),]+\\))\\)?)")

(defun xpkg--provided ()
  (let (features)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward xpkg-provided-regexp nil t)
	(unless (save-match-data
		  (or (nth 3 (syntax-ppss))   ; in string
		      (nth 4 (syntax-ppss)))) ; in comment
	  (dolist (feature (cons (match-string 1)
				 (when (match-string 2)
				   (split-string (match-string 2) " " t))))
	    (add-to-list 'features (intern feature))))))
    (xpkg--sanitize-provided features)))

(defun xpkg--sanitize-provided (provided)
  (let (sanitized)
    (dolist (feature provided)
      (unless (member feature sanitized)
	(push feature sanitized)))
    (sort sanitized #'string<)))

(defconst xpkg-required-regexp "\
\(\\(?:cc-\\)?require[\s\t\n]+'\
\\([^(),\s\t\n]+\\)\
\\(?:\\(?:[\s\t\n]+\\(?:nil\\|\".*\"\\)\\)\
\\(?:[\s\t\n]+\\(?:nil\\|\\(t\\)\\)\\)?\\)?)")

(defun xpkg--required ()
  (let (hard soft)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward xpkg-required-regexp nil t)
	(let ((feature (intern (match-string 1))))
	  (cond ((save-match-data
		   (or (nth 3 (syntax-ppss))    ; in string
		       (nth 4 (syntax-ppss))))) ; in comment
		((match-string 2)
		 (unless (or (member feature hard)
			     (member feature soft))
		   (push feature soft)))
		((not (member feature hard))
		 (setq soft (remove feature soft))
		 (push feature hard))))))
    (xpkg--sanitize-required (list (list hard soft)))))

(defun xpkg--sanitize-required (required &optional exclude)
  (let (hard soft)
    (dolist (requ required)
      (setq hard (append (nth 0 requ) hard)
	    soft (append (nth 1 requ) soft)))
    (setq hard (xpkg--sanitize-required-1 hard exclude)
	  soft (xpkg--sanitize-required-1 soft (append exclude hard)))
    (if soft
	(list hard soft)
      (when hard
	(list hard)))))

(defun xpkg--sanitize-required-1 (required &optional exclude)
  (let (sanitized)
    (dolist (feature required)
      (unless (or (member feature exclude)
		  (member feature sanitized))
	(push feature sanitized)))
    (sort sanitized #'string<)))


;;; Utilities.

(defun xpkg-log (format-string &rest args)
  "Display a message at the bottom of the screen.
The message also goes into the `*Messages*' and `*xpkg-log*' buffers."
  (with-current-buffer (get-buffer-create "*xpkg-log*")
    (goto-char (point-max))
    (insert (concat (apply #'message format-string args) "\n"))))

(defun xpkg-show-log ()
  "Pop to the buffer named \"*xpkg-log*\" if it exists."
  (interactive)
  (let ((buffer (get-buffer "*xpkg-log*")))
    (when buffer
      (pop-to-buffer buffer))))

(provide 'xpkg)
;;; xpkg.el ends here
