;;; xpkg.el --- extract information from Emacs Lisp packages

;; Copyright (C) 2010-2011  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20101001
;; Updated: 20110308
;; Version: 0.1.2-git
;; Homepage: https://github.com/tarsius/xpkg
;; Keywords: docs, libraries, packages

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

;; Extract information from Emacs Lisp packages.
;; Packages are required to be stored inside git repositories.
;; Also see package `elx' which extracts information from libraries.

;;; Code:

(require 'cl)
(require 'magit)
(require 'elx)
(require 'elm)

(defun xpkg-metadata (ref config)
  "Return the metadata of REF in the current git repository."
  (let* ((name (plist-get config :name))
	 (fetcher (plist-get config :fetcher))
	 (features (xpkg-features ref config nil t))
	 (hard-deps (nth 1 features))
	 (soft-deps (nth 2 features))
	 (mainfile (xpkg-mainfile ref config)))
    (elx-with-file (cons ref mainfile)
      (let ((wikipage (or (let ((page (plist-get config :wikipage)))
			    (when page
			      (concat "http://www.emacswiki.org/" page)))
			  (elx-wikipage mainfile name nil t))))
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
	      :keywords (elx-keywords mainfile t)
	      :homepage (or (elx-homepage)
			    (plist-get config :homepage)
			    (when (eq 'wiki (plist-get config :fetcher))
			      wikipage))
	      :wikipage wikipage
	      :commentary (unless (plist-get config :bad-encoding)
			    (elx-commentary mainfile)))))))

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
	(files (elx-elisp-files (cons default-directory ref))))
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

(defsubst xpkg-asort (variable)
  (set variable (sort* (symbol-value variable) 'string< :key 'car)))

(defvar xpkg-keyword-alist nil
  "Alist of known keywords and the associated packages.
Has the form ((KEYWORD PACKAGE...)...).")

(defun xpkg-initialize-keyword-alist (packages)
  "Initialize the value of `xpkg-keyword-alist'.
PACKAGES is a list of the form ((NAME REPO REF CONFIG)...)."
  (interactive)
  (setq xpkg-keyword-alist nil)
  (mapc-with-progress-reporter
   "Initializing keyword alist..."
   (lambda (elt)
     (apply 'xpkg-keywords (append elt (list t t))))
   packages)
  (xpkg-asort 'xpkg-keyword-alist))

(defun xpkg-keywords (ref config &optional associate nosort)
  "Process the keywords of REF in REPO of the package named NAME.

Return a sorted list of the provided keywords.  If optional ASSOCIATE is
non-nil associate the package with the the defined keywords in the value
of variable `xpkg-keyword-alist'."
  (let ((keywords (elx-with-file (cons ref (xpkg-mainfile ref config))
		    (elx-keywords))))
    (if (not associate)
	keywords
      (dolist (keyword keywords)
	(let ((keylist (assoc keyword xpkg-keyword-alist)))
	  (if keylist
	      (unless (member name (cdr keylist))
		(setcdr keylist (sort (cons name (cdr keylist)) 'string<)))
	    (push (list keyword name) xpkg-keyword-alist))))
      (if nosort
	  keywords
	(xpkg-asort 'xpkg-keyword-alist)))))

(defconst xpkg-emacs-features (bound-and-true-p xpkg-emacs-features)
  "List of features provided by Emacs.

This variable should be set and saved when a new version of GNU Emacs is
being targeted and then remain constant.")

(defun xpkg-initialize-emacs-features (&optional directory)
  "Initialize the value of `xpkg-emacs-features'.

Optional DIRECTORY is the directory where the lisp files of the Emacs
installation for which you want `xpkg-emacs-features' to be set is
located, if it is nil or not provided is the lisp directory of the
running Emacs instance."
  (interactive (list (read-directory-name "Lisp directory: ")))
  (with-no-warnings
    (setq xpkg-emacs-features
	  (elx-provided
	   (or directory
	       (file-name-directory (find-library-name "version")))))))

(defvar xpkg-feature-alist nil
  "Alist of known features and the providing packages.

Each element is a cons cell whose car is a feature symbol and whose cdr is
the providing package, a string.  This variable has to be set when using
function `xpkg-features' with the DEPENDENCIES argument; this can
be done by first calling this function for all known packages.")

(defun xpkg-initialize-feature-alist (packages)
  "Recreate the value of `xpkg-feature-alist'.

PACKAGES is a list of the form ((NAME REPOSITORY REF)...).
If multiple packages provide the same features this is logged."
  (interactive)
  (setq xpkg-feature-alist nil)
  (mapc-with-progress-reporter
   "Initializing feature alist..."
   (lambda (elt)
     (apply 'xpkg-features (append elt (list t nil t))))
   packages)
  (mapc-with-progress-reporter
   "Checking feature consistency..."
   (lambda (elt)
     (apply 'xpkg-features (append elt (list t t t))))
   packages)
  (xpkg-asort 'xpkg-feature-alist))

(defun xpkg-features (ref config &optional associate dependencies batch)
  "Process the features of the package named NAME.

REPO is the path to the git repository containing the package; it may be
bare.  REF has to be an existing commit in that repository.

If optional DEPENDENCIES is non-nil return a list of the form:

  ((PROVIDED-FEATURE...)
   ((PROVIDING-PACKAGE HARD-REQUIRED-FEATURE...)...)
   ((PROVIDING-PACKAGE SOFT-REQUIRED-FEATURE...)...)
   (BUNDLED-FEATURE...))

Otherwise return:

  (PROVIDED-FEATURE...)

If optional ASSOCIATE is non-nil associate the provided features with the
package in the value of variable `xpkg-feature-alist', if appropriate.

Also see the source comments of this function for more information."
  (let* ((name (plist-get config :name))
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
	 (exclude-path     (plist-get config :exclude-path))
	 provided required bundled)
    (dolist (file (elx-elisp-files-git (cons default-directory ref)))
      ;; Files that match `exclude-path' are ignored completely; neither
      ;; the provided nor required features appear anywhere in the return
      ;; value of this function at all.
      (unless (and exclude-path (string-match exclude-path file))
	(dolist (prov (elx-with-file (cons ref file) (elx--buffer-provided)))
	  ;; If a package bundles required dependencies we do not want
	  ;; these features to appear in the list of provided features so
	  ;; that other packages do not pull in these packages instead of
	  ;; the real upstream package.
	  ;;
	  ;; We do this even if the upstream package is not mirrored yet;
	  ;; which means that other packages depending on features from
	  ;; the upstream package will have unresolved dependencies,  but
	  ;; that is still better than instead depending on the current
	  ;; package whose purpose might be something completely different
	  ;; but just happens to provide a common dependency.  However for
	  ;; the current package these features are not considered to be
	  ;; missing.
	  ;;
	  ;; If a bundled feature is not provided by any mirrored package
	  ;; that feature is neither listed as required nor a provided
	  ;; feature of this package in the output of this function.
	  ;;
	  ;; If a bundled feature is also provided by the mirrored
	  ;; upstream package that package along with the feature shows up
	  ;; in the list of required packages.
	  (if (member prov exclude-provided)
	      (add-to-list 'bundled prov)
	    (when prov
	      (add-to-list 'provided prov))
	    (when dependencies
	      ;; Even if some of the features provided by this file are
	      ;; excluded do not exclude the required features.  We do
	      ;; this because the file might be legitimately belong to the
	      ;; package but might never-the-less illegitimately provide
	      ;; a foreign feature to indicate that is a drop-in
	      ;; replacement or whatever.
	      (push (elx-with-file (cons ref file) (elx--buffer-required))
		    required))))))
    (setq provided (elx--sanitize-provided provided t))
    (when associate
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
      ;; another package depends on a particular ref had to be
      ;; chosen whose provided features are recorded to calculate the
      ;; dependencies of other packages.  The latest tagged ref
      ;; of the "main" vendor or if no tagged ref exists it's tip
      ;; has been chosen for this purpose, but this is controlled by the
      ;; callers of this function not itself.
      (dolist (prov provided)
	(let ((elt (assoc prov xpkg-feature-alist)))
	  (if elt
	      (unless (equal (cdr elt) name)
		(xpkg-log "Feature %s provided by %s and %s"
			  prov (cdr elt) name)
		(when (eq (intern name) prov)
		  (aput 'xpkg-feature-alist prov name)))
	    (aput 'xpkg-feature-alist prov name))))
      (unless batch
	;; Keep `xpkg-feature-alist' sorted.
	(xpkg-asort 'xpkg-feature-alist)))
    (if (not dependencies)
	;; This function usually is called to update/create revision epkgs
	;; and to updated the value of variable `xpkg-feature-alist' by
	;; side-effect.  However in some cases we only need to do the
	;; latter so it is possible to skip the step of determining the
	;; dependencies.
	provided
      (setq required (elx--sanitize-required required provided t))
      (list provided
	    (xpkg-lookup-required name ref 'hard (nth 0 required)
				  exclude-required)
	    (xpkg-lookup-required name ref 'soft (nth 1 required)
				  exclude-required)
	    (sort bundled 'string<)))))

(defun xpkg-lookup-required (name ref type required exclude)
  "Return the packages providing all features in list REQUIRED.

The returned value has the form: ((PACKAGE FEATURE...)...).  If a feature
is provided by a package that is part of Emacs PACKAGE is \"emacs\" even
if the real package is also mirrored.  If the package providing a feature
is not known PACKAGE is nil and if the feature also is not part of EXCLUDE
a warning is also shown.  The other arguments are only used for these
waring messages."
  (let (packages)
    (dolist (feature required)
      (let ((package (xpkg-lookup-required-1 feature exclude)))
	(case package
	  (:excluded)
	  (nil
	   (xpkg-log "%s (%s): %s required %s not available"
		     name ref type feature))
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

(defun xpkg-lookup-required-1 (feature exclude)
  "Return a string representing the package that provides FEATURE."
  (cond ((member feature xpkg-emacs-features) "emacs")
	((member feature exclude) :excluded)
	((not (string-match "-autoloads?$" (symbol-name feature)))
	 (cdr (assoc feature xpkg-feature-alist)))
	(t
	 (xpkg-lookup-required-1
	  (intern (substring (symbol-name feature) 0
			     (match-beginning 0)))
	  exclude))))

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
