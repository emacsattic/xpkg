;;; xpkg.el --- extract information from Emacs Lisp packages

;; Copyright (C) 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20101001
;; Updated: 20101117
;; Version: 0.1.1
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
(require 'lgit)
(require 'elx)

(defun xpkg-metadata (name repo rev &optional branch)
  "Return the metadata of the package named NAME.

The metadata is extracted from revision REV in the git repository REPO.

If optional BRANCH is specified it should be the branch containing REV.
It is only used to get the branch homepage from \".git/config\" if it
can not be determined otherwise."
  (let* ((features (xpkg-features name repo rev nil t))
	 (hard-deps (nth 1 features))
	 (soft-deps (nth 2 features))
	 (mainfile (xpkg-mainfile name repo rev)))
    (lgit-with-file repo rev mainfile
      (let ((wikipage
	     (or (elx-wikipage mainfile name nil t)
		 (let ((page (lgit-get repo "xpkg.wikipage")))
		   (when page
		     (concat "http://www.emacswiki.org/" page))))))
	(list :summary (elx-summary nil t)
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
			    (lgit-branch-get repo branch "elm-homepage")
			    (when (equal (or branch rev) "emacswiki")
			      wikipage))
	      :wikipage wikipage
	      :commentary
	      (unless (lgit-branch-get repo branch "xpkg-no-commentary")
		(elx-commentary mainfile)))))))

(defun xpkg-mainfile (name repo rev)
  "Return the mainfile of the package named NAME.

The mainfile is extracted from revision REV in the git repository REPO,
and is returned as a path relative to REPO.  If the mainfile can't be
determined nil is returned.

If the revision contains only one file whose basename matches the regular
expression\"\\\\.el\\\\(\\\\.in\\\\)$\" return it's basename.  Otherwise return
the basename of the file matching NAME or NAME with \"-mode\" added to or
removed from the end, whatever makes sense; case is ignored.  If no match
for NAME is found then the value of the git variable \"elm.mainfile\" is
returned, if it is defined and the respective file actually exists in the
specified revision."
  (let ((files (elx-elisp-files-git repo rev)))
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
	    (let ((mainfile (lgit-get repo "elm.mainfile")))
	      (when (and mainfile (member mainfile files))
		mainfile)))))))

(defmacro xpkg-with-mainfile (name repo rev &rest body)
 "Execute BODY in a buffer containing the mainfile of the package named NAME."
 (declare (indent 3) (debug t))
 (let ((repo-sym (make-symbol "--xpkg-with-mainfile-repo--"))
       (rev-sym (make-symbol "--xpkg-with-mainfile-rev--")))
   `(let ((,repo-sym ,repo)
	  (,rev-sym ,rev))
      (lgit-with-file ,repo-sym ,rev-sym
		      (or (xpkg-mainfile name ,repo-sym ,rev-sym)
			  (error "The mainfile can not be determined"))
	,@body))))

(defsubst xpkg-asort (variable)
  (set variable (sort* (symbol-value variable) 'string< :key 'car)))

(defvar xpkg-keyword-alist nil
  "Alist of known keywords and the associated packages.
Each element is a cons cell whose car is a keyword string and whose cdr is
a list of associated packages.")

(defun xpkg-initialize-keyword-alist (packages)
  "Initialize the value of `xpkg-keyword-alist'.
PACKAGES is a list of the form ((NAME REPOSITORY REVISION)...)."
  (interactive)
  (setq xpkg-keyword-alist nil)
  (mapc-with-progress-reporter
   "Initializing keyword alist..."
   (lambda (elt)
     (apply 'xpkg-keywords (append elt (list t t))))
   packages)
  (xpkg-asort 'xpkg-keyword-alist))

(defun xpkg-keywords (name repo rev &optional associate batch)
  "Process the keywords of the package named NAME.

REPO is the path to the git repository containing the package; it may be
bare.  REV has to be an existing revision in that repository.

The provided keywords are returned.

If optional ASSOCIATE is non-nil associate the package with the the
defined keywords in the value of variable `xpkg-keyword-alist'."
  (let ((keywords (xpkg-with-mainfile name repo rev (elx-keywords))))
    (if (not associate)
	keywords
      (dolist (keyword keywords)
	(let ((keylist (assoc keyword xpkg-keyword-alist)))
	  (if keylist
	      (unless (member name (cdr keylist))
		(setcdr keylist (sort (cons name (cdr keylist)) 'string<)))
	    (push (list keyword name) xpkg-keyword-alist))))
      (if batch
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

(defun xpkg-initialize-feature-alist (packages &optional check)
  "Recreate the value of `xpkg-feature-alist'.
PACKAGES is a list of the form ((NAME REPOSITORY REVISION)...).
If multiple packages provide the same features this is logged.
If optional CHECK is non-nil also report unsatisfied dependencies."
  (interactive)
  (setq xpkg-feature-alist nil)
  (mapc-with-progress-reporter
   "Initializing feature alist..."
   (lambda (elt)
     (apply 'xpkg-features (append elt (list t nil t))))
   packages)
  (when check
    (mapc-with-progress-reporter
     "Checking feature consistency..."
     (lambda (elt)
       (apply 'xpkg-features (append elt (list t t t))))
     packages))
  (xpkg-asort 'xpkg-feature-alist))

(defsubst xpkg-get-all (repo variable)
  (cdr (lgit repo 1 "config --get-all %s" variable)))

(defun xpkg-features (name repo rev &optional associate dependencies batch)
  "Process the features of the package named NAME.

REPO is the path to the git repository containing the package; it may be
bare.  REV has to be an existing revision in that repository.

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
  (let (required
	bundled
	(provided (mapcar 'intern (xpkg-get-all repo "elm.include-provided")))
	;; Sometimes features provided by a packages repository have to be
	;; excluded from the list of features provided by the package.
	;; This usually is the case when a package bundles libraries that
	;; originate from another package.  Note that this is sometimes
	;; done even for bundled packages that are not mirrored themselves
	;; (yet).
	;;
	;; Likewise required features sometimes are not real dependencies;
	;; E.g. when they are only required to test the package.
	;;
	;; These git variables are used to record features and files that
	;; should be ignored.  For more information see below.
	(exclude-required
	 (mapcar #'intern (xpkg-get-all repo "elm.exclude-required")))
	(exclude-provided
	 (mapcar #'intern (xpkg-get-all repo "elm.exclude-provided")))
	(exclude-path     (cadr (lgit repo 1 "config elm.exclude-path")))
	(include-required (xpkg-get-all repo "elm.include-required")))
    (dolist (file (elx-elisp-files-git repo rev))
      ;; Files that match "elm.exclude-path" are ignored completely; that
      ;; is neither the features they provide nor those they require
      ;; appear anywhere in the return value of this function at all.
      ;; Use this e.g. for libraries that contain only tests.
      (unless (and exclude-path (string-match exclude-path file))
	(dolist (prov (lgit-with-file repo rev file (elx--buffer-provided)))
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
	  ;; but just happens to have a common dependency.  However for
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
	      (push (lgit-with-file repo rev file (elx--buffer-required))
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
      ;; as the tips of all vendor branches and these different revisions
      ;; might differ in what features they provide.  If the caller of this
      ;; function could not control whether associations are updated or not
      ;; this could result in seemingly random change depending on what
      ;; revision was last processed.
      ;;
      ;; Since Emacs provides no way to specify what version of a package
      ;; another package depends on a particular revision had to be
      ;; chosen whose provided features are recorded to calculate the
      ;; dependencies of other packages.  The latest tagged revision
      ;; of the "main" vendor or if no tagged revision exists it's tip
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
	    (xpkg-lookup-required name rev 'hard (nth 0 required)
				  exclude-required)
	    (xpkg-lookup-required name rev 'soft (nth 1 required)
				  exclude-required)
	    (sort bundled 'string<)))))

(defun xpkg-lookup-required (name rev type required exclude)
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
		     name rev type feature))
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
