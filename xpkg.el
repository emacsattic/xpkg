;;; xpkg.el --- extract information from Emacs Lisp packages

;; Copyright (C) 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20101001
;; Updated: 20101001
;; Version: 0.1
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
;; Also see package `elx'.

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
  (let ((features (xpkg-features name repo rev nil t))
	(mainfile (xpkg-mainfile name repo rev)))
    (lgit-with-file repo rev mainfile
      (let ((wikipage
	     (or (elx-wikipage mainfile name nil t)
		 (let ((page (or (lgit-get repo "elm.wikipage")
				 (lgit-get repo "xpkg.wikipage"))))
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
	      :required (unless (equal (cdr features) '(nil nil))
			  (if (equal (cddr features) '(nil))
			      (list (cadr features))
			    (cdr features)))
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

(defun xpkg-features (name repo rev
				  &optional associate dependencies batch)
  "Process the features of the package named NAME.

REPO is the path to the git repository containing the package; it may be
bare.  REV has to be an existing revision in that repository.

If optional DEPENDENCIES is non-nil return a list of the form:

  ((PROVIDED-FEATURE...)
   ((PROVIDING-PACKAGE HARD-REQUIRED-FEATURE...)...)
   ((PROVIDING-PACKAGE HARD-REQUIRED-FEATURE...)...))

Otherwise return:

  (PROVIDED-FEATURE...)

If optional ASSOCIATE is non-nil associate the provided features with the
package in the value of variable `xpkg-feature-alist', if appropriate.

Also see the source comments of this function for more information."
  (let (required provided bundled
	;; Sometimes features provided by a packages repository have to be
	;; excluded from the list of features provided by the package.
	;; This usually is the case when a package bundles libraries that
	;; originate from another package.  Note that this is sometimes
	;; done even for bundled packages are not mirrored themselves (yet).
	;;
	;; This does not result in unresolved dependencies (also see below),
	;; on the opposite the package actually providing the bundled
	;; features is not even added to the list of dependencies so we can
	;; be sure the bundled libraries are loaded which might differ from
	;; those of the package they originate from.
	;;
	;; If no other package depends on the same features (or does also
	;; bundle them) this is a good solution.  However when other
	;; packages also require the packages providing these features
	;; the libraries in the original packages conflict with the
	;; bundled libraries - we simply can't do anything about that and
	;; must hope the file that gets loaded based in it's position in
	;; `load-path' works for all packages that depend on it.
	;;
	;; Features are excluded by setting the git variables "elm.exclude"
	;; (can be specified multiple times, matching features are excluded)
	;; and "elm.exclude-path" (files whose path match are excluded) in
	;; the packages repository.
	(exclude-required
	 (mapcar #'intern (xpkg-get-all repo "elm.exclude-required")))
	(exclude-provided
	 (mapcar #'intern (nconc (xpkg-get-all repo "elm.exclude-provided")
				 (xpkg-get-all repo "elm.exclude"))))
	(exclude-path (cadr (lgit repo 1 "config elm.exclude-path"))))
    (dolist (file (elx-elisp-files-git repo rev))
      (dolist (prov (lgit-with-file repo rev file
		      (elx--buffer-provided)))
	(if (or (member prov exclude-provided)
		(and exclude-path (string-match exclude-path file)))
	    (push prov bundled)
	  (when prov
	    (push prov provided))
	  (when dependencies
	    ;; Even if some of the features provided by this file are
	    ;; excluded do not exclude the required features if at least
	    ;; one of the provided features is not excluded.  We do this
	    ;; because the file might be legitimately belong the the
	    ;; package but might never-the-less illegitimately provide
	    ;; a foreign feature to indicate that is a drop-in replacement
	    ;; or whatever.
	    ;;
	    ;; If multiple features are provided and not excluded then the
	    ;; required features are added multiple times to the list of
	    ;; required features at this point but that is not a problem
	    ;; as duplicates are later removed.
	    ;;
	    ;; Since it is rare that a file provides multiple features, we
	    ;; don't care if we extract the dependencies multiple times.
	    (push (lgit-with-file repo rev file (elx--buffer-required))
		  required)))))
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
      ;; The value of `xpkg-feature-alist' is not updated always
      ;; updated when this function is called because is called for all
      ;; versions as well as the tips of all vendor branches and these
      ;; different revisions might differ in what features they provide.
      ;; If the caller of this function could not control whether
      ;; associations are updated or not could seemingly randomly change
      ;; depending on what revision was last processed.
      ;;
      ;; Since Emacs provides no way to specify what version of a package
      ;; another package depends on a particular revision had to be
      ;; choosen whose provided features are recorded to calculate the
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
	provided
      ;; This function usually is called to update/create revision epkgs
      ;; and to updated the value of variable `xpkg-feature-alist' by
      ;; side-effect.  However in some cases we only need to do the latter
      ;; so it is possible to skip the step of determine the dependencies.
      ;; In this case we also return nil.
      (setq required (elx--sanitize-required required
						  provided t))
      (let ((hard (xpkg-lookup-required (nth 0 required) exclude-required))
	    (soft (xpkg-lookup-required (nth 1 required) exclude-required)))
	;; If the package providing a particular feature can not be
	;; determined and the providing library also isn't bundled report
	;; a warning here.
	(dolist (dep (cdr (assoc nil hard)))
	  (unless (memq dep bundled)
	    (xpkg-log "%s (%s): hard required %s not available" name rev dep)))
	(dolist (dep (cdr (assoc nil soft)))
	  (unless (memq dep bundled)
	    (xpkg-log "%s (%s): soft required %s not available" name rev dep)))
	(when (or (member name hard)
		  (member name soft))
	  (error "fatal: %s requires itself" name))
	(list provided hard soft)))))

(defun xpkg-lookup-required (required exclude)
  "Return the packages providing all features in list REQUIRED.
The returned value has the form: ((PACKAGE FEATURE...)...)."
  (let (packages)
    (dolist (feature required)
      (let ((package (xpkg-lookup-required-1 feature exclude)))
	(unless (eq package :excluded)
	  (let ((entry (assoc package packages)))
	    (if entry
		(unless (memq feature (cdr entry))
		  (setcdr entry (sort (cons feature (cdr entry))
				      'string<)))
	      (push (list package feature) packages))))))
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
  "Pop to the buffer named \"*xpkg-log*\"."
  (interactive)
  (let ((buffer (get-buffer "*xpkg-log*")))
    (when buffer
      (pop-to-buffer buffer))))

(provide 'xpkg)
;;; xpkg.el ends here
