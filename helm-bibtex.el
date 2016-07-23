;;; helm-bibtex.el --- A BibTeX bibliography manager based on Helm

;; Copyright 2014 Titus von der Malsburg <malsburg@posteo.de>

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Version: 1.0.0
;; Package-Version: 20160716.946
;; Package-X-Original-Version: 20160711.855
;; Package-X-Original-Version: 20160606.1514
;; Package-X-Original-Version: 20160325.1526
;; Package-X-Original-Version: 20160323.2235
;; Package-X-Original-Version: 20160321.1728
;; Package-X-Original-Version: 20160314.1613
;; Package-X-Original-Version: 20160310.1300
;; Package-Requires: ((helm "1.5.5") (parsebib "1.0") (s "1.9.0") (dash "2.6.0") (f "0.16.2") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A BibTeX bibliography manager based on Helm.
;;
;; News:
;; - 02/25/2016: Support for pre- and postnotes for pandoc-citeproc
;;   citations.
;; - 11/23/2015: Added support for keeping all notes in one
;;   org-file.  See customization variable `helm-bibtex-notes-path'.
;; - 11/10/2015: Added support for PDFs specified in a BibTeX
;;   field.  See customization variable `helm-bibtex-pdf-field'.
;; - 11/09/2015: Improved insertion of LaTeX cite commands.
;;
;; See NEWS.org for old news.
;;
;; Key features:
;; - Quick access to your bibliography from within Emacs
;; - Tightly integrated workflows
;; - Provides instant search results as you type
;; - Powerful search expressions
;; - Open the PDFs, URLs, or DOIs associated with an entry
;; - Insert LaTeX cite commands, Ebib links, or Pandoc citations,
;;   BibTeX entries, or plain text references at point, attach PDFs to
;;   emails
;; - Attach notes to publications
;; - Quick access to online bibliographic databases such as Pubmed,
;;   arXiv, Google Scholar, Library of Congress, etc.
;;
;; See the github page for details:
;;
;;    https://github.com/tmalsburg/helm-bibtex

;;; Install:

;; Put this file in a directory included in your load path or install
;; helm-bibtex from MELPA (preferred).  Then add the following in your
;; Emacs startup file:
;;
;;     (require 'helm-bibtex)
;;
;; Alternatively, you can use autoload:
;;
;;     (autoload 'helm-bibtex "helm-bibtex" "" t)
;;
;; Requirements are parsebib, helm, s, dash, and f.  The easiest way
;; to install these packages is through MELPA.  Make sure helm is
;; properly configured (see
;; https://github.com/emacs-helm/helm#install-from-emacs-packaging-system).
;;
;; Let helm-bibtex know where it can find your bibliography by setting
;; the variable `helm-bibtex-bibliography'.  See the manual for more details:
;;
;;   https://github.com/tmalsburg/helm-bibtex#minimal-configuration

;;; Usage:

;; You can search entries using the command `helm-bibtex'.  Select an
;; entry and press TAB to access all available actions.  At the end of
;; the list of matches you find some dummy entries that can be used
;; for searching in online databases.  Apart from that, familiarize
;; yourself with Helm.  It's more powerful that you might think.



(require 'helm)
(require 'helm-net)
(require 'helm-easymenu)
(require 'parsebib)
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)

(defgroup helm-bibtex nil
  "Helm plugin for searching entries in a BibTeX bibliography."
  :group 'helm)

(defcustom helm-bibtex-bibliography nil
  "The BibTeX file or list of BibTeX files."
  :group 'helm-bibtex
  :type '(choice file (repeat file)))

(defcustom helm-bibtex-library-path nil
  "A directory or list of directories in which PDFs are
stored.  Helm-bibtex assumes that the names of these PDFs are
composed of the BibTeX-key plus a \".pdf\" suffix."
  :group 'helm-bibtex
  :type '(choice directory (repeat directory)))

(defcustom helm-bibtex-pdf-open-function 'find-file
  "The function used for opening PDF files.  This can be an
arbitrary function that takes one argument: the path to the PDF
file.  The default is `find-file' which opens the PDF in
Emacs (either with docview or, if installed, the much superior
pdf-tools.  When set to `helm-open-file-with-default-tool', the
systems default viewer for PDFs is used."
  :group 'helm-bibtex
  :type 'function)

(defcustom helm-bibtex-pdf-symbol "#"
  "Symbol used to indicate that a PDF file is available for a
publication.  This should be a single character."
  :group 'helm-bibtex
  :type 'string)

(defcustom helm-bibtex-dropbox-path "~/Dropbox"
  "Symbol used to indicate the Dropbox directory."
  :group 'helm-bibtex
  :type 'string)

(defcustom helm-bibtex-format-citation-functions
  '((org-mode      . helm-bibtex-format-citation-ebib)
    (latex-mode    . helm-bibtex-format-citation-cite)
    (markdown-mode . helm-bibtex-format-citation-pandoc-citeproc)
    (default       . helm-bibtex-format-citation-default))
  "The functions used for formatting citations.  The publication
can be cited, for example, as \cite{key} or ebib:key depending on
the major mode of the current buffer.  Note that the functions
should accept a list of keys as input.  With multiple marked
entries one can insert multiple keys at once,
e.g. \cite{key1,key2}. See the functions
`helm-bibtex-format-citation-ebib' and
`helm-bibtex-format-citation-cite' as examples."
  :group 'helm-bibtex
  :type '(alist :key-type symbol :value-type function))

(defcustom helm-bibtex-notes-path nil
  "The place where notes are stored.  This is either a file, in
which case all notes are stored in that file, or a directory, in
which case each publication gets its own notes file in that
directory.  In the latter case, helm-bibtex assumes that the
names of the note files are composed of the BibTeX-key plus a
suffix that is specified in `helm-bibtex-notes-extension'."
  :group 'helm-bibtex
  :type '(choice file directory))

(defcustom helm-bibtex-notes-template-multiple-files
  "#+TITLE: Notes on: ${title} (${year})\n\n"
  "Template used to create a new note when each note is stored in
a separate file.  '${field-name}' can be used to insert the value
of a BibTeX field into the template."
  :group 'helm-bibtex
  :type 'string)

(defcustom helm-bibtex-notes-template-one-file
  "\n* ${title} (${year})\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :END:\n\n"
  "Template used to create a new note when all notes are stored
in one file.  '${field-name}' can be used to insert the value of
a BibTeX field into the template."
  :group 'helm-bibtex
  :type 'string)

(defcustom helm-bibtex-notes-key-pattern
  ":Custom_ID: +%s\\( \\|$\\)"
  "The pattern used to find entries in the notes file.  Only
relevant when all notes are stored in one file.  The key can be
inserted into the pattern using the `format` function."
  :group 'helm-bibtex
  :type 'string)

(defcustom helm-bibtex-notes-extension ".org"
  "The extension of the files containing notes.  This is only
used when `helm-bibtex-notes-path' is a directory (not a file)."
  :group 'helm-bibtex
  :type 'string)

;(defcustom helm-bibtex-notes-symbol "✎"
(defcustom helm-bibtex-notes-symbol "+"
  "Symbol used to indicate that a publication has notes.  This
should be a single character."
  :group 'helm-bibtex
  :type 'string)

(defcustom helm-bibtex-fallback-options
  '(("Google Scholar" . "https://scholar.google.co.uk/scholar?q=%s")
    ("arXiv" . helm-bibtex-arxiv))
  "Alist of online sources that can be used to search for
publications.  The key of each entry is the name of the online
source.  The value is the URL used for retrieving results.  This
URL must contain a %s in the position where the search term
should be inserted.  Alternatively, the value can be a function
that will be called when the entry is selected."
  :group 'helm-bibtex
  :type '(alist :key-type string
                :value-type (choice (string :tag "URL")
                            (function :tag "Function"))))

(defcustom helm-bibtex-browser-function nil
  "The browser that is used to access online resources.  If
nil (default), the value of `browse-url-browser-function' is
used.  If that value is nil, Helm uses the first available
browser in `helm-browse-url-default-browser-alist'"
  :group 'helm-bibtex
  :type '(choice
          (const         :tag "Default" :value nil)
          (function-item :tag "Emacs interface to w3m" :value w3m-browse-url)
          (function-item :tag "Emacs W3" :value  browse-url-w3)
          (function-item :tag "W3 in another Emacs via `gnudoit'"
                         :value  browse-url-w3-gnudoit)
          (function-item :tag "Mozilla" :value  browse-url-mozilla)
          (function-item :tag "Firefox" :value browse-url-firefox)
          (function-item :tag "Chromium" :value browse-url-chromium)
          (function-item :tag "Galeon" :value  browse-url-galeon)
          (function-item :tag "Epiphany" :value  browse-url-epiphany)
          (function-item :tag "Netscape" :value  browse-url-netscape)
          (function-item :tag "eww" :value  eww-browse-url)
          (function-item :tag "Mosaic" :value  browse-url-mosaic)
          (function-item :tag "Mosaic using CCI" :value  browse-url-cci)
          (function-item :tag "Text browser in an xterm window"
                         :value browse-url-text-xterm)
          (function-item :tag "Text browser in an Emacs window"
                         :value browse-url-text-emacs)
          (function-item :tag "KDE" :value browse-url-kde)
          (function-item :tag "Elinks" :value browse-url-elinks)
          (function-item :tag "Specified by `Browse Url Generic Program'"
                         :value browse-url-generic)
          (function-item :tag "Default Windows browser"
                         :value browse-url-default-windows-browser)
          (function-item :tag "Default Mac OS X browser"
                         :value browse-url-default-macosx-browser)
          (function-item :tag "GNOME invoking Mozilla"
                         :value browse-url-gnome-moz)
          (function-item :tag "Default browser"
                         :value browse-url-default-browser)
          (function      :tag "Your own function")
          (alist         :tag "Regexp/function association list"
                         :key-type regexp :value-type function)))

(defcustom helm-bibtex-additional-search-fields nil
  "The fields that are used for searching in addition to author,
editor, title, year, BibTeX key, and entry type."
  :group 'helm-bibtex
  :type 'list)

(defcustom helm-bibtex-no-export-fields nil
  "A list of fields that should be ignored when exporting BibTeX
entries."
  :group 'helm-bibtex
  :type 'list)

(defcustom helm-bibtex-cite-commands '("cite" "Cite" "parencite"
"Parencite" "footcite" "footcitetext" "textcite" "Textcite"
"smartcite" "Smartcite" "cite*" "parencite*" "supercite" "autocite"
"Autocite" "autocite*" "Autocite*" "citeauthor" "Citeauthor"
"citeauthor*" "Citeauthor*" "citetitle" "citetitle*" "citeyear"
"citeyear*" "citedate" "citedate*" "citeurl" "nocite" "fullcite"
"footfullcite" "notecite" "Notecite" "pnotecite" "Pnotecite"
"fnotecite")
  "The list of LaTeX cite commands.  When creating LaTeX
citations, these can be accessed as future entries in the
minibuffer history, i.e. by pressing the arrow down key.  The
default entries are taken from biblatex.  There is currently no
special support for multicite commands and volcite et al.  These
commands can be used but helm-bibtex does not prompt for their
extra arguments."
  :group 'helm-bibtex
  :type '(choice string (repeat string)))

(defcustom helm-bibtex-cite-default-command "cite"
  "The LaTeX cite command that is used if the user doesn't enter
anything when prompted for such a command."
  :group 'helm-bibtex
  :type 'string)

(defcustom helm-bibtex-cite-prompt-for-optional-arguments t
  "If t, helm-bibtex prompts for pre- and postnotes for
LaTeX cite commands.  Choose nil for no prompts."
  :group 'helm-bibtex
  :type 'boolean)

(defcustom helm-bibtex-cite-default-as-initial-input nil
  "This variable controls how the default command defined in
`helm-bibtex-cite-default-command' is used.  If t, it is inserted
into the minibuffer before reading input from the user.  If nil,
it is used as the default if the user doesn't enter anything."
  :group 'helm-bibtex
  :type 'boolean)

(defcustom helm-bibtex-pdf-field nil
  "The name of the BibTeX field in which the path to PDF files is
stored or nil if no such field should be used.  If an entry has
no value for this field, or if the specified file does not exist,
or if this variable is nil, helm-bibtex will look up the PDF in
the directories listed in `helm-bibtex-library-path'."
  :group 'helm-bibtex
  :type 'string)

(defcustom helm-bibtex-full-frame t
  "Non-nil means open `helm-bibtex' using the entire window. When
nil, the window will split below."
  :group 'helm-bibtex
  :type 'boolean)

(easy-menu-add-item nil '("Tools" "Helm" "Tools") ["BibTeX" helm-bibtex t])

(defvar helm-bibtex-bibliography-hash nil
  "The hash of the content of the configured bibliography
files.  If this hash has not changed since the bibliography was
last parsed, a cached version of the parsed bibliography will be
used.")

(defvar helm-bibtex-cached-candidates nil
  "The a list of candidates obtained when the configured
bibliography files were last parsed.")


(defun helm-bibtex-init ()
  "Checks that the files and directories specified by the user
actually exist."
  (mapc (lambda (file)
          (unless (f-file? file)
                  (user-error "BibTeX file %s could not be found." file)))
        (-flatten (list helm-bibtex-bibliography))))


(defun helm-bibtex-candidates ()
  "Reads the BibTeX files and returns a list of conses, one for
each entry.  The first element of these conses is a string
containing authors, editors, title, year, type, and key of the
entry.  This is string is used for matching.  The second element
is the entry (only the fields listed above) as an alist."
  ;; Open configured bibliographies in temporary buffer:
  (with-temp-buffer
    (mapc #'insert-file-contents
          (-flatten (list helm-bibtex-bibliography)))
    ;; Check hash of bibliography and reparse if necessary:
    (let ((bibliography-hash (secure-hash 'sha256 (current-buffer))))
      (unless (and helm-bibtex-cached-candidates
                   (string= helm-bibtex-bibliography-hash bibliography-hash))
        (message "Loading bibliography ...")
        (let* ((entries (helm-bibtex-parse-bibliography))
               (entries (helm-bibtex-resolve-crossrefs entries))
               (entries (helm-bibtex-prepare-entries entries))
               (entries (sort entries 'helm-bibtex-cmp-by-year))
               ;;(entries (nreverse entries)))
              )
          (setq helm-bibtex-cached-candidates
                (--map (cons (helm-bibtex-clean-string
                              (s-join " " (-map #'cdr it))) it)
                       entries)))
        (setq helm-bibtex-bibliography-hash bibliography-hash))
      helm-bibtex-cached-candidates)))

(defun helm-bibtex-cmp-by-year (e1 e2)
  (if (not (string< (helm-bibtex-get-value "year" e1) (helm-bibtex-get-value "year" e2))) t nil))

(defun helm-bibtex-resolve-crossrefs (entries)
  "Expand all entries with fields from cross-references entries."
   (cl-loop
    with entry-hash =
      (cl-loop
       with ht = (make-hash-table :test #'equal :size (length entries))
       for entry in entries
       for key = (helm-bibtex-get-value "=key=" entry)
       ;; Other types than proceedings and books can be
       ;; cross-referenced, but I suppose that isn't really used:
       if (member (downcase (helm-bibtex-get-value "=type=" entry))
                  '("proceedings" "book"))
       do (puthash (downcase key) entry ht)
       finally return ht)
    for entry in entries
    for crossref = (helm-bibtex-get-value "crossref" entry)
    if crossref
      collect (append entry (gethash (downcase crossref) entry-hash))
    else
      collect entry))

(defun helm-bibtex-parse-bibliography ()
  "Parse the BibTeX entries listed in the current buffer and
return a list of entry keys in the order in which the entries
appeared in the BibTeX files."
  (goto-char (point-min))
  (cl-loop
   for entry-type = (parsebib-find-next-item)
   while entry-type
   unless (member-ignore-case entry-type '("preamble" "string" "comment"))
   collect (-map (lambda (it) (cons (downcase (car it)) (cdr it)))
                 (parsebib-read-entry entry-type))))

(defun helm-bibtex-get-entry (entry-key)
  "Given a BibTeX key this function scans all bibliographies
listed in `helm-bibtex-bibliography' and returns an alist of the
record with that key.  Fields from crossreferenced entries are
appended to the requested entry."
  (let* ((entry (helm-bibtex-get-entry1 entry-key))
         (crossref (helm-bibtex-get-value "crossref" entry))
         (crossref (when crossref (helm-bibtex-get-entry1 crossref))))
    (cl-remove-duplicates (append entry crossref)
                          :test (lambda (x y) (string= (s-downcase x) (s-downcase y)))
                          :key 'car :from-end t)))

(defun helm-bibtex-get-entry1 (entry-key &optional do-not-find-pdf)
  (with-temp-buffer
    (mapc #'insert-file-contents
          (-flatten (list helm-bibtex-bibliography)))
    (goto-char (point-min))
    (re-search-forward (concat "^@\\(" parsebib--bibtex-identifier
                               "\\)[[:space:]]*[\(\{][[:space:]]*"
                               (regexp-quote entry-key) "[[:space:]]*,"))
    (let ((entry-type (match-string 1)))
      (reverse (helm-bibtex-prepare-entry (parsebib-read-entry entry-type) nil do-not-find-pdf)))))

(defun helm-bibtex-prepare-entries (entries)
  "Do some preprocessing of the entries."
  (cl-loop
   with fields = (append '("title" "year" "crossref")
                         (-map (lambda (it) (if (symbolp it) (symbol-name it) it))
                               helm-bibtex-additional-search-fields))
   for entry in entries
   collect (helm-bibtex-prepare-entry entry
            (cons (if (assoc-string "author" entry 'case-fold) "author" "editor") fields))))

(defun helm-bibtex-find-pdf-in-field (key-or-entry)
  "Returns the path of the PDF specified in the field
`helm-bibtex-pdf-field' if that file exists.  Returns nil if no
file is specified, or if the specified file does not exist, or if
`helm-bibtex-pdf-field' is nil."
  (when helm-bibtex-pdf-field
    (let* ((entry (if (stringp key-or-entry)
                      (helm-bibtex-get-entry1 key-or-entry t)
                    key-or-entry))
           (value (helm-bibtex-get-value helm-bibtex-pdf-field entry)))
      (cond
       ((not value) nil)         ; Field not defined.
       ((f-file? value) value)   ; A bare path was found.
       (t
        ; Assuming Zotero/Mendeley/JabRef format:
        (cl-loop  ; Looping over the files:
         for record in (s-split ";" value)
         for record = (s-split ":" record)
         for file-name = (nth 0 record)
         for path = (or (nth 1 record) "")
         ; f-full prepends missing slashes, so we don't need a special
         ; case for Mendeley which omits the beginning slash.
         if (f-file? (f-full path)) collect (f-full path)
         else if (f-file? (f-full (f-join path file-name)))
         collect (f-full (f-join path file-name))
         else if (f-file? (f-full (f-join helm-bibtex-library-path path)))
         collect (f-full (f-join helm-bibtex-library-path path))
         ))))))

(defun helm-bibtex-find-pdf-in-library (key-or-entry)
  "Searches the directories in `helm-bibtex-library-path' for a
PDF whose names is composed of the BibTeX key plus \".pdf\".  The
path of the first matching PDF is returned."
  (let* ((key (if (stringp key-or-entry) key-or-entry
                (helm-bibtex-get-value "=key=" key-or-entry)))
         (path (-first 'f-file?
                       (--map (f-join it (s-concat key ".pdf"))
                              (-flatten (list helm-bibtex-library-path))))))
    (when path (list path))))

(defun helm-bibtex-find-pdf (key-or-entry)
  "Returns the path of the PDF associated with the specified
entry.  This is either the path specified in the field
`helm-bibtex-pdf-field' or, if that does not exist, the first PDF
in any of the directories in `helm-bibtex-library-path' whose
name is \"<bibtex-key>.pdf\".  Returns nil if no PDF is found."
  (or (helm-bibtex-find-pdf-in-field key-or-entry)
      (helm-bibtex-find-pdf-in-library key-or-entry)))

(defun helm-bibtex-prepare-entry (entry &optional fields do-not-find-pdf)
  "Prepare ENTRY for display.
ENTRY is an alist representing an entry as returned by
parsebib-read-entry. All the fields not in FIELDS are removed
from ENTRY, with the exception of the \"=type=\" and \"=key=\"
fields. If FIELDS is empty, all fields are kept. Also add a
=has-pdf= and/or =has-note= field, if they exist for ENTRY.  If
DO-NOT-FIND-PDF is non-nil, this function does not attempt to
find a PDF file."
  (when entry ; entry may be nil, in which case just return nil
    (let* ((fields
            (when fields
              (append fields
                      (list "=venue=" "=comment=" "=type=" "=key=" "=has-pdf=" "=has-note="))))
           ; Check for PDF:
           (entry (if (and (not do-not-find-pdf) (helm-bibtex-find-pdf entry))
                      (cons (cons "=has-pdf=" helm-bibtex-pdf-symbol) entry)
                    entry))
           ;; entry key
           (entry-key (cdr (assoc "=key=" entry)))
           ;; venue
           (entry (let* ((booktitle (helm-bibtex-get-value "booktitle" entry ""))
                         (journal (helm-bibtex-get-value "journal" entry "")))
                    ;; if `booktitle' or `journal' field is not empty
                    (if (not (and (string= "" booktitle) (string= "" journal)))
                        (cons (cons "=venue=" (concat booktitle journal)) entry)
                      entry)))
           ;; comment
           (entry (let* ((comment (helm-bibtex-get-value "comment" entry "")))
                    ;; if `comment' field is not empty
                    (if (not (string= "" comment))
                      (cons (cons "=comment=" (substring comment 0 1)) entry)
                      entry)))
           ; Check for notes:
           (entry (if (or
                       ;; One note file per entry:
                       (and helm-bibtex-notes-path
                            (f-directory? helm-bibtex-notes-path)
                            (f-file? (f-join helm-bibtex-notes-path
                                             (s-concat entry-key
                                                       helm-bibtex-notes-extension))))
                       ;; All notes in one file:
                       (and helm-bibtex-notes-path
                            (f-file? helm-bibtex-notes-path)
                            (with-current-buffer (find-file-noselect helm-bibtex-notes-path)
                              (goto-char (point-min))
                              (re-search-forward (format helm-bibtex-notes-key-pattern entry-key) nil t))))
                      (cons (cons "=has-note=" helm-bibtex-notes-symbol) entry)
                    entry))
           ; Remove unwanted fields:
           (entry (if fields
                       (--filter (member-ignore-case (car it) fields) entry)
                     entry)))
      ;; Normalize case of entry type:
      (setcdr (assoc "=type=" entry) (downcase (cdr (assoc "=type=" entry))))
      ;; Remove duplicated fields:
      (cl-remove-duplicates entry
                            :test (lambda (x y) (string= (s-downcase x) (s-downcase y)))
                            :key 'car :from-end t))))


;; The function `window-width' does not necessarily report the correct
;; number of characters that fit on a line.  This is a
;; work-around.  See also this bug report:
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=19395
(defun helm-bibtex-window-width ()
  (if (and (not (featurep 'xemacs))
           (display-graphic-p)
           overflow-newline-into-fringe
           (/= (frame-parameter nil 'left-fringe) 0)
           (/= (frame-parameter nil 'right-fringe) 0))
      (window-body-width)
    (1- (window-body-width))))

(defun helm-bibtex-candidates-formatter (candidates source)
  "Formats BibTeX entries for display in results list."
  (cl-loop
   with width = (with-helm-window (helm-bibtex-window-width))
   for entry in candidates
   for entry = (cdr entry)
   for entry-key = (helm-bibtex-get-value "=key=" entry)
   if (assoc-string "author" entry 'case-fold)
     for fields = '("author" "title" "year" "=has-pdf=" "=has-note=" "=comment=" "=venue=")
   else
     for fields = '("editor" "title" "year" "=has-pdf=" "=has-note=" "=comment=" "=venue=")
   for fields = (-map (lambda (it)
                        (helm-bibtex-clean-string
                          (helm-bibtex-get-value it entry " ")))
                      fields)
   for fields = (-update-at 0 'helm-bibtex-shorten-authors fields)
   collect
   (cons (s-format "$0  $1 $2 $3$4$5 $6" 'elt
                   (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s))
                              fields (list 14 (- width 40) 4 1 1 1 14)))
         entry-key)))


(defun helm-bibtex-clean-string (s)
  "Removes quoting and superfluous white space from BibTeX field
values."
  (if s (replace-regexp-in-string "[\n\t ]+" " "
         (replace-regexp-in-string "[\"{}]+" "" s))
    nil))

(defun helm-bibtex-shorten-authors (authors)
  "Returns a comma-separated list of the surnames in authors."
  (if authors
      (cl-loop for a in (s-split " and " authors)
               for p = (s-split "," a t)
               for sep = "" then ", "
               concat sep
               if (eq 1 (length p))
               concat (-last-item (s-split " +" (car p) t))
               else
               concat (car p))
    nil))

(defun helm-bibtex-open-pdf (_)
  "Open the PDFs associated with the marked entries using the
function specified in `helm-bibtex-pdf-open-function'.  All paths
in `helm-bibtex-library-path' are searched.  If there are several
matching PDFs for an entry, the first is opened."
  (--if-let
      (-flatten
       (-map 'helm-bibtex-find-pdf (helm-marked-candidates :with-wildcard t)))
      (-each it helm-bibtex-pdf-open-function)
    (message "No PDF(s) found.")))

(defun helm-bibtex-open-pdf-zathura(_)
  "Open the PDFs associated with the marked entries in Zathura.  All paths
in `helm-bibtex-library-path' are searched.  If there are several
matching PDFs for an entry, the first is opened."
  (--if-let
      (-flatten
       (-map 'helm-bibtex-find-pdf (helm-marked-candidates :with-wildcard t)))
      (-each it (lambda(fpath)
				  (call-process "zathura" nil 0 nil fpath)))
    (message "No PDF(s) found.")))



(defun helm-bibtex-open-pdf-okular (_)
  "Open the PDFs associated with the marked entries in Okular.  All paths
in `helm-bibtex-library-path' are searched.  If there are several
matching PDFs for an entry, the first is opened."
  (--if-let
      (-flatten
       (-map 'helm-bibtex-find-pdf (helm-marked-candidates :with-wildcard t)))
      (-each it (lambda(fpath)
				  (call-process "okular" nil 0 nil fpath)))
    (message "No PDF(s) found.")))


(defun helm-bibtex-open-url-or-doi (_)
  "Open the associated URL or DOI in a browser."
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (dolist (key keys)
      (let* ((entry (helm-bibtex-get-entry key))
             (url (helm-bibtex-get-value "url" entry))
             (doi (helm-bibtex-get-value "doi" entry))
             (browse-url-browser-function
              (or helm-bibtex-browser-function
                  browse-url-browser-function)))
        (if url (helm-browse-url url)
          (if doi (helm-browse-url
                   (s-concat "http://dx.doi.org/" doi)))
          (message "No URL or DOI found for this entry: %s"
                   key))))))

(defun helm-bibtex-format-citation-default (keys)
  "Default formatter for keys, separates multiple keys with commas."
  (s-join ", " keys))

(defvar helm-bibtex-cite-command-history nil
  "History list for LaTeX citation commands.")

(defun helm-bibtex-format-citation-cite (keys)
  "Formatter for LaTeX citation commands.  Prompts for the command and
for arguments if the commands can take any."
  (let* ((initial (when helm-bibtex-cite-default-as-initial-input helm-bibtex-cite-default-command))
         (default (unless helm-bibtex-cite-default-as-initial-input helm-bibtex-cite-default-command))
         (default-info (if default (format " (default \"%s\")" default) ""))
         (cite-command (completing-read
                        (format "Cite command%s: " default-info)
                        helm-bibtex-cite-commands nil nil initial
                        'helm-bibtex-cite-command-history default nil)))
    (if (member cite-command '("nocite" "supercite"))  ; These don't want arguments.
        (format "\\%s{%s}" cite-command (s-join ", " keys))
      (let ((prenote  (if helm-bibtex-cite-prompt-for-optional-arguments (read-from-minibuffer "Prenote: ") ""))
            (postnote (if helm-bibtex-cite-prompt-for-optional-arguments (read-from-minibuffer "Postnote: ") "")))
        (if (and (string= "" prenote) (string= "" postnote))
            (format "\\%s{%s}" cite-command (s-join ", " keys))
          (format "\\%s[%s][%s]{%s}" cite-command prenote postnote (s-join ", " keys)))))))

(defun helm-bibtex-format-citation-pandoc-citeproc (keys)
  "Formatter for pandoc-citeproc citations."
  (let* ((prenote  (if helm-bibtex-cite-prompt-for-optional-arguments (read-from-minibuffer "Prenote: ") ""))
         (postnote (if helm-bibtex-cite-prompt-for-optional-arguments (read-from-minibuffer "Postnote: ") ""))
         (prenote  (if (string= "" prenote)  "" (concat prenote  " ")))
         (postnote (if (string= "" postnote) "" (concat ", " postnote))))
    (format "[%s%s%s]" prenote (s-join "; " (--map (concat "@" it) keys)) postnote)))

(defun helm-bibtex-format-citation-ebib (keys)
  "Formatter for ebib references."
  (s-join ", "
          (--map (format "ebib:%s" it) keys)))

(defun helm-bibtex-format-citation-org-link-to-PDF (keys)
  "Formatter for org-links to PDF.  Uses first matching PDF if
several are available.  Entries for which no PDF is available are
omitted."
  (s-join ", " (cl-loop
                for key in keys
                for pdfs = (helm-bibtex-find-pdf key)
                append (--map (format "[[%s][%s]]" it key) pdfs))))

(defun helm-bibtex-insert-citation (_)
  "Insert citation at point.  The format depends on
`helm-bibtex-format-citation-functions'."
  (let ((keys (helm-marked-candidates :with-wildcard t))
        (format-function
         (cdr (or (assoc major-mode helm-bibtex-format-citation-functions)
                  (assoc 'default   helm-bibtex-format-citation-functions)))))
    (with-helm-current-buffer
      (insert
       (funcall format-function keys)))))

(defun helm-bibtex-insert-reference (_)
  "Insert a reference for each selected entry."
  (let* ((keys (helm-marked-candidates :with-wildcard t))
         (refs (--map
                (s-word-wrap fill-column
                             (concat "\n- " (helm-bibtex-apa-format-reference it)))
                keys)))
    (with-helm-current-buffer
      (insert "\n" (s-join "\n" refs) "\n"))))

(defun helm-bibtex-apa-format-reference (key)
  "Returns a plain text reference in APA format for the
publication specified by KEY."
  (let*
   ((entry (helm-bibtex-get-entry key))
    (ref (pcase (downcase (helm-bibtex-get-value "=type=" entry))
           ("article"
            (s-format
             "${author} (${year}). ${title}. ${journal}, ${volume}(${number}), ${pages}.${doi}"
             'helm-bibtex-apa-get-value entry))
           ("inproceedings"
            (s-format
             "${author} (${year}). ${title}. In ${editor}, ${booktitle} (pp. ${pages}). ${address}: ${publisher}."
             'helm-bibtex-apa-get-value entry))
           ("book"
            (s-format
             "${author} (${year}). ${title}. ${address}: ${publisher}."
             'helm-bibtex-apa-get-value entry))
           ("phdthesis"
            (s-format
             "${author} (${year}). ${title} (Doctoral dissertation). ${school}, ${address}."
             'helm-bibtex-apa-get-value entry))
           ("inbook"
            (s-format
             "${author} (${year}). ${title}. In ${editor} (Eds.), ${booktitle} (pp. ${pages}). ${address}: ${publisher}."
             'helm-bibtex-apa-get-value entry))
           ("incollection"
            (s-format
             "${author} (${year}). ${title}. In ${editor} (Eds.), ${booktitle} (pp. ${pages}). ${address}: ${publisher}."
             'helm-bibtex-apa-get-value entry))
           ("proceedings"
            (s-format
             "${editor} (Eds.). (${year}). ${booktitle}. ${address}: ${publisher}."
             'helm-bibtex-apa-get-value entry))
           ("unpublished"
            (s-format
             "${author} (${year}). ${title}. Unpublished manuscript."
             'helm-bibtex-apa-get-value entry))
           (_
            (s-format
             "${author} (${year}). ${title}."
             'helm-bibtex-apa-get-value entry)))))
    (replace-regexp-in-string "\\([.?!]\\)\\." "\\1" ref))) ; Avoid sequences of punctuation marks.

(defun helm-bibtex-apa-get-value (field entry &optional default)
  "Return FIELD or ENTRY formatted following the APA
guidelines.  Return DEFAULT if FIELD is not present in ENTRY."
  (let ((value (helm-bibtex-get-value field entry))
        (entry-type (helm-bibtex-get-value "=type=" entry)))
    (if value
       (pcase field
         ;; https://owl.english.purdue.edu/owl/resource/560/06/
         ("author" (helm-bibtex-apa-format-authors value))
         ("editor"
          (if (string= entry-type "proceedings")
              (helm-bibtex-apa-format-editors value)
            (helm-bibtex-apa-format-editors value)))
         ;; When referring to books, chapters, articles, or Web pages,
         ;; capitalize only the first letter of the first word of a
         ;; title and subtitle, the first word after a colon or a dash
         ;; in the title, and proper nouns. Do not capitalize the first
         ;; letter of the second word in a hyphenated compound word.
         ("title" (replace-regexp-in-string ; remove braces
                   "[{}]"
                   ""
                    (replace-regexp-in-string ; upcase initial letter
                    "^[[:alpha:]]"
                    'upcase
                    (replace-regexp-in-string ; preserve stuff in braces from being downcased
                     "\\(^[^{]*{\\)\\|\\(}[^{]*{\\)\\|\\(}.*$\\)\\|\\(^[^{}]*$\\)"
                     (lambda (x) (downcase (s-replace "\\" "\\\\" x)))
                     value))))
         ("booktitle" value)
         ;; Maintain the punctuation and capitalization that is used by
         ;; the journal in its title.
         ("pages" (s-join "–" (s-split "[^0-9]+" value t)))
         ("doi" (s-concat " http://dx.doi.org/" value))
         (_ value))
      "")))

(defun helm-bibtex-apa-format-authors (value)
  (cl-loop for a in (s-split " and " value t)
           if (s-index-of "{" a)
             collect
             (replace-regexp-in-string "[{}]" "" a)
             into authors
           else if (s-index-of "," a)
             collect
             (let ((p (s-split " *, *" a t)))
               (concat
                (car p) ", "
                (s-join " " (-map (lambda (it) (concat (s-left 1 it) "."))
                                  (s-split " " (cadr p))))))
             into authors
           else
             collect
             (let ((p (s-split " " a t)))
               (concat
                (-last-item p) ", "
                (s-join " " (-map (lambda (it) (concat (s-left 1 it) "."))
                                  (-butlast p)))))
             into authors
           finally return
             (let ((l (length authors)))
               (cond
                 ((= l 1) (car authors))
                 ((< l 8) (concat (s-join ", " (-butlast authors))
                                  ", & " (-last-item authors)))
                 (t (concat (s-join ", " authors) ", ..."))))))

(defun helm-bibtex-apa-format-editors (value)
  (cl-loop for a in (s-split " and " value t)
           if (s-index-of "," a)
             collect
             (let ((p (s-split " *, *" a t)))
               (concat
                (s-join " " (-map (lambda (it) (concat (s-left 1 it) "."))
                                  (s-split " " (cadr p))))
                " " (car p)))
             into authors
           else
             collect
             (let ((p (s-split " " a t)))
               (concat
                (s-join " " (-map (lambda (it) (concat (s-left 1 it) "."))
                                  (-butlast p)))
                " " (-last-item p)))
             into authors
           finally return
             (let ((l (length authors)))
               (cond
                 ((= l 1) (car authors))
                 ((< l 8) (concat (s-join ", " (-butlast authors))
                                  ", & " (-last-item authors)))
                 (t (concat (s-join ", " authors) ", ..."))))))

(defun helm-bibtex-get-value (field entry &optional default)
  "Return the requested value or `default' if the value is not
defined.  Surrounding curly braces are stripped."
  (let ((value (cdr (assoc-string field entry 'case-fold))))
    (if value
        (s-collapse-whitespace
         (replace-regexp-in-string
          "\\(^[[:space:]]*[\"{][[:space:]]*\\)\\|\\([[:space:]]*[\"}][[:space:]]*$\\)"
          ""
          value))
      default)))

(defun helm-bibtex-insert-key (_)
  "Insert BibTeX key at point."
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (with-helm-current-buffer
      (insert
        (funcall 'helm-bibtex-format-citation-default keys)))))

(defun helm-bibtex-insert-bibtex (_)
  "Insert BibTeX entry at point."
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (with-helm-current-buffer
      (insert (s-join "\n" (--map (helm-bibtex-make-bibtex it) keys))))))

(defun helm-bibtex-copy-bibtex (_)
  "copy BibTeX entry."
  (let ((keys (helm-marked-candidates :with-wildcard t)))
      (kill-new (s-join "\n" (--map (helm-bibtex-make-bibtex it) keys)))))

(defun helm-bibtex-make-bibtex (key)
  (let* ((entry (helm-bibtex-get-entry key))
         (entry-type (helm-bibtex-get-value "=type=" entry)))
    (format "@%s{%s,\n%s}\n"
            entry-type key
            (cl-loop
             for field in entry
             for name = (car field)
             for value = (cdr field)
             unless (member
                     name
                     (append
                      (-map
                       (lambda (it)
                         (if (symbolp it)
                             (symbol-name it)
                           it))
                       helm-bibtex-no-export-fields)
                      '("=venue=" "=comment=" "=type=" "=key=" "=has-pdf="
                        "=has-note=" "crossref" "keywords" "file" "comment"
                        "owner" "timestamp" "__markedentry" "groups")))
             concat
             (format "  %s = %s,\n" name value)))))

(defun helm-bibtex-add-PDF-attachment (_)
  "Attach the PDFs of the selected entries where available."
  (--if-let
      (-flatten
       (-map 'helm-bibtex-find-pdf (helm-marked-candidates :with-wildcard t)))
      (-each it 'mml-attach-file)
    (message "No PDF(s) found.")))


(defun helm-bibtex-send-pdf-dropbox (_)
  "Attach the PDFs of the selected entries where available."
  (--if-let
      (-flatten
       (-map 'helm-bibtex-find-pdf (helm-marked-candidates :with-wildcard t)))
      (-each it (lambda(fpath) (f-copy fpath helm-bibtex-dropbox-path)))
    (message "No PDF(s) found.")))

(define-minor-mode helm-bibtex-notes-mode
  "Minor mode for managing helm-bibtex notes."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'helm-bibtex-exit-notes-buffer)
	    (define-key map (kbd "C-c C-w") 'org-refile)
	    (define-key map (kbd "C-c C-h") 'helm-bibtex-resume-session)
            map)
  (org-set-local
   'header-line-format
   (substitute-command-keys
    " Finish \\[helm-bibtex-exit-notes-buffer], refile \\[org-refile], back \\[helm-bibtex-resume-session]")))

;; Define global minor mode. This is needed to the toggle minor mode.
(define-globalized-minor-mode helm-bibtex-notes-global-mode helm-bibtex-notes-mode helm-bibtex-notes-mode)

(defun helm-bibtex-exit-notes-buffer ()
  "Exit notes buffer and delete its window.
This will also disable `helm-bibtex-notes-mode' and remove the header
line."
  (interactive)
  (widen)
  (helm-bibtex-notes-global-mode -1)
  (org-set-local
   'header-line-format nil)
  (save-buffer)
  (let ((window (get-buffer-window (file-name-nondirectory helm-bibtex-notes-path))))
    (if (and window (not (one-window-p window)))
	(delete-window window)
      (switch-to-buffer (other-buffer)))))

(defun helm-bibtex-resume-session ()
  "Exit notes buffer (if active) and resume helm-bibtex session."
  (interactive)
  (let ((notes-buffer (get-file-buffer (file-name-nondirectory helm-bibtex-notes-path))))
    (when (equal notes-buffer (current-buffer))
      (helm-bibtex-exit-notes-buffer))
    (helm-resume "*helm bibtex*")))

(defun helm-bibtex-edit-notes (key)
  "Open the notes associated with the entry using `find-file'."
  (if (f-directory? helm-bibtex-notes-path)
      ;; One notes file per publication:
      (let ((path (f-join helm-bibtex-notes-path
                          (s-concat key helm-bibtex-notes-extension))))
        (find-file path)
        (unless (f-exists? path)
          (insert (s-format helm-bibtex-notes-template-multiple-files
                            'helm-bibtex-apa-get-value
                            (helm-bibtex-get-entry key)))))
    ;; One file for all notes:
    (unless (and buffer-file-name
                 (f-same? helm-bibtex-notes-path buffer-file-name))
      (find-file-other-window helm-bibtex-notes-path))
    (widen)
    (show-all)
    (goto-char (point-min))
    (if (re-search-forward (format helm-bibtex-notes-key-pattern key) nil t)
        ;; Existing entry found:
        (when (eq major-mode 'org-mode)
          (org-narrow-to-subtree)
          (re-search-backward "^\*+ " nil t)
          (org-cycle-hide-drawers nil)
          (helm-bibtex-notes-mode 1))
      ;; Create a new entry:
      (let ((entry (helm-bibtex-get-entry key)))
        (goto-char (point-max))
        (insert (s-format helm-bibtex-notes-template-one-file
                          'helm-bibtex-apa-get-value
                          entry)))
      (when (eq major-mode 'org-mode)
        (org-narrow-to-subtree)
        (re-search-backward "^\*+ " nil t)
        (org-cycle-hide-drawers nil)
        (goto-char (point-max))
        (helm-bibtex-notes-mode 1)))))

(defun helm-bibtex-buffer-visiting (file)
  (or (get-file-buffer file)
      (find-buffer-visiting file)))

(defun helm-bibtex-show-entry (key)
  "Show the entry in the BibTeX file."
  (catch 'break
    (dolist (bibtex-file (-flatten (list helm-bibtex-bibliography)))
      (let ((buf (helm-bibtex-buffer-visiting bibtex-file)))
        (find-file bibtex-file)
        (goto-char (point-min))
        (if (re-search-forward
             (concat "^@\\(" parsebib--bibtex-identifier
                     "\\)[[:space:]]*[\(\{][[:space:]]*"
                     (regexp-quote key) "[[:space:]]*,") nil t)
            (throw 'break t)
          (unless buf
            (kill-buffer)))))))

(defun helm-bibtex-fallback-action (url-or-function)
  (let ((browse-url-browser-function
          (or helm-bibtex-browser-function
              browse-url-browser-function)))
    (cond
      ((stringp url-or-function)
        (helm-browse-url (format url-or-function (url-hexify-string helm-pattern))))
      ((functionp url-or-function)
        (funcall url-or-function))
      (t (error "Don't know how to interpret this: %s" url-or-function)))))

(defun helm-bibtex-arxiv ()
  "Search for the current `helm-pattern' in arXiv."
  (let* ((browse-url-browser-function
          (or helm-bibtex-browser-function
              browse-url-browser-function))
         (terms (s-split "\s+" helm-pattern))
         (terms (-map 'url-hexify-string terms))
         (terms (if (> (length terms) 1) (cons "AND" terms) terms)))
    (helm-browse-url (format "http://arxiv.org/find/all/1/all:+%s/0/1/0/all/0/1"
                             (s-join "+" terms)))))

(defun helm-bibtex-fallback-candidates ()
  "Compile list of fallback options.  These consist of the online
resources defined in `helm-bibtex-fallback-options' plus one
entry for each BibTeX file that will open that file for editing."
  (let ((bib-files (-flatten (list helm-bibtex-bibliography))))
    (-concat
      (--map (cons (s-concat "Create new entry in " (f-filename it))
                   `(lambda ()
                      (find-file ,it)
                      (goto-char (point-max))
                      (newline)))
             bib-files)
      helm-bibtex-fallback-options)))


(defvar helm-source-bibtex
  (helm-build-sync-source "BibTeX entries"
    :init 'helm-bibtex-init
    :candidates 'helm-bibtex-candidates
    :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
    :action (helm-make-actions
		"Open PDF in Emacs"   'helm-bibtex-open-pdf
        "Open PDF in Zathura" 'helm-bibtex-open-pdf-zathura
        "Open PDF in Okular"  'helm-bibtex-open-pdf-okular
        "Insert citation"     'helm-bibtex-insert-citation
        "Insert reference"    'helm-bibtex-insert-reference
        "Insert BibTeX key"   'helm-bibtex-insert-key
        "Insert BibTeX entry" 'helm-bibtex-insert-bibtex
        "Copy Bibtex entry"   'helm-bibtex-copy-bibtex
        "Send PDF to Dropbox" 'helm-bibtex-send-pdf-dropbox
        "Edit notes"          'helm-bibtex-edit-notes
        "Show entry"          'helm-bibtex-show-entry))
"Source for searching in BibTeX files.")

(defvar helm-source-fallback-options
  '((name            . "Fallback options")
    (match             (lambda (_candidate) t))
    (candidates      . helm-bibtex-fallback-candidates)
    (no-matchplugin)
    (nohighlight)
    (action          . helm-bibtex-fallback-action))
  "Source for online look-up.")

;;;###autoload
(defun helm-bibtex (&optional arg)
  "Search BibTeX entries.
With a prefix ARG the cache is invalidated and the bibliography
reread."
  (interactive "P")
  (when arg
    (setq helm-bibtex-bibliography-hash ""))
  (helm :sources '(helm-source-bibtex helm-source-fallback-options)
        :full-frame helm-bibtex-full-frame
        :buffer "*helm bibtex*"
        :candidate-number-limit 200))

(provide 'helm-bibtex)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-bibtex.el ends here
