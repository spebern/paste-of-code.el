;;; paste-of-code.el --- paste code on https://paste.ofcode.org -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Bernhard Specht

;; Author: Bernhard Specht <bernhard@specht.net>
;; Keywords: lisp
;; Version: 0.0.1
;; Package-Requires: ((request "0.2.0"))

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

;; Paste code on https://paste.ofcode.org for many different languages

;;; Code:

(require 'request)

(defvar paste-of-code--cookie-regexp "session=\\([^;]+\\)"
  "Regular expression to get the Session ID from the response's headers.")

(defvar paste-of-code--cookie-string ""
  "Cookies for https://paste.ofcode.org.")

(defvar paste-of-code--major-mode-to-language
  `(("emacs-lisp-mode"  . "emacs")
    ("python-mode"      . "python")
    ("perl6-mode"       . "perl6")
    ("c-mode"           . "c")
    ("c++-mode"         . "cpp")
    ("js2-mode"         . "js")
    ("js-mode"          . "js")
    ("go-mode"          . "go")
    ("rust-mode"        . "rust")
    ("octave-mode"      . "octave")
    ("java-mode"        . "java")
    ("ada-mode"         . "ada")
    ("ruby-mode"        . "rb")
    ("clojure-mode"     . "clojure")
    ("julia-mode"       . "julia")
    ("shell-mode"       . "bash")
    ("xml-mode"         . "xml")
    ("yaml-mode"        . "yaml")
    ("typescript-mode"  . "ts")
    ("scheme-mode"      . "scheme")
    ("scala"            . "scala")
    ("latex-mode"       . "tex")
    ("switft-mode"      . "switft")
    ("switft3-mode"     . "switft")
    ("tcl-mode"         . "tcl")
    ("racket-mode"      . "racket")
    ("common-lisp-mode" . "common-lisp")
    ("php-mode"         . "php")
    ("objc-mode"        . "objective-c")
    ("json-mode"        . "json")
    ("sql-mode"         . "sql")
    ("html-mode"        . "html")
    ("fsharp-mode"      . "fsharp")
    ("erlang-mode"      . "erlang")
    ("elixir-mode"      . "elixir")
    ("cobol-mode"       . "cobol")
    ("d-mode"           . "d")
    ("css-mode"         . "css")
    ("brainfuck-mode"   . "brainfuck")
    ("csharp-mode"      . "csharp")
    ("j-mode"           . "j")
    ("fortran-mode"     . "fortran")
    ("cmake-mode"       . "cmake")
    ("cuda-mode"        . "cuda")
    ("puppet-mode"      . "puppet")
    ("lua-mode"         . "lua")
    ("matlab-mode"      . "matlab")
    ("perl-mode"        . "perl")
    ("cperl-mode"       . "perl")))

(defun paste-of-code//fetch-cookie ()
  "Fetch cookie from 'https://paste.ofcode.org for further communication."
  (let* ((response (request
		    "https://paste.ofcode.org"
		    :type "GET"
		    :sync t))
       	 (header (request-response-header response "set-cookie"))
       	 (cookie-string (progn
       			  (string-match paste-of-code--cookie-regexp cookie-str)
       			  (match-string 1 cookie-str))))
       (setq paste-of-code--cookie-string cookie-string)))

(defun paste-of-code//paste-code ()
  "Upload code and copy link into 'kill-ring'.  The language will be determined by the major mode in the buffer."
  (let* ((response (request
		    "https://paste.ofcode.org"
		    :type "POST"
		    :sync t
		    :headers `(("referer" . "https://paste.ofcode.org")
			       ("cookie"  . ,(format "ofcode=%s" paste-of-code--cookie-string)))
		    :data `(("code"     . ,(buffer-string))
			    ("language" . ,(cdr (assoc (format "%s" major-mode) paste-of-code--major-mode-to-language)))
			    ("notabot"  . "most_likely"))))
	 (paste-of-code-link (request-response-url response)))
    (kill-new paste-of-code-link)))

(defun paste-of-code-paste-code ()
  "Upload current buffer and copy link into 'kill-ring'.  The language will be determined by the major mode in the current buffer."
  (interactive)
  (paste-of-code//fetch-cookie)
  (paste-of-code//paste-code))

;;; paste-of-code.el ends here
