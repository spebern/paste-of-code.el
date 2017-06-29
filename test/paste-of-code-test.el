(require 'ert)

(load-file "./paste-of-code.el")

(ert-deftest paste-of-code-paste-code ()
  (should (string-match "https://paste.ofcode.org/\\(\\w+\\)"
	      (paste-of-code--paste-code "print 'hello world'" "python"))))
