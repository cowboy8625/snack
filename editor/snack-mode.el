;; Copyright (C) 2021 Alexey Kutepov <reximkut@gmail.com>

;; Author: Alexey Kutepov <reximkut@gmail.com>
;; URL: https://github.com/tsoding/porth

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(defconst snack-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; C/C++/Rust style comments
    (modify-syntax-entry ?/ ". 124b")
    (modify-syntax-entry ?* ". 23")
    (modify-syntax-entry ?\n "> b")
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"")
    (syntax-table))
  "Syntax table for `snack-mode'.")

(eval-and-compile
  (defconst snack-keywords
    '("if" "elif" "else" "do" "true" "false" "let" "copy" "swap" "drop" "over" "rot"
    "while" "end" "or" "not" "and" "memory" "const" "word" "in" "use" "return"
    "max" "syscall1" "syscall2" "syscall3" "syscall4" "syscall5" "syscall6")))


(defconst snack-highlights
  `((,(regexp-opt snack-keywords 'symbols) . font-lock-keyword-face)))

;;;###autoload
(define-derived-mode snack-mode prog-mode "snack"
  "Major Mode for editing Snack source code."
  :syntax-table snack-mode-syntax-table
  (setq font-lock-defaults '(snack-highlights))
  (setq-local comment-start "// "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.snack\\'" . snack-mode))

(provide 'snack-mode)

;;; snack-mode.el ends here
