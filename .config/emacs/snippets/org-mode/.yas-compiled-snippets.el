;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("sum" "\\sum${1:_{${2:i=1}\\}}${3:^{${4:N}\\}} $0" "\\sum_{}^{}"
                        (and
                         (texmathp)
                         'auto)
                        nil nil "/home/stefanom/.config/emacs/snippets/org-mode/sum" nil nil)
                       ("im" "`(org-download-clipboard)`\n" "image clipboard"
                        (and
                         (not
                          (texmathp)))
                        nil nil "/home/stefanom/.config/emacs/snippets/org-mode/img" nil nil)
                       ("Bra" "\\\\bracket{$1} | $2 | \\\\bracket{$3} $0" "\\bracket{} | | \\bracket{}"
                        (and
                         (texmathp)
                         'auto)
                        nil nil "/home/stefanom/.config/emacs/snippets/org-mode/fullbracket" nil nil)
                       ("eqs" "\\begin{equation*}\n$1\n\\end{equation*}" "\\begin{equation*}"
                        (and
                         (not
                          (texmathp)))
                        nil nil "/home/stefanom/.config/emacs/snippets/org-mode/env_equationstar" nil nil)
                       ("eq" "\\begin{equation}\n$1\n\\end{equation}" "\\begin{equation}"
                        (and
                         (not
                          (texmathp)))
                        nil nil "/home/stefanom/.config/emacs/snippets/org-mode/env_equation" nil nil)
                       ("bo" "\\begin{bx}\n$1\n\\end{bx}" "box"
                        (and
                         (not
                          (texmathp)))
                        nil nil "/home/stefanom/.config/emacs/snippets/org-mode/env_bx" nil nil)
                       ("ali" "\\begin{align}\n$1\n\\end{align}" "\\begin{align}"
                        (and
                         (not
                          (texmathp)))
                        nil nil "/home/stefanom/.config/emacs/snippets/org-mode/env_align" nil nil)
                       ("bra" "\\\\bracket{$1}$0" "\\bracket{}"
                        (and
                         (texmathp)
                         'auto)
                        nil nil "/home/stefanom/.config/emacs/snippets/org-mode/bracket" nil nil)
                       ("src" "#+begin_src ${1:lang}\n$2\n#+begin_src\n" "begin_src" nil nil nil "/home/stefanom/.config/emacs/snippets/org-mode/begin_src" nil nil)))


;;; Do not edit! File generated at Tue Dec 19 22:25:13 2023
