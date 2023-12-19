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
                       ("src" "#+begin_src ${1:lang}\n$2\n#+begin_src\n" "begin_src" nil nil nil "/home/stefanom/.config/emacs/snippets/org-mode/begin_src" nil nil)))


;;; Do not edit! File generated at Mon Dec 18 22:28:41 2023
