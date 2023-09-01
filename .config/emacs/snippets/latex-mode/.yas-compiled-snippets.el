;;; Compiled snippets and support files for `latex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'latex-mode
                     '(("eps" "\\begin{figure}[h!]\n\\centering\n\\includegraphics[width=${1:width}cm,keepaspectratio]{${2:image path}.eps}\n\\caption{${3:caption}}\n${4:\\label{fig:${5:label}}\n\\end{figure}\n" ".esp figure" nil nil nil "/home/stefanom/.config/emacs/snippets/latex-mode/eps-figure" nil nil)
                       ("multicols" "\\begin{multicols}{${1:columns number}}\n$2\n\\end{multicols}\n" "Multicols Environment" nil nil nil "/home/stefanom/.config/emacs/snippets/latex-mode/env_multicols" nil nil)
                       ("frame" "\\begin{frame}{${1:title}}\n$2\n\\end{frame}" "Frame Environment" nil nil nil "/home/stefanom/.config/emacs/snippets/latex-mode/env_frame" nil nil)
                       ("figure" "\\begin{figure}[h!]\n    \\centering\n    \\includegraphics[width=${1:width}cm,keepaspectratio]{${2:image path}}\n    \\caption{${3:caption}}\n    ${4:\\label{fig:${5:label}}\n\\end{figure}\n" "Figure Environment" nil nil nil "/home/stefanom/.config/emacs/snippets/latex-mode/env_figure" nil nil)
                       ("center" "\\begin{center}\n$1\n\\end{center}" "Center Environment" nil nil nil "/home/stefanom/.config/emacs/snippets/latex-mode/env_center" nil nil)))


;;; Do not edit! File generated at Fri Sep  1 02:47:18 2023
