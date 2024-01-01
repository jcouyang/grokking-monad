(setq org-latex-classes
          '(
  ("tufte-book" "\\documentclass[11pt]{tufte-book}"
   ("\\part{%s}" . "\\part*{%s}")
   ("\\chapter{%s}" . "\\chapter*{%s}")
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
