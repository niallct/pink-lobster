\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{cricalm}[2023/01/01 Cricket Almanack]

%\newcommand{\headlinecolor}{\normalcolor}
\LoadClass[twoside]{report}
%\RequirePackage{xcolor}
%\definecolor{slcolor}{HTML}{882B21}

\renewcommand{\footnotesize}{\fontsize{4}{5}\selectfont}
\renewcommand{\normalsize}{\fontsize{6}{7}\selectfont}
\renewcommand{\large}{\fontsize{8}{10}\selectfont}
\renewcommand{\Large}{\fontsize{10}{12}\selectfont}

\setlength{\textwidth}{7.4cm}
\setlength{\textheight}{14cm}
\setcounter{secnumdepth}{0}

\usepackage[paperheight=15.8cm,paperwidth=10.2cm,textwidth=7.4cm]{geometry}
%\usepackage[paperheight=16cm,paperwidth=12cm,textwidth=10cm]{geometry}
%

\usepackage{titlesec}
\titleformat{\chapter}{\sffamily\centering\bfseries\Large}{}{0em}{\MakeUppercase}
\titleformat{\section}{\sffamily\centering\bfseries\large}{}{0em}{\MakeUppercase}
\titleformat{\subsection}{\sffamily\bfseries\normalsize}{}{0em}{}
\titleformat{\subsubsection}{\sffamily\normalsize}{}{0em}{}
%~ \titleformat{\section}{\centering\large}{}{0em}{\MakeUppercase}[\titlerule]
%~ \titleformat{<command>}[<shape>]{<format>}{<label>}{<sep>}{<before-code>}[<after-code>]

\makeatletter
\def\subsectionmark#1{%
  \markright {{%
      \ifnum \c@secnumdepth >\z@
      \thesubsection. \ %
      \fi
      #1}}}
\makeatother

\usepackage{fancyhdr}
\renewcommand{\headrulewidth}{0pt}
\pagestyle{fancy}
\fancyhf{}
\fancyhead[EL]{\thepage}
\fancyhead[OR]{\thepage}
\fancyhead[C]{\textit\rightmark}

\usepackage{microtype}
\usepackage[british]{babel}