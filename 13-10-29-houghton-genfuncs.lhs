%% -*- mode: LaTeX; compile-command: "mk" -*-
\documentclass[xcolor=svgnames,12pt]{beamer}

\usepackage{haskell}
%include lhs2TeX-extra.fmt

\usepackage{brent}
\usepackage[outputdir=diagrams]{diagrams-latex}
\graphicspath{{images/}}

\renewcommand{\onelinecomment}{\quad--- \itshape}
\renewcommand{\Varid}[1]{{\mathit{#1}}}

% \setbeamertemplate{footline}{\insertframenumber}

\setbeamertemplate{items}[circle]

\mode<presentation>
{
  \usetheme{default}                          % use a default (plain) theme

  \setbeamertemplate{navigation symbols}{}    % don't show navigation
                                              % buttons along the
                                              % bottom
  \setbeamerfont{normal text}{family=\sffamily}

  % XX remove this before giving actual talk!
  % \setbeamertemplate{footline}[frame number]
  % {%
  %   \begin{beamercolorbox}{section in head/foot}
  %     \vskip2pt
  %     \hfill \insertframenumber
  %     \vskip2pt
  %   \end{beamercolorbox}
  % }

  \AtBeginSection[]
  {
    \begin{frame}<beamer>
      \frametitle{}
      \begin{center}
        {\Huge \insertsectionhead}
      \end{center}
    \end{frame}
  }
}

\defbeamertemplate*{title page}{customized}[1][]
{
  \vbox{}
  \vfill
  \begin{centering}
    \begin{beamercolorbox}[sep=8pt,center,#1]{title}
      \usebeamerfont{title}\inserttitle\par%
      \ifx\insertsubtitle\@@empty%
      \else%
        \vskip0.25em%
        {\usebeamerfont{subtitle}\usebeamercolor[fg]{subtitle}\insertsubtitle\par}%
      \fi%
    \end{beamercolorbox}%
    \vskip1em\par
    {\usebeamercolor[fg]{titlegraphic}\inserttitlegraphic\par}
    \vskip1em\par
    \begin{beamercolorbox}[sep=8pt,center,#1]{author}
      \usebeamerfont{author}\insertauthor
    \end{beamercolorbox}
    \begin{beamercolorbox}[sep=8pt,center,#1]{institute}
      \usebeamerfont{institute}\insertinstitute
    \end{beamercolorbox}
    \begin{beamercolorbox}[sep=8pt,center,#1]{date}
      \usebeamerfont{date}\insertdate
    \end{beamercolorbox}
  \end{centering}
  \vfill
}

% uncomment me to get 4 slides per page for printing
% \usepackage{pgfpages}
% \pgfpagesuselayout{4 on 1}[uspaper, border shrink=5mm]

% \setbeameroption{show only notes}

\renewcommand{\emph}{\textbf}

\title{Title!}
\date{Houghton College \\ October 29, 2013}
\author{Brent Yorgey}
\titlegraphic{}  % \includegraphics[width=2in]{foo}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{frame}[fragile]
   \titlepage
%   \hfill \includegraphics[width=0.5in]{plclub}
\end{frame}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{frame}[fragile]{Overview}
  XXX
\end{frame}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Part 1: The Combinatorial Zoo}
\label{sec:zoo}

\begin{frame}{``Combinatorial structure''?}

XXX picture

\begin{center}
  Think ``data structure, without any data''.

  Set of shapes.  
  Set of shapes sorted out by size.  Picture --- buckets of shapes
\end{center}
\end{frame}

\begin{frame}[fragile]{Examples}
  Ordered pairs, unordered pairs, lists, binary trees, cycles, bags,
  n-ary trees, octopi, permutations, functions, graphs (each on a
  separate slide)
\end{frame}

\begin{frame}{Questions}
  Given a particular class of combinatorial structures, we can ask:
  \begin{itemize}
  \item How many are there (of a given size)?
  \item How many with some property $P$?
  \item Can we list them all?
  \item Can we generate them at random?
  \item \dots
  \end{itemize}
\end{frame}

\section{Part 2: Taming the Zoo}
\label{sec:taming}

\begin{frame}{The Algebra of Things}
A language for describing (some) combinatorial structures.

XXX pictures of trees, list, perms with their algebraic expression
next to them
\end{frame}

\begin{frame}{Combinatorial sum}
Disjoint union.

Throw all the shapes together.  Note size stays the same.
\end{frame}

\begin{frame}{Zero}
  empty set.  identity for sum.
\end{frame}

\begin{frame}{Combinatorial product}
  Pairing. $size(s,t) = size(s) + size(t)$.
\end{frame}

\begin{frame}{Unit}
  A set with a single structure.  $size(BOX) = 0$.
\end{frame}

\begin{frame}{Singleton}
  Similar to unit, but with size 1.
\end{frame}

\begin{frame}{Examples}

\end{frame}

\begin{frame}{Composition!}

\end{frame}

\begin{frame}{Other things}
  
\end{frame}

\section{Part 3: Generating Functions}
\label{sec:gen-funcs}

\begin{frame}{Generating functions}
  ``A generating function is a clothesline on which we hang up a
  sequence of numbers for display.''  --- Herbert Wilf

\[ f(x) = 1 + x + 2x^2 + 5x^3 + 14x^4 + 42x^5 + \dots \]
\end{frame}

\begin{frame}{GFs for combinatorial structures}
   \[ F(x) = \sum_{n \geq 0} |F_n| x^n = |F_0| + |F_1|x + |F_2|x^2 + \dots \]
\end{frame}

\begin{frame}{Examples}
  X(x) = x
  0(x) = 0
  1(x) = 1
  L(x) = 1 + x + x^2 + x^3 + \dots
\end{frame}

\begin{frame}{Sum}
  \[ |(F+G)_n| = |F_n| + |G_n| \]

  so

  \[ (F+G)(x) = F(x) + G(x) \]
\end{frame}

\begin{frame}{Product}
  \[ |(FG)_n| = \sum_{0 \leq k \leq n} |F_k| |G_{n-k}| \]

  so

  \[ (FG)(x) = F(x) G(x) \]

  XXX add some pictures
\end{frame}

\begin{frame}{Example}
  \[ T = 1 + X \cdot T \cdot T \]

  so etc. XXX
\end{frame}

\section{Part 4: Semirings}
\label{sec:semirings}

\begin{frame}{Wilf again}
  ``A generating function is a clothesline on which we hang up a
  sequence of numbers for display.''

  XXX strike out ``numbers'', replace with ``stuff''?
\end{frame}

\begin{frame}{Semirings}
  A \emph{semiring} is:
  \begin{itemize}
  \item A set $S$
  \item A binary operation $+$, with identity $0 \in S$
  \item A binary operation $\cdot$, with identity $1 \in S$
  \item (\dots and a few other laws)
  \end{itemize}
\end{frame}

\begin{frame}{Examples}
  You already know some examples!

  XXX examples
\end{frame}

\begin{frame}
  XXX generalize
\end{frame}

\end{document}
