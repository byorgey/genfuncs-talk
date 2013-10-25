%% -*- mode: LaTeX; compile-command: "mk" -*-
\documentclass[xcolor=svgnames,12pt]{beamer}

\usepackage{haskell}
%include lhs2TeX-extra.fmt

\usepackage{brent}
\usepackage[backend=ps,extension=eps,outputdir=diagrams]{diagrams-latex}
\graphicspath{{images/}}
\usepackage{ulem}

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

\title{Trees and Things \\ (with Semirings!)}
\date{Houghton College \\ October 29, 2013}
\author{Brent Yorgey}
\titlegraphic{}  % \includegraphics[width=2in]{foo}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{frame}[fragile]
   \titlepage
%   \hfill \includegraphics[width=0.5in]{plclub}
\end{frame}

\title{Trees and Things \\ \sout{(with Semirings!)}}

\begin{frame}[fragile]
  \titlepage
\end{frame}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{frame}{Trees}
  \begin{center}
    \includegraphics[width=3in]{tree.jpg}

  This is a tree.
  \end{center}
\end{frame}

\begin{frame}[fragile]{Trees}
  \begin{center}
    \begin{diagram}[height=150]
      import Structures

      dia = binTree (parseBTree "((()())(((()())(()()))(()())))")
        # centerXY # pad 1.1
    \end{diagram}

    This is \dots also a tree.
  \end{center}
\end{frame}

\begin{frame}[fragile]{Trees}
A tree is either\dots

\begin{minipage}{0.4\linewidth}
  \begin{center}
    \begin{diagram}[width=50]
      import Structures
      dia = nil
    \end{diagram}

    empty,
  \end{center}
\end{minipage}
OR
\begin{minipage}{0.5\linewidth}
  \begin{center}
    \begin{diagram}[width=50]
      import Structures
      dia = nil -- XXX
    \end{diagram}

    a node with two subtrees.
  \end{center}
\end{minipage}
\end{frame}

\begin{frame}
  %% show a few example trees of size 50.  say what we mean by size.
\end{frame}

\begin{frame}
  \begin{center}
    \emph{How many different trees are there of size 50?}
  \end{center}
\end{frame}

\section{Part 1: The Combinatorial Zoo}
\label{sec:zoo}

\begin{frame}[fragile]{Combinatorial structures}
Informally: ``dots arranged in some shape''.

%% XX if time, make variant pictures to go with words like "can
%% generalize this... different color dots, labels instead of dots,
%% ..."

\bigskip
\begin{center}
  \begin{diagram}[width=200]
    import Structures

    dia = theGraph
      # rotateBy (1/4) # sized (Width 4)
      # centerXY # pad 1.1
    -- XXX use some different structure?
  \end{diagram}
\end{center}

\end{frame}

\begin{frame}[fragile]{Species}
\begin{center}
A ``species'' is a \emph{family of related structures}.

\begin{diagram}[width=200]
import Structures
import Diagrams.TwoD.Layout.CirclePacking

dia = (renderCirclePacking (approxRadius 8) . map (pad 1.5 . centerXY . binTree) . concat . take 7 $ allBinTrees)
    # centerXY # pad 1.1
\end{diagram}
%$
\end{center}
\end{frame}

\begin{frame}[fragile]{Species, by size}
\begin{center}
\begin{diagram}[width=300]
import Structures
import Control.Lens ((&), (.~))

dia = binTreeBuckets with
    # centerXY # pad 1.1
\end{diagram}

\bigskip

``size'' $=$ number of dots.
\end{center}
\end{frame}

\begin{frame}[fragile]{Examples}
\begin{center}
\begin{diagram}[width = 300]
import Structures

dia = bucketed ([[], [], [list 2]] ++ repeat [])
    # centerXY # pad 1.1
\end{diagram}

\bigskip

Pairs
\end{center}
\end{frame}

\begin{frame}[fragile]{Examples}
\begin{center}
\begin{diagram}[width = 300]
import Structures

dia = listBuckets with
    # centerXY # pad 1.1
\end{diagram}
%$

\bigskip

Lists
\end{center}
\end{frame}

\begin{frame}[fragile]{Examples}
\begin{center}
\begin{diagram}[width=300]
import Structures
import Control.Lens ((&), (.~))

dia = binTreeBuckets with
    # centerXY # pad 1.1
\end{diagram}

\bigskip

Binary trees
\end{center}
\end{frame}

\begin{frame}[fragile]{Examples}
\begin{center}
\begin{diagram}[width=300]
import Structures
import Control.Lens ((&), (.~))

dia = bucketed (map (:[]) . map cyc $ [0..])
    # centerXY # pad 1.1
\end{diagram}
%$

\bigskip

Cycles
\end{center}
\end{frame}

\begin{frame}[fragile]{Examples}
\begin{center}
\begin{diagram}[width=300]
import Structures
import Control.Lens ((&), (.~))

dia = bucketed (map (map tree) allTrees
                # zipWith scale [1,1,1,0.7,0.4,0.2]
               )
    # centerXY # pad 1.1
\end{diagram}
%$

\bigskip

$n$-ary trees
\end{center}
\end{frame}

%% XXX add other examples?  Organic molecules? etc?

\begin{frame}{Questions}
  Given a particular family of structures (species), we can ask:
  \begin{itemize}
  \item How many are there (of a given size)?
  \item How many with some property $P$?
  \item Can we list them all?
  \item Can we generate them at random?
  \item $\dots$?
  \end{itemize}
\end{frame}

\section{Part 2: Taming the Zoo}
\label{sec:taming}

\begin{frame}[fragile]{The Algebra of Species}
An algebraic language for systematically describing (some) species.

\begin{center}
\begin{minipage}{0.3 \textwidth}
\begin{center}
\begin{diagram}[width=75, height=75]
import Structures

dia = tree (parseTree "(()((()())()))")
    # centerXY # pad 1.1
\end{diagram}
\[ T = 1 + X \cdot T \cdot T \]
\end{center}
\end{minipage}
\begin{minipage}{0.3 \textwidth}
\begin{center}
\begin{diagram}[width=75, height=75]
import Structures

dia = list 5 # centerXY # pad 1.1
\end{diagram}
\[ L = 1 + X \cdot L \]
\end{center}
\end{minipage}
\begin{minipage}{0.3 \textwidth}
  \begin{center}
    \begin{diagram}[width=75, height=75]
      import Structures

      dia = hcat' with {sep=1} [cyc 5, cyc 2, cyc 3]
          # centerXY # pad 1.1
    \end{diagram}
  \[ S = E_+ \circ C \]
  \end{center}
\end{minipage}
\end{center}
\end{frame}

% \begin{frame}
% XXX idea: build up species compositionally, from primitives and operations.
% \end{frame}

\begin{frame}[fragile]{Sum = OR}
\begin{center}
An $(F+G)$-structure is \emph{either} an $F$-structure \emph{or} a $G$-structure.

\bigskip

\begin{diagram}[width=300]
import Structures
import Control.Lens ((&), (.~))

dia =
  (vcat' with {sep = 3} . map alignR)
  [ tRow
  , lRow
  , hrule (width lRow)
  , tlRow
  ]
  # centerXY # pad 1.02
  where
    tRow = hcat' with {sep = 3} [text' 5 "T", binTreeBuckets (with & showIndices .~ False)]
    lRow = hcat' with {sep = 3} [text' 5 "L", listBuckets (with & showIndices .~ False)]
    tlRow = hcat' with {sep = 3}
      [ text' 5 "T + L"
      , bucketed
        ( map (map (pad 1.3 . centerXY . binTree)) allBinTrees
        # zipWith (++) (map ((:[]) . list) [0..])
        # zipWith scale [1,1,0.5, 0.15, 0.15, 0.08]
        )
      ]
\end{diagram}
\end{center}
\end{frame}

\begin{frame}[fragile]{Zero}
  \begin{center}
    \begin{diagram}[width=300]
      import Structures

      dia = hcat' with {sep=3} [text' 10 "0", bucketed (repeat [])]
        # centerXY # pad 1.1
    \end{diagram}

    \bigskip

    %% XXX make into actual diagram
    \[ 0 + F = F + 0 = F \]
  \end{center}
\end{frame}

\begin{frame}[fragile]{Product = AND}
  \begin{center}
    \begin{diagram}[width=300]
import Structures
import Control.Lens ((&), (.~))

dia =
  (vcat' with {sep = 3} . map alignR)
  [ tRow
  , lRow
  , hrule (width lRow)
  , tlRow
  ]
  # centerXY # pad 1.02
  where
    tRow = hcat' with {sep = 3} [text' 5 "T", binTreeBuckets (with & showIndices .~ False)]
    lRow = hcat' with {sep = 3} [text' 5 "L", listBuckets (with & showIndices .~ False)]
    tlRow = hcat' with {sep = 3}
      [ text' 5 "T • L"
      , bucketed (repeat [])  -- XXX todo!
      ]
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{One}
  \begin{center}
    \begin{diagram}[width=300]
      import Structures

      dia = hcat' with {sep=3} [text' 10 "1", bucketed ([square 1 # fc black] : repeat [])]
        # centerXY # pad 1.1
    \end{diagram}

    \bigskip

    % XXX draw isomorphism

    \[ F \cdot 1 = 1 \cdot F = F \]
  \end{center}
\end{frame}

\begin{frame}[fragile]{Singleton}
  \begin{center}
    \begin{diagram}[width=300]
      import Structures

      dia = hcat' with {sep=3} [text' 10 "X", bucketed ([] : [dot] : repeat [])]
        # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}{Examples}

\end{frame}

\begin{frame}{Other things}
  \begin{itemize}
  \item Other primitive species: bags, cycles, \dots
  \item Other operations: composition, Cartesian product, \dots
  \end{itemize}
\end{frame}

\section{Part 3: Generating Functions}
\label{sec:gen-funcs}

\begin{frame}{Generating functions}
  \begin{center}
    %% XXX ceci n'est pas un...
    \onslide<2-> Generating functions are not functions.

    \bigskip

    \onslide<3->``A generating function is a clothesline
    on which we hang up a sequence of numbers for display.''  ---
    Herbert Wilf

    \[ f(x) = 1 + x + 2x^2 + 5x^3 + 14x^4 + 42x^5 + \dots \]
  \end{center}
\end{frame}

\begin{frame}[fragile]{Generating functions}
  \begin{center}
    \[ f(x) = 1 + x + 2x^2 + 5x^3 + 14x^4 + 42x^5 + \dots \]

    \bigskip

    \begin{diagram}[width=300]
      import Structures

      dia = hcat' with {sep=3}
        [ text' 5 "f"
        , bucketed (map ((:[]) . text' 8 . show) [1,1,2,5,14,42])
        ]
        # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions}
  \begin{center}
    \begin{diagram}[width=300]
      import Structures
      import Control.Lens ((&), (.~))

      dia =
        (vcat' with {sep=3} . map alignR)
        [ hcat' with {sep=3} [text' 8 "T", binTreeBuckets (with & showIndices .~ False)]
        , hcat' with {sep=3} [text' 8 "T(x)", bucketed (map ((:[]) . text' 8 . show) [1,1,2,5,14,42])]
        ]
        # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

% \begin{frame}{Examples}
%   $X(x) = x$
%   $0(x) = 0$
%   $1(x) = 1$
%   $L(x) = 1 + x + x^2 + x^3 + \dots$
% \end{frame}

% \begin{frame}{Sum}
%   \[ ||(F+G)_n|| = ||F_n|| + ||G_n|| \]

%   so

%   \[ (F+G)(x) = F(x) + G(x) \]
% \end{frame}

% \begin{frame}{Product}
%   \[ ||(FG)_n|| = \sum_{0 \leq k \leq n} ||F_k|| ||G_{n-k}|| \]

%   so

%   \[ (FG)(x) = F(x) G(x) \]

%   XXX add some pictures
% \end{frame}

% \begin{frame}{Example}
%   \[ T = 1 + X \cdot T \cdot T \]

%   so etc. XXX
% \end{frame}

\begin{frame}{}

\end{frame}

\section{Part 4: Semirings}
\label{sec:semirings}

\begin{frame}{Wilf again}
  \begin{center}
    ``A generating function is a clothesline on which we hang up a
    sequence of \sout{numbers} \emph{things} for display.''
  \end{center}
\end{frame}

\begin{frame}{Semirings}
  A \emph{semiring} is:
  \begin{itemize}
  \item A set $S$
  \item A binary operation $+$, with identity $0 \in S$
  \item A binary operation $\cdot$, with identity $1 \in S$
  \item ($\dots$ and a few other laws)
  \end{itemize}

  \onslide<2->
    \bigskip
    You already know some examples!

\end{frame}

\begin{frame}{Examples}
  \begin{itemize}
  \item<+-> Booleans (true/false), with AND ($\land$) and OR ($\lor$)
  \item<+-> The integers, with the usual $+$ and $\cdot$
  \item<+-> Finite sets, with disjoint union ($\uplus$) and Cartesian
    product ($\times$)
  \end{itemize}
\end{frame}

\begin{frame}{Generalized generating functions}
  Given a semiring $S$, we can build another semiring of generating
  functions with coefficients from $S$.
\end{frame}

\begin{frame}

\end{frame}

\end{document}
