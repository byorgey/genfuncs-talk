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
        \includegraphics[width=1in]{\sectionimg}
        \bigskip

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

\def\sectionimg{tree.jpg}

\setcounter{section}{-1}
\section{Part 0: Trees}
\label{sec:trees}

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

    This is also a tree.
  \end{center}
\end{frame}

\begin{frame}{Trees, trees, and more trees}
  \begin{itemize}
  \item Phylogenetic trees
  \item Binary search trees
  \item Decision trees
  \item Parse trees
  \item Game trees
  \item Family trees
  \item Directory trees
  \item Playoff brackets
  \item \dots
  \end{itemize}
\end{frame}

\begin{frame}[fragile]{Binary trees}
A binary tree
\begin{minipage}[c]{0.1\linewidth}
\begin{diagram}[width=30]
  import Structures
  dia = subtree # centerXY # pad 1.1
\end{diagram}
\end{minipage}
is\dots

\bigskip

\begin{minipage}{0.4\linewidth}
  \begin{center}
    \begin{diagram}[width=25]
      import Structures
      dia = nil # centerXY # pad 1.1
    \end{diagram}

    empty,
  \end{center}
\end{minipage}
OR
\begin{minipage}{0.5\linewidth}
  \begin{center}
    \begin{diagram}[width=100]
      import Structures
      dia = treeDef # centerXY # pad 1.1
    \end{diagram}

    a node AND two binary trees.
  \end{center}
\end{minipage}
\end{frame}

\begin{frame}[fragile]{Binary trees}
  \begin{center}
    \begin{diagram}[width=300]
      import Structures
      import Data.List.Split
      dia = (vcat' with {sep = 6} . map (centerX . hcat' with {sep=6}) . chunksOf 6 . map binTree . take 12 . concat $ allBinTrees)
          # centerXY # pad 1.1
    \end{diagram}
    %$
  \end{center}
\end{frame}

\begin{frame}[fragile]{A binary tree}
  \begin{center}
    \begin{diagram}[width=300,height=200]
import Structures
import BoltzmannTrees
import System.IO.Unsafe

bigTreeSize = 300

Just bigTree = unsafePerformIO (runGenM bigTreeSize 0.1 genTree)
dia = (binTree . toBTree $ bigTree)   -- $
  # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}{A question}
  \begin{center}
    \emph{Q: How many different binary trees are there of size 50?}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Binary trees by size}
\begin{center}
\begin{diagram}[width=300]
import Structures
import Control.Lens ((&), (.~))

dia = hcat' with {sep = 3} [bucketed allBinTreesD, text' 7 "?"]
    # centerXY # pad 1.1
\end{diagram}
\end{center}
\end{frame}

\def\sectionimg{algebra.jpg}

\section{Part 1: The Algebra of Things}
\label{sec:algebra}

\begin{frame}[fragile]{Binary trees, again}
A binary tree
\begin{minipage}[c]{0.1\linewidth}
\begin{diagram}[width=30]
  import Structures
  dia = subtree # centerXY # pad 1.1
\end{diagram}
\end{minipage}
is\dots \onslide<2->{\hfill \framebox{$T = 1 + X\cdot T \cdot T$} \hfill}

\bigskip

\begin{minipage}{0.4\linewidth}
  \begin{center}
    \begin{diagram}[width=25]
      import Structures
      dia = nil # centerXY # pad 1.1
    \end{diagram}

    empty,
  \end{center}
\end{minipage}
OR
\begin{minipage}{0.5\linewidth}
  \begin{center}
    \begin{diagram}[width=100]
      import Structures
      dia = treeDef # centerXY # pad 1.1
    \end{diagram}

    a node AND two binary trees.
  \end{center}
\end{minipage}
\end{frame}

\begin{frame}[fragile]{Species}
  \begin{center}
\begin{diagram}[width=300,height=200]
import Structures
import Control.Lens ((&), (.~))

dia =
  (vcat' with {sep = 5} . map (hcat' with {sep = 3}))
  [ [text' 5 "T", bucketed' (with & showIndices .~ False) allBinTreesD]
  , [text' 5 "F", drawSpecies (with & showIndices .~ False) speciesA]
  , [text' 5 "G", drawSpecies with speciesB]
  ]
  # centerXY # pad 1.02
\end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Sum = OR}
\begin{center}
An $(F+G)$-structure is \emph{either} an $F$-structure \emph{or} a $G$-structure.

\bigskip

\begin{diagram}[width=300]
import Structures
import Control.Lens ((&), (.~))

dia =
  vc3
  [ fRow
  , gRow
  , hrule (width fgRow)
  , fgRow
  ]
  # centerXY # pad 1.02
  where
    fRow = hcat' with {sep = 3} [text' 5 "F", drawSpecies (with & showIndices .~ False) speciesA]
    gRow = hcat' with {sep = 3} [text' 5 "G", drawSpecies (with & showIndices .~ False) speciesB]
    fgRow = hcat' with {sep = 3}
      [ text' 5 "F + G"
      , drawSpecies with (speciesA %+ speciesB)
      ]
\end{diagram}
\end{center}
\end{frame}

\begin{frame}[fragile]{Zero}
  \begin{center}
    \begin{diagram}[width=300]
      import Structures
      import Control.Lens ((&), (.~))

      dia =
        vc3r rows # centerXY # pad 1.02
        where
          rows = hc3
            [ [text' 10 "0", bucketed' (with & showIndices .~ False) (repeat [])]
            , [text' 5 "F", drawSpecies (with & showIndices .~ False) speciesA]
            , [hrule (width (rows !! 3))]
            , [text' 5 "0 + F", drawSpecies (with & showIndices .~ False) speciesA]
            ]
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Product = AND}
  \begin{center}
    An $(F \cdot G)$-structre is an ordered pair of an $F$-structure \emph{and}
    a $G$-structure.
    \bigskip

    \begin{diagram}[width=200]
      import Structures
      dia = pair (binTree treeA) (binTree treeB)
        # centerXY # pad 1.1
    \end{diagram}
    \[ T \cdot T \]
  \end{center}
\end{frame}

\begin{frame}[fragile]{Building trees with products}
  \begin{center}
    \begin{diagram}[width=250]
      import Structures
      dia = pair dot (pair (binTree treeA) (binTree treeB))
        # centerXY # pad 1.1
    \end{diagram}
    \[ X \cdot (T \cdot T) \]
  \end{center}
\end{frame}

\begin{frame}[fragile]{Building trees with products}
  \begin{center}
    \begin{diagram}[width=300]
      import Structures
      import Diagrams.TwoD.Layout.Tree

      pp = pair dot (pair (binTree treeA) (binTree treeB))

      dia = hcat' with {sep = 10}
        [ pp
        , text' 6 "≅"
        , binTree (BNode () treeA treeB) # centerY
        ]
        # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=200]
      import Structures

      dia = (hcat' with {sep=1} . map alignB)
        [ drawBucket with (speciesA !! 3)
        , vcat' with {sep=1}
          [ drawBucket with (speciesB !! 2)
          , drawBucket with ((speciesA !! 3) %* (speciesB !! 2))
          ]
        ]
        # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures

      dia = productGrid speciesA speciesB (const Nothing)
          # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures

      dia = productGrid speciesA speciesB f # centerXY # pad 1.1
        where
          f ((3,2),b) = Just (b, mempty)
          f _         = Nothing
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      {-# LANGUAGE TupleSections #-}
      import Structures
      dia = productGrid speciesA speciesB (Just . (,mempty) . snd) # centerXY # pad 1.1
    \end{diagram}
    %$
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      dia = prodSum 0 # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      dia = prodSum 1 # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      dia = prodSum 2 # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      dia = prodSum 3 # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{One}
  \begin{center}
    \begin{diagram}[width=300]
      import Structures

      dia = hc3 [text' 10 "1", drawSpecies with speciesOne]
        # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{One}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      {-# LANGUAGE TupleSections #-}
      import Structures

      dia = productGrid speciesOne speciesB (Just . (,mempty) . snd) # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Singleton}
  \begin{center}
    \begin{diagram}[width=300]
      import Structures

      dia = hc3 [text' 10 "X", drawSpecies with speciesX]
        # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

% \begin{frame}[fragile]{Singleton}
%   \begin{center}
%     \begin{diagram}[width=300,height=200]
%       {-# LANGUAGE TupleSections #-}
%       import Structures

%       dia = productGrid speciesX speciesB (Just . (,mempty) . snd) # centerXY # pad 1.1
%     \end{diagram}
%   \end{center}
% \end{frame}

% \begin{frame}{Other things}
%   \begin{itemize}
%   \item Other primitive species: bags, cycles, \dots
%   \item Other operations: composition, Cartesian product, \dots
%   \end{itemize}
% \end{frame}

\section{Part 3: Generating Functions}
\label{sec:gen-funcs}

\begin{frame}{Generating functions}
  \begin{center}
    ``A generating function is a clothesline
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

      dia = hc3
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
        (vc3r . map hc3)
        [ [text' 8 "T", bucketed' (with & showIndices .~ False) allBinTreesD ]
        , [text' 8 "T(x)", drawGF [1,1,2,5,14,42]]
        ]
        # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: 0}
  \begin{center}
    \begin{diagram}[width=300]
      import Structures
      import Control.Lens ((&), (.~))

      dia =
        (vc3r . map hc3)
        [ [text' 8 "0", drawSpecies (with & showIndices .~ False) (repeat zero)]
        , [text' 8 "0(x)", drawGF (repeat 0)]
        ]
        # centerXY # pad 1.1
    \end{diagram}

    \[ 0(x) = 0 + 0x + 0x^2 + 0x^3 + 0x^4 + \dots = 0 \]
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: 1}
  \begin{center}
    \begin{diagram}[width=300]
      import Structures
      import Control.Lens ((&), (.~))

      dia =
        (vc3r . map hc3)
        [ [text' 8 "1", drawSpecies (with & showIndices .~ False) speciesOne]
        , [text' 8 "1(x)", drawGF (1 : repeat 0)]
        ]
        # centerXY # pad 1.1
    \end{diagram}

    \[ 1(x) = 1 + 0x + 0x^2 + 0x^3 + 0x^4 + \dots = 1 \]
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: $X$}
  \begin{center}
    \begin{diagram}[width=300]
      import Structures
      import Control.Lens ((&), (.~))

      dia =
        (vc3r . map hc3)
        [ [text' 8 "X", drawSpecies (with & showIndices .~ False) speciesX]
        , [text' 8 "X(x)", drawGF (0 : 1 : repeat 0)]
        ]
        # centerXY # pad 1.1
    \end{diagram}

    \[ X(x) = 0 + 1x + 0x^2 + 0x^3 + 0x^4 + \dots = x \]
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: sum}
  %% XXX
  foo
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  %% XXX
  bar
\end{frame}

\begin{frame}{Trees, again}
  \[ T = 1 + X \cdot T \cdot T \]

  \onslide<2>

  \[ T(x) = 1 + x T(x)^2 \]
\end{frame}

\begin{frame}
  \[ x T(x)^2 - T(x) + 1 = 0 \]
  \[ T(x) = \frac{1 \pm \sqrt{1 - 4x}}{2x} \]
\end{frame}

\end{document}

% \begin{frame}[fragile]{Species}
% \begin{center}
% A ``species'' is a \emph{family of related structures}.

% \begin{diagram}[width=200]
% import Structures
% import Diagrams.TwoD.Layout.CirclePacking

% dia = (renderCirclePacking (approxRadius 8) . map (pad 1.5 . centerXY . binTree) . concat . take 6 $ allBinTrees)
%     # centerXY # pad 1.1
% \end{diagram}
% %$
% \end{center}
% \end{frame}


% \section{Part 2: Taming the Zoo}
% \label{sec:taming}

% \begin{frame}[fragile]{The Algebra of Species}
% An algebraic language for systematically describing (some) species.

% \begin{center}
% \begin{minipage}{0.3 \textwidth}
% \begin{center}
% \begin{diagram}[width=75, height=75]
% import Structures

% dia = tree (parseTree "(()((()())()))")
%     # centerXY # pad 1.1
% \end{diagram}
% \[ T = 1 + X \cdot T \cdot T \]
% \end{center}
% \end{minipage}
% \begin{minipage}{0.3 \textwidth}
% \begin{center}
% \begin{diagram}[width=75, height=75]
% import Structures

% dia = list 5 # centerXY # pad 1.1
% \end{diagram}
% \[ L = 1 + X \cdot L \]
% \end{center}
% \end{minipage}
% \begin{minipage}{0.3 \textwidth}
%   \begin{center}
%     \begin{diagram}[width=75, height=75]
%       import Structures

%       dia = hcat' with {sep=1} [cyc 5, cyc 2, cyc 3]
%           # centerXY # pad 1.1
%     \end{diagram}
%   \[ S = E_+ \circ C \]
%   \end{center}
% \end{minipage}
% \end{center}
% \end{frame}

% % \begin{frame}
% % XXX idea: build up species compositionally, from primitives and operations.
% % \end{frame}

% \begin{frame}{Examples}

% \end{frame}

% \begin{frame}{}

% \end{frame}

% \section{Part 4: Semirings}
% \label{sec:semirings}

% \begin{frame}{Wilf again}
%   \begin{center}
%     ``A generating function is a clothesline on which we hang up a
%     sequence of \sout{numbers} \emph{things} for display.''
%   \end{center}
% \end{frame}

% \begin{frame}{Semirings}
%   A \emph{semiring} is:
%   \begin{itemize}
%   \item A set $S$
%   \item A binary operation $+$, with identity $0 \in S$
%   \item A binary operation $\cdot$, with identity $1 \in S$
%   \item ($\dots$ and a few other laws)
%   \end{itemize}

%   \onslide<2->
%     \bigskip
%     You already know some examples!

% \end{frame}

% \begin{frame}{Examples}
%   \begin{itemize}
%   \item<+-> Booleans (true/false), with AND ($\land$) and OR ($\lor$)
%   \item<+-> The integers, with the usual $+$ and $\cdot$
%   \item<+-> Finite sets, with disjoint union ($\uplus$) and Cartesian
%     product ($\times$)
%   \end{itemize}
% \end{frame}

% \begin{frame}{Generalized generating functions}
%   Given a semiring $S$, we can build another semiring of generating
%   functions with coefficients from $S$.
% \end{frame}

% \begin{frame}

% \end{frame}

% \begin{frame}[fragile]{Examples}
% \begin{center}
% \begin{diagram}[width = 300]
% import Structures

% dia = bucketed ([[], [], [list 2]] ++ repeat [])
%     # centerXY # pad 1.1
% \end{diagram}

% \bigskip

% Pairs
% \end{center}
% \end{frame}

% \begin{frame}[fragile]{Examples}
% \begin{center}
% \begin{diagram}[width = 300]
% import Structures

% dia = listBuckets with
%     # centerXY # pad 1.1
% \end{diagram}
% %$

% \bigskip

% Lists
% \end{center}
% \end{frame}

% \begin{frame}[fragile]{Examples}
% \begin{center}
% \begin{diagram}[width=300]
% import Structures
% import Control.Lens ((&), (.~))

% dia = binTreeBuckets with
%     # centerXY # pad 1.1
% \end{diagram}

% \bigskip

% Binary trees
% \end{center}
% \end{frame}

% \begin{frame}[fragile]{Examples}
% \begin{center}
% \begin{diagram}[width=300]
% import Structures
% import Control.Lens ((&), (.~))

% dia = bucketed (map (:[]) . map cyc $ [0..])
%     # centerXY # pad 1.1
% \end{diagram}
% %$

% \bigskip

% Cycles
% \end{center}
% \end{frame}

% \begin{frame}[fragile]{Examples}
% \begin{center}
% \begin{diagram}[width=300]
% import Structures
% import Control.Lens ((&), (.~))

% dia = bucketed (map (map tree) allTrees
%                 # zipWith scale [1,1,1,0.7,0.4,0.2]
%                )
%     # centerXY # pad 1.1
% \end{diagram}
% %$

% \bigskip

% $n$-ary trees
% \end{center}
% \end{frame}

% %% XXX add other examples?  Organic molecules? etc?

% \begin{frame}{Questions}
%   Given a particular family of structures (species), we can ask:
%   \begin{itemize}
%   \item How many are there (of a given size)?
%   \item How many with some property $P$?
%   \item Can we list them all?
%   \item Can we generate them at random?
%   \item $\dots$?
%   \end{itemize}
% \end{frame}
