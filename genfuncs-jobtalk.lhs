%% -*- mode: LaTeX; compile-command: "mk" -*-
\documentclass[xcolor=svgnames,12pt]{beamer}

\usepackage{haskell}
%include lhs2TeX-extra.fmt

\usepackage{brent}
\usepackage[backend=pgf,outputdir=diagrams]{diagrams-latex}
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
\date{Williams College \\ February 26, 2014}
\author{Brent Yorgey}
\titlegraphic{}  % \includegraphics[width=2in]{foo}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{frame}[fragile]
   \titlepage
%   \hfill \includegraphics[width=0.5in]{plclub}
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
        # p1
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
  dia = subtree # p1
\end{diagram}
\end{minipage}
is\dots

\bigskip

\begin{minipage}{0.4\linewidth}
  \begin{center}
    \begin{diagram}[width=25]
      import Structures
      dia = nil # p1
    \end{diagram}

    empty,
  \end{center}
\end{minipage}
OR
\begin{minipage}{0.5\linewidth}
  \begin{center}
    \begin{diagram}[width=100]
      import Structures
      dia = treeDef # p1
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
      dia = (vcat' (with & sep .~ 6) . map (centerX . hcat' (with & sep.~6)) . chunksOf 6 . map binTree . take 12 . concat $ allBinTrees)
          # p1
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
  # p1
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

dia = hcat' (with & sep .~ 3) [bucketed allBinTreesD, text' 7 "?"]
    # p1
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
  dia = subtree # p1
\end{diagram}
\end{minipage}
is\dots \onslide<2->{\hfill \framebox{$T = 1 + X\cdot T \cdot T$} \hfill}

\bigskip

\begin{minipage}{0.4\linewidth}
  \begin{center}
    \begin{diagram}[width=25]
      import Structures
      dia = nil # p1
    \end{diagram}

    empty,
  \end{center}
\end{minipage}
OR
\begin{minipage}{0.5\linewidth}
  \begin{center}
    \begin{diagram}[width=100]
      import Structures
      dia = treeDef # p1
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
  (vcat' (with & sep .~ 5) . map (hcat' (with & sep .~ 3)))
  [ [text' 5 "T", bucketed' (with & showIndices .~ False) allBinTreesD]
  , [text' 5 "F", drawSpecies' (with & showIndices .~ False) speciesA]
  , [text' 5 "G", drawSpecies speciesB]
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
  vc3r
  [ fRow
  , gRow
  , hrule (width fgRow)
  , fgRow
  ]
  # centerXY # pad 1.02
  where
    fRow = hc3 [text' 5 "F", drawSpecies' (with & showIndices .~ False) speciesA]
    gRow = hc3 [text' 5 "G", drawSpecies' (with & showIndices .~ False) speciesB]
    fgRow = hc3
      [ text' 5 "F + G"
      , drawSpecies (speciesA %+ speciesB)
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
          rows = map hc3
            [ [text' 10 "0", bucketed' (with & showIndices .~ False) (repeat [])]
            , [text' 5 "F", drawSpecies' (with & showIndices .~ False) speciesA]
            , [hrule (width (rows !! 3))]
            , [text' 5 "0 + F", drawSpecies' (with & showIndices .~ False) speciesA]
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
        # p1
    \end{diagram}
    \[ T \cdot T \]
  \end{center}
\end{frame}

\begin{frame}[fragile]{Building trees with products}
  \begin{center}
    \begin{diagram}[width=250]
      import Structures
      dia = pair dot (pair (binTree treeA) (binTree treeB))
        # p1
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

      dia = hcat' (with & sep .~ 10)
        [ pp
        , text' 6 "≅"
        , binTree (BNode () treeA treeB) # centerY
        ]
        # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=200]
      import Structures

      dia = (hcat' (with & sep.~1) . map alignB)
        [ drawBucket with (speciesA !! 3)
        , vcat' (with & sep.~1)
          [ drawBucket with (speciesB !! 2)
          , drawBucket with ((speciesA !! 3) %* (speciesB !! 2))
          ]
        ]
        # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures

      dia = productGrid speciesA speciesB (const Nothing)
          # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures

      dia = productGrid speciesA speciesB f # p1
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
      dia = productGrid speciesA speciesB (Just . (,mempty) . snd) # p1
    \end{diagram}
    %$
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      dia = prodSum 0 speciesA speciesB # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      dia = prodSum 1 speciesA speciesB # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      dia = prodSum 2 speciesA speciesB # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Computing products}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      dia = prodSum 3 speciesA speciesB # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{One}
  \begin{center}
    \begin{diagram}[width=300]
      import Structures

      dia = hc3 [text' 10 "1", drawSpecies speciesOne]
        # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{One}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      {-# LANGUAGE TupleSections #-}
      import Structures

      dia = productGrid speciesOne speciesB (Just . (,mempty) . snd) # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Singleton}
  \begin{center}
    \begin{diagram}[width=300]
      import Structures

      dia = hc3 [text' 10 "X", drawSpecies speciesX]
        # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Binary trees, yet again}
A binary tree
\begin{minipage}[c]{0.1\linewidth}
\begin{diagram}[width=30]
  import Structures
  dia = subtree # p1
\end{diagram}
\end{minipage}
is\dots \hfill \framebox{$T = 1 + X\cdot T \cdot T$} \hfill

\bigskip

\begin{minipage}{0.4\linewidth}
  \begin{center}
    \begin{diagram}[width=25]
      import Structures
      dia = nil # p1
    \end{diagram}

    empty,
  \end{center}
\end{minipage}
OR
\begin{minipage}{0.5\linewidth}
  \begin{center}
    \begin{diagram}[width=100]
      import Structures
      dia = treeDef # p1
    \end{diagram}

    a node AND two binary trees.
  \end{center}
\end{minipage}
\end{frame}

\def\sectionimg{clothesline.jpg}

\section{Part 2: Generating Functions}
\label{sec:gen-funcs}

\begin{frame}{Generating functions}
  \begin{center}
    Generating functions are \emph{infinite polynomials}.
    \[ f(x) = 1 + x + 2x^2 + 5x^3 + 14x^4 + 42x^5 + \dots \]

    ``A generating function is a clothesline
    on which we hang up a sequence of numbers for display.''  ---
    Herbert Wilf
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
        # p1
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
        # p1
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
        [ [text' 8 "0", drawSpecies' (with & showIndices .~ False) (repeat zero)]
        , [text' 8 "0(x)", drawGF (repeat 0)]
        ]
        # p1
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
        [ [text' 8 "1", drawSpecies' (with & showIndices .~ False) speciesOne]
        , [text' 8 "1(x)", drawGF (1 : repeat 0)]
        ]
        # p1
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
        [ [text' 8 "X", drawSpecies' (with & showIndices .~ False) speciesX]
        , [text' 8 "X(x)", drawGF (0 : 1 : repeat 0)]
        ]
        # p1
    \end{diagram}

    \[ X(x) = 0 + 1x + 0x^2 + 0x^3 + 0x^4 + \dots = x \]
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: sum}
  \begin{center}
\begin{diagram}[width=300]
import Structures
import Control.Lens ((&), (.~))

dia =
  vc3r
  [ fRow
  , gRow
  , hrule (width fgRow)
  , fgRow
  ]
  # centerXY # pad 1.02
  where
    fRow = hc3 [text' 8 "F(x)", drawGF' (with & showIndices .~ False) gf1]
    gRow = hc3 [text' 8 "G(x)", drawGF' (with & showIndices .~ False) gf2]
    fgRow = hc3
      [ text' 8 "(F+G)(x)"
      , drawGF (gf1 %+ gf2)
      ]
    gf1, gf2 :: [Int]
    gf1 = [1..]
    gf2 = [1,1,2,5,14,42]
\end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: sum}
  \begin{center}
\begin{diagram}[width=300]
import Structures
import Control.Lens ((&), (.~))

dia =
  vc3r
  [ fRow
  , gRow
  , hrule (width fgRow)
  , fgRow
  ]
  # centerXY # pad 1.02
  where
    fRow = hc3 [text' 8 "F(x)", drawGF' (with & showIndices .~ False & showX .~ True) gf1]
    gRow = hc3 [text' 8 "G(x)", drawGF' (with & showIndices .~ False & showX .~ True) gf2]
    fgRow = hc3
      [ text' 8 "(F+G)(x)"
      , drawGF' (with & showX .~ True) (gf1 %+ gf2)
      ]
    gf1, gf2 :: [Int]
    gf1 = [1..]
    gf2 = [1,1,2,5,14,42]
\end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures

      dia = productGrid gf1 gf2 (const Nothing) # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures

      dia = productGrid gf1 gf2 f # p1
        where
          f ((3,2),b) = Just (b, mempty)
          f _         = Nothing
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      {-# LANGUAGE TupleSections #-}
      import Structures
      dia = productGrid gf1 gf2 (Just . (,mempty) . snd) # p1
    \end{diagram}
    %$
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      dia = prodSum 0 gf1 gf2 # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      dia = prodSum 1 gf1 gf2 # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      dia = prodSum 2 gf1 gf2 # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      dia = prodSum 3 gf1 gf2 # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      import Control.Lens ((&), (.~))

      dia = productGrid' (with & showX .~ True) gf1 gf2 (const Nothing) # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      import Control.Lens ((&), (.~))

      dia = productGrid' (with & showX .~ True) gf1 gf2 f # p1
        where
          f ((3,2),b) = Just (b, mempty)
          f _         = Nothing
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      {-# LANGUAGE TupleSections #-}
      import Structures
      import Control.Lens ((&), (.~))
      dia = productGrid' (with & showX .~ True) gf1 gf2 (Just . (,mempty) . snd) # p1
    \end{diagram}
    %$
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      import Control.Lens ((&), (.~))
      dia = prodSum' (with & showX .~ True) 0 gf1 gf2 # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      import Control.Lens ((&), (.~))
      dia = prodSum' (with & showX .~ True) 1 gf1 gf2 # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      import Control.Lens ((&), (.~))
      dia = prodSum' (with & showX .~ True) 2 gf1 gf2 # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Species and generating functions: product}
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Structures
      import Control.Lens ((&), (.~))
      dia = prodSum' (with & showX .~ True) 3 gf1 gf2 # p1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}{Homomorphism}
  To sum up so far:

  \begin{align*}
0(x) &= 0 \\
1(x) &= 1 \\
X(x) &= x \\
(F+G)(x) &= F(x) + G(x) \\
(F\cdot G)(x) &= F(x) \cdot G(x)
  \end{align*}
  \begin{center}
    This sort of ``structure-preserving mapping'' is called a
    \emph{homomorphism}.
  \end{center}
\onslide<2->
\[ \left(\frac{d}{dx}F\right)(x) = \frac{d}{dx}[F(x)] \onslide<3->{\text{ !?}} \]
\end{frame}

\begin{frame}
  \begin{center}
  \includegraphics[width=4in]{universe.jpg}

  Homomorphisms are among the most beautiful things in the universe!
\end{center}
\end{frame}

\begin{frame}{Trees, again}
  \[ T = 1 + X \cdot T \cdot T \]

  \onslide<2->
  \[ T(x) = 1 + x T(x)^2 \]

  \onslide<3->
  \[ x T(x)^2 - T(x) + 1 = 0 \]

  \onslide<4->
  \[ T(x) = \frac{1 \pm \sqrt{1 - 4x}}{2x} \]

  \onslide<5->
  \[ T(x) = \sum_{n \geq 0} \frac{1}{n+1} \binom{2n}{n} x^n \]
\end{frame}

\begin{frame}{The answer!}
  \begin{center}
    There are \[ \frac{100!}{51 \cdot 50! \cdot 50!} =
    1978261657756160653623774456 \] binary trees of size 50.
  \end{center}
\end{frame}

\end{document}


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
