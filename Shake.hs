import           Development.Shake
import           Development.Shake.FilePath

lhs2TeX  = "lhs2TeX"
pdflatex = "pdflatex"
rubber   = "rubber"
bibtex   = "bibtex"

main = shake shakeOptions $ do

    want ["genfuncs-jobtalk.pdf"]

    "*.tex" %> \output -> do
        let input = replaceExtension output "lhs"
        need [input, "Structures.hs"]
        command_ [] lhs2TeX $ ["--poly", "-o", output] ++ [input]

    "*.pdf" %> \output -> do
        let input = replaceExtension output "tex"
        need [input, "Diagrams.hs"]
        command_ [] pdflatex ["--enable-write18", input]
--        system' pdflatex $ ["--enable-write18", input]
