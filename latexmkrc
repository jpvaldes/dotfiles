# generate pdf and run skim as viewer
$pdflatex = 'pdflatex -interaction=nonstopmode -synctex=1 %O %S';
$pdf_previewer = 'open -a skim';

# add files to cleanup with latexmk -C or -c
$clean_ext = 'bbl rel %R-blx.bib %R.synctex.gz';