echo 'Report compilation is started.'
cd data
echo 'Figures generation is started.'
python3 draw.py
echo 'Figures generation is finished.'
cd ../


echo 'PDF generation is started.'
echo '  First step.'
pdflatex SortsPerformanceComparisonReport.tex
echo '  Bibliography generation.'
bibtex SortsPerformanceComparisonReport
echo '  Second step.'
pdflatex SortsPerformanceComparisonReport.tex
pdflatex SortsPerformanceComparisonReport.tex
echo 'Report compilation is finished successfully.'
