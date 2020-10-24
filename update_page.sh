python get_pubs.py > publications.md
rm scholar.log
git add publications.md

Rscript dive_data/gen_plots.R
git add dive_data/dive_summaries.png
git add dive_data/dives.csv
