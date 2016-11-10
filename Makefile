
analyze.md: ctable-counts.rds analyze.Rmd
	R CMD Sweave analyze.Rmd

ctable-counts.rds:
	Rscript preprocess.R

