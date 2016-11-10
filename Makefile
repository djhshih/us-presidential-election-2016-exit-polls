
analyze.md: ctable-counts.rds
	R CMD Sweave analyze.Rmd

ctable-counts.rds:
	Rscript preprocess.R

