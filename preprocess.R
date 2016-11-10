# Preprocess exit poll for 2016 general presidential election

# Input: feature-conditional contingency tables retrieved from
#        http://www.cnn.com/election/results/exit-polls
# Output: count contingency tables and class-conditional contingency tables

# Voters are classified into the following *classes*
#   1. Clinton supporters (those who voted for Clinton)
#   2. Trump supporters (those who voted fro Trump)
#   3. Other (those who voted for another candidate or 
#      did not indicate whom they voted for)

# The different characteristics of a voter consistute the *features*,
# which would be useful for prediction of voter preference, especially
# if individual level data were available.

# Contingency tables may contain feature-conditional frequencies or
# probabilities, and these numbers present the proportion of each type of 
# voter who voted for each candidate. 
# The feature-conditional frequencies help answer questions regarding the
# voter preference of each segment of the population, such as:
# For whom are women likely to vote?
# The original contingency tables are in this form.

# Contingency tables may alternatively contain class-conditional frequencies
# or probabilities, and these numbers present the proportion of each class of
# voters who exhibit a certain characteristic.
# The class-conditional frequencies directly answers questions pertaining to
# the characteristics of supporters for a certain candidate, such as:
# Are Trump supporters predominately male?


library(io);

read_percentage <- function(s) {
	if (s == "n/a") {
		NA
	} else {
		as.numeric(sub("%", "", s)) / 100
	}
}

read_sample_size <- function(s) {
	as.integer(sub(" respondents", "", s))
}

count_tables <- function(lines) {
	sum(lines == "")
}


## read class-conditional contingency tables

lines <- readLines("contingency-tables.txt");

# each table is followed by a blank line
sep.idx <- which(lines == "");

# number of contingency tables
n.fconds <- length(sep.idx);

# contingency table dimensions
ccond.colnames <- c("total", "clinton", "trump", "other/no answer");
ccond.ncols <- length(ccond.colnames);
# ignore table name line (1), supporter classes lines (3), 
# sample size line (1), and the blank separator line (1)
# remaining lines contain the data, with 5 linees per row
ccond.nrows <- ((sep.idx - 1 - 3 - 1 - 1) - c(0, sep.idx[-length(sep.idx)])) / 5;
# sample size reported in contingency table
ccond.ns <- integer(n.fconds);

fconds <- list();
ccond.names <- character(n.fconds);

i <- 1;
k <- 1;
while (i < length(lines)) {
	# read table name
	ccond.names[k] <- lines[i];

	# skip the supporter classes
	i <- i + 4;

	# read table
	rnames <- character(ccond.nrows[k]);
	ccond <- matrix(NA, nrow=ccond.nrows[k], ncol=ccond.ncols);
	while (i < sep.idx[k] - 1) {
		for (r in 1:ccond.nrows[k]) {
			# read rowname
			rnames[r] <- lines[i];
			i <- i + 1;
			# read data in the row
			for (s in 1:ccond.ncols) {
				ccond[r, s] <- read_percentage(lines[i]);
				i <- i + 1;
			}
		}
	}
	rownames(ccond) <- rnames;
	colnames(ccond) <- ccond.colnames;
	ccond.ns[k] <- read_sample_size(lines[i]);
	fconds[[k]] <- ccond;

	i <- i + 1;
	
	# check for blank line
	stopifnot(lines[i] == "");

	# increment line number to next table
	i <- i + 1;

	k <- k + 1;
}
names(fconds) <- ccond.names;


## convert class-conditional probabilities in contingency tables to counts

counts <- mapply(
	function(x, n) {
		# determine total counts
		row.totals <- round(n * x[,1]);
		# determine conditional counts
		round(row.totals * x[,-1]);
	}, fconds, ccond.ns
);

## convert counts to feature-conditional probabilities

cconds <- lapply(counts,
	function(x) {
		totals <- colSums(x, na.rm=TRUE);
		apply(x, 1, function(r) r / totals)
	}
);

## output contingency tables

qwrite(counts, "ctable-counts.rds");
qwrite(fconds, "ctable-prop_feature-conditionals.rds");
qwrite(cconds, "ctable-prop_class-conditionals.rds");

