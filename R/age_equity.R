setwd( "~/Copy/SCF" )

library(mitools)	# allows analysis of multiply-imputed survey data
library(survey)		# load survey package (analyzes complex design surveys)
library(downloader)	# downloads and then runs the source() function on scripts from github
library(foreign) 	# load foreign package (converts data files into R)


# for example, load the 2010 survey of consumer finances into memory

filenames <- c("scf1989.rda",
			   "scf1992.rda",
			   "scf1995.rda",
			   "scf1998.rda",
			   "scf2001.rda",
			   "scf2004.rda",
			   "scf2007.rda",
			   "scf2010.rda",
			   "scf2013.rda")

source_url( "https://raw.github.com/ajdamico/usgsd/master/Survey%20of%20Consumer%20Finances/scf.survey.R" , prompt = FALSE )
eqt.by.age <- c()
veqt.by.age <- c()

for(i in 1:length(filenames)){
	load( filenames[i] )


	# memory conservation step #

	# for machines with 4gb or less, it's necessary to subset the five implicate data frames to contain only
	# the columns necessary for your particular analysis.  if running the code below generates a memory-related error,
	# simply uncomment these lines and re-run the program:


	# define which variables from the five imputed iterations to keep
	vars.to.keep <- c( 'y1' , 'yy1' , 'wgt' , 'one' , 'five' , 'hdebt',
		               'networth' , 'checking', 'agecl' , 'hhsex' ,
		               'race', 'stocks', 'equity', 'asset', 'stmutf')
	# note: this throws out all other variables (except the replicate weights)
	# so if you need additional columns for your analysis,
	# add them to the `vars.to.keep` vector above


	# restrict each `imp#` data frame to only those variables
	imp1 <- imp1[ , vars.to.keep ]
	imp2 <- imp2[ , vars.to.keep ]
	imp3 <- imp3[ , vars.to.keep ]
	imp4 <- imp4[ , vars.to.keep ]
	imp5 <- imp5[ , vars.to.keep ]

	imp1$pstock <- imp1$equity / imp1$asset
	imp2$pstock <- imp2$equity / imp2$asset
	imp3$pstock <- imp3$equity / imp3$asset
	imp4$pstock <- imp4$equity / imp4$asset
	imp5$pstock <- imp5$equity / imp5$asset


	# clear up RAM
	gc()

	# end of memory conservation step #


	# turn off scientific notation in most output
	options( scipen = 20 )


	# load two svyttest functions (one to conduct a df-adjusted t-test and one to conduct a multiply-imputed t-test)
	# now that this function has been loaded into r, you can view its source code by uncommenting the line below
	# scf.MIcombine
	# scf.svyttest


	# construct an imputed replicate-weighted survey design object
	# build a new replicate-weighted survey design object,
	# but unlike most replicate-weighted designs, this object includes the
	# five multiply-imputed data tables - imp1 through imp5
	scf.design <- 
		svrepdesign( 
			
			# use the main weight within each of the imp# objects
			weights = ~wgt , 
			
			# use the 999 replicate weights stored in the separate replicate weights file
			repweights = rw[ , -1 ] , 
			
			# read the data directly from the five implicates
			data = imputationList( list( imp1 , imp2 , imp3 , imp4 , imp5 ) ) , 

			scale = 1 ,

			rscales = rep( 1 / 998 , 999 ) ,

			# use the mean of the replicate statistics as the center
			# when calculating the variance, as opposed to the main weight's statistic
			mse = TRUE ,
			
			type = "other" ,

			combined.weights = TRUE
		)


	debt.by.age <-
		scf.MIcombine( 
			with( 
				scf.design , 
				svyby( 
					~pstock, 
					~agecl , 
					svymean,
					na.rm=TRUE 
				) 
			) 
		)

	# print the results to the screen 
	debt.by.age

	eqt.by.age <- cbind(eqt.by.age, debt.by.age$coef)
	veqt.by.age <- cbind(veqt.by.age, debt.by.age$coef)

}

load("scf1986_panel.rda")

dac$equity1986 <- dac$c1401 + dac$c1407
dac$equity1983 <- dac$c1402 + dac$c1408

dac$c1447[dac$c1447==0] <- NA 
dac$c1448[dac$c1448==0] <- NA 

dac$pstock1986 <- dac$equity1986/dac$c1449
dac$pstock1983 <- dac$equity1983/dac$c1450

dac$age1986 <- dac$c1603

qcut2 <- function(x, n) {
  findInterval(x, quantile(x, seq(0, 1, length = n + 1)), all.inside = T)
}

dac$dum <- factor(qcut2(dac$age1986,6))
eqt.by.age <- cbind(aggregate(dac$pstock1983,by=list(dac$dum),FUN=mean,na.rm=TRUE)[,2],
					aggregate(dac$pstock1986,by=list(dac$dum),FUN=mean,na.rm=TRUE)[,2],
					eqt.by.age)
load("sfcc1963_panel.rda")

dac$equity1963 <- dac$x113 + dac$x114 + dac$x115 + dac$x116 + dac$x117
dac$equity1962 <- dac$x123 + dac$x124 + dac$x125 + dac$x126 + dac$x127

dac$asset1963 <-
dac$asset1962 <-

dac$pstock1963 <- dac$equity1963 / dac$asset1963
dac$pstock1962 <- dac$equity1962 / dac$asset1962

dac$age1962 <- dac$x359
dac$dum <- factor(qcut2(dac$age1962,6))

#TODO : calculate assets