
library('boot') 	# For doing bootstrap estimates for the CIs of the mean
library(MASS)

read.data <- function(fname) {
	
	dat <- read.csv(fname)
	colnames(dat) <- c( 'expected.val', # sum of expected benefits multiplied by the expected prob of succsess for all projectes selected
						'true.expected.val', # sum of the true benefits x true prob of success for all selected projects
						'revealed.val', # true outcome: sum of the true benefits of all successful projects (given have rolled dice and seen which ones failed)

						'expected.cost', # These don't incoporate success of failure, just expected cost
						'revealed.cost', # true cost of all selected projects

						'expected.val.all.success', # estimated value of all projects selected (assuming all successful)
						'revealed.val.all.success', # true value of all projects selected (assuming all successful)

						'summed.expected.prob',  # Sum of expected prob of success for all projects selected
						'summed.revealed.prob',  # Sum of revealed prob of success for all projects selected
						'num.projects.selected' # The number of projects chosen
						)

	# To 
		
	# remove any entries with NaN's in them
	
	rows.with.NAs <- length(which(is.na(dat$revealed.val)==TRUE))
	dat <- na.omit(dat)

	cat('Reading in file', fname, '[omiting ', rows.with.NAs, 'rows with NAs]\n') 

	return(dat)
}
 

rescale.results <- function(res, ran){

	# make a new df to hold the results
	rescaled <- ran; rescaled[,] <- -999

	
	
	for(i in 1:dim(ran)[2]) {

		rescaled[,i] <- (res[,i] - ran[,i])/max(ran)

	}

	# hist(res$expected.cost)#, xlim=c(0,10))
	# hist(ran$expected.cost)#, xlim=c(0,10))
	# hist(rescaled$expected.cost)#, xlim=c(0,10))
	return(rescaled)
}


 plot.results <- function(dat, title.text) {

 	# Calculate surprises in cost and benefit
	cost.surprise <- (dat$expected.cost - dat$revealed.cost)/dat$revealed.cost

	# Note as some of the revealed benefits come out to be zero, for now I've
	# just made the suprise in benefit the absolute difference and not
	# propotional difference as per the others.
	benefit.surprise <- (dat$revealed.val - dat$expected.val) /dat$revealed.val
	benefit.surprise.all.success <- (dat$revealed.val.all.success - dat$expected.val.all.success)/dat$revealed.val.all.success

	
	summed.expected.prob.surprise <- (dat$summed.revealed.prob - dat$summed.expected.prob)/dat$summed.revealed.prob
	
	expected.CE.all.succ <- dat$expected.val.all.success * dat$summed.expected.prob / dat$expected.cost
	revealed.CE.all.succ <- dat$revealed.val.all.success * dat$summed.revealed.prob / dat$revealed.cost
	CE.all.succ.suprise <- (revealed.CE.all.succ - expected.CE.all.succ) / revealed.CE.all.succ

	expected.CE <- dat$expected.val * dat$summed.expected.prob / dat$expected.cost
	revealed.CE <- dat$revealed.val * dat$summed.revealed.prob / dat$revealed.cost
	CE.surprise <- (revealed.CE - expected.CE) / revealed.CE


	expected.benCost <- dat$expected.val.all.success/dat$expected.cost
	revealed.benCost <- dat$revealed.val.all.success/dat$revealed.cost
	benCost.surprise<- (revealed.benCost - expected.benCost) / revealed.benCost

	pch.val <- '.'
	pch.val <- 20

	par(mfrow=c(4,2))

	# browser()
	# Plot the cost surprise
	plot(dat$expected.cost,dat$revealed.cost, pch=pch.val, main='true vs expected cost')
	abline(a=0, b=1, col='red')
	hist(cost.surprise, main=paste('Cost surprise, m=',round(mean(cost.surprise),2)), xlim=c(-0.3,0.3), breaks=40)
	abline(v=0, col='red')
	title( title.text, outer=TRUE, line=-1.5) 	
	
	# plot the benefit surprise all successeful
	plot(dat$expected.val.all.success,dat$revealed.val.all.success, pch=pch.val, main='true vs expeted benefit (all success)')
		# , xlim=c(0,2.5), ylim=c(0,2) )
	abline(a=0, b=1, col='red')
	hist(benefit.surprise.all.success, main=paste('Benefit surprise (all success), m=', round(mean(benefit.surprise.all.success),2)), xlim=c(-0.6,0.5), breaks=50)
	abline(v=0, col='red')

	# plot the benefit surprise
	plot(dat$expected.val,dat$revealed.val, pch=pch.val, main='true vs expeted benefit')
		# , xlim=c(0,2.5), ylim=c(0,2) )
	abline(a=0, b=1, col='red')
	hist(benefit.surprise, main=paste('Surprise in benefit m=', round(mean(benefit.surprise[which(benefit.surprise > -Inf)]),2)), breaks=50, xlim=c(-5,1))
	abline(v=0, col='red')

	# plot the prob success surprise
	# plot(dat$summed.expected.prob,dat$summed.revealed.prob, pch=pch.val, main='true vs expeted prob succ')
	# 	# , xlim=c(0,2.5), ylim=c(0,2) )
	# abline(a=0, b=1, col='red')
	# hist(summed.expected.prob.surprise, main='Surprise in prob success', breaks=50)
	# abline(v=0, col='red')


	# plot the cost efficiency
	# plot(expected.CE.all.succ, revealed.CE.all.succ, pch=pch.val, main='true vs expeted CE (all succ)')	
	# abline(a=0, b=1, col='red')
	# hist(CE.all.succ.suprise, breaks=30, xlim=c(-1.3, 0.8), main=paste('Surprise in CE (all suc) m=', round(mean(CE.all.succ.suprise),2) ) )
	# abline(v=0, col='red')

	plot(expected.CE, revealed.CE, pch=pch.val, main='true vs expeted CE')	
	abline(a=0, b=1, col='red')
	hist(CE.surprise, breaks=30, xlim=c(-5, 1), main=paste('Surprise in CE, m=', round(mean(CE.surprise[which(CE.surprise > -Inf)]),2) ) )
	abline(v=0, col='red')


	# plot the benefit: cost
	# plot(expected.benCost, revealed.benCost, pch=pch.val, main='true vs expeted benefit:cost')	
	# abline(a=0, b=1, col='red')
	# hist(benCost.surprise, breaks=30, xlim=c(-1, 1), main='Prop surprise in benefit:cost')
	# abline(v=0, col='red')


	# hist(benefit.surprise.all.success, breaks=30, main='Surprise in benefit if all successful')
	# abline(v=0, col='red')

	
	# correlation <- round(cor(benefit.surprise, cost.surprise),2)
	# tit <- paste('cost surp vs benefit surp (cor=', correlation, ')',sep='')
	# plot(benefit.surprise, cost.surprise, main=tit, pch=pch.val)
	# abline(h=0, col='red')
	# abline(v=0, col='red')

	# hist(benCost.surprise, breaks=50, main='Surprise in benefit/cost')
	# abline(v=0, col='red')

	# browser()


	# To save the calculated suprise values for futher use, add them to the
	# dataframe and return it.
	return( cbind(dat, summed.expected.prob.surprise, benefit.surprise.all.success, expected.benCost, revealed.benCost, 
		benefit.surprise, cost.surprise, benCost.surprise, expected.CE.all.succ, 
		revealed.CE.all.succ, CE.all.succ.suprise) )

}




plot.performance.vs.suprise <- function() {

	# Now make the plots of the performance versus surprise

	pch.val <- '.'
	pch.val <- 20

	plot( ppp$expected.cost, ppp$cost.surprise, col='red', pch=pch.val,
		xlim=c(2890100, 2.5e7), main='expected cost vs surprise')
	points( ran$expected.cost, ran$cost.surprise, col='black', pch=pch.val)
	legend('topright', c('ppp', 'random'), col=c('red','black'), pch=c(1,1) )
	abline(h=0, col='grey')
	title( 'PERFORMANCE VS SUPRISE: COST', outer=TRUE, line=-1.5) 	

	plot( ppp$expected.cost, ppp$cost.surprise, col='red', pch=pch.val,
		xlim=c(2890100, 2.5e7), main='expected cost vs surprise')
	points( opt$expected.cost, opt$cost.surprise, col='green', pch=pch.val)
	legend('topright', c('ppp', 'optimal'), col=c('red','green'), pch=c(1,1) )
	abline(h=0, col='grey')

	plot( ppp$revealed.cost, ppp$cost.surprise, col='red', pch=pch.val,
		xlim=c(2890100, 5.5e7), ylim=c(min(ran$cost.surprise), max(ran$cost.surprise)), 
		main='true cost vs surprise')
	points( ran$revealed.cost, ran$cost.surprise, col='black', pch=pch.val)
	legend('topright', c('ppp', 'random'), col=c('red','black'), pch=c(1,1) )
	abline(h=0, col='grey')

	plot( ppp$revealed.cost, ppp$cost.surprise, col='red', pch=pch.val,
		xlim=c(2890100, 5.5e7), ylim=c(min(opt$cost.surprise), max(opt.true$cost.surprise)), 
		main='true cost vs suprise')
	points( opt$revealed.cost, opt$cost.surprise, col='green', pch=pch.val)
	points( opt.true$revealed.cost, opt.true$cost.surprise, col='blue', pch=pch.val)
	legend('topright', c('ppp', 'optimal', 'true opt'), col=c('red','green', 'blue'), 
		pch=c(1,1,1) )
	abline(h=0, col='grey')


	xmin <- min(ppp$revealed.cost, ran$revealed.cost, opt$revealed.cost, opt.true$revealed.cost)
	xmax <- max(ppp$revealed.cost, ran$revealed.cost, opt$revealed.cost)
	ymin <- min(ppp$cost.surprise,ran$cost.surprise, opt$cost.surprise)
	ymax <- max(ppp$cost.surprise,ran$cost.surprise, opt$cost.surprise)

	plot( mean(ppp$revealed.cost), mean(ppp$cost.surprise), col='red', pch=1, 
		main='revealed benefit vs surprise in b', xlim=c(xmin, xmax), ylim=c(ymin,  ymax))
	points( mean(ran$revealed.cost), mean(ran$cost.surprise), col='black', pch=1)
	points( mean(opt$revealed.cost), mean(opt$cost.surprise), col='blue', pch=1)
	points( mean(opt.true$revealed.cost), mean(opt.true$cost.surprise), col='green', pch=1)

	legend('topright', c('ppp', 'random', 'opt', 'opt true'), col=c('red','black', 'blue', 'green'), pch=c(1,1,1,1) )
	abline(h=0, col='grey')




	par(mfrow=c(3,2))

	plot( ppp$expected.val, ppp$benefit.surprise, col='red', pch=pch.val, main='expected benefit vs surprise')
	points( ran$expected.val, ran$benefit.surprise, col='black', pch=pch.val)
	legend('topright', c('ppp', 'random'), col=c('red','black'), pch=c(1,1) )
	abline(h=0, col='grey')
	title( 'PERFORMANCE VS SUPRISE: BENEFIT', outer=TRUE, line=-1.5) 	

	plot( ppp$expected.val, ppp$benefit.surprise, col='red', pch=pch.val, main='expected benefit vs surprise')
	points( opt$expected.val, opt$benefit.surprise, col='green', pch=pch.val)
	legend('topright', c('ppp', 'opt'), col=c('red','green'), pch=c(1,1) )
	abline(h=0, col='grey')

	plot( ppp$revealed.val, ppp$benefit.surprise, col='red', pch=pch.val, main='revealed benefit vs surprise')
	points( ran$revealed.val, ran$benefit.surprise, col='black', pch=pch.val)
	legend('topright', c('ppp', 'random'), col=c('red','black'), pch=c(1,1) )
	abline(h=0, col='grey')

	plot( ppp$revealed.val, ppp$benefit.surprise, col='red', pch=pch.val, main='revealed benefit vs surprise')
	points( opt$revealed.val, opt$benefit.surprise, col='blue', pch=pch.val)
	points( opt.true$revealed.val, opt.true$benefit.surprise, col='green', pch=pch.val)

	legend('topright', c('ppp', 'opt', 'opt true'), col=c('red','blue', 'green'), pch=c(1,1,1) )
	abline(h=0, col='grey')

    my.mean = function(x, indices) {
        return( mean( x[indices] ) )
    }

	plot.segments <- function(x,y,col){

        # boostrap the 95 CIs on the estimations of the mean
        # see eg end of http://www.stat.wisc.edu/~larget/stat302/chap3.pdf

        x.boot = boot(x, my.mean, 1000)
        xbci <- boot.ci(x.boot)

        y.boot = boot(y, my.mean, 1000)
        ybci <- boot.ci(y.boot)

		#segments( mean(x)-sd(x), mean(y), mean(x)+sd(x), mean(y), col=col )
		#segments( mean(x), mean(y)-sd(y), mean(x), mean(y) + sd(y), col=col )

        # use normal
        # segments( xbci$normal[2], mean(y), xbci$normal[3], mean(y), col=col )
        # segments( mean(x), ybci$normal[2], mean(x), ybci$normal[3], col=col )

        # use BCa
        segments( xbci$bca[4], mean(y), xbci$bca[5], mean(y), col=col )
        segments( mean(x), ybci$bca[4], mean(x), ybci$bca[5], col=col )



        #browser()
	}
	plot.points.and.segments <- function(x, y, col){
		points( mean(x), mean(y), col=col, pch=1, cex=1.5)
		plot.segments(x, y, col)
	}

	

	par(mfrow=c(1,1))

	plot( mean(ppp$revealed.val), mean(ppp$benefit.surprise[which(ppp$benefit.surprise > -Inf)]),
		col='red', pch=1, xlab='revealed benefit', ylab='surprise in benefit', 
		xlim=c(1, 8.5), ylim=c(-4,  1), 
		# xlim=c(2.8, 6), ylim=c(-0.75,  0.05), 
		cex=1.5)
	plot.segments(ppp$revealed.val, ppp$benefit.surprise[which(ppp$benefit.surprise > -Inf)], col='red' )
	
	
	plot.points.and.segments(ran$revealed.val, ran$benefit.surprise[which(ran$benefit.surprise > -Inf)], col='black')
	plot.points.and.segments(opt$revealed.val, opt$benefit.surprise[which(opt$benefit.surprise > -Inf)], col='blue')
	plot.points.and.segments(opt.true$revealed.val, opt.true$benefit.surprise[which(opt.true$benefit.surprise > -Inf)], col='green')


	legend('topright', c('ppp', 'random', 'opt', 'opt true'), col=c('red','black', 'blue', 'green'), pch=c(1,1,1,1) )
	abline(h=0, col='grey')


	plot( ppp$revealed.val, ppp$benefit.surprise,
		col='red', pch=1, xlab='revealed benefit', ylab='surprise in benefit', 
		xlim=c(1, 8.5), ylim=c(-4,  1), 
		cex=1.5)

	# maybe do a 2D density plot?? See http://www2.warwick.ac.uk/fac/sci/moac/people/students/peter_cock/r/density/ 

	x <- ran$revealed.val[which(ran$benefit.surprise > -Inf)]
	y <- ran$benefit.surprise[which(ran$benefit.surprise > -Inf)]
	
	density <- kde2d(x,y, n=361)
	filled.contour(density, xlim=c(1, 8.5), ylim=c(-4,  1))
	 browser()

}

