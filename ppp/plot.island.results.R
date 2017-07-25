
# To run: 
#  source( 'plot.island.results.R' )

rm(list=ls()) 

source( 'plot.island.results.functions.R' )


# pdf( 'island.eradic.plots_nisl3_nsp3.pdf' )
# pdf( 'island.eradic.plots_nisl4_nsp4.pdf' )

# Plot the resuts for the 5 different strategies for selecting projects


p <- function(fname) {
	#x <- '/Users/ascelin/Google Drive/Papers/Optimizers curse/chris_matlab/'
	#x <- '/Users/ascelin/Dropbox/optimizers_curse/sims/Chris/'

	x <- '/Users/ascelin/Dropbox/optimizers_curse/sims/Chris/unif_dist/'
	return(paste(x,fname, sep=''))

}

# 

# ran <- read.data('random_allocation_nisl3_nsp3_rep1000.csv') # new 
#ran <- read.data('random_allocation_nisl3_nsp4_rep1000.csv')                   
ran <- read.data(p('random_allocation_nisl3_nsp4_rep1000.csv'))
ran <- plot.results(ran, 'RANDOM')

par(mfrow=c(3,2))
#ppp <- read.data(p('ppp_allocation_nisl3_nsp4_rep2500.csv'))
#ppp <- read.data(p('ppp_allocation_nisl3_nsp4_rep1000.csv'))
ppp <- read.data(p('ppp_allocation_nisl3_nsp4_rep1000.csv'))
ppp <- plot.results(ppp, 'PPP')


# par(mfrow=c(3,2))
#cb <- read.data(p('cost_ben_allocation_nisl3_nsp4_rep2500.csv'))
#cb <- read.data(p('cost_ben_allocation_nisl3_nsp4_rep1000.csv'))
# cb <- read.data(p('cost_ben_allocation_nisl3_nsp4_rep1000.csv'))
# cb <- plot.results(cb, 'COST:BEN')

par(mfrow=c(3,2))
#opt <- read.data(p('opt_allocation_nisl3_nsp4_rep2500.csv'))
#opt <- read.data(p('opt_allocation_nisl3_nsp4_rep1000.csv'))
opt <- read.data(p('opt_allocation_nisl3_nsp4_rep1000.csv'))
opt<- plot.results(opt, 'OPTIMAL')


# par(mfrow=c(3,2))
# opt.true <- read.data(p('opt_allocation_true_nisl3_nsp4_rep25000.csv'))
# opt.true <- read.data(p('opt_allocation_true_nisl4_nsp4_rep1000.csv'))
 opt.true <- read.data(p('opt_allocation_true_nisl3_nsp4_rep1000.csv'))
 opt.true<- plot.results(opt.true, 'TRUE OPTIMAL')



plot.performance.vs.suprise()



plot.suprise.dists <- function(x, name) {

	cat('\n **** Surprise results for', name, '****' ) 

	hist(x$benefit.surprise.all.success, breaks=30, xlab='Surprise in expected benefit', xlim=c(-2,1), main='')
	abline(v=0, col='red')
	title(  name, outer=TRUE, line=-1.2) 	
	cat('\n mean benefit suprise', mean(x$benefit.surprise.all.success), '+/-', sd(x$benefit.surprise.all.success))

	# hist(x$summed.expected.prob.surprise, breaks=30, main='Surprise in expected prob success', xlim=c(-0.3,0.3))
	# abline(v=0, col='red')
	# cat('\n mean expected.prob success suprise', mean(x$summed.expected.prob.surprise), '+/-',sd(x$summed.expected.prob.surprise) )

	hist(x$cost.surprise, breaks=30, xlab='Surprise in expected cost', xlim=c(-1,1), main='')
	abline(v=0, col='red')
	cat('\n mean cost suprise', mean(x$cost.surprise), '+/-', sd(x$cost.surprise))
	
	if(name=='ran') b=50 else b=10

	# hist(x$CE.all.succ.suprise, breaks=b, main='Surprise in Cost effectiveness', xlim=c(-1,4))
	# abline(v=0, col='red')
	# cat('\n mean CE suprise', mean(x$CE.all.succ.suprise), '+/-', sd(x$CE.all.succ.suprise))

	cat('\n')

}

par(mfrow=c(2,1))

# plot.suprise.dists(ran, 'Random')
# plot.suprise.dists(ppp, 'Cost effectiveness')
# plot.suprise.dists(opt, 'opt')
pch.val <- '.'
pch.val <- 20

plot( ppp$expected.benCost, ppp$benCost.surprise, col='red', pch=pch.val, main='expected b:c vs surprise in b:c')
points( ran$expected.benCost, ran$benCost.surprise, col='black', pch=pch.val)
legend('topright', c('ppp', 'random'), col=c('red','black'), pch=c(1,1) )
abline(h=0, col='grey')
title( 'PERFORMANCE VS SUPRISE: BENEFIT:COST', outer=TRUE, line=-1.5) 	

plot( ppp$expected.benCost, ppp$benCost.surprise, col='red', pch=pch.val, main='expected b:c vs surprise in b:c')
points( opt$expected.benCost, opt$benCost.surprise, col='green', pch=pch.val)
legend('topright', c('ppp', 'opt'), col=c('red','green'), pch=c(1,1) )
abline(h=0, col='grey')

plot( ppp$revealed.benCost, ppp$benCost.surprise, col='red', pch=pch.val, main='revealed b:c vs surprise in b:c')
points( ran$revealed.benCost, ran$benCost.surprise, col='black', pch=pch.val)
legend('topright', c('ppp', 'random'), col=c('red','black'), pch=c(1,1) )
abline(h=0, col='grey')

plot( ppp$revealed.benCost, ppp$benCost.surprise, col='red', pch=pch.val, main='revealed b:c vs surprise in b:c')
points( opt$revealed.benCost, opt$benCost.surprise, col='green', pch=pch.val)
legend('topright', c('ppp', 'opt'), col=c('red','green'), pch=c(1,1) )
abline(h=0, col='grey')




plot( ppp$expected.CE.all.succ, ppp$CE.all.succ.suprise, col='red', pch=pch.val, main='expected CE vs surprise in CE')
points( ran$expected.CE.all.succ, ran$CE.all.succ.suprise, col='black', pch=pch.val)
legend('topright', c('ppp', 'random'), col=c('red','black'), pch=c(1,1) )
abline(h=0, col='grey')
title( 'PERFORMANCE VS SUPRISE: CE', outer=TRUE, line=-1.5) 	

plot( ppp$expected.CE.all.succ, ppp$CE.all.succ.suprise, col='red', pch=pch.val, main='expected b:c vs surprise in b:c')
points( opt$expected.CE.all.succ, opt$CE.all.succ.suprise, col='green', pch=pch.val)
legend('topright', c('ppp', 'opt'), col=c('red','green'), pch=c(1,1) )
abline(h=0, col='grey')


plot( ppp$expected.val, ppp$benefit.surprise, col='red', pch=pch.val, main='Expected Ben vs surprise in Ben')
points( ran$expected.val, ran$benefit.surprise, col='black', pch=pch.val)

points( mean(ppp$expected.val), mean(ppp$benefit.surprise), col='blue', pch=20)
points( mean(ran$expected.val), mean(ran$benefit.surprise), col='pink', pch=20)

legend('topright', c('ppp', 'random'), col=c('red','black'), pch=c(1,1) )
abline(h=0, col='grey')
title( 'PERFORMANCE VS SUPRISE: CE', outer=TRUE, line=-1.5) 	

plot( ppp$expected.val, ppp$benefit.surprise, col='red', pch=pch.val, main='Expected Ben vs surprise in Ben')
points( opt$expected.val, opt$benefit.surprise, col='green', pch=pch.val)
points( mean(ppp$expected.val), mean(ppp$benefit.surprise), col='blue', pch=40)
points( mean(opt$expected.val), mean(opt$benefit.surprise), col='black', pch=40)

legend('topright', c('ppp', 'opt'), col=c('red','green'), pch=c(1,1) )
abline(h=0, col='grey')


# dev.off()