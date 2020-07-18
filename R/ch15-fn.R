# [Ch-15 Functions] ----------------------------------------------------------------------------------
# source("E:/R-stat/Digeng/ch15-function.txt")

# [Ch-15 Function Manual] -----------------------------------------

#' @title Manual for Ch15. Functions
#' @description Ch15. Nonparametric Methods
#' @param fn Function number (1,2,31,32,4,51,52,61,62,7,8), Default: 0
#' @return None.
#' 
#' @examples 
#' ch15.man()
#' ch15.man(2)
#' ch15.man(51)
#' @rdname ch15.man
#' @export 
ch15.man = function(fn=0) {
    if (0 %in% fn) {
	cat("[1] norm.diag\t\tInvestigate the Normality of Data\n")
	cat("[2] signtest.plot  \tSign Test (Binomial Test)\n")
	cat("[31] runs.dist\t\tDistribution of Run Test Statistic\n")
	cat("[32] runstest.plot  \tRun Test with a Plot\n")
	cat("[4] corr.spear\t\tPearson Correlation Coefficient & Spearman Correlation Coefficient\n")
	cat("[51] ranksum.dist \tDistribution of Wilcoxon Rank Sum Test Statistic\n")
	cat("[52] ranksum.plot \tWilcoxon Rank Sum Test\n")
	cat("[61] signrank.dist \tDistribution of Wilcoxon Signed Rank Test Statistic\n")
	cat("[62] signrank.plot \tWilcoxon Signed Rank Test\n")
	cat("[7] kruswall.plot   \tKruskal Wallis Test\n")
	cat("[8] friedman.plot \tFriedman Test\n")
    }
    if (1 %in% fn) {
	cat("[1] Investigate the Normality of Data\n")
	cat("norm.diag(x, xrng, by=1, dig=4, dc=c(\"cyan\", 2, 4))\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Data vector\n")
	cat("[Optional Input]--------------------------\n")
	cat("xrng\t Range of x-axis (default=mean \U00B1 3 \U00D7 stdev)\n")
	cat("by\t Histogram class interval (default=1)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("dc\t Color vector (default=c(\"cyan\", 2, 4))\n")
    }
    if (2 %in% fn) {
	cat("[2] Sign Test (Binomial Test)\n")
	cat("signtest.plot(x, mu0=0, side=\"two\", dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Data vector\n")
	cat("[Optional Input]--------------------------\n")
	cat("mu0\t Mean value under the null hypothesis (default=0)\n")
	cat("side\t Type of alternative hypothesis (default=\"two\")\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (31 %in% fn) {
	cat("[3-1] Distribution of Runs Test Statistic\n")
	cat("require(randomizeBE)\n")
	cat("runs.dist(n1=2:20, n2=2:20, alp=0.05, tab=TRUE, side=\"two\", plot=FALSE)\n")
	cat("[Optional Input]--------------------------\n")
	cat("n1\t Number of data in group 1 (default=2:20)\n")
	cat("n2\t Number of data in group 2 (default=2:20)\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("tab\t Logical value for printing critical value table (default=TRUE)\n")
	cat("side\t Type of alternative hypothesis (default=\"two\")\n")
	cat("plot\t Logical value for plotting run distribution (default=FALSE)\n\n")
    }
    if (32 %in% fn) {
	cat("[3-2] Runs Test\n")
	cat("require(randomizeBE)\n")
	cat("runstest.plot(x, n1, n2, alp=0.05, side=\"two\", dig=4, plot=TRUE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Data vector (or number of runs)\n")
	cat("[Optional Input]--------------------------\n")
	cat("n1\t Number of data in group 1 (required if raw data are not given)\n")
	cat("n2\t Number of data in group 2 (required if raw data are not given)\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("side\t Type alternative hypothesis (default=\"two\")\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("plot\t Logical value for plotting run test results (default=TRUE)\n")
    }
    if (4 %in% fn) {
	cat("[4] Pearson Correlation Coefficient & Spearman Correlation Coefficient\n")
	cat("corr.spear(x, y, r0=0, xl, yl, mt, step=1:2, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Vector of x-data\n")
	cat("y\t Vector of y-data\n")
	cat("[Optional Input]--------------------------\n")
	cat("r0\t Correlation coefficient value under the null hypothesis\n")
	cat("xl\t Name of x-data\n")
	cat("yl\t Name of y-data\n")
	cat("mt\t Title of scatter plot\n")
	cat("step\t Steps of the analysis (default =1:2)\n")
	cat("\t 1\t Pearson correlation coefficient, correlation test, and the confidence interval\n")
	cat("\t 2\t Spearman correlation coefficient, correlation test, and the confidence interval\n")
	cat("\t 3\t Scatter plot\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (51 %in% fn) {
	cat("[5-1] Distribution of Wilcoxon Rank Sum Test Statistic\n")
	cat("ranksum.dist(n1, n2=3:10, tab=TRUE, plot=FALSE, dig=4)\n")
	cat("[Optional Input]--------------------------\n")
	cat("n1\t Number of data in group 1 (default=1:min(n2))\n")
	cat("n2\t Number of data in group 2 (default=3:10)\n")
	cat("tab\t Logical value for printing critical value table (default=TRUE)\n")
	cat("plot\t Logical value for plotting rank sum distribution (default=FALSE)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n\n")
    }
    if (52 %in% fn) {
	cat("[5-2] Wilcoxon Rank Sum Test\n")
	cat("ranksum.plot(x, y, side=\"two\", xlab=\"Rank Sum statistic\", dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Data vector in group 1\n")
	cat("y\t Data vector in group 2\n")
	cat("[Optional Input]--------------------------\n")
	cat("side\t Type of alternative hypothesis (default=\"two\")\n")
	cat("xlab\t Label of x-axis (default=\"Rank Sum statistic\")\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (61 %in% fn) {
	cat("[6-1] Distribution of Wilcoxon Signed Rank Test Statistic\n")
	cat("signrank.dist(nv=5:50, av, tab=TRUE, plot=FALSE, dig=4)\n")
	cat("[Optional Input]--------------------------\n")
	cat("nv\t Number of signed data (default=5:50)\n")
	cat("av\t Probability vector (default=c(0.005, 0.01, 0.025, 0.05, 0.95, 0.975, 0.99, 0.995))\n")
	cat("tab\t Logical value for printing quantile table (default=TRUE)\n")
	cat("plot\t Logical value for plotting test statistic distribution (default=FALSE)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n\n")
    }
    if (62 %in% fn) {
	cat("[6-2] Wilcoxon Signed Rank Test\n")
	cat("signrank.plot(x, y, mu0=0, side=\"two\", xlab=\"Signed Rank Sum\", dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Data vector in group 1\n")
	cat("y\t Data vector in group 2\n")
	cat("[Optional Input]--------------------------\n")
	cat("mu0\t Mean difference under the null hypothesis (default=0)\n")
	cat("side\t Type of alternative hypothesis (default=\"two\")\n")
	cat("xlab\t Label of x-axis (default=\"Signed Rank Sum\")\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (7 %in% fn) {
	cat("[7] Kruskal Wallis Test\n")
	cat("kruswall.plot(x, y, dig=4, plot=FALSE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Data vector\n")
	cat("y\t Vector of factor levels\n")
	cat("[Optional Input]--------------------------\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("plot\t Logical value for plotting test results (default=FALSE)\n")
    }
    if (8 %in% fn) {
	cat("[8] Friedman Test\n")
	cat("friedman.plot(x, a, b, dig=4, plot=FALSE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Data vector\n")
	cat("a\t Vector of factor levels\n")
	cat("b\t Vector of block levels\n")
	cat("[Optional Input]--------------------------\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("plot\t Logical value for plotting test results (default=FALSE)\n")
    }
}

# [15-1] Investigate the Normality of Data
#' @title Diagnosis of Normality
#' @description Investigate the Normality of Data
#' @param x Data vector
#' @param xrng Range of x-axis, Default: c(mean-3stdev, mean+3stdev)
#' @param by Histogram class interval, Default: 1
#' @param dig Number of digits below the decimal point, Default: 4
#' @param dc Color vector, Default: c("cyan", 2, 4)
#' @return None.
#' 
#' @examples 
#' (x = c(1,2,5,7, rep(8,7), rep(9,5), rep(10,4)))
#' norm.diag(x)
#' @rdname norm.diag
#' @export
norm.diag = function(x, xrng, by=1, dig=4, dc=c("cyan",2,4)) {
    # Set the range of x-axis
	xm = mean(x)
	xs = sd(x)
	if (missing(xrng)) {
		x1 = min(x, xm-3*xs)
		x2 = max(x, xm+3*xs)
		xrng = c(x1, x2)
	}
    # Diagnosis histogram
	win.graph(8, 4)
	par(mfrow=c(1,2))
	hist(x, prob=T, breaks=seq(xrng[1], xrng[2], by=by), col=dc[1])
	lines(density(x), lwd=2, col=dc[2])
	xax = seq(xrng[1], xrng[2], length=100)
	lines(xax, dnorm(xax, xm, xs), lwd=2, col=dc[3])
    # Normal probability plot
	qqnorm(x, pch=19, xlab="Theoretical Quantile", ylab="Sample Quantile")
	grid(col=3)
	qqline(x, col=dc[2])
    # Test of normality (Shapiro-Wilk's Test) 
	shap = shapiro.test(x)
	cat("Normality Test (Shapiro-Wilk's Test) -----\n")
	cat("Test Statistic =", round(shap$stat, dig), "\t P-value =", shap$p.val, "\n")
}

# [15-2] Sign Test (Binomial Test)
#' @title Sign Test (Binomial Test)
#' @description Sign Test (Binomial Test) with a Plot
#' @param x Data vector
#' @param mu0 Mean value under the null hypothesis, Default: 0
#' @param side Type of alternative hypothesis, Default: 'two'
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' (x = c(1,2,5,7, rep(8,7), rep(9,5), rep(10,4)))
#' signtest.plot(x=x, mu0=7, side="up")
#' @rdname signtest.plot
#' @export 
signtest.plot = function(x, mu0=0, side="two", dig=4) {
	d = x-mu0
    # Number of data greater than mu0
	np = sum(d > 0)
	n = sum(d != 0)
	pv1 = 1-pbinom(np-1, n, 0.5)
	pv2 = pbinom(np, n, 0.5)
	cat("Total number of data =", length(x), "\n")
	cat("Number of data except", mu0, "=", n, "\n")
	cat("Number of data greater than", mu0, "=", np, "\n")
    # Normal approximation method (continuity correction)
	mu = n/2
	sigsq = n/4
	sig = sqrt(sigsq)
	apv1 = pnorm(np-0.5, mu, sig,lower.tail=F)
	apv2 = pnorm(np+0.5, mu, sig,lower.tail=T)
    # Calculate and print p-value
	if (any(grepl(side, c("up", "greater")))) {
		pv = pv1
		apv = apv1
	} else if (any(grepl(side, c("low", "less")))) {
		pv = pv2
		apv = apv2
	} else {
		pv = 2*min(pv1, pv2)
		apv = 2*min(apv1, apv2)
	}
	cat("Exact p-value (binomial distribution) =", round(pv, dig), "\n")
	cat("Normal approx. p-value (continuity correction) =", round(apv, dig), "\n")
    # Plot distribution of the test statistic
	xa = 0:n
	xca = (0:(10*n))/10
	pdf = dbinom(xa, n, 0.5)
	ymax = max(pdf)*1.05
	ymin = -0.1*max(pdf)
	win.graph(7,5)
	plot(xa, pdf, type="n", xlab="Sign Statistic (positive sign)", ylab="f(x)", ylim=c(ymin, ymax),
		main=paste0("Distribution of Sign Teat Statistic (n=", n, ")"))
      # Normal approximation
	lines(xca, dnorm(xca, mu, sig), col=4)
	abline(h=0)
      # Distribution of the test statistic
	lines(xa, dbinom(xa, n, 0.5), type="h", lwd=7, col=grey(0.5))
      # Central location
	segments(mu, 0, mu, dnorm(mu, mu, sig), lty=2, col=2)
	text(mu, ymin/2, labels=mu, col=4)
      # Critical value
	segments(np, 0, np, dbinom(np, n, 0.5), lwd=2, col=2)
	text(np, dbinom(np, n, 0.5), labels=np, col=2, pos=3)
      # P-value
	if (any(grepl(side, c("up", "greater")))) {
		lines(np:n, dbinom(np:n, n, 0.5),  type="h", col=2, lwd=7)
		text(n, ymin/2, labels=paste0("pv=", round(pv, 4)), col=4, pos=2)
	} else if (any(grepl(side, c("low", "less")))) {
		lines(0:np, dbinom(0:np, n, 0.5),  type="h", col=2, lwd=7)
		text(0, ymin/2, labels=paste0("pv=", round(pv, 4)), col=4, pos=4)
	} else {
		lines(np:n, dbinom(np:n, n, 0.5),  type="h", col=2, lwd=7)
		lines(0:(n-np), dbinom(0:(n-np), n, 0.5),  type="h", col=2, lwd=7)
		text(n, ymin/2, labels=paste0("pv1=", round(pv/2, 4)), col=4, pos=2)
		text(0, ymin/2, labels=paste0("pv2=", round(pv/2, 4)), col=4, pos=4)
	}
}

# [15-3] Run Test
# Calculate the critical values of run test (two sided) ---------------------------------
cvruns.exact = function (alpha=0.05, n1, n2) {
	nmin = min(n1, n2)
	nmax = max(n1, n2)
	rmax = ifelse(n1 == n2, 2 * n1, 2 * min(n1, n2) + 1)
    # Lower critical value
	pv1 = pruns.exact(2, n1, n2)
	if (pv1 > alpha) {
		cr1 = NA
	} else {
		for (r in 2:nmin) 	{	
			cp = pruns.exact(r, n1, n2)
			if (cp <= alpha) cr1 = r 	}
	}
    # Upper critical value
	pv2 = pruns.exact(rmax, n1, n2)
	if (pv2 > alpha) {
		cr2 = NA
	} else {
		for (r in rmax:nmax) 	{	
			cp = pruns.exact(r, n1, n2)
			if (cp <= alpha) cr2 = r 	}
	}  
	return(list(lcr=cr1, ucr=cr2))
}
# Probability mass function of run test statistic -------------------------------------
druns.exact = function (r, n1, n2)
{	if (r <= 1) stop("Number of runs must be >1")
	pv1 = ifelse(r<=2, 0, pruns.exact(r-1, n1, n2, tail="lower"))
	pruns.exact(r, n1, n2, tail="lower") - pv1
}
# Vectorized probability mass function ------------------------------------
Vdruns.exact = Vectorize(druns.exact, "r")

# [15-31] Distribution of Run Test Statistic ------------------------------------
#' @title Distribution of Runs
#' @description Distribution of Runs Test Statistic
#' @param n1 Number of data in group 1, Default: 2:20
#' @param n2 Number of data in group 2, Default: 2:20
#' @param alp Level of significance, Default: 0.05
#' @param tab Print critical value table? Default: TRUE
#' @param side Type of alternative hypothesis, Default: 'two'
#' @param plot Plot run distribution? Default: FALSE
#' @return None.
#' 
#' @examples 
#' require(randomizeBE)
#' runs.dist(n1=2:10, n2=2:10)
#' runs.dist(n1=c(5,20), n2=c(5,20), tab=FALSE, plot=TRUE)
#' @rdname runs.dist
#' @export
runs.dist = function(n1=2:20, n2=2:20, alp=0.05, tab=TRUE, side="two", plot=FALSE) {
    # Critical value table of run test
	nn1 = length(n1)
	nn2 = length(n2)
	lcv = ucv = array(NA, dim=c(nn1, nn2))
	rownames(lcv) = rownames(ucv) = n1
	colnames(lcv) = colnames(ucv) = n2
    # Calculate critical values
	alpha = ifelse (grepl(side, "two- sided"), alp, 2*alp)
	for (k1 in 1:nn1) {	
		for (k2 in 1:nn2) {
			temp = cvruns.exact(alpha, n1[k1], n2[k2])
			lcv[k1, k2] = temp$lcr
			ucv[k1, k2] = temp$ucr
		}
	}
      if (tab) {
	if (any(grepl(side, c("low", "less")))) {
		cat("Lower (one-sided) Critical Value (Level of significance =", 100*alp, "%) ----------\n")
		print(lcv)
	} else if (any(grepl(side, c("up", "greater")))) {
		cat("Upper (one-sided) Critical Value (Level of significance =", 100*alp, "%) ----------\n")
		print(ucv)
	} else {
		cat("Lower (two-sided) Critical Value (Level of significance =", 100*alp, "%) ----------\n")
		print(lcv)
		cat("Upper (two-sided) Critical Value (Level of significance =", 100*alp, "%) ----------\n")
		print(ucv)
	}
       }
    # Plot the Distribution of Run Test Statistic
      if (plot) {
	mm = max(nn1, nn2)
	if (nn1==1) n1=rep(n1, mm)
	if (nn2==1) n2=rep(n2, mm)
	wc=c(1,2,3,2,3,3,4,4,3,4)
	wr=c(1,1,1,2,2,2,2,2,3,3)
	ww=c(4,6,9,7,9,9,9,9,9,9)
	wl=c(3,3,3,6,6,6,6,6,9,9)
	win.graph(ww[mm], wl[mm])
	par(mfrow=c(wr[mm], wc[mm]))

	for (k in 1:mm) {
  		rmax = ifelse(n1[k] == n2[k], 2 * n1[k], 2 * min(n1[k], n2[k]) + 1)
    		x = 2:rmax
    		plot(x, Vdruns.exact(x,n1[k],n2[k]), type = "h", lwd=4, col=2, ylab="f(x)",
       			main = paste0("Runs PDF (n1 = ", n1[k], ", n2 =", n2[k], ")"))  
	}
      }
}

# [15-32] Runs Test
#' @title Runs Test
#' @description Runs Test with a Plot
#' @param x Data vector (or number of runs)
#' @param n1 Number of data in group 1 (required if raw data are not given)
#' @param n2 Number of data in group 2 (required if raw data are not given)
#' @param alp Level of significance, Default: 0.05
#' @param side Type of alternative hypothesis, Default: 'two'
#' @param dig Number of digits below the decimal point, Default: 4
#' @param plot Plot runs test results? Default: TRUE
#' @return None.
#' 
#' @examples 
#' require(randomizeBE)
#' x = c(1,1,0,0,1,0,rep(1,7), rep(0,7))
#' runstest.plot(x)
#'
#' x = rep(0, 50)
#' x[c(1:2, 8:10, 17:18, 21, 26:27, 29:31, 36:37, 41:44, 49)] = 1
#' runstest.plot(x)
#' @rdname runstest.plot
#' @export
runstest.plot = function(x, n1, n2, alp=0.05, side="two", dig=4, plot=TRUE) {
    # Calculate the number of runs
	nn = length(x)
	if (nn>1) {
		dval = sort(unique(x))
		n1 = sum(x==dval[1])
		n2 = sum(x==dval[2])
		r1 = length(rle(x)[[1]])
	} else r1 = x
    # Normal approximation
	mu = 2*n1*n2/(n1+n2)+1
	vr = 2*n1*n2*(2*n1*n2-n1-n2)/(n1+n2)^2/(n1+n2-1)
    # Calculate p-value w.r.t the type of alternative hypothesis
	if (any(grepl(side, c("low", "less")))) {
		pv = pruns.exact(r1, n1, n2, tail="lower")
		h1 = "One-sided (positive correlation)"
		area = 2:r1
		xpt = r1
		pos = 2
		z0 = (r1+0.5-mu)/sqrt(vr)
		pv2 = pnorm(z0)
		ppv = pv
	} else if (any(grepl(side, c("up", "greater")))) {
		pv = pruns.exact(r1, n1, n2, tail="upper")
		h1 = "One-sided (negative correlation)"
		area = r1:(n1+n2)
		xpt = r1
		pos = 4
		z0 = (r1-0.5-mu)/sqrt(vr)
		pv2 = 1 - pnorm(z0)
		ppv = pv
	} else {
		pv = pruns.exact(r1, n1, n2, tail="2-sided")
		h1 = "Two-sided"
		r2 = mu + (mu-r1)
		area = c(2:min(r1,r2), max(r1,r2):(n1+n2))
		xpt = sort(c(r1, r2))
		pos = c(2, 4)
		z0 = ifelse(r1<mu, (r1+0.5-mu)/sqrt(vr), (r1-0.5-mu)/sqrt(vr))
		pv2 = 2*pnorm(-abs(z0))
		plow = pruns.exact(min(r1,r2), n1, n2, tail="lower")
		pupp = pruns.exact(max(r1,r2), n1, n2, tail="upper")
		ppv = c(plow, pupp)
	}
    # Print test results -------------------------------
	cat(paste0("Runs test (n1=", n1, ",  n2=", n2, ") : ", h1), "\n")
	cat("Number of Runs =", r1, "\t p-value =", round(pv, dig), "\n")
	cat("E(R) =", round(mu, dig), "\t\t Var(R) =", round(vr, dig), "\n")
	cat("Normal appr. (cont. corr.) \t Z0 =", 
		round(z0, dig), "\t p-value =", round(pv2, dig), "\n")
    # Plot distribution
      if (plot) {
	xa = 2:(n1+n2)
	mt = paste0("Runs Test (n1=", n1, ",  n2=", n2, ") : ", h1)
	ya = Vdruns.exact(xa, n1, n2)
	ymax = max(ya, dnorm(mu, mu, sqrt(vr)))
	win.graph(7,5)
	plot(xa, ya, type="h", lwd=5, ylab="f(r)", xlab="Number of Runs",
		ylim=c(0, ymax), main=mt, col=grey(0.5))
	xa2 = seq(2, n1+n2, length=100)
	lines(xa2, dnorm(xa2, mu, sqrt(vr)), lty=1, col="green4")
	lines(area, Vdruns.exact(area, n1, n2), type="h", lwd=5, col=2)
	text(xpt, druns.exact(r1,n1,n2), labels=round(ppv, 4), col=4, pos=pos)
      }
}

# [15-4] Pearson Correlation Coefficient & Spearman Correlation Coefficient
#' @title Pearson & Spearman Correlation Coefficient
#' @description Pearson Correlation Coefficient & Spearman Correlation Coefficient
#' @param x Vector of x-data
#' @param y Vector of y-data
#' @param r0 Correlation coefficient value under the null hypothesis, Default: 0
#' @param xl Name of x-data
#' @param yl Name of y-data
#' @param mt Title of scatter plot
#' @param step Steps of the analysis, Default: 1:2
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' x = c(10,7,0,1,5, 2,8,6,4,9, 3,0,2,4,6, 8)
#' y = c(75,77,91,64,79, 81,90,86,82,76, 89,93,80,87,83, 78)
#' corr.spear(x, y, r0=0, xl="Play", yl="Score", step=1:3)
#' @rdname corr.spear
#' @export
corr.spear = function(x, y, r0=0, xl, yl, mt, step=1:2, alp=0.05, dig=4) {
    # Labels & simple regression
	if (missing(xl)) xl = deparse(substitute(x))
	if (missing(yl)) yl = deparse(substitute(y))
	nn = length(x)
	lm1 = lm(y~x)
    # Pearson correlation coefficient by cor( ) function
	Sxx = sum(x^2)-sum(x)^2/nn
	Syy = sum(y^2)-sum(y)^2/nn
	Sxy = sum(x*y)-sum(x)*sum(y)/nn
	rxy = Sxy/sqrt(Sxx*Syy)
      if (1 %in% step) {
	cat("[Step 1-1] Pearson Correlation Coefficient ------------------------\n")
	cat(paste0("Sxx = ", round(sum(x^2), dig), " - ", round(sum(x), dig), "\U00B2 / ", nn,
		" = ", round(Sxx, dig)), "\n")
	cat(paste0("Syy = ", round(sum(y^2), dig), " - ", round(sum(y), dig), "\U00B2 / ", nn,
		" = ", round(Syy, dig)), "\n")
	cat(paste0("Sxy = ", round(sum(x*y), dig), " - (", round(sum(x), dig), " \U00D7 ", 
		round(sum(y), dig), ") / ", nn, " = ", round(Sxy, dig)), "\n")
	cat(paste0("Corr(x,y) = ", round(Sxy, dig), " / \U221A(", 
		round(Sxx, dig), " \U00D7 ", round(Syy, dig), ") = ", round(rxy, dig)), "\n")
      # Correlation test by cor.test( ) function
	ct = cor.test(x, y, conf.level =1-alp)
	T0 = rxy*sqrt((nn-2)/(1-rxy^2))
	pv0 = 2*pt(-abs(T0), nn-2)
	cat("[Step 1-2] Pearson Correlation Test ------------------------\n")
          if (r0==0) {
	cat(paste0("T-stat = ", round(rxy, dig), " \U00D7 \U221A(", nn-2, " / (1 - ", 
		round(abs(rxy), dig), "\U00B2)) = ", round(ct$stat, dig), "\n"))
	cat(paste0("P-value = P(|T", nn-2, "| > ", round(abs(T0), dig), ") = ", round(pv0, dig), "\n"))
          } else {
	Z0 = sqrt(nn-3)*0.5*(log((1+rxy)/(1-rxy))-log((1+r0)/(1-r0)))
	pv0 = 2*pnorm(-abs(Z0))
	cat(paste0("Z-statistic = \U221A(", nn-3, ") \U00D7 0.5 \U00D7 (log((1+", round(rxy, dig), 
		")/(1-", round(rxy, dig), ")) - log((1+", r0, ")/(1-", r0, "))) = ", round(Z0, dig), "\n"))
	cat(paste0("P-value = P(|Z| > ", round(abs(Z0), dig), ") = ", round(pv0, dig), "\n"))
          }
      # Confidence interval
	cat(paste0(100*(1-alp), "% Confidence Interval = [", 
		round(ct$conf[1], dig), ", ", round(ct$conf[2], dig), "]\n"))
      }
    # Spearman correlation coefficient ------------------------------------------
	x2 = rank(x)
	y2 = rank(y)
	lm2 = lm(y2~x2)
	Sxx2 = sum(x2^2)-sum(x2)^2/nn
	Syy2 = sum(y2^2)-sum(y2)^2/nn
	Sxy2 = sum(x2*y2)-sum(x2)*sum(y2)/nn
	rxy2 = Sxy2/sqrt(Sxx2*Syy2)
      if (2 %in% step) {
	cat("[Step 2-1] Spearman correlation coefficient ------------------------\n")
	cat(paste0("Srx.x = ", round(sum(x2^2), dig), " - ", round(sum(x2), dig), "\U00B2 / ", nn,
		" = ", round(Sxx2, dig)), "\n")
	cat(paste0("Sry.y = ", round(sum(y2^2), dig), " - ", round(sum(y2), dig), "\U00B2 / ", nn,
		" = ", round(Syy2, dig)), "\n")
	cat(paste0("Srx.y = ", round(sum(x2*y2), dig), " - (", round(sum(x2), dig), " \U00D7 ", 
		round(sum(y2), dig), ") / ", nn, " = ", round(Sxy2, dig)), "\n")
	cat(paste0("Corr(rx,ry) = ", round(Sxy2, dig), " / \U221A(", 
		round(Sxx2, dig), " \U00D7 ", round(Syy2, dig), ") = ", round(rxy2, dig)), "\n")
      # Correlation test by cor.test( ) function
	ct2 = cor.test(x2, y2, conf.level =1-alp)
	T02 = rxy2*sqrt((nn-2)/(1-rxy2^2))
	pv02 = 2*pt(-abs(T02), nn-2)
	cat("[Step 2-2] Spearman correlation test ------------------------\n")
          if (r0==0) {
	cat(paste0("T-stat = ", round(rxy2, dig), " \U00D7 \U221A(", nn-2, " / (1 - ", 
		round(abs(rxy2), dig), "\U00B2)) = ", round(ct2$stat, dig), "\n"))
	cat(paste0("P-value = P(|T", nn-2, "| > ", round(abs(T02), dig), ") = ", round(pv02, dig), "\n"))
          } else {
	Z02 = sqrt(nn-3)*0.5*(log((1+rxy2)/(1-rxy2))-log((1+r0)/(1-r0)))
	pv02 = 2*pnorm(-abs(Z02))
	cat(paste0("Z-statistic = \U221A(", nn-3, ") \U00D7 0.5 \U00D7 (log((1+", round(rxy2, dig), 
		")/(1-", round(rxy2, dig), ")) - log((1+", r0, ")/(1-", r0, "))) = ", round(Z02, dig), "\n"))
	cat(paste0("p-value = P(|Z| > ", round(abs(Z02), dig), ") = ", round(pv02, dig), "\n"))
          }
      # Confidence interval
	cat(paste0(100*(1-alp), "% Confidence Interval = [", 
		round(ct2$conf[1], dig), ", ", round(ct2$conf[2], dig), "]\n"))
      }
     # Scatter plot -----------------------------------------
      if (3 %in% step) {
	cat("[Step 3] Scatter plot ------------------------\n")
	if (missing(mt)) mt = paste("Scatter Plot of", yl, "vs.", xl)
	win.graph(8,4)
	par(mfrow=c(1,2))
	y11 = floor(min(y, lm1$fit))
	y12 = ceiling(max(y, lm1$fit))
	plot(x, y, pch=19, main=mt, xlab=xl, ylab=yl, ylim=c(y11, y12))
	grid(col=3)
	abline(lm1, col=2)
	
	mt2 = paste0("Scatter Plot of r(", yl, ") vs. r(", xl, ")")
	y21 = floor(min(y2, lm2$fit))
	y22 = ceiling(max(y2, lm2$fit))
	plot(x2, y2, pch=19, main=mt2, xlab=paste0("r(", xl, ")"), ylab=paste0("r(", yl, ")"), ylim=c(y21, y22))
	grid(col=3)
	abline(lm2, col=2)
      }	
}

# [15-51] Distribution of Wilcoxon Rank Sum Test Statistic
#' @title Distribution of Wilcoxon Rank Sum
#' @description Distribution of Wilcoxon Rank Sum Test Statistic
#' @param n1 Number of data in group 1 (default=1:min(n2))
#' @param n2 Number of data in group 2, Default: 3:10
#' @param tab Print critical value table? Default: TRUE
#' @param plot Plot rank sum distribution? Default: FALSE
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' ranksum.dist(n2=3:5)
#' ranksum.dist(n1=c(2,4,6,8), n2=10, tab=FALSE, plot=TRUE)
#' @rdname ranksum.dist
#' @export
ranksum.dist = function(n1, n2=3:10, tab=TRUE, plot=FALSE, dig=4) {
    # Distribution table of Wilcoxon rank sum test statistic 
      if (tab) {
      # pwilcox( ) function --> cumulative distribution
	for (k2 in n2) { nu = floor(k2^2/2) + 1
		pv = array(NA, dim=c(nu, k2))
		colnames(pv) = paste0("n1=", 1:k2)
		rownames(pv) = paste0("U=", 0:(nu-1))
		for (k1 in 1:k2) pv[,n1] = pwilcox(0:(nu-1), k1, k2)
		cat(paste0("n2=", k2))
		print(round(pv, dig)) 	
	}
       }
    # Plot the distribution
      if (plot) {
	win.graph(7, 5)
	if (missing(n1)) stop("Input for n1 is required for plotting...")
	if (missing(n2)) stop("Input for n2 is required for plotting...")
	nn = max(length(n1), length(n2))
	if (length(n1)==1) n1=rep(n1, nn)
	if (length(n2)==1) n2=rep(n2, nn)

	if (nn<6) { dcol=c(1, 2, "green4", 4, "purple", 6)
	} else dcol=rainbow(nn)
	lab = rep("", nn)

	xa = 0:(max(n1)*max(n2))
	plot(xa, dwilcox(xa, min(n1), min(n2)), type="n", main="Wilcoxon Rank Sum Distribution",
		lwd=2, xlab="Rank Sum Statistic(U)", ylab="f(u)")
	for (k in 1:nn) {
  	      lines(xa, dwilcox(xa, n1[k], n2[k]), type="s", lwd=2, col=dcol[k])
	      lab[k] = paste0("(n1,n2)=(", n1[k], ",", n2[k],")")
	}
	legend("topright", lab, lwd=2, col=dcol[1:nn], text.col=dcol[1:nn])
      }
}

# [15-52] Wilcoxon Rank Sum Test
#' @title Wilcoxon Rank Sum Test
#' @description Wilcoxon Rank Sum Test with a Plot
#' @param x Data vector in group 1
#' @param y Data vector in group 2
#' @param side Type of alternative hypothesis, Default: 'two'
#' @param xlab Label of x-axis, Default: 'Rank Sum statistic'
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' x = c(1,5,7,8,8,8,9)
#' y = c(2,8,8,8,8,9,9,9,9,10,10,10,10)
#' ranksum.plot(x, y)
#' @rdname ranksum.plot
#' @export 
ranksum.plot = function(x, y, side="two", xlab="Rank Sum statistic", dig=4) {
    # Test statistic
	n1 = length(x); n2 = length(y)
	U1 = sum(rank(c(x,y))[1:n1]) - n1*(n1+1)/2
	U2 = sum(rank(c(x,y))[(n1+1):(n1+n2)]) -n2*(n2+1)/2
	pv1 = pwilcox(U2, n1, n2)
	pv2 = pwilcox(U1, n1, n2)
	U = min(U1, U2)
	pv = 2*pwilcox(U, n1, n2)
  	cat(paste0("n1=",n1,"\t n2=",n2,"\t U1=",U1,"\t U2=",U2), "\n")
    # Normal approximation method (without continuity correction)
	mu = n1*n2/2
	sigsq = n1*n2*(n1+n2+1)/12
	sig = sqrt(sigsq)
    # P-value
	if (any(grepl(side, c("up", "greater")))) {
		pv = pv1
		apv = pnorm(U2, mu, sig)
		Z0 = (U2 - mu)/sig
		cat(paste0("U-stat=",U2),"\t ")
	} else if (any(grepl(side, c("low", "less")))) {
		pv = pv2
		apv = pnorm(U1, mu, sig)
		Z0 = (U1 - mu)/sig
		cat(paste0("U-stat=",U1),"\t ")
	} else {
		pv = 2*min(pv1, pv2)
		apv = 2*pnorm(U, mu, sig)
		Z0 = (U - mu)/sig
		cat(paste0("U-stat=",U),"\t ")
	}
	cat(paste0("P-value=", round(pv, dig)), "\n")
	cat(paste0("Normal appr.\t E(U)=", mu, "\t Var(U)=", round(sigsq, 4)), "\n")
	cat(paste0("Z0=", round(Z0, dig), "\t appr. p-value =", round(apv, 4)), "\n")
    # Plot the probability distribution of test statistic ----------------------
	# [corr] xmax = ((n1+n2)*(n1+n2+1)/2 - n1*(n1+1)/2)/2
	xmax = n1*n2
	xa = 0:xmax
	xca = (0:(10*xmax))/10
	pdf = dwilcox(xa, n1, n2)
	ymax = max(pdf)*1.05
	ymin = -0.1*max(pdf)
      # Empty plot
	win.graph(7,5)
	plot(xa, pdf, type="n", xlab=xlab, ylab="f(u)", ylim=c(ymin, ymax),
		main=paste0("Wilcoxon Rank Sum Test (n1=", n1, ", n2=", n2,")"))
      # Probability distribution function
	lines(xa, dwilcox(xa, n1, n2), type="h", lwd=3, col=grey(0.5))
      # Central location
	segments(mu, 0, mu, dnorm(mu, mu, sig), lty=2, col=2)
	text(mu, ymin/2, labels=mu, col=4)
      # Critical region and p-value
	if (any(grepl(side, c("up", "greater")))) {
	    # Upper critical region (one-sided)
		text(U1, dwilcox(U1, n1, n2), labels=U1, col=4, pos=3)
		lines(U1:xmax, dwilcox(U1:xmax, n1, n2),  type="h", col=2, lwd=3)
		text((U1+xmax)/2, ymin/2, labels=round(pv, dig), col=2)
	} else if (any(grepl(side, c("low", "less")))) {
	    # Lower critical region (one-sided)
		text(U1, dwilcox(U1, n1, n2), labels=U1, col=4, pos=3)
		lines(0:U1, dwilcox(0:U1, n1, n2),  type="h", col=2, lwd=3)
		text(U1/2, ymin/2, labels=round(pv, dig), col=2)
	} else {
	    # Lower critical region (two-sided)
		text(U1, dwilcox(U1, n1, n2), labels=U1, col=4, pos=3)
		lines(0:U1, dwilcox(0:U1, n1, n2),  type="h", col=2, lwd=3)
		text(U1/2, ymin/2, labels=round(pv/2, dig), col=2)
	    # Upper critical region (two-sided)
		text(U2, dwilcox(U2, n1, n2), labels=U2, col=4, pos=3)
		lines(U2:xmax, dwilcox(U2:xmax, n1, n2),  type="h", col=2, lwd=3)
		text((U2+xmax)/2, ymin/2, labels=round(pv/2, dig), col=2)
	}
      # Normal approximation
	lines(xca, dnorm(xca, mu, sig), col="green4")
	abline(h=0)
}

# [15-61] Distribution of Wilcoxon Signed Rank Test Statistic
#' @title Distribution of Wilcoxon Signed Rank
#' @description Distribution of Wilcoxon Signed Rank Test Statistic
#' @param nv Number of signed data, Default: 5:50
#' @param av Probability vector (default=c(0.005, 0.01, 0.025, 0.05, 0.95, 0.975, 0.99, 0.995))
#' @param tab Print quantile table? Default: TRUE
#' @param plot Plot test statistic distribution? Default: FALSE
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' signrank.dist(nv=5:15)
#' signrank.dist(nv=c(5,7,10,20), tab=FALSE, plot=TRUE)
#' @rdname signrank.dist
#' @export
signrank.dist = function(nv=5:50, av, tab=TRUE, plot=FALSE, dig=4) {
	if (missing(av)) av = c(0.005, 0.01, 0.025, 0.05, 0.95, 0.975, 0.99, 0.995)
    # Distribution table of Wilcoxon rank sum test statistic
	nn = length(nv)
	na = length(av)
      # qsignrank( ) function --> quantile table
      if (tab) {
	cv = array(NA, dim=c(nn, na))
	colnames(cv) = av
	rownames(cv) = paste0("n=", nv)
	for (i in 1:na) cv[, i] = qsignrank(av[i], nv)
	print(cv)
       }
    # Plot distribution
      if (plot) {
      # Set graphic window
	nc = switch(nn, 1, 2, 3, 2, 3, 3, 3, 3, 3)
	nr = switch(nn, 1, 1, 1, 2, 2, 2, 3, 3, 3)
	wc = switch(nn, 7, 7, 9, 7, 9, 9, 9, 9, 9)
	wr = switch(nn, 5, 4, 4, 6, 6, 6, 9, 9, 9)
	win.graph(wc, wr)
	par(mfrow=c(nr, nc))
      # Plot	
	for(n in nv) {
	  mu = n*(n+1)/4
	  vw = n*(n+1)*(2*n+1)/24
	  dw = sqrt(vw)
  	  xa = 0:(n*(n+1)/2)
  	  ymax = max(dsignrank(xa, n = n), dnorm(mu, mu, dw))
	  plot(xa, dsignrank(xa, n = n), type = "h", lwd=2, col=2, ylab="f(w)", xlab="w",
       		ylim=c(0, ymax), main = paste0("W-Signed Rank (n = ", n, ")"))
	  xa2 = seq(0, mu*2, length=100)
	  lines(xa2, dnorm(xa2, mu, dw), col=4)
	}
      }
}

# [15-62] Wilcoxon Signed Rank Test
#' @title Wilcoxon Signed Rank Test
#' @description Wilcoxon Signed Rank Test with a Plot
#' @param x Data vector in group 1
#' @param y Data vector in group 2
#' @param mu0 Mean difference under the null hypothesis, Default: 0
#' @param side Type of alternative hypothesis, Default: 'two'
#' @param xlab Label of x-axis, Default: 'Signed Rank Sum'
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' x = c(38, 26, 34, 5, 68, 30, 35, 19, 33, 69)
#' y = c(28, 21, 31, 11, 59, 28, 28, 23, 32, 38)
#' signrank.plot(x=x, y=y, side="up")
#' @rdname signrank.plot
#' @export
signrank.plot = function(x, y, mu0=0, side="two", xlab="Signed Rank Sum", dig=4) {
    # Test statistic
	d = x-y-mu0
	d = d[d !=0]
	n = length(d)
	rv = rank(abs(d))
	w1 = sum(rv[d>0])
	w2 = sum(rv[d<0])
	pv1 = psignrank(w2, n)
	pv2 = psignrank(w1, n)
  	cat(paste0("n=",length(x), "\t effe. n=", n, "\t W1=",w1,"\t W2=",w2,"\n"))
    # Normal approximation method (continuity correction)
	mu = n*(n+1)/4
	sigsq = n*(n+1)*(2*n+1)/24
	sig = sqrt(sigsq)
	apv1 = pnorm(w2+0.5, mu, sig)
	apv2 = pnorm(w1+0.5, mu, sig)
    # P-value
	if (any(grepl(side, c("up","greater")))) {
		pv = pv1
		apv = apv1
		cat(paste0("W-stat=",w1),"\t ")
		Z0 = (w1-0.5-mu)/sig
	} else if (any(grepl(side, c("low","less")))) {
		pv = pv2
		apv = apv2
		cat(paste0("W-stat=",w1),"\t ")
		Z0 = (w1+0.5-mu)/sig
	} else {
		pv = 2*min(pv1, pv2)
		apv = 2*min(apv1, apv2)
		cat(paste0("W-stat=",min(w1, w2)), "\t ")
		Z0 = (min(w1,w2)+0.5-mu)/sig
	}
	cat(paste0("P-value=", round(pv, dig)), "\n")
	cat(paste0("E(W)=", mu, "\t Var(W)=", round(sigsq, dig)), "\n")
	cat(paste0("Normal appr. (cont. corr.) Z0=", round(Z0, dig), 
		"\t p-value=", round(apv, dig)), "\n")
    # Plot distribution of test statistic
	xmax = n*(n+1)/2
	xa = 0:xmax
	xca = (0:(10*xmax))/10
	pdf = dsignrank(xa, n)
	ymax = max(pdf)*1.05
	ymin = -0.1*max(pdf)
      # Empty plot
	win.graph(7,5)
	plot(xa, pdf, type="n", xlab=xlab, ylab="f(u)", ylim=c(ymin, ymax),
		main=paste0("Wilcoxon Sign Rank Sum Test (n=", n, ")"))
      # Normal approximation
	lines(xca, dnorm(xca, mu, sig), col="green4")
	abline(h=0)
      # Distribution function of test statistic
	lines(xa, pdf, type="h", lwd=3, col=grey(0.6))
      # Central location
	segments(mu, 0, mu, dnorm(mu, mu, sig), lty=2, col=2)
	text(mu, ymin/2, labels=mu, col=4)
      # Critical region
	if (any(grepl(side, c("up","greater")))) {
	    # upper critical region Display
		text(w1, dsignrank(w1, n), labels=w1, col=4, pos=3)
		lines(w1:xmax, dsignrank(w1:xmax, n),  type="h", col=2, lwd=3)
		text((w1+xmax)/2, ymin/2, labels=round(pv, dig), col=2)
	} else if (any(grepl(side, c("low","less")))) {
	    # lower critical region Display
		text(w1, dsignrank(w1, n), labels=w1, col=4, pos=3)
		lines(0:w1, dsignrank(0:w1, n),  type="h", col=2, lwd=3)
		text(w1/2, ymin/2, labels=round(pv, dig), col=2)
	} else {
	    # lower critical region Display
		wmin=min(w1,w2)
		wmax=max(w1,w2)
		text(wmin, dsignrank(wmin, n), labels=wmin, col=4, pos=3)
		lines(0:wmin, dsignrank(0:wmin, n),  type="h", col=2, lwd=3)
		text(wmin/2, ymin/2, labels=round(pv/2, dig), col=2)
	    # upper critical region Display
		text(wmax, dsignrank(wmax, n), labels=wmax, col=4, pos=3)
		lines(wmax:xmax, dsignrank(wmax:xmax, n),  type="h", col=2, lwd=3)
		text((wmax+xmax)/2, ymin/2, labels=round(pv/2, dig), col=2)
	}
}

# [15-7] Kruskal Wallis Test
#' @title Kruskal Wallis Test
#' @description Kruskal Wallis Test with a Plot
#' @param x Data vector
#' @param y Vector of factor levels
#' @param dig Number of digits below the decimal point, Default: 4
#' @param plot Plot test results? Default: FALSE
#' @return None.
#' 
#' @examples 
#' x = c(4.6,10.6,8.0,25.0,7.1, 2.9,10.1,3.2,3.8,6.6, 6.8,9.4,26.5,12.8,8.3, 3.4,3.9,6.0,8.6,5.0)
#' y = rep(1:4, each=5)
#' kruswall.plot(x, y, plot=TRUE)
#' @rdname kruswall.plot
#' @export
kruswall.plot = function(x, y, dig=4, plot=FALSE) {
    # Test statistic
	nn = length(x)
	kk = length(unique(y))
	ni = as.vector(table(y))
	ns = c(0, cumsum(ni)) 
	rx = rank(x)
	rs = tapply(rx, y, sum)

	rtab = matrix(0, kk, max(ni))
	for (k in 1:kk) rtab[k, 1:ni[k]] = rx[(ns[k]+1):ns[k+1]]
	rtab = cbind(rtab, rs)
	rownames(rtab) = paste0("Group", 1:kk)
	colnames(rtab) = c(1:max(ni), "Sum")

    # Print test results
	cat("Rank Sums for each Group -----------\n")
	print(rtab)
	# for (k in 1:kk) cat(paste0("group",k), rx[(ns[k]+1):ns[k+1]], "\t Sum =", rs[k], "\n")

	H = 12/nn/(nn+1)*sum(rs^2/ni) - 3*(nn+1)
	pv = pchisq(H, kk-1, lower.tail=F)
	cat("Kruskal Wallis Test ----------\n")
	cat(paste0("H = (12 / ", nn," / ",nn+1,") \U00D7 ", round(sum(rs^2/ni), dig), 
		" - 3 \U00D7 ",nn+1, " = ", round(H, dig)), "\n")
	cat(paste0("df=", kk-1, "\t p-value=", round(pv, dig)), "\n")
    # Plot distribution of test statistic
	if (plot) {
		# Utilize chitest.plot2( ) function in chapter 12
		chitest.plot2(stat=H, df=kk-1, side="up")
	}
}

# [15-8] Friedman Test
#' @title Friedman Test
#' @description Friedman Test with a Plot
#' @param x Data vector
#' @param a Vector of factor levels
#' @param b Vector of block levels
#' @param dig Number of digits below the decimal point, Default: 4
#' @param plot Plot test results? Default: FALSE
#' @return None.
#' 
#' @examples 
#' x = c(71,77,75,79,88, 70,94,74,74,83, 72,95,77,80,90, 94,64,76,76,89)
#' a = rep(1:4, each=5)
#' b = rep(1:5, 4)
#' friedman.plot(x, a, b, plot=TRUE)
#' @rdname friedman.plot
#' @export
friedman.plot = function(x, a, b, dig=4, plot=FALSE) {
    # Test statistic
	nn = length(x)
	kk = length(unique(a))
	rr = length(unique(b))
	af = as.factor(a)
	bf = as.factor(b)
	rx = tapply(x, b, rank)
	urx = unlist(rx)
	rxm = matrix(urx, kk, rr)
	a2 = rep(1:kk, rr)
	ra = tapply(urx, a2, sum)

	rtab = cbind(rxm, ra)
	rownames(rtab) = paste0("Group", 1:kk)
	colnames(rtab) = c(1:rr, "Sum")

    # Print test results
	cat("Rank Sum within each Group -----------\n")
	print(rtab)
	# for (k in 1:kk) cat(paste0("group",k), "\t ", rxm[k,], "\t Sum =", ra[k], "\n")
	F = 12/kk/(kk+1)/rr*sum(ra^2)-3*rr*(kk+1)
	pv = pchisq(F, kk-1, lower.tail=FALSE)
	cat("Friedman Test ----------\n")
	cat(paste0("F = (12 / ", kk," / ",kk+1," / ",rr, ") \U00D7 ", sum(ra^2), 
		" - 3 \U00D7 ",rr," \U00D7 ",kk+1, " = ", round(F, dig)), "\n")
	cat(paste0("df=", kk-1, "\t p-value=", round(pv, dig)), "\n")

    # Plot distribution of test statistic
	if (plot) {
		# Utilize chitest.plot2( ) function in chapter 12
		chitest.plot2(stat=F, df=kk-1, side="up")
	}
}