# [Ch-10. Functions] ----------------------------------------------------------------------------------
# [Ch-10. Function Manual] -----------------------------------------
# source("E:/R-stat/Digeng/ch10-function.txt")
#' @title Manual for Ch10. Functions
#' @description Ch10. Inference on a Single Population
#' @param fn Function number, Default: 0
#' @return None.
#' 
#' @examples 
#' ch10.man()
#' ch10.man(3:4)
#' @rdname ch10.man
#' @export
ch10.man = function(fn=0) {
    if (0 %in% fn) {
	cat("[1] pmean.ci\t\tConfidence Interval for a Population Mean (known variance)\n")
	cat("[2] meantest1.plot \tHypothesis Test for a Population Mean (known variance)\n")
	cat("[3] pmean.ci2\t\tConfidence Interval for the Population Mean (unknown variance)\n")
	cat("[4] meantest2.plot \tHypothesis Test for a Population Mean (unknown variance)\n")
	cat("[5] prob.ci\t\tConfidence Interval for a Population Proportion\n")
	cat("[6] bntest.plot\t\tPlot the Result of Exact Binomial Test\n")
	cat("[7] bntest2.plot  \tPlot the Results of Three Types of Binomial Tests\n")
	cat("[8] var1.ci\t\tConfidence Interval for a Population Variance\n")
	cat("[9] var1.test\t\tHypothesis Test for a Population Variance\n")
	cat("[10] chitest.plot   \tPlot the Result of the Chi-square Test\n")
	cat("[11] cimean.sim    \tSimulate the Confidence Interval for a Population Mean\n")
	cat("[12] civar.sim\t\tSimulate the Confidence Interval for a Population Variance\n")
	cat("[13] ciprob.sim\t\tSimulate the Confidence Interval for a Population Proportion\n")
	cat("[14] meanpower.plot \tPower Function of the Test for a Population Mean\n")
	cat("[15] meanchar.plot  \tOperating Characteristic Curves of the Test for a Population Mean\n")
    }
    if (1 %in% fn) {
	cat("[1] Confidence Interval for a Population Mean (known variance)\n")
	cat("pmean.ci(xb, sig, n, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("xb\t Sample mean, or sample data\n")
	cat("sig\t Population standard deviation\n")
	cat("n\t Sample size (unnecessary when sample data are given)\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (2 %in% fn) {
	cat("[2] Hypothesis Test for a Population Mean (known variance)\n")
	cat("meantest1.plot(xb, mu0, sig, n, prng, side=\"two\", mt, dig=4, xlab=\"sample mean\")\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("xb\t Sample mean, or sample data\n")
	cat("mu0\t Population mean value under the null hypothesis\n")
	cat("sig\t Population standard deviation\n")
	cat("n\t Sample size (unnecessary when sample data are given)\n")
	cat("[Optional Input]--------------------------\n")
	cat("prng\t Range of x-axis\n")
	cat("side \t Type of the alternative hypothesis (\"up\", \"low\", \"two\") (default=\"two\")\n")
	cat("mt\t Graph title\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("xlab\t Label of x-axis (default=\"Sample Mean\")\n")
    }
    if (3 %in% fn) {
	cat("[3] Confidence Interval for the Population Mean (unknown variance)\n")
	cat("pmean.ci2(xb, sig, n, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("xb\t Sample mean, or sample data\n")
	cat("sig\t Sample standard deviation (unnecessary when sample data are given)\n")
	cat("n\t Sample size (unnecessary when sample data are given)\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n\n")
    }
    if (4 %in% fn) {
	cat("[4] Hypothesis Test for a Population Mean (unknown variance)\n")
	cat("meantest2.plot(xb, mu0, sig, n, prng=c(-4,4), side=\"two\", mt, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("xb\t Sample mean, or sample data\n")
	cat("mu0\t Population mean value under the null hypothesis\n")
	cat("sig\t Sample standard deviation (unnecessary when sample data are given)\n")
	cat("n\t Sample size (unnecessary when sample data are given)\n")
	cat("[Optional Input]--------------------------\n")
	cat("prng\t Range of x-axis (default=c(-4,4))\n")
	cat("side \t Type of the alternative hypothesis (\"up\", \"low\", \"two\") (default=\"two\")\n")
	cat("mt\t Graph title\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (5 %in% fn) {
	cat("[5] Confidence Interval for a Population Proportion\n")
	cat("prob.ci(n, x, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("n\t Sample size\n")
	cat("x\t Number of successes in a sample\n")
	cat("[Optional Input]\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (6 %in% fn) {
	cat("[6] Plot the Result of Exact Binomial Test\n")
	cat("bntest.plot(x, n, p0, alp=0.05, side=\"two\", dig=4, dcol)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Vector of number of successes\n")
	cat("n\t Sample size\n")
	cat("p0\t Population proportion value under the null hypothesis\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("side \t Type of the alternative hypothesis (\"up\", \"low\", \"two\") (default=\"two\")\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("dcol\t Colors of the probability bars\n")
    }
    if (7 %in% fn) {
	cat("[7] Plot the Results of Three Types of Binomial Tests\n")
	cat("bntest2.plot(x, n, p0, alp=0.05, side=\"two\", dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Number of successes\n")
	cat("n\t Sample size\n")
	cat("p0\t Population proportion value under the null hypothesis\n")
	cat("[Optional Input]\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("side \t Type of the alternative hypothesis (\"up\", \"low\", \"two\") (default=\"two\")\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (8 %in% fn) {
	cat("[8] Confidence Interval for a Population Variance\n")
	cat("var1.ci(x, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Data vector\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n\n")
    }
    if (9 %in% fn) {
	cat("[9] Hypothesis Test for a Population Variance\n")
	cat("var1.test(x, n, var0, alp=0.05, side=\"two\", dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Data vector (or sample variance)\n")
	cat("n\t Sample size (necessary when the sample variance is given)\n")
	cat("var0\t Population variance value under the null hypothesis\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("side \t Type of the alternative hypothesis (\"up\", \"low\", \"two\") (default=\"two\")\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (10 %in% fn) {
	cat("[10] Plot the Result of the Chi-square Test\n")
	cat("chitest.plot(stat, df, prng, side=\"two\", mt, dig=4, ppt=20)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("stat\t Chi-square test statistic\n")
	cat("df\t Degree of freedom\n")
	cat("[Optional Input]--------------------------\n")
	cat("prng\t Range of x-axis\n")
	cat("side \t Type of the alternative hypothesis (\"up\", \"low\", \"two\") (default=\"two\")\n")
	cat("mt\t Graph title\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("ppt\t Number of plotting points in critical region\n")
    }
    if (11 %in% fn) {
	cat("[11] Simulate the Confidence Interval for a Population Mean\n")
	cat("cimean.sim(n, mu=0, sig=1, alp=0.05, N=100, seed=9857, dig=4, plot=TRUE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("n\t Sample size\n")
	cat("[Optional Input]--------------------------\n")
	cat("mu\t Population mean value (default=0)\n")
	cat("sig\t Population standard deviation (default=1)\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("N\t Number of iterations (default=100)\n")
	cat("seed\t Seed value for generating random numbers (default=9857)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("plot\t Logical value for plot (default=TRUE)\n")
    }
    if (12 %in% fn) {
	cat("[12] Simulate the Confidence Interval for a Population Variance\n")
	cat("civar.sim(n, mu=0, sig=1, alp=0.05, N=100, seed=9857, dig=4, plot=TRUE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("n\t Sample size\n")
	cat("[Optional Input]--------------------------\n")
	cat("mu\t Population mean value (default=0)\n")
	cat("sig\t Population standard deviation (default=1)\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("N\t Number of iterations (default=100)\n")
	cat("seed\t Seed value for generating random numbers (default=9857)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("plot\t Logical value for plot (default=TRUE)\n")
    }
    if (13 %in% fn) {
	cat("[13] Simulate the Confidence Interval for a Population Proportion\n")
	cat("ciprob.sim(n, p=0.5, alp=0.05, N=100, seed=9857, dig=4, plot=TRUE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("n\t Sample size\n")
	cat("[Optional Input]--------------------------\n")
	cat("p\t Population proportion value (default=0.5)\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("N\t Number of iterations (default=100)\n")
	cat("seed\t Seed value for generating random numbers (default=9857)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("plot\t Logical value for plot (default=TRUE)\n")
    }
    if (14 %in% fn) {
	cat("[14] Power Function of the Test for a Population Mean\n")
	cat("meanpower.plot(mu0, mu1, sig, nv, alp=0.05, prng, side=\"two\", mt, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("mu0\t Population mean value under the null hypothesis\n")
	cat("mu1\t Vector of population mean values for which the power should be calculated\n")
	cat("sig\t Population standard deviation\n")
	cat("nv\t Sample size vector\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("prng\t Range of x-axis\n")
	cat("side \t Type of the alternative hypothesis (\"up\", \"low\", \"two\") (default=\"two\")\n")
	cat("mt\t Graph title\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (15 %in% fn) {
	cat("[15] Operating Characteristic Curves of the Test for a Population Mean\n")
	cat("meanchar.plot(mu0, mu1, sig, nv, alp=0.05, prng, side=\"two\", mt, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("mu0\t Population mean value under the null hypothesis\n")
	cat("mu1\t Vector of population mean values for which the power should be calculated\n")
	cat("sig\t Population standard deviation\n")
	cat("nv\t Sample size vector\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("prng\t Range of x-axis\n")
	cat("side \t Type of the alternative hypothesis (\"up\", \"low\", \"two\") (default=\"two\")\n")
	cat("mt\t Graph title\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
}

# [10-1] Confidence Interval for a Population Mean (known variance)
#' @title Confidence Interval for a Mean (known variance)
#' @description Confidence Interval for a Population Mean (known variance)
#' @param xb Sample mean, or sample data
#' @param sig Population standard deviation
#' @param n Sample size (unnecessary when sample data are given)
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' pmean.ci(xb=199.5, sig=5, n=50)
#' pmean.ci(xb=199.5, sig=5, n=50, dig=3)
#' pmean.ci(rnorm(100, 20, 3), sig=3)
#' @rdname pmean.ci
#' @export
pmean.ci = function(xb, sig, n, alp=0.05, dig=4) {
	if (length(xb)>1) {
		n = length(xb)
		xb = mean(xb)
	}
	err = qnorm(1-alp/2)*sig/sqrt(n)
	cat(paste0("[", round(xb, dig), " \U00B1 ", round(qnorm(1-alp/2), dig), "\U00D7", 
		round(sig, dig), "/\U221A", n,
		"] = [", round(xb, dig), " \U00B1 ", round(err, dig), "] = [", 
		round(xb-err, dig), ", ", round(xb+err, dig),"]"), "\n")
}

# [10-2] Hypothesis Test for a Population Mean (known variance)
#' @title Hypothesis Test for a Mean (known variance)
#' @description Hypothesis Test for a Population Mean (known variance)
#' @param xb Sample mean, or sample data
#' @param mu0 Population mean value under the null hypothesis
#' @param sig Population standard deviation
#' @param n Sample size (unnecessary when sample data are given)
#' @param prng Range of x-axis
#' @param side Type of the alternative hypothesis ("up", "low", "two"), Default: 'two'
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @param xlab Label of x-axis, Default: 'Sample Mean'
#' @return None.
#' 
#' @examples 
#' meantest1.plot(xb=12.64, mu0=12.5, sig=0.5, n=40, side="up")
#' meantest1.plot(rnorm(40, 12.6, 0.5), mu0=12.5, sig=0.5, side="up")
#' 
#' @rdname meantest1.plot
#' @export
meantest1.plot = function(xb, mu0, sig, n, prng, side="two", mt, dig=4, xlab="Sample Mean") {
	if (length(xb)>1) {
		n = length(xb)
		xb = mean(xb)
	}
    # Standard error and the test statistic
	se = sig/sqrt(n)
	z0 = (xb-mu0)/se
	cat(paste0("Z0 = (", round(xb, dig), " - ", round(mu0, dig), ") / (",
		round(sig, dig), "/\U221A", n, ") = ", round(z0, dig)), "\n")
	if (missing(prng)) prng=c(mu0-4*se, mu0+4*se)
	if (missing(mt)) mt = paste0("Dist. of ", xlab, 
		" under H0 ~ N(", mu0, ", ", round(se,3), "\U00B2)")
    # Plot the PDF
	xa = seq(prng[1], prng[2], length.out=101)
	win.graph(7,5)
	plot(xa, dnorm(xa, mu0, se), type="n", xlab=xlab, ylab="pdf", 
		ylim=c(-0.1, 1)*max(dnorm(xa, mu0, se)),
		main=mt)
    # P-value and the critical region
	if (side=="up") {
		pv = pnorm(xb, mu0, se, lower.tail=FALSE)
		cat("P-v = P(Z > Z0) =", round(pv, dig), "\n")
		cord.x = c(xb, seq(xb, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, dnorm(seq(xb, prng[2], length.out=20), mu0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(xb, 0, xb, dnorm(xb, mu0, se), lwd=2, col=2)
		text(xb, dnorm(xb, mu0, se)*0.9, round(pv, 4), pos=4, col=2)
		text(xb, 0, round(xb, 4), pos=1, col=4)
	} else if (side=="low") {
		pv = pnorm(xb, mu0, se)
		cat("P-v = P(Z < Z0) =", round(pv, dig), "\n")
		cord.x = c(prng[1], seq(prng[1], xb, length.out=20), xb) 
		cord.y = c(0, dnorm(seq(prng[1], xb, length.out=20), mu0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(xb, 0, xb, dnorm(xb, mu0, se), lwd=2, col=2)
		text(xb, dnorm(xb, mu0, se)*0.9, round(pv, 4), pos=2, col=2)
		text(xb, 0, round(xb, 4), pos=1, col=4)
	} else {
		mlow = ifelse(xb>mu0, 2*mu0-xb, xb)
		mup = ifelse(xb>mu0, xb, 2*mu0-xb)
		pv = 2*pnorm(mlow, mu0, se)
		cat("P-v = 2\U00D7P(Z > |Z0|) =", round(pv, dig), "\n")
		cord.x = c(mup, seq(mup, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, dnorm(seq(mup, prng[2], length.out=20), mu0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		cord.x = c(prng[1], seq(prng[1], mlow, length.out=20), mlow) 
		cord.y = c(0, dnorm(seq(prng[1], mlow, length.out=20), mu0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(c(mlow, mup), 0, c(mlow, mup), dnorm(xb, mu0, se), lwd=2, col=2)
		text(c(mlow, mup), dnorm(xb, mu0, se)*0.9, round(pv/2, 4), pos=c(2,4), col=2)
		text(c(mlow, mup), 0, round(c(mlow, mup), 4), pos=1, col=4)
	}
	abline(h=0); abline(v=mu0, lty=2, lwd=2, col="green3")
	lines(xa, dnorm(xa, mu0, se), type="l", lwd=2, col=4)
}

# [10-3] Confidence Interval for the Population Mean (unknown variance)
#' @title Confidence Interval for the Mean (unknown variance)
#' @description Confidence Interval for the Population Mean (unknown variance)
#' @param xb Sample mean, or sample data
#' @param sig Sample standard deviation (unnecessary when sample data are given)
#' @param n Sample size (unnecessary when sample data are given)
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' pmean.ci2(xb=199.5, sig=5, n=16, alp=0.05, dig=3)
#' pmean.ci2(rnorm(20, 199.5, 5))
#' @rdname pmean.ci2
#' @export 
pmean.ci2 = function(xb, sig, n, alp=0.05, dig=4) {
	if (length(xb)>1) {
		n = length(xb)
		sig = sd(xb)
		xb = mean(xb)
	}
	err = qt(1-alp/2, n-1)*sig/sqrt(n)
	cat(paste0("[", round(xb, dig), " \U00B1 ", round(qt(1-alp/2, n-1), dig), "\U00D7", 
		round(sig, dig), "/\U221A", n,
		"] = [", round(xb, dig), " \U00B1 ", round(err, dig), "] = [", 
		round(xb-err, dig), ", ", round(xb+err, dig),"]"), "\n")
}

# [10-4] Hypothesis Test for a Population Mean (unknown variance)
#' @title Hypothesis Test for a Mean (unknown variance)
#' @description Hypothesis Test for a Population Mean (unknown variance)
#' @param xb Sample mean, or sample data
#' @param mu0 Population mean value under the null hypothesis
#' @param sig Sample standard deviation (unnecessary when sample data are given)
#' @param n Sample size (unnecessary when sample data are given)
#' @param prng Range of x-axis, Default: c(-4, 4)
#' @param side Type of the alternative hypothesis ("up", "low", "two"), Default: 'two'
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' meantest2.plot(xb=12.65, mu0=12.5, sig=0.57, n=40, side="up")
#' meantest2.plot(rnorm(40, 12.65, 0.57), side="two")
#' @rdname meantest2.plot
#' @export
meantest2.plot = function(xb, mu0, sig, n, prng=c(-4,4), side="two", mt, dig=4) {
	if (length(xb)>1) {
		n = length(xb)
		sig = sd(xb)
		xb = mean(xb)
	}
    # Standard error and the test statistic
	se = sig/sqrt(n)
	t0 = (xb-mu0)/se
	cat(paste0("T0 = (", round(xb, dig), " - ", round(mu0, dig), ") / (",
		round(sig, dig), "/\U221A", n, ") = ", round(t0, dig)), "\n")
	df = n-1
	if (missing(mt)) mt = paste0("Dist. of the Test Statistic under H0 ~ t(", round(df, 3),")")
    # Plot the PDF
	xa = seq(prng[1], prng[2], length.out=101)
	win.graph(7,5)
	plot(xa, dt(xa, df), type="n", xlab="Test Statistic", ylab="pdf", 
		ylim=c(-0.1, 1)*max(dt(xa, df)), main=mt)
    # P-value and the critical region
	if (side=="up") {
		pv = pt(t0, df, lower.tail=FALSE)
		cat("P-v = P(T > T0) =", round(pv, dig), "\n")
		cord.x = c(t0, seq(t0, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, dt(seq(t0, prng[2], length.out=20), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(t0, 0, t0, dt(t0, df), lwd=2, col=2)
		text(t0, dt(t0, df)*0.9, round(pv, dig), pos=4, col=2)
		text(t0, 0, round(t0, dig), pos=1, col=4)
	} else if (side=="low") {
		pv = pt(t0, df)
		cat("P-v = P(T < T0) =", round(pv, dig), "\n")
		cord.x = c(prng[1], seq(prng[1], t0, length.out=20), t0) 
		cord.y = c(0, dt(seq(prng[1], t0, length.out=20), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(t0, 0, t0, dt(t0, df), lwd=2, col=2)
		text(t0, dt(t0, df)*0.9, round(pv, dig), pos=2, col=2)
		text(t0, 0, round(t0, dig), pos=1, col=4)
	} else {
		mlow = ifelse(t0>0, -t0, t0)
		mup = ifelse(t0>0, t0, -t0)
		pv = 2*pt(mlow, df)
		cat("P-v = 2\U00D7P(T > |T0|) =", round(pv, dig), "\n")
		cord.x = c(mup, seq(mup, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, dt(seq(mup, prng[2], length.out=20), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		cord.x = c(prng[1], seq(prng[1], mlow, length.out=20), mlow) 
		cord.y = c(0, dt(seq(prng[1], mlow, length.out=20), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(c(mlow, mup), 0, c(mlow, mup), dt(t0, df), lwd=2, col=2)
		text(c(mlow, mup), dt(t0, df)*0.9, round(pv/2, dig), pos=c(2,4), col=2)
		text(c(mlow, mup), 0, round(c(mlow, mup), dig), pos=1, col=4)
	}
	abline(h=0); abline(v=0, lty=2, lwd=2, col="green3")
	lines(xa, dt(xa, df), type="l", lwd=2, col=4)
}

# [10-5] Confidence Interval for a Population Proportion
#' @title Confidence Interval for a Population Proportion
#' @description Confidence Interval for a Population Proportion
#' @param n Sample size
#' @param x Number of successes in a sample
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' prob.ci(n=200, x=15)
#' @rdname prob.ci
#' @export
prob.ci = function(n, x, alp=0.05, dig=4) {
	p = x/n
	err = qnorm(1-alp/2)*sqrt(p*(1-p)/n)
	cat(paste0("[", p, " \U00B1 ", round(qnorm(1-alp/2), dig), "\U00D7\U221A(", 
		round(p, dig), "\U00D7", round(1-p, dig), "/", n,
		")] = [", p, " \U00B1 ", round(err, dig), "] = [", 
		round(p-err, dig), ", ", round(p+err, dig),"]"), "\n")
}

# [10-6] Plot the Result of Exact Binomial Test
#' @title Exact Binomial Test
#' @description Plot the Result of Exact Binomial Test
#' @param x Vector of number of successes
#' @param n Sample size
#' @param p0 Population proportion value under the null hypothesis
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @param dcol Colors of the probability bars
#' @return None.
#' 
#' @examples 
#' bntest.plot(x=2:4, n=10, p0=0.1, side="up")
#' (x=6:4, n=20, p0=0.5, side="two")
#' @rdname bntest.plot
#' @export
bntest.plot = function(x, n, p0, alp=0.05, side="two", dig=4, dcol) {
    # Exact binomial test
	nx = length(x)
	nside = grep(side, c("lower", "upper", "two.sided"))

	# Exact binomial test
	side2 = switch(nside, "less", "greater", "two.sided") 

	simb1 = simb2 = rep("", nx)
	tag1 = tag2 = rep("", nx)
	pv1 = pbinom(x, n, p0)
	pv2 = 1 - pbinom(pmax(0, x-1), n, p0)
	pv3 = rep(NA, nx)
	for (k in 1:nx) pv3[k] = sum(dbinom(0:n, n, p0)[dbinom(0:n, n, p0) <= dbinom(x[k], n, p0)])
	pval = switch(nside, pv1, pv2, pv3)

	for (k in 1:nx) {
	      # Two sided case
	      if (nside==3) {
		tag1[k] = paste0("\U03A3[f(y) \U2264 f(", x[k], ")] = ")
		tag2[k] = paste0("\U03A3[f(y)\U2264","f(", x[k], ")]=")
	      }
	      simb1[k] = switch(nside, paste0("P(X \U2264 ", x[k], ") = "), paste0("P(X \U2265 ", x[k], ") = "), tag1[k])
	      simb2[k] = switch(nside, paste0("P(X\U2264", x[k], ")="), paste0("P(X\U2265", x[k], ")="), tag2[k]) 
	      bo = binom.test(x[k], n, p0, alt=side2, conf=1-alp)
	      cat(paste0("X = ", x[k], "\t P-v = ", simb1[k], round(pval[k], dig), 
		"\t ", 100*(1-alp), "%-CI = [", round(bo$conf[1], dig), ", ", round(bo$conf[2], dig),"]"), "\n")
	}
    # Range of x
	xa = 0:n
    # Plot the probability distribution
	if (missing(dcol)) dcol=c(4, 2, "green4", "orange", "purple")
	win.graph(7,5)
	plot(xa, dbinom(xa, n, p0), type="h", lwd=7, col=grey(0.7), ylim=c(0, 1.1*max(dbinom(xa, n, p0))),
	main=paste0("B(", n, ", ", p0,") Distribution & P-value"), xlab="x", ylab="f(x)" )
	if (n <= 12) text(xa, dbinom(xa, n, p0), round(dbinom(xa, n, p0), dig), col=1, pos=3, cex=0.8)

	lab=rep("", nx)
	for (k in 1:nx) {
		xa2 = switch(nside, 0:x[k], x[k]:n, (0:n)[dbinom(0:n, n, p0) <= dbinom(x[k], n, p0)])
		lines(xa2, dbinom(xa2, n, p0), type="h", lwd=5, col=dcol[k])			
		lab[k] = paste0(simb2[k], round(pval[k], dig))
	}
		legend("topright", lab, text.col=dcol[1:nx])
}

# [10-7] Plot the Results of Three Types of Binomial Tests
#' @title Three Types of Binomial Tests
#' @description Plot the Results of Three Types of Binomial Tests
#' @param x Number of successes
#' @param n Sample size
#' @param p0 Population proportion value under the null hypothesis
#' @param alp Level of significance, Default: 0.05
#' @param side Type of the alternative hypothesis ("up", "low", "two"), Default: 'two'
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' bntest2.plot(x=15, n=200, p0=0.1, alp=0.05, side="low")
#' bntest2.plot(x=15, n=200, p0=0.1, alp=0.05, side="two")
#' @rdname bntest2.plot
#' @export
bntest2.plot = function(x, n, p0, alp=0.05, side="two", dig=4) {
    # Alternative hypothesis
	nside = grep(side, c("lower", "upper", "two.sided"))

    # Exact binomial test
	side2 = switch(nside, "less", "greater", "two.sided") 
	bo = binom.test(x, n, p0, alt=side2)

	pve1 = pbinom(x, n, p0)
	pve2 = 1 - pbinom(pmax(0, x-1), n, p0)
	pve3 = sum(dbinom(0:n, n, p0)[dbinom(0:n, n, p0) <= dbinom(x, n, p0)])
	pvale = switch(nside, pve1, pve2, pve3)

	# Two sided case
	tag1 = paste0("\U03A3[f(y) \U2264 f(", x, ")] = ")
	tag2 = paste0("\U03A3[f(y)\U2264","f(", x, ")]=")

	simb = switch(nside, "P(Z < Z0) = ", "P(Z > Z0) = ", "2\U00D7P(Z > |Z0|) = ") 
	simbe1 = switch(nside, paste0("P(X \U2264 ", x, ") = "), paste0("P(X \U2265 ", x, ") = "), tag1)
	simbe2 = switch(nside, paste0("P(X\U2264", x, ")="), paste0("P(X\U2265", x, ")="), tag2) 
	bo = binom.test(x, n, p0, alt=side2, conf=1-alp)
	cat(paste0("Exact Binomial Test:\t x/n = ", round(bo$est, dig), "\t P-v = ", simbe1, round(bo$p.val, dig)), "\n")
    # Normal approximation
	ph = x/n
	se = sqrt(n*p0*(1-p0))
	tstat = (x-n*p0)/se
	pv = switch(nside, pnorm(tstat), 1-pnorm(tstat), 2*(1-pnorm(abs(tstat))) )
	cat(paste0("Normal approximation:\t Z0 = ", round(tstat, dig), "\t P-v = ", simb, round(pv, dig)), "\n")
    # Normal approximation (continuity correction)
	tstat2 = switch(nside, (x+0.5-n*p0)/se, (x-0.5-n*p0)/se,
		ifelse(x<n*p0, (x+0.5-n*p0)/se, (x-0.5-n*p0)/se) )
	pv2 = switch(nside, pnorm(tstat2), 1-pnorm(tstat2), 2*(1-pnorm(abs(tstat2))) )
	cat(paste0("Continuity correction:\t Z0 = ", round(tstat2, dig), "\t P-v = ", simb, round(pv2, dig)), "\n")
    # Range of x
	x1 = max(0, floor(n*p0 - 4*se))
	x2 = min(n, ceiling(n*p0 + 4*se))
	xa = x1:x2
	ymax = dbinom(n*p0, n, p0)*1.1
    # Plot the probability distribution
	win.graph(7,5)
	plot(xa, dbinom(xa, n, p0), type="h", lwd=5, col=grey(0.7), ylim=c(0, ymax),
		main=paste0("B(", n, ", ", p0,") Distribution & P-value"), xlab="x", ylab="f(x)" )
	abline(h=0, col=grey(0.4))

	xa2 = switch(nside, x1:x, x:x2, (x1:x2)[dbinom(x1:x2, n, p0) <= dbinom(x, n, p0)])
	lines(xa2, dbinom(xa2, n, p0), type="h", lwd=5, col=2)

    # Normal approximation
	xa3 = seq(x1, x2, length=100)
	lines(xa3, dnorm(xa3, n*p0, se), col="green2")

	xo = round(2*n*p0-x, 0)
	if (x<n*p0) {txa4 = c(x, x+0.5, xo, xo-0.5)
	} else {	txa4 = c(xo, xo+0.5, x, x-0.5) }
	xa4 = switch(nside, c(x, x+0.5), c(x, x-0.5), txa4)
	if (nside==1) abline(v=xa4, lty=2, col=c(4, "purple"))
	if (nside==2) abline(v=xa4, lty=2, col=c(4, "purple"))
	if (nside==3) abline(v=xa4, lty=2, col=c(4, "purple"))

	lab1 = paste("Exact :", round(pvale, dig))
	lab2 = paste("Normal :", round(pv, dig))
	lab3 = paste("Correct :", round(pv2, dig))
	legend("topright", c(lab1, lab2, lab3), text.col=c(2, 4, "purple"))
}

# [10-8] Confidence Interval for a Population Variance
#' @title Confidence Interval for a Variance
#' @description Confidence Interval for a Population Variance
#' @param x Data vector
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' x = c(20.0, 21.5, 20.9, 19.8, 22.5, 20.3, 23.6, 18.0, 23.3, 17.8)
#' var1.ci(x, dig=3)
#' var1.ci(rnorm(36))
#' @rdname var1.ci
#' @export
var1.ci = function(x, alp=0.05, dig=4) {
	n = length(x)
	xss = sum(x^2)-sum(x)^2/n
	xv = var(x)
	cv1 = qchisq(alp/2, n-1); cv2 = qchisq(1-alp/2, n-1)
	cat(paste0((1-alp)*100, "% CI = [", round(xss, dig), " / ", round(cv2, dig), ", ",
		round(xss, dig), " / ", round(cv1, dig), "] = [",
		round(xss/cv2, dig), ", ", round(xss/cv1, dig), "]"), "\n")
	invisible(list(var=xv, conf=xss/c(cv2,cv1)))  
}

# [10-9] Hypothesis Test for a Population Variance
#' @title Hypothesis Test for a Variance
#' @description Hypothesis Test for a Population Variance
#' @param x Data vector (or sample variance)
#' @param n Sample size (necessary when the sample variance is given)
#' @param var0 Population variance value under the null hypothesis
#' @param alp Level of significance, Default: 0.05
#' @param side Type of the alternative hypothesis ("up", "low", "two"), Default: 'two'
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' var1.test(x=1.24, n=25, var0=0.8, side="up")
#'
#' x = c(20.0, 21.5, 20.9, 19.8, 22.5, 20.3, 23.6, 18.0, 23.3, 17.8)
#' var1.test(x=x, var0=2, side="two")
#' @rdname var1.test
#' @export
var1.test = function(x, n, var0, alp=0.05, side="two", dig=4) {
	if (length(x) >1) {
		n = length(x)
		xss = sum(x^2)-sum(x)^2/n
		svar = var(x)
		cat(paste0("n = ", n, "\tSxx = ", round(xss, dig), "\tVar(X) = ", round(svar, dig)), "\n")
	} else {	svar = x 
		xss = (n-1)*svar
	}
    # Calculate the test statistic and p-value
	nside = grep(side, c("less", "greater", "two.sided"))
	if (length(nside)==0) nside = grep(side, c("low", "upp", "two-sided"))
	chi0 = (n-1)*svar / var0
	pv = switch(nside, pchisq(chi0, n-1), pchisq(chi0, n-1, lower=F), 
		2*min(pchisq(chi0, n-1), pchisq(chi0, n-1, lower=F)) )
	cat(paste0("Chi0 = ", n-1, " \U00D7 ", round(svar, dig), " / ", var0, " = ", 
		round(chi0, dig), "\t P-v = ", round(pv, dig)), "\n")
	cv1 = qchisq(alp/2, n-1); cv2 = qchisq(1-alp/2, n-1)
	cat(paste0((1-alp)*100, "% CI = [", round(xss, dig), " / ", round(cv2, dig), ", ",
		round(xss, dig), " / ", round(cv1, dig), "] = [",
		round(xss/cv2, dig), ", ", round(xss/cv1, dig), "]"), "\n")
	invisible(list(var=svar, stat=chi0, df=n-1, conf=xss/c(cv2,cv1)))  
}


# [10-10] Plot the Result of the Chi-square Test
#' @title Plot the Chi-square Test
#' @description Plot the Result of the Chi-square Test
#' @param stat Chi-square test statistic
#' @param df Degree of freedom
#' @param prng Range of x-axis
#' @param side Type of the alternative hypothesis ("up", "low", "two"), Default: 'two'
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @param ppt Number of plotting points in critical region, Default: 20
#' @return None.
#' 
#' @examples 
#' chi0 = var1.test(x=1.24, n=25, var0=0.8, side="up")
#' chitest.plot(stat=chi0$stat, df=chi0$df, side="up")
#'
#' x = c(20.0, 21.5, 20.9, 19.8, 22.5, 20.3, 23.6, 18.0, 23.3, 17.8)
#' res = var1.test(x=x, var0=2, side="two")
#' chitest.plot(stat=res$stat, df=res$df, side="two")
#' @rdname chitest.plot
#' @export
chitest.plot = function(stat, df, prng, side="two", mt, dig=4, ppt=20) {
    # Plot the PDF
	if (missing(prng)) prng = c(0, qchisq(0.999, df))
	if (missing(mt)) mt = bquote(bold("Chi-Square Test :")~chi^2 ~( .(df) ))

	xa = seq(prng[1], prng[2], length=100)

	win.graph(7, 5)
	plot(xa, dchisq(xa, df), type="n", xlab="Test Statistic(x)", ylab="f(x)", 
		ylim=c(-0.1, 1)*max(dchisq(xa, df)), main=mt)
    # P-value and the critical region
	plow = pchisq(stat, df)
	if (any(grepl(side, c("greater", "up")))) {
		pv = 1-plow
		cat(paste0("Chi0 = ", round(stat, dig), "\t P-v = ", round(pv, dig)), "\n")
		cord.x = c(stat, seq(stat, prng[2], length.out=ppt), prng[2]) 
		cord.y = c(0, dchisq(seq(stat, prng[2], length.out=ppt), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(stat, 0, stat, dchisq(stat, df), lwd=2, col=2)
		text(stat, dchisq(stat, df)*0.9, round(pv, 4), pos=4, col=2)
		text(stat, 0, round(stat, 4), pos=1, col=4)
	} else if (any(grepl(side, c("less", "low")))) {
		pv = plow
		cat(paste0("Chi0 = ", round(stat, dig), "\t P-v = ", round(pv, dig)), "\n")
		cord.x = c(prng[1], seq(prng[1], stat, length.out=ppt), stat) 
		cord.y = c(0, dchisq(seq(prng[1], stat, length.out=ppt), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(stat, 0, stat, dchisq(stat, df), lwd=2, col=2)
		text(stat, dchisq(stat, df)*0.9, round(pv, 4), pos=2, col=2)
		text(stat, 0, round(stat, 4), pos=1, col=4)
	} else if (any(grepl(side, c("two.sided", "two-sided")))) {
		pv = 2*min(plow, 1-plow)
		cat(paste0("Chi0 = ", round(stat, dig), "\t P-v = ", round(pv, dig)), "\n")
		mlow =qchisq(pv/2, df)
		mup =qchisq(1-pv/2, df)
		cord.x = c(mup, seq(mup, prng[2], length.out=ppt), prng[2]) 
		cord.y = c(0, dchisq(seq(mup, prng[2], length.out=ppt), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		cord.x = c(prng[1], seq(prng[1], mlow, length.out=ppt), mlow) 
		cord.y = c(0, dchisq(seq(prng[1], mlow, length.out=ppt), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(c(mlow, mup), 0, c(mlow, mup), dchisq(c(mlow, mup), df), lwd=2, col=2)
		text(c(mlow, mup), dchisq(c(mlow, mup), df)*0.9, round(pv/2, 4), pos=c(2,4), col=2)
		text(c(mlow, mup), 0, round(c(mlow, mup), 4), pos=1, col=4)
	}
	abline(h=0); abline(v=qchisq(0.5, df), lty=2, lwd=2, col="green3")
	lines(xa, dchisq(xa, df), type="l", lwd=2, col=4)
}

# [10-11] Simulate the Confidence Interval for a Population Mean
#' @title Simulate the Confidence Interval for a Mean
#' @description Simulate the Confidence Interval for a Population Mean
#' @param n Sample size
#' @param mu Population mean value, Default: 0
#' @param sig Population standard deviation, Default: 1
#' @param alp Level of significance, Default: 0.05
#' @param n Sample size, Default: 100
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param dig Number of digits below the decimal point, Default: 4
#' @param plot Plot confidence intervals? Default: TRUE
#' @return None.
#' 
#' @examples 
#' cimean.sim(n=16, mu=10, sig=2)
#' cimean.sim(n=16, mu=10, sig=2, N=10000, plot=FALSE)
#' @rdname cimean.sim
#' @export
cimean.sim=function(n, mu=0, sig=1, alp=0.05, N=100, seed=9857, dig=4, plot=TRUE) {
    # Create the matrix of confidence intervals
	ci = matrix(0, nrow=N, ncol=3)
	ir = 1:N
    # Tail value of the t-distribution
	tv = qt(1-alp/2, n-1)
    # Set seed for reproducibility
	set.seed(seed)
    # Generate random numbers and calculate the confidence intervals
	for (i in ir) {
		x = rnorm(n, mu, sig)
		xm = mean(x)
		xs = sd(x)
		lcl = xm-tv*xs/sqrt(n)
		ucl = xm+tv*xs/sqrt(n)
		ci[i, ] = c(lcl, xm, ucl)
	}
	if (plot) {
	    # Display graph
		win.graph(7, 4)
		plot(ir, ci[ ,2], type="p", pch=19, cex=0.6, col=1, ylim=c(min(ci), max(ci)), 
			main="Confidence Intervals for a Population Mean", 
				ylab="Confidence Interval", xlab="Iteration")
		abline(h=mu, col=2)
		arrows(ir, ci[ ,1], ir, ci[ ,3], length=0.03, code=3, angle=90, lwd=1.5,
			col=ifelse((ci[ , 1]>mu | ci[ , 3]<mu), 2, 4) )
	}
    # Number of confidence intervals without the population mean
	nup = sum(ci[ , 1]>mu)
	nlow = sum(ci[ ,3]<mu)
	cat(paste0("P(LCL > ", mu, ") = ", nup, " / ", N, " = ", nup/N,
		"\t P(UCL < ", mu, ") = ", nlow, " / ", N, " = ", nlow/N), "\n")
}

# [10-12] Simulate the Confidence Interval for a Population Variance
#' @title Simulate the Confidence Interval for a Variance
#' @description Simulate the Confidence Interval for a Population Variance
#' @param n Sample size
#' @param mu Population mean value, Default: 0
#' @param sig Population standard deviation, Default: 1
#' @param alp Level of significance, Default: 0.05
#' @param n Sample size, Default: 100
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param dig Number of digits below the decimal point, Default: 4
#' @param plot Plot confidence intervals? Default: TRUE
#' @return None.
#' 
#' @examples 
#' civar.sim(n=16, mu=10, sig=2)
#' civar.sim(n=16, mu=10, sig=2, N=10000, plot=FALSE)
#' @rdname civar.sim
#' @export
civar.sim=function(n, mu=0, sig=1, alp=0.05, N=100, seed=9857, dig=4, plot=TRUE) {
    # Create the matrix of confidence intervals
	ci = matrix(0, nrow=N, ncol=3)
	ir = 1:N
    # Tail value of the chi-square distribution
	cv1 = qchisq(alp/2, n-1)
	cv2 = qchisq(1-alp/2, n-1)
    # Set seed for reproducibility
	set.seed(seed)
    # Generate random numbers and calculate the confidence intervals
	for (i in ir) {
		x = rnorm(n, mu, sig)
		xm = var(x)
		xss = xm*(n-1)
		lcl = xss/cv2
		ucl = xss/cv1
		ci[i, ] = c(lcl, xm, ucl)
	}
	if (plot) {
	    # graph Display
		win.graph(7, 4)
		plot(ir, ci[ ,2], type="p", pch=19, cex=0.6, col=1, ylim=c(min(ci), max(ci)), 
			main="Confidence Intervals for a Population Variance", 
				ylab="Confidence Interval", xlab="Iteration")
		abline(h=sig^2, col=2)
		arrows(ir, ci[ ,1], ir, ci[ ,3], length=0.03, code=3, angle=90, lwd=1.5,
			col=ifelse((ci[ , 1]>sig^2 | ci[ , 3]<sig^2), 2, 4) )
	}
   # Number of confidence intervals without the population variance
	nup = sum(ci[ , 1]>sig^2)
	nlow = sum(ci[ ,3]<sig^2)
	cat(paste0("P(LCL > ", sig^2, ") = ", nup, " / ", N, " = ", nup/N,
		"\t P(UCL < ", sig^2, ") = ", nlow, " / ", N, " = ", nlow/N), "\n")
}

# [10-13] Simulate the Confidence Interval for a Population Proportion
#' @title Simulate the Confidence Interval for a Proportion
#' @description Simulate the Confidence Interval for a Population Proportion
#' @param n Sample size
#' @param p Population proportion value, Default: 0.5
#' @param alp Level of significance, Default: 0.05
#' @param n Sample size, Default: 100
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param dig Number of digits below the decimal point, Default: 4
#' @param plot Plot confidence intervals? Default: TRUE
#' @return None.
#' 
#' @examples 
#' ciprob.sim(n=16, p=0.6, alp=0.05, N=100)
#' ciprob.sim(n=16, p=0.6, alp=0.05, N=10000, plot=FALSE)
#' @rdname ciprob.sim
#' @export
ciprob.sim=function(n, p=0.5, alp=0.05, N=100, seed=9857, dig=4, plot=TRUE) {
 	ir = 1:N
    # Tail value of the standard normal distribution
	zv = qnorm(1-alp/2)
    # Set seed for reproducibility
	set.seed(seed)
    # Generate random numbers and calculate the confidence intervals
	xm = rbinom(N, n, p)
	xp = xm/n
	xv = xp*(1-xp)/n
	lcl = pmax(0, xp - zv*sqrt(xv))
	ucl = pmin(1, xp + zv*sqrt(xv))
	ci = cbind(lcl, xp, ucl)
	# Display graph
	if (plot) {
		win.graph(7, 4)
		plot(ir, ci[ ,2], type="p", pch=19, cex=0.6, col=1, ylim=c(min(ci), max(ci)), 
			main="Confidence Intervals for a Proportion", 
				ylab="Confidence Interval", xlab="Iteration")
		abline(h=p, col=2)
		arrows(ir, ci[ ,1], ir, ci[ ,3], length=0.03, code=3, angle=90, lwd=1.5,
			col=ifelse((ci[ , 1]>p | ci[ , 3]<p), 2, 4) )
	}
    # Number of confidence intervals without the population proportion
	nup = sum(ci[ , 1]>p)
	nlow = sum(ci[ ,3]<p)
	cat(paste0("P(LCL > ", p, ") = ", nup, " / ", N, " = ", nup/N,
		"\t P(UCL < ", p, ") = ", nlow, " / ", N, " = ", nlow/N), "\n")
}

# [10-14] Power Function of the Test for a Population Mean
#' @title Power Function of the Test for a Mean
#' @description Power Function of the Test for a Population Mean
#' @param mu0 Population mean value under the null hypothesis
#' @param mu1 Vector of population mean values for which the power should be calculated
#' @param sig Population standard deviation
#' @param nv Sample size vector
#' @param alp Level of significance, Default: 0.05
#' @param prng Range of x-axis
#' @param side Type of the alternative hypothesis ("up", "low", "two"), Default: 'two'
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' n = c(10, 30, 50, 100)
#' meanpower.plot(mu0=100, mu1=102, sig=5, nv=n, side="up")
#' meanpower.plot(mu0=100, mu1=102, sig=5, nv=n, side="two")
#' @rdname meanpower.plot
#' @export 
meanpower.plot = function(mu0, mu1, sig, nv, alp=0.05, prng, side="two", mt, dig=4) {
    # Type of the alternative hypothesis
	nside = grep(side, c("low", "up", "two"))
    # Set plot variables
	nn = length(nv)
	se0 = sig/sqrt(min(nv))
	if (missing(prng)) prng = switch(nside, c(mu0-4*se0, mu0), c(mu0, mu0+4*se0), c(mu0-4*se0, mu0+4*se0))
	x1 = prng[1]-(prng[2]-prng[1])*0.15
	if (missing(mt)) mt = bquote(bold("Power Function")~~psi(mu)~~(mu[0]~"="~ .(mu0)~","~sigma~"="~ .(sig)) )

    # Define the power function
	pwr1 = function (n, mu) pnorm(-qnorm(1-alp)+n^0.5*(mu0-mu)/sig)
	pwr2 = function (n, mu) 1-pnorm(qnorm(1-alp)+n^0.5*(mu0-mu)/sig)
	pwr3 = function (n, mu) {pnorm(-qnorm(1-alp/2)+n^0.5*(mu0-mu)/sig)+
			1-pnorm(qnorm(1-alp/2)+n^0.5*(mu0-mu)/sig) }
    # Calculate the power
	if (nside==1) {
		power = pwr1(nv, mu1)
	} else if (nside==2) {
		power = pwr2(nv, mu1)
	} else if (nside==3) {
		power = pwr3(nv, mu1)
	}
	names(power) = nv
	print(round(power, dig))
    # Set axis and display graph
	xa = seq(prng[1], prng[2], length.out=100)
	win.graph(7,5)
	if (nn>5) {dcol=rainbow(nn)
	} else dcol=c(2, 4, "green4", "purple", 6)
    	# Power function
	if (nside==1) {
		plot(xa, pwr1(nv[1], xa), type="n", ylim=c(0,1), xlim=c(x1, prng[2]), 
			main=mt, ylab=expression(psi(mu)), xlab=expression(mu))
		for (i in 1:nn) lines(xa, pwr1(nv[i], xa), lwd=2, col=dcol[i])
		grid(col=3)
		abline(h=alp, lty=2, col=2)
		text(mu0-(prng[2]-prng[1])*0.05, alp, labels=bquote(alpha==.(alp)), col=2, pos=1)
	    # Illustration
	          	if (!(missing(mu1))) {
			abline(v=mu1, lty=2, col=4)
			segments(prng[1], pwr1(nv, mu1), mu1, pwr1(nv, mu1), lty=2, col=4)
			text(x1, pwr1(nv, mu1), labels=format(pwr1(nv, mu1), digits=3), pos=4, col=2)
		}
	    # Display sample size
		legend("bottom", paste0("n=", nv), col=dcol[1:nn], lwd=2, bg="white")
	} else if (nside==2) {
		plot(xa, pwr2(nv[1], xa), type="n", ylim=c(0,1), xlim=c(x1, prng[2]), 
			main=mt, ylab=expression(psi(mu)), xlab=expression(mu))
		for (i in 1:nn) lines(xa, pwr2(nv[i], xa), lwd=2, col=dcol[i])
		grid(col=3)
		abline(h=alp, lty=2, col=2)
		text(mu0, alp, labels=bquote(alpha==.(alp)), col=2, pos=1)
	    # Display sample size
		legend("right", paste0("n=", nv), col=dcol[1:nn], lwd=2, bg="white")
	    # Illustration
	          	if (!(missing(mu1))) {
			abline(v=mu1, lty=2, col=4)
			segments(prng[1], pwr2(nv, mu1), mu1, pwr2(nv, mu1), lty=2, col=4)
			text(x1, pwr2(nv, mu1), labels=format(pwr2(nv, mu1), digits=3), pos=4, col=2)
		}

	} else if (nside==3) {
		plot(xa, pwr3(nv[1], xa), type="n", ylim=c(0,1), xlim=c(x1, prng[2]), 
			main=mt, ylab=expression(psi(mu)), xlab=expression(mu))
		for (i in 1:nn) lines(xa, pwr3(nv[i], xa), lwd=2, col=dcol[i])
		grid(col=3)
		abline(h=alp, lty=2, col=2)
		text(mu0, alp, labels=bquote(alpha==.(alp)), col=2, pos=1)
	    # Display sample size
		legend("right", paste0("n=", nv), col=dcol[1:nn], lwd=2, bg="white")
	    # Illustration
	          	if (!(missing(mu1))) {
			mu2=mu0-(mu1-mu0)
			abline(v=c(mu1, mu2), lty=2, col=4)
			segments(prng[1], pwr3(nv, mu1), mu1, pwr3(nv, mu1), lty=2, col=4)
			text(x1, pwr3(nv, mu1), labels=format(pwr3(nv, mu1), digits=3), pos=4, col=2)
		}
	}
}

# [10-15] Operating Characteristic Curves of the Test for a Population Mean
#' @title Operating Characteristic Curves of the Test for a Mean
#' @description Operating Characteristic Curves of the Test for a Population Mean
#' @param mu0 Population mean value under the null hypothesis
#' @param mu1 Vector of population mean values for which the power should be calculated
#' @param sig Population standard deviation
#' @param nv Sample size vector
#' @param alp Level of significance, Default: 0.05
#' @param prng Range of x-axis
#' @param side Type of the alternative hypothesis ("up", "low", "two"), Default: 'two'
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' n = c(10, 30, 50, 100)
#' meanchar.plot(mu0=100, mu1=98, sig=5, nv=n, side="low")
#' meanchar.plot(mu0=100, mu1=98, sig=5, nv=n, side="two")
#' @rdname meanchar.plot
#' @export
meanchar.plot = function(mu0, mu1, sig, nv, alp=0.05, prng, side="two", mt, dig=4) {
    # Type of the alternative hypothesis
	nside = grep(side, c("low", "up", "two"))
    # Set plot variables
	nn = length(nv)
	se0 = sig/sqrt(min(nv))
	if (missing(prng)) prng = switch(nside, c(mu0-4*se0, mu0), c(mu0, mu0+4*se0), c(mu0-4*se0, mu0+4*se0))
	x1 = prng[1]-(prng[2]-prng[1])*0.15
	if (missing(mt)) mt = bquote(bold("OC Curve")~~1-psi(mu)~~(mu[0]~"="~ .(mu0)~","~sigma~"="~ .(sig)) )

    # Define the OC curve function
	char1 = function (n, mu) 1-pnorm(-qnorm(1-alp)+n^0.5*(mu0-mu)/sig)
	char2 = function (n, mu) pnorm(qnorm(1-alp)+n^0.5*(mu0-mu)/sig)
	char3 = function (n, mu) {-pnorm(-qnorm(1-alp/2)+n^0.5*(mu0-mu)/sig)+
			pnorm(qnorm(1-alp/2)+n^0.5*(mu0-mu)/sig) }
    # Calculate the OC curve values
	if (nside==1) {
		char = char1(nv, mu1)
	} else if (nside==2) {
		char = char2(nv, mu1)
	} else if (nside==3) {
		char = char3(nv, mu1)
	}
	names(char) = nv
	print(round(char, dig))
    # Set axis and display graph
	xa = seq(prng[1], prng[2], length.out=100)
	win.graph(7,5)
	if (nn>5) {dcol=rainbow(nn)
	} else dcol=c(2, 4, "green4", "purple", 6)
    	# OC curve
	if (nside==1) {
		plot(xa, char1(nv[1], xa), type="n", ylim=c(0,1), xlim=c(x1, prng[2]), 
			main=mt, ylab=expression(1-psi(mu)), xlab=expression(mu))
		for (i in 1:nn) lines(xa, char1(nv[i], xa), lwd=2, col=dcol[i])
		grid(col=3)
	    # Level of significance
		abline(h=1-alp, lty=2, col=2)
		text(mu0-(prng[2]-prng[1])*0.05, 1-alp, labels=bquote(alpha==.(alp)), col=2, pos=3)
	    # Illustration
	          	if (!(missing(mu1))) {
			abline(v=mu1, lty=2, col=4)
			segments(prng[1], char1(nv, mu1), mu1, char1(nv, mu1), lty=2, col=4)
			text(x1, char1(nv, mu1), labels=round(char1(nv, mu1), 3), pos=4, col=2)
		}
	    # Display sample size
		legend("top", paste0("n=", nv), col=dcol[1:nn], lwd=2, bg="white")
	} else if (nside==2) {
		plot(xa, char2(nv[1], xa), type="n", ylim=c(0,1), xlim=c(x1, prng[2]), 
			main=mt, ylab=expression(1-psi(mu)), xlab=expression(mu))
		for (i in 1:nn) lines(xa, char2(nv[i], xa), lwd=2, col=dcol[i])
		grid(col=3)
	    # Level of significance
		abline(h=1-alp, lty=2, col=2)
		text(mu0, 1-alp, labels=bquote(alpha==.(alp)), col=2, pos=3)
	    # Display sample size
		legend("right", paste0("n=", nv), col=dcol[1:nn], lwd=2, bg="white")
	    # Illustration
	          	if (!(missing(mu1))) {
			abline(v=mu1, lty=2, col=4)
			segments(prng[1], char2(nv, mu1), mu1, char2(nv, mu1), lty=2, col=4)
			text(x1, char2(nv, mu1), labels=round(char2(nv, mu1), 3), pos=4, col=2)
		}

	} else if (nside==3) {
		plot(xa, char3(nv[1], xa), type="n", ylim=c(0,1), xlim=c(x1, prng[2]), 
			main=mt, ylab=expression(1-psi(mu)), xlab=expression(mu))
		for (i in 1:nn) lines(xa, char3(nv[i], xa), lwd=2, col=dcol[i])
		grid(col=3)
	    # Level of significance
		abline(h=1-alp, lty=2, col=2)
		text(mu0, 1-alp, labels=bquote(alpha==.(alp)), col=2, pos=3)
	    # Display sample size
		legend("right", paste0("n=", nv), col=dcol[1:nn], lwd=2, bg="white")
	    # Illustration
	          	if (!(missing(mu1))) {
			mu2=mu0-(mu1-mu0)
			abline(v=c(mu1, mu2), lty=2, col=4)
			segments(prng[1], char3(nv, mu1), mu1, char3(nv, mu1), lty=2, col=4)
			text(x1, char3(nv, mu1), labels=round(char3(nv, mu1), 3), pos=4, col=2)
		}
	}
}