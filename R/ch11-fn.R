# [Ch-11 Functions] ----------------------------------------------------------------------------------
# [Ch-11 Function Manual] -----------------------------------------
# source("E:/R-stat/Digeng/ch11-function.txt")

#' @title Manual for Ch11. Functions
#' @description Ch11. Inference on Two Populations
#' @param fn Function number (0~8), Default: 0
#' @return None.
#' 
#' @examples 
#' ch11.man()
#' ch11.man(1)
#' @rdname ch11.man
#' @export
ch11.man = function(fn=0) {
    if (0 %in% fn) {
	cat("[1] mean2.ci\t\tConfidence Interval for the Difference of Two Population Means\n")
	cat("[2] mean2test.plot \tHypothesis Test for the Difference of Two Population Means\n")
	cat("[3] normtest.plot  \tPlot the PDF of Test Statistic with the Normal Distribution\n")
	cat("[4] ttest.plot\t\tPlot the PDF of Test Statistic with the T-distribution\n")
	cat("[5] prob2.ci\t\tConfidence Interval for the Difference of Population Proportions\n")
	cat("[6] prob2test.plot  \tTest for the Difference of Population Proportions (Large Sample)\n")
	cat("[7] ftest.plot\t\tPlot the PDF of the F-test Statistic\n")
	cat("[8] var2.ci\t\tConfidence Interval for the Ratio of Two Population Variances\n")
	cat("[9] var2.test\t\tHypothesis Test on Two Population Variances\n")
	cat("[10] civar2.sim\t\tSimulate the Confidence Interval for a the Ratio of Population Variances\n")
    }
    if (1 %in% fn) {
	cat("[1] Confidence Interval for the Difference of Two Population Means\n")
	cat("mean2.ci(xb1, xb2, s1, s2, n1, n2, pvar=\"equal\", alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("xb1\t Sample mean of population1 (or sample data)\n")
	cat("xb2\t Sample mean of population2 (or sample data)\n")
	cat("s1\t Standard deviation of population1 (optional for unknown variance)\n")
	cat("s2\t Standard deviation of population2 (optional for unknown variance)\n")
	cat("n1\t Sample size of population1 (unnecessary if data are given)\n")
	cat("n2\t Sample size of population2 (unnecessary if data are given)\n")
	cat("[Optional Input]--------------------------\n")
	cat("pvar\t Status of variance (one of \"known\", \"equal\", \"unequal\")\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n\n")
    }
    if (2 %in% fn) {
	cat("[2] Hypothesis Test for the Difference of Two Population Means\n")
	cat("mean2test.plot(xb1, xb2, s1, s2, n1, n2, d0=0, prng, side=\"two\", pvar=\"equal\", mt, dig=4, xlab)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("xb1\t Sample mean of population1 (or sample data)\n")
	cat("xb2\t Sample mean of population2 (or sample data)\n")
	cat("s1\t Standard deviation of population1 (optional for unknown variance)\n")
	cat("s2\t Standard deviation of population2 (optional for unknown variance)\n")
	cat("n1\t Sample size of population1 (unnecessary if data are given)\n")
	cat("n2\t Sample size of population2 (unnecessary if data are given)\n")
	cat("[Optional Input]--------------------------\n")
	cat("d0\t Difference of two population means under the null hypothesis (default=0)\n")
	cat("prng\t Range of x-axis (default = d0 \U00B1 4 \U00D7 se)\n")
	cat("side\t Type of the alternative hypothesis (default=\"two\")\n")
	cat("pvar\t Status of variance (one of \"known\", \"equal\", \"unequal\")\n")
	cat("mt\t Graph title\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("xlab\t Label of x-axis\n")
    }
    if (3 %in% fn) {
	cat("[3] Plot the PDF of Test Statistic with the Normal Distribution\n")
	cat("normtest.plot(md, mu0=0, se=1, prng=c(-4,4), sided=\"two\", xlab=\"Sample Mean\")\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("md\t Difference of sample means or test statistic\n")
	cat("[Optional Input]--------------------------\n")
	cat("m0\t Difference of population means under the null hypothesis (default=0)\n")
	cat("se\t Standard error of the difference of sample means (default=1)\n")
	cat("prng\t Range of x-axis (default = c(-4,4))\n")
	cat("side\t Type of the alternative hypothesis (default=\"two\")\n")
	cat("xlab\t Label of x-axis (default=\"Sample Mean\")\n")
    }
    if (4 %in% fn) {
	cat("[4] Plot the PDF of Test Statistic with the T-distribution\n")
	cat("ttest.plot(md, deg, prng=c(-4,4), sided=\"two\")\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("md\t T-test statistic for the difference of population means\n")
	cat("deg\t Degree of freedom\n")
	cat("[Optional Input]--------------------------\n")
	cat("prng\t Range of x-axis (default = c(-4,4))\n")
	cat("side\t Type of the alternative hypothesis (default=\"two\")\n")
    }
    if (5 %in% fn) {
	cat("[5] Confidence Interval for the Difference of Population Proportions\n")
	cat("prob2.ci(n1, x1, n2, x2, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("n1\t Sample size of population1\n")
	cat("x1\t Number of successes in samples from population1\n")
	cat("n2\t Sample size of population2\n")
	cat("x2\t Number of successes in samples from population2\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default = 0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (6 %in% fn) {
	cat("[6] Test for the Difference of Population Proportions (Large Sample)\n")
	cat("prob2test.plot(n1, x1, n2, x2, prng, side=\"two\", mt, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("n1\t Sample size of population1\n")
	cat("x1\t Number of successes in samples from population1\n")
	cat("n2\t Sample size of population2\n")
	cat("x2\t Number of successes in samples from population2\n")
	cat("[Optional Input]--------------------------\n")
	cat("prng\t Range of x-axis (default = d0 \U00B1 4*se)\n")
	cat("side\t Type of the alternative hypothesis (default=\"two\")\n")
	cat("mt\t Graph title\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (7 %in% fn) {
	cat("[7] Plot the PDF of the F-test Statistic\n")
	cat("ftest.plot(fstat, deg, pmax=0.995, sided=\"two\")\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("fstat\t F-test statistic for the ratio of two population variances\n")
	cat("deg\t Vector of degree of freedoms\n")
	cat("[Optional Input]--------------------------\n")
	cat("pmax\t Maximum probability for quantiles in x-axis (default=0.995)\n")
	cat("side\t Type of the alternative hypothesis (default=\"two\")\n")
    }
    if (8 %in% fn) {
	cat("[8] Confidence Interval for the Ratio of Two Population Variances\n")
	cat("var2.ci(s1, s2, n1, n2, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("s1\t Sample standard deviation of population1 (or sample data)\n")
	cat("s2\t Sample standard deviation of population2 (or sample data)\n")
	cat("n1\t Sample size of population1 (unnecessary if data are given)\n")
	cat("n2\t Sample size of population2 (unnecessary if data are given)\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n\n")
    }
    if (9 %in% fn) {
	cat("[9] Hypothesis Test on Two Population Variances\n")
	cat("var2.test(s1, s2, n1, n2, alp=0.05, side=\"two\", dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("s1\t Standard deviation of population1 (or sample data)\n")
	cat("s2\t Standard deviation of population2 (or sample data)\n")
	cat("n1\t Sample size of population1 (unnecessary if data are given)\n")
	cat("n2\t Sample size of population2 (unnecessary if data are given)\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("side\t Type of the alternative hypothesis (default=\"two\")\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (10 %in% fn) {
	cat("[10] Simulate the Confidence Interval for a the Ratio of Population Variances\n")
	cat("civar2.sim(n1, n2, sig1, sig2, alp=0.05, N=100, seed=9857, dig=4, plot=TRUE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("n1\t Sample size of population1\n")
	cat("n2\t Sample size of population2\n")
	cat("sig1\t Standard deviation of population1\n")
	cat("sig2\t Standard deviation of population2\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("N\t Number of iterations (default=100)\n")
	cat("seed\t Seed value for generating random numbers (default=9857)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("plot\t Logical value for plot (default=TRUE)\n")
    }
}
# [11-8] Confidence Interval for the Ratio of Two Population Variances
#' @title CI for the Ratio of Two Population Variances
#' @description Confidence Interval for the Ratio of Two Population Variances
#' @param s1 Sample standard deviation of population1
#' @param s2 Sample standard deviation of population2
#' @param n1 Sample size of population1
#' @param n2 Sample size of population2
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' var2.ci(s1=3.5, s2=5.8, n1=25, n2=34)
#'
#' x = rnorm(20, 199, 4)
#' y = rnorm(25, 200, 2)
#' var2.ci(sd(x), sd(y), length(x), length(y))
#' @rdname var2.ci
#' @export
var2.ci = function(s1, s2, n1, n2, alp=0.05, dig=4) {
	if (missing(s1) | missing(s2)) stop("Input two standard deviations or data vectors...")
	if (length(s1)==1 & missing(n1)) stop("Input n1, the first sample size...")
	if (length(s2)==1 & missing(n2)) stop("Input n2, the second sample size...")
	if (length(s1)>1) {	n1=length(s1)
			s1=sd(s1)
	}
	if (length(s2)>1) {	n2=length(s2)
			s2=sd(s2)
	}
	F0=s1^2/s2^2
	df1=n1-1; df2=n2-1
	cv1=qf(alp/2, df1, df2)
	cv2=qf(1-alp/2, df1, df2)
	lcl = round(F0/cv2, dig)
	ucl = round(F0/cv1, dig)
	cat("F0 = s1^2/s2^2 =", round(s1^2,dig), "/", round(s2^2,dig), "=", round(F0, dig), "\n")
	cat("[LCL, UCL] =", paste0("[", round(F0,dig), "/", round(cv2,dig), ", ", 
		round(F0,dig), "/", round(cv1,dig), "]"), "=", paste0("[", lcl, ", ", ucl, "]"), "\n")
}

# [11-9] Hypothesis Test on Two Population Variances
#' @title Hypothesis Test on Two Population Variances
#' @description Hypothesis Test on Two Independent Normal Population Variances
#' @param s1 Sample standard deviation of population1
#' @param s2 Sample standard deviation of population2
#' @param n1 Sample size of population1
#' @param n2 Sample size of population2
#' @param alp Level of significance, Default: 0.05
#' @param side Type of the alternative hypothesis, Default: 'two'
#' @param dig Number of digits below the decimal point, Default: 4
#' @return Test Statistic (F0) and the p-value (pv)
#' 
#' @examples 
#' var2.test(s1=3.5, s2=5.8, n1=25, n2=34)
#'
#' x = rnorm(20, 199, 4)
#' y = rnorm(25, 200, 2)
#' var2.test(sd(x), sd(y), length(x), length(y), side="up")
#' @rdname var2.test
#' @export
var2.test = function(s1, s2, n1, n2, alp=0.05, side="two", dig=4) {
	if (missing(s1) | missing(s2)) stop("Input two standard deviations or data vectors...")
	if (length(s1)==1 & missing(n1)) stop("Input n1, the first sample size...")
	if (length(s2)==1 & missing(n2)) stop("Input n2, the second sample size...")
	if (length(s1)>1) {	n1=length(s1)
			s1=sd(s1)
	}
	if (length(s2)>1) {	n2=length(s2)
			s2=sd(s2)
	}

	F0=s1^2/s2^2
	df1=n1-1; df2=n2-1
	if (side=="up") {cv=qf(1-alp, df1, df2)
		pv = 1- pf(F0, df1, df2)
		simb=ifelse(F0 < cv, " < ", " > ")
		res=ifelse(F0 >= cv, "Reject H0", "Accept H0")
		cat(paste0("F0 = ", round(F0, 4), simb, round(cv, 4), " \U21D2 ", res,
			"\t(p-v = ", round(pv, dig), ")\n"))
	} else if (side=="low") {cv=qf(alp, df1, df2)
		pv = pf(F0, df1, df2)
		simb=ifelse(F0 < cv, " < ", " > ")
		res=ifelse(F0 < cv, "Reject H0", "Accept H0")
		cat(paste0("F0 = ", round(F0, 4), simb, round(cv, 4), " \U21D2 ", res, 
			"\t(p-v = ", round(pv, dig), ")\n"))
	} else {cv=qf(c(alp/2, 1-alp/2), df1, df2)
		pv=2*min(c(pf(F0, df1, df2), 1-pf(F0, df1, df2)))
		simb=ifelse(F0<cv[1] | F0>cv[2], " \U2209 ", " \U2208 ")
		res=ifelse(F0<cv[1] | F0>cv[2], "Reject H0", "Accept H0")
		cat(paste0("F0 = ", round(F0, 4), simb, 
			"[", round(cv[1], 4), ", ", round(cv[2], 4), "] \U21D2 ", res, 
			"\t(p-v = ", round(pv, dig), ")\n"))
	}
	invisible(c(F0, pv))
}

# [11-1] Confidence Interval for the Difference of Two Population Means
#' @title CI for the Difference of Two Means
#' @description Confidence Interval for the Difference of Two Population Means
#' @param xb1 Sample mean of population1 (or sample data)
#' @param xb2 Sample mean of population2 (or sample data)
#' @param s1 Standard deviation of population1 (optional for unknown variance)
#' @param s2 Standard deviation of population2 (optional for unknown variance)
#' @param n1 Sample size of population1 (unnecessary if data are given)
#' @param n2 Sample size of population2 (unnecessary if data are given)
#' @param pvar Status of variance (one of "known", "equal", "unequal"), Default: 'equal'
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' mean2.ci(xb1=198.5, xb2=201.3, s1=5, s2=5, n1=25, n2=34, pvar="known")
#' mean2.ci(xb1=198.5, xb2=201.3, s1=4.8, s2=5.1, n1=25, n2=34, pvar="equal")
#' mean2.ci(xb1=198.5, xb2=201.3, s1=2.8, s2=5.5, n1=25, n2=34, pvar="unequal")
#'
#' x = rnorm(20, 199, 2)
#' y = rnorm(25, 200, 2)
#' mean2.ci(x, y, pvar="equal")
#' @rdname mean2.ci
#' @export
mean2.ci = function(xb1, xb2, s1, s2, n1, n2, pvar="equal", alp=0.05, dig=4) {
    # Case of sample data input
	if (length(xb1) > 1) {
		indata = TRUE
		n1 = length(xb1)
		n2 = length(xb2)
		sig1 = sd(xb1)
		sig2 = sd(xb2)
		xb1 = mean(xb1)
		xb2 = mean(xb2)
		cat("xb1 =", round(xb1, dig), "\t xb2 =", round(xb2, dig), "\n")
		cat("std1 =", round(sig1, dig), "\t std2 =", round(sig2, dig), "\n")
	} else indata = FALSE
    # [1] Case of known variance
      if (grepl(pvar, "known")) {
	err = qnorm(1-alp/2)*sqrt(s1^2/n1 + s2^2/n2)
	xd = xb1-xb2
	cat(paste0("[(", round(xb1, dig), " - ", round(xb2, dig), ") \U00B1 ", round(qnorm(1-alp/2), dig), 
		" \U00D7 \U221A(", round(s1^2, dig), "/", n1, " + ", round(s2^2, dig), "/", n2,
		")]\n = [", round(xd, dig), " \U00B1 ", round(err, dig), "] = [", 
		round(xd-err, dig), ", ", round(xd+err, dig),"]"), "\n")
    # [2] Case of unknown, but equal variances
      } else if (grepl(pvar, "equal")) {
        # Calculate standard error and the confidence interval
	if (indata) {
		s1 = sig1
		s2 = sig2
	}
	df = n1+n2-2
	sp2 = ((n1-1)*s1^2 + (n2-1)*s2^2) / df
	sp = sqrt(sp2)
	err = qt(1-alp/2, df)*sp*sqrt(1/n1 + 1/n2)
	xd = xb1-xb2
	cat("Sp\U00B2 =", round(sp2, dig), "\t Sp =", round(sp, dig), "\t df =", df, "\n")
	cat(paste0("[(", round(xb1, dig), " - ", round(xb2, dig), ") \U00B1 ", round(qt(1-alp/2,df), dig), 
		" \U00D7 ", round(sp, dig), " \U00D7 \U221A(", "1/", n1, "+1/", n2,
		")]\n = [", round(xd, dig), " \U00B1 ", round(err, dig), "] = [", 
		round(xd-err, dig), ", ", round(xd+err, dig),"]"), "\n")
    # [3] Case of unknown and inequal variances
      } else {
        # Standard error and the confidence interval
	if (indata) {
		s1 = sig1
		s2 = sig2
	}
	df = (s1^2/n1+s2^2/n2)^2 / ((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
	tdf = qt(1-alp/2, df)
	err = tdf*sqrt(s1^2/n1 + s2^2/n2)
	xd = xb1-xb2
	cat("nu* =", round(df, dig), "\t t(nu*) =", round(tdf, dig), "\n")
	cat(paste0("[(", round(xb1, dig), " - ", round(xb2, dig), ") \U00B1 ", round(tdf, dig), 
		" \U00D7 \U221A(", round(s1^2, dig), "/", n1, " + ", round(s2^2, dig), "/", n2,
		")]\n = [", round(xd, dig), " \U00B1 ", round(err, dig), "] = [", 
		round(xd-err, dig), ", ", round(xd+err, dig),"]"), "\n")
      }
}

# [11-2] Hypothesis Test for the Difference of Two Population Means
#' @title Hypothesis Test for the Difference of Two Means
#' @description Hypothesis Test for the Difference of Two Population Means
#' @param xb1 Sample mean of population1 (or sample data)
#' @param xb2 Sample mean of population2 (or sample data)
#' @param s1 Standard deviation of population1 (optional for unknown variance)
#' @param s2 Standard deviation of population2 (optional for unknown variance)
#' @param n1 Sample size of population1 (unnecessary if data are given)
#' @param n2 Sample size of population2 (unnecessary if data are given)
#' @param d0 Difference of two population means under the null hypothesis, Default: 0
#' @param prng Range of x-axis, Default: [d0-4se, d0+4se]
#' @param side Type of the alternative hypothesis, Default: 'two'
#' @param pvar Status of variance (one of "known", "equal", "unequal"), Default: 'equal'
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @param xlab Label of x-axis
#' @return None.
#' 
#' @examples 
#' mean2test.plot(xb1=198.5, xb2=201.3, s1=5, s2=5, n1=25, n2=34, pvar="known")
#' mean2test.plot(xb1=198.5, xb2=201.3, s1=4.8, s2=5.1, n1=25, n2=34, pvar="equal")
#' mean2test.plot(xb1=198.5, xb2=201.3, s1=2.8, s2=5.5, n1=25, n2=34, pvar="unequal")
#' 
#' x = rnorm(20, 199, 2)
#' y = rnorm(25, 200, 2)
#' mean2test.plot(x, y, pvar="equal")
#' @rdname mean2test.plot
#' @export 
mean2test.plot = function(xb1, xb2, s1, s2, n1, n2, d0=0, prng, side="two", pvar="equal", mt, dig=4, xlab) {
    # Case of sample data input
	if (length(xb1) >1) {
		indata = TRUE
		n1 = length(xb1)
		n2 = length(xb2)
		sig1 = sd(xb1)
		sig2 = sd(xb2)
		xb1 = mean(xb1)
		xb2 = mean(xb2)
		cat("xb1 =", round(xb1, dig), "\t xb2 =", round(xb2, dig), "\n")
		cat("std1 =", round(sig1, dig), "\t std2 =", round(sig2, dig), "\n")
	} else indata = FALSE
    # [1] Case of known variance
      if (grepl(pvar, "known")) {
	se = sqrt(s1^2/n1+s2^2/n2)
	xd = xb1 - xb2
	z0 = (xd - d0)/se
	cat(paste0("Z0 = (", round(xb1, dig), " - ", round(xb2, dig), " - ", d0, ") / \U221A(",
		round(s1^2, dig), "/", n1, " + ", round(s2^2, dig), "/", n2,
		") = ", round(z0, dig)), "\n")
	xmax = max(abs(xd), 4)
	if (missing(prng)) prng=c(d0-xmax*se, d0+xmax*se)
	if (missing(mt)) mt = bquote(bold("Distribution of the Mean Difference under H0: ")~N( .(d0)~","~ .(round(se,3))^2))
	if (missing(xlab)) xlab = "Difference of Sample Means"
    # Plot the PDF
	xa = seq(prng[1], prng[2], length.out=100)
	win.graph(7,5)
	plot(xa, dnorm(xa, d0, se), type="n", xlab=xlab, ylab="pdf", 
		ylim=c(-0.1, 1)*max(dnorm(xa, d0, se)),
		main=mt)
    # P-value and the critical region
	if (side=="up" | grepl(side, "greater")) {
		pv = pnorm(xd, d0, se, lower.tail=FALSE)
		cat("P-v = P(Z > Z0) =", round(pv, dig), "\n")
		cord.x = c(xd, seq(xd, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, dnorm(seq(xd, prng[2], length.out=20), d0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(xd, 0, xd, dnorm(xd, d0, se), lwd=2, col=2)
		text(xd, dnorm(xd, d0, se)*0.9, round(pv, 4), pos=4, col=2)
		text(xd, 0, round(xd, 4), pos=1, col=4)
	} else if (side=="low" | grepl(side, "less")) {
		pv = pnorm(xd, d0, se)
		cat("P-v = P(Z < Z0) =", round(pv, dig), "\n")
		cord.x = c(prng[1], seq(prng[1], xd, length.out=20), xd) 
		cord.y = c(0, dnorm(seq(prng[1], xd, length.out=20), d0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(xd, 0, xd, dnorm(xd, d0, se), lwd=2, col=2)
		text(xd, dnorm(xd, d0, se)*0.9, round(pv, 4), pos=2, col=2)
		text(xd, 0, round(xd, 4), pos=1, col=4)
	} else if (grepl(side, "two sided")) {
		mlow = ifelse(xd>d0, 2*d0-xd, xd)
		mup = ifelse(xd>d0, xd, 2*d0-xd)
		pv = 2*pnorm(mlow, d0, se)
		cat("P-v = 2 \U00D7 P(Z > |Z0|) =", round(pv, dig), "\n")
		cord.x = c(mup, seq(mup, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, dnorm(seq(mup, prng[2], length.out=20), d0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		cord.x = c(prng[1], seq(prng[1], mlow, length.out=20), mlow) 
		cord.y = c(0, dnorm(seq(prng[1], mlow, length.out=20), d0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(c(mlow, mup), 0, c(mlow, mup), dnorm(xd, d0, se), lwd=2, col=2)
		text(c(mlow, mup), dnorm(xd, d0, se)*0.9, round(pv/2, 4), pos=c(2,4), col=2)
		text(c(mlow, mup), 0, round(c(mlow, mup), 4), pos=1, col=4)
	}
	abline(h=0); abline(v=d0, lty=2, lwd=2, col="green3")
	lines(xa, dnorm(xa, d0, se), type="l", lwd=2, col=4)
	invisible(list(stat=z0, pval=pv))    
    # [2] Case of unknown, but equal variances
      } else if (grepl(pvar, "equal")) {
	if (indata) {
		s1 = sig1
		s2 = sig2
	}
    # Standard error and the test statistic
	sp2 =((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
	se = sqrt(sp2*(1/n1+1/n2))
	xd = xb1 - xb2
	t0 = (xd-d0)/se
	cat("Sp\U00B2 =", round(sp2, dig), "\t s.e.=", round(se, dig), "\n")
	cat(paste0("T0 = (", round(xb1, dig), " - ", round(xb2, dig), " - ", d0, ") / \U221A(",
		round(sp2, dig), " \U00D7 (1/", n1, "+1/", n2,
		")) = ", round(t0, dig)), "\n")
	df = n1+n2-2
	xmax = max(abs(t0), 4)
	if (missing(prng)) prng=c(-xmax, xmax)
	if (missing(mt)) mt = paste0("Distribution of the Test Statistic under H0: t(", round(df, 3),")")
	if (missing(xlab)) xlab = "Test Statistic"
    # Plot the PDF
	xa = seq(prng[1], prng[2], length.out=101)
	win.graph(7,5)
	plot(xa, dt(xa, df), type="n", xlab=xlab, ylab="pdf", 
		ylim=c(-0.1, 1)*max(dt(xa, df)), main=mt)
    # P-value and the critical region
	if (side=="up" | grepl(side, "greater")) {
		pv = pt(t0, df, lower.tail=FALSE)
		cat("P-v = P(T > T0) =", round(pv, dig), "\n")
		cord.x = c(t0, seq(t0, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, dt(seq(t0, prng[2], length.out=20), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(t0, 0, t0, dt(t0, df), lwd=2, col=2)
		text(t0, dt(t0, df)*0.9, round(pv, dig), pos=4, col=2)
		text(t0, 0, round(t0, dig), pos=1, col=4)
	} else if (side=="low" | grepl(side, "less")) {
		pv = pt(t0, df)
		cat("P-v = P(T < T0) =", round(pv, dig), "\n")
		cord.x = c(prng[1], seq(prng[1], t0, length.out=20), t0) 
		cord.y = c(0, dt(seq(prng[1], t0, length.out=20), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(t0, 0, t0, dt(t0, df), lwd=2, col=2)
		text(t0, dt(t0, df)*0.9, round(pv, dig), pos=2, col=2)
		text(t0, 0, round(t0, dig), pos=1, col=4)
	} else if (grepl(side, "two sided")) {
		mlow = ifelse(t0>0, -t0, t0)
		mup = ifelse(t0>0, t0, -t0)
		pv = 2*pt(mlow, df)
		cat("P-v = 2 \U00D7 P(T > |T0|) =", round(pv, dig), "\n")
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
	invisible(list(stat=t0, df=df, sp2=sp2, pval=pv))
    # [3] Case of unknown and inequal variances
      } else {
	if (indata) {
		s1 = sig1
		s2 = sig2
	}	
    # Standard error and the test statistic
	df = (s1^2/n1+s2^2/n2)^2 / ((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
	se = sqrt(s1^2/n1 + s2^2/n2)
	xd = xb1 - xb2
	t0 = (xd-d0)/se
	cat("nu* =", round(df, dig), "\t s.e =", round(se, dig), "\n")
	cat(paste0("T0 = (", round(xb1, dig), " - ", round(xb2, dig), " - ", d0, ") / \U221A(",
		round(s1^2, dig), "/", n1, " + ", round(s2^2, dig), "/", n2,
		") = ", round(t0, dig)), "\n")
	xmax = max(abs(t0), 4)
	if (missing(prng)) prng=c(-xmax, xmax)
	if (missing(mt)) mt = paste0("Distribution of the Test Statistic under H0: t(", round(df, 3),")")
	if (missing(xlab)) xlab = "Test Statistic"
    # Plot the PDF
	xa = seq(prng[1], prng[2], length.out=101)
	win.graph(7,5)
	plot(xa, dt(xa, df), type="n", xlab=xlab, ylab="pdf", 
		ylim=c(-0.1, 1)*max(dt(xa, df)), main=mt)
    # P-value and the critical region
	if (side=="up" | grepl(side, "greater")) {
		pv = pt(t0, df, lower.tail=FALSE)
		cat("P-v = P(T > T0) =", round(pv, dig), "\n")
		cord.x = c(t0, seq(t0, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, dt(seq(t0, prng[2], length.out=20), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(t0, 0, t0, dt(t0, df), lwd=2, col=2)
		text(t0, dt(t0, df)*0.9, round(pv, dig), pos=4, col=2)
		text(t0, 0, round(t0, dig), pos=1, col=4)
	} else if (side=="low" | grepl(side, "less")) {
		pv = pt(t0, df)
		cat("P-v = P(T < T0) =", round(pv, dig), "\n")
		cord.x = c(prng[1], seq(prng[1], t0, length.out=20), t0) 
		cord.y = c(0, dt(seq(prng[1], t0, length.out=20), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(t0, 0, t0, dt(t0, df), lwd=2, col=2)
		text(t0, dt(t0, df)*0.9, round(pv, dig), pos=2, col=2)
		text(t0, 0, round(t0, dig), pos=1, col=4)
	} else if (grepl(side, "two sided")) {
		mlow = ifelse(t0>0, -t0, t0)
		mup = ifelse(t0>0, t0, -t0)
		pv = 2*pt(mlow, df)
		cat("P-v = 2 \U00D7 P(T > |T0|) =", round(pv, dig), "\n")
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
	invisible(list(stat=t0, df=df, se=se, pval=pv))
      }
}

# [11-3] Plot the PDF of Test Statistic under the Normal Distribution
#' @title Plot the PDF of Test Statistic under the Normal Distribution
#' @description Plot the PDF of Test Statistic under the Normal Distribution
#' @param md Difference of sample means or test statistic
#' @param mu0 Difference of population means under the null hypothesis, Default: 0
#' @param se Standard error of the difference of sample means, Default: 1
#' @param prng Range of x-axis, Default: c(-4, 4)
#' @param side Type of the alternative hypothesis, Default: 'two'
#' @param xlab Label of x-axis, Default: 'Sample Mean'
#' @param pvout Print p-value? Default: TRUE
#' @return None.
#' 
#' @examples 
#' normtest.plot(11.5-10.4, se=sqrt(2.2/20+2.4/25))
#' normtest.plot(1.93, xlab="Test Statistic")
#' @rdname normtest.plot
#' @export 
normtest.plot = function(md, mu0=0, se=1, prng=c(-4,4), side="two", xlab="Sample Mean", pvout=TRUE) {
	xa = seq(prng[1], prng[2], length.out=101)
	plot(xa, dnorm(xa, mu0, se), type="n", xlab=xlab, ylab="pdf", 
		ylim=c(-0.1, 1)*max(dnorm(xa, mu0, se)),
		main=bquote(bold("Distribution of ")~bold(.(xlab))~ bold("under H0 :")~" N("~ .(mu0)~","~.(round(se,3))^2~")"))
    # P-value and the critical region
	if (side=="up" | grepl(side, "greater")) {
		pv = pnorm(md, mu0, se, lower.tail=FALSE)
		if (pvout) cat("P-v =", pv, "\n")
		cord.x = c(md, seq(md, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, dnorm(seq(md, prng[2], length.out=20), mu0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(md, 0, md, dnorm(md, mu0, se), lwd=2, col=2)
		text(md, dnorm(md, mu0, se)*0.9, round(pv, 4), pos=4, col=2)
		text(md, 0, round(md, 4), pos=1, col=4)
	} else if (side=="low" | grepl(side, "less")) {
		pv = pnorm(md, mu0, se)
		if (pvout) cat("P-v =", pv, "\n")
		cord.x = c(prng[1], seq(prng[1], md, length.out=20), md) 
		cord.y = c(0, dnorm(seq(prng[1], md, length.out=20), mu0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(md, 0, md, dnorm(md, mu0, se), lwd=2, col=2)
		text(md, dnorm(md, mu0, se)*0.9, round(pv, 4), pos=2, col=2)
		text(md, 0, round(md, 4), pos=1, col=4)
	} else if (grepl(side, "two sided")) {
		mlow = ifelse(md>mu0, 2*mu0-md, md)
		mup = ifelse(md>mu0, md, 2*mu0-md)
		pv = 2*pnorm(mlow, mu0, se)
		if (pvout) cat("P-v =", pv, "\n")
		cord.x = c(mup, seq(mup, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, dnorm(seq(mup, prng[2], length.out=20), mu0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		cord.x = c(prng[1], seq(prng[1], mlow, length.out=20), mlow) 
		cord.y = c(0, dnorm(seq(prng[1], mlow, length.out=20), mu0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(c(mlow, mup), 0, c(mlow, mup), dnorm(md, mu0, se), lwd=2, col=2)
		text(c(mlow, mup), dnorm(md, mu0, se)*0.9, round(pv/2, 4), pos=c(2,4), col=2)
		text(c(mlow, mup), 0, round(c(mlow, mup), 4), pos=1, col=4)
	}
	abline(h=0); abline(v=mu0, lty=2, lwd=2, col="green3")
	lines(xa, dnorm(xa, mu0, se), type="l", lwd=2, col=4)
}

# [11-4] Plot the PDF of Test Statistic with the T-distribution
#' @title Plot the PDF of Test Statistic with the T-distribution
#' @description Plot the PDF of Test Statistic with the T-distribution
#' @param md T-test statistic for the difference of population means
#' @param deg Degree of freedom
#' @param prng Range of x-axis, Default: c(-4, 4)
#' @param side Type of the alternative hypothesis, Default: 'two'
#' @param dig Number of digits below the decimal point, Default: 4
#' @param mt Plot title
#' @param pvout Print p-value? Default: TRUE
#' @return None.
#' 
#' @examples 
#' ttest.plot(1.96, deg=24)
#' @rdname ttest.plot
#' @export
ttest.plot = function(md, deg, prng=c(-4,4), side="two", dig=4, mt, pvout=TRUE) {
	xa = seq(prng[1], prng[2], length.out=101)
	if (missing(mt)) mt = paste0("Distribution of the Test Statistic under H0: t(", round(deg,3),")")
    # Plot the PDF
	plot(xa, dt(xa, deg), type="n", xlab="Test Statistic", ylab="pdf", 
		ylim=c(-0.1, 1)*max(dt(xa, deg)), main=mt)
    # P-value and the critical region
	if (side=="up"| grepl(side, "greater")) {
		pv = pt(md, deg, lower.tail=FALSE)
		if (pvout) cat("P-v =", pv, "\n")
		cord.x = c(md, seq(md, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, dt(seq(md, prng[2], length.out=20), deg), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(md, 0, md, dt(md, deg), lwd=2, col=2)
		text(md, dt(md, deg)*0.9, round(pv, dig), pos=4, col=2)
		text(md, 0, round(md, dig), pos=1, col=4)
	} else if (side=="low"| grepl(side, "less")) {
		pv = pt(md, deg)
		if (pvout) cat("P-v =", pv, "\n")
		cord.x = c(prng[1], seq(prng[1], md, length.out=20), md) 
		cord.y = c(0, dt(seq(prng[1], md, length.out=20), deg), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(md, 0, md, dt(md, deg), lwd=2, col=2)
		text(md, dt(md, deg)*0.9, round(pv, dig), pos=2, col=2)
		text(md, 0, round(md, dig), pos=1, col=4)
	} else if (grepl(side, "two side")) {
		mlow = ifelse(md>0, -md, md)
		mup = ifelse(md>0, md, -md)
		pv = 2*pt(mlow, deg)
		if (pvout) cat("P-v =", pv, "\n")
		cord.x = c(mup, seq(mup, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, dt(seq(mup, prng[2], length.out=20), deg), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		cord.x = c(prng[1], seq(prng[1], mlow, length.out=20), mlow) 
		cord.y = c(0, dt(seq(prng[1], mlow, length.out=20), deg), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(c(mlow, mup), 0, c(mlow, mup), dt(md, deg), lwd=2, col=2)
		text(c(mlow, mup), dt(md, deg)*0.9, round(pv/2, dig), pos=c(2,4), col=2)
		text(c(mlow, mup), 0, round(c(mlow, mup), dig), pos=1, col=4)
	}
	abline(h=0); abline(v=0, lty=2, lwd=2, col="green3")
	lines(xa, dt(xa, deg), type="l", lwd=2, col=4)
}

# [11-5]  Confidence Interval for the Difference of Population Proportions
#' @title Confidence Interval for the Difference of Proportions
#' @description Confidence Interval for the Difference of Population Proportions
#' @param n1 Sample size of population1
#' @param x1 Number of successes in samples from population1
#' @param n2 Sample size of population2
#' @param x2 Number of successes in samples from population2
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' prob2.ci(n1=160, x1=12, n2=200, x2=13, alp=0.1)
#' prob2.ci(n1=160, x1=12, n2=200, x2=13, alp=0.05)
#' @rdname prob2.ci
#' @export 
prob2.ci = function(n1, x1, n2, x2, alp=0.05, dig=4) {
	p1 = x1/n1
	p2 = x2/n2
	pd = p1-p2
	err = qnorm(1-alp/2)*sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
	cat(paste0("[(", round(p1, dig), " - ", round(p2, dig), ") \U00B1 ", round(qnorm(1-alp/2), dig), 
		" \U00D7 \U221A(", round(p1, dig), " \U00D7 ", round(1-p1,dig), "/", n1, 
		" + ", round(p2, dig), " \U00D7 ", round(1-p2,dig), "/", n2, 
		")]\n = [", round(pd, dig), " \U00B1 ", round(err, dig), "] = [", 
		round(pd-err, dig), ", ", round(pd+err, dig),"]"), "\n")
}

# [11-6] Test for the Difference of Population Proportions (Large Sample)
#' @title Test for the Difference of Proportions (Large Sample)
#' @description Test for the Difference of Population Proportions (Large Sample)
#' @param n1 Sample size of population1
#' @param x1 Number of successes in samples from population1
#' @param n2 Sample size of population2
#' @param x2 Number of successes in samples from population2
#' @param prng Range of x-axis, Default: [d0-4se, d0+4se]
#' @param side Type of the alternative hypothesis, Default: 'two'
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' prob2test.plot(n1=150, x1=12, n2=250, x2=10, side="up", dig=4)
#' prob2test.plot(n1=150, x1=12, n2=250, x2=10, side="two", dig=4)
#' @rdname prob2test.plot
#' @export
prob2test.plot = function(n1, x1, n2, x2, prng, side="two", mt, dig=4) {
    # Test statistic
	p1 = x1/n1
	p2 = x2/n2
	ph = (x1+x2)/(n1+n2)
	se = sqrt(ph*(1-ph)*(1/n1 + 1/n2))
	xd = p1 - p2
	z0 = xd/se
	cat("p1 =", round(p1, dig), "\t p2 =", round(p2, dig), "\t ph =", round(ph, dig), "\n")
	cat(paste0("se = \U221A(", round(ph, dig), " \U00D7 ", round(1-ph, dig), " \U00D7 (1/", 
		n1, " + 1/", n2, ")) =", round(se, dig)), "\n")
	cat(paste0("Z0 = (", round(p1, dig), " - ", round(p2, dig), ") / ",
		round(se, dig), " = ", round(z0, dig)), "\n")
	if (missing(prng)) prng=c(-4*se, 4*se)
	if (missing(mt)) mt=bquote(bold("Distribution of Proportion Difference under H0:")~" N(0,"~ .(round(se,3))^2~")")
    # Plot the PDF
	xa = seq(prng[1], prng[2], length.out=100)
	win.graph(7,5)
	plot(xa, dnorm(xa, 0, se), type="n", xlab="Difference of Population Proportions", ylab="pdf", 
		ylim=c(-0.1, 1)*max(dnorm(xa, 0, se)), main=mt)
    # P-value and the critical region
	if (side=="up" | grepl(side, "greater")) {
		pv = 1-pnorm(z0)
		cat("P-v = P(Z > Z0) =", round(pv, dig), "\n")
		cord.x = c(xd, seq(xd, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, dnorm(seq(xd, prng[2], length.out=20), 0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(xd, 0, xd, dnorm(xd, 0, se), lwd=2, col=2)
		text(xd, dnorm(xd, 0, se)*0.9, round(pv, 4), pos=4, col=2)
		text(xd, 0, round(xd, 4), pos=1, col=4)
	} else if (side=="low" | grepl(side, "less")) {
		pv = pnorm(z0)
		cat("P-v = P(Z < Z0) =", round(pv, dig), "\n")
		cord.x = c(prng[1], seq(prng[1], xd, length.out=20), xd) 
		cord.y = c(0, dnorm(seq(prng[1], xd, length.out=20), 0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(xd, 0, xd, dnorm(xd, 0, se), lwd=2, col=2)
		text(xd, dnorm(xd, 0, se)*0.9, round(pv, 4), pos=2, col=2)
		text(xd, 0, round(xd, 4), pos=1, col=4)
	} else if (grepl(side, "two sided")) {
		mlow = ifelse(xd>0, -xd, xd)
		mup = ifelse(xd>0, xd, -xd)
		pv = 2*(1-pnorm(abs(z0)))
		cat("P-v = 2 \U00D7 P(Z > |Z0|) =", round(pv, dig), "\n")
		cord.x = c(mup, seq(mup, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, dnorm(seq(mup, prng[2], length.out=20), 0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		cord.x = c(prng[1], seq(prng[1], mlow, length.out=20), mlow) 
		cord.y = c(0, dnorm(seq(prng[1], mlow, length.out=20), 0, se), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(c(mlow, mup), 0, c(mlow, mup), dnorm(xd, 0, se), lwd=2, col=2)
		text(c(mlow, mup), dnorm(xd, 0, se)*0.9, round(pv/2, 4), pos=c(2,4), col=2)
		text(c(mlow, mup), 0, round(c(mlow, mup), 4), pos=1, col=4)
	}
	abline(h=0); abline(v=0, lty=2, lwd=2, col="green3")
	lines(xa, dnorm(xa, 0, se), type="l", lwd=2, col=4)
}

# [11-7] Plot the PDF of the F-test Statistic
#' @title Plot the PDF of the F-test Statistic
#' @description Plot the PDF of the F-test Statistic
#' @param fstat F-test statistic for the ratio of two population variances
#' @param deg Vector of degree of freedoms
#' @param pmax Maximum probability for quantiles in x-axis, Default: 0.995
#' @param side Type of the alternative hypothesis, Default: 'two'
#' @return None.
#' 
#' @examples 
#' vo = var.test(rnorm(20, 10, 2.4), rnorm(25, 12, 1.8))
#' ftest.plot(fstat=vo$stat, deg=as.vector(vo$para), pmax=0.9999, side="two")
#' @rdname ftest.plot
#' @export 
ftest.plot = function(fstat, deg, pmax=0.995, side="two") {
    # Plot the PDF
	prng = c(0,qf(pmax, deg[1], deg[2]))
	xa = seq(prng[1], prng[2], length.out=101)
	plot(xa, df(xa, deg[1], deg[2]), type="n", xlab="F-statistic", ylab="pdf", 
		ylim=c(-0.1, 1)*max(df(xa, deg[1], deg[2])),
		main=paste0("Distribution of F-statistic under H0: F(", deg[1],", ", deg[2], ")"))
    # P-value and the critical region
	plow = pf(fstat, deg[1], deg[2])
	if (side=="up" | grepl(side, "greater")) {
		pv = 1-plow
		cat("P-v =", pv, "\n")
		cord.x = c(fstat, seq(fstat, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, df(seq(fstat, prng[2], length.out=20), deg[1], deg[2]), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(fstat, 0, fstat, df(fstat, deg[1], deg[2]), lwd=2, col=2)
		text(fstat, df(fstat, deg[1], deg[2])*0.9, round(pv, 4), pos=4, col=2)
		text(fstat, 0, round(fstat, 4), pos=1, col=4)
	} else if (side=="low" | grepl(side, "less")) {
		pv = plow
		cat("P-v =", pv, "\n")
		cord.x = c(prng[1], seq(prng[1], fstat, length.out=20), fstat) 
		cord.y = c(0, df(seq(prng[1], fstat, length.out=20), deg[1], deg[2]), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(fstat, 0, fstat, df(fstat, deg[1], deg[2]), lwd=2, col=2)
		text(fstat, df(fstat, deg[1], deg[2])*0.9, round(pv, 4), pos=2, col=2)
		text(fstat, 0, round(fstat, 4), pos=1, col=4)
	} else if (grepl(side, "two sided")) {
		pv = 2*min(plow, 1-plow)
		cat("P-v =", pv, "\n")
		mlow =qf(pv/2, deg[1], deg[2])
		mup =qf(1-pv/2, deg[1], deg[2])
		cord.x = c(mup, seq(mup, prng[2], length.out=20), prng[2]) 
		cord.y = c(0, df(seq(mup, prng[2], length.out=20), deg[1], deg[2]), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		cord.x = c(prng[1], seq(prng[1], mlow, length.out=20), mlow) 
		cord.y = c(0, df(seq(prng[1], mlow, length.out=20), deg[1], deg[2]), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(c(mlow, mup), 0, c(mlow, mup), df(c(mlow), deg[1], deg[2]), lwd=2, col=2)
		text(c(mlow, mup), df(c(mlow), deg[1], deg[2]), round(pv/2, 4), pos=c(2,4), col=2)
		text(c(mlow, mup), 0, round(c(mlow, mup), 4), pos=1, col=4)
	}
	abline(h=0); abline(v=qf(0.5, deg[1], deg[2]), lty=2, lwd=2, col="green3")
	lines(xa, df(xa, deg[1], deg[2]), type="l", lwd=2, col=4)
}

# [11-8] Simulate the Confidence Interval for a the Ratio of Population Variances
#' @title Simulate the Confidence Interval for a the Ratio of Variances
#' @description Simulate the Confidence Interval for a the Ratio of Population Variances
#' @param n1 Sample size of population1
#' @param n2 Sample size of population2
#' @param sig1 Standard deviation of population1
#' @param sig2 Standard deviation of population2
#' @param alp Level of significance, Default: 0.05
#' @param N Number of iterations, Default: 100
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param dig Number of digits below the decimal point, Default: 4
#' @param plot Logical value for plot, Default: TRUE
#' @return None.
#' 
#' @examples 
#' civar2.sim(n1=25, n2=16, sig1=sqrt(8), sig2=2)
#' civar2.sim(n1=25, n2=16, sig1=sqrt(8), sig2=2, N=10000, plot=F)
#' @rdname civar2.sim
#' @export
civar2.sim=function(n1, n2, sig1, sig2, alp=0.05, N=100, seed=9857, dig=4, plot=TRUE) {
    # Ratio of population variances
	vr0 = sig1^2/sig2^2
    # Create the matrix of confidence intervals
	ci = matrix(0, nrow=N, ncol=3)
	ir = 1:N
    # Tail values of the F-distribution
	fv1 = qf(alp/2, n1-1, n2-1)
	fv2 = qf(1-alp/2, n1-1, n2-1)
    # Set seed for reproducibility
	set.seed(seed)
    # Generate random numbers and calculate the confidence intervals
	for (i in ir) {
		x = rnorm(n1, 0, sig1)
		y = rnorm(n2, 0, sig2)
		xv = var(x)
		yv = var(y)
		xm = xv/yv
		lcl = xm/fv2
		ucl = xm/fv1
		ci[i, ] = c(lcl, xm, ucl)
	}
	if (plot) {
	    # Plot the PDF
		win.graph(7, 4)
		plot(ir, ci[ ,2], type="p", pch=19, cex=0.6, col=1, ylim=c(min(ci), max(ci)), 
			main="Confidence Intervals for Ratio of Population Variances", 
				ylab="Confidence Interval", xlab="Iteration")
		abline(h=vr0, col=2)
		arrows(ir, ci[ ,1], ir, ci[ ,3], length=0.03, code=3, angle=90, lwd=1.5,
			col=ifelse((ci[ , 1]>vr0 | ci[ , 3]<vr0), 2, 4) )
	}
    # Number of confidence intervals without the ratio of population variances
	nup = sum(ci[ , 1]>vr0)
	nlow = sum(ci[ ,3]<vr0)
	cat(paste0("P(LCL > ", vr0, ") = ", nup, "/", N, " = ", nup/N,
		"\t P(UCL < ", vr0, ") = ", nlow, "/", N, " = ", nlow/N), "\n")
}