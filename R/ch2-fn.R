# [Ch-2 Functions] ----------------------------------------------------------------------------------
# [Ch-2 Function Manual] -----------------------------------------

#' Chapter 2 Functions Manual 
#' 
#' This function allows you to review the 7 functions in ch2. 
#' @param fn Number 0 to 7 for function. Defaults to 0. 
#' @keywords Ch2 Descriptive Statistics 
#' @return None
#' @examples 
#' ch2.man()
#' ch2.man(3:5)
#' @export 
ch2.man = function(fn=0) {
    if (0 %in% fn) {
	cat("[1] freq.table\t\t Making a Frequency Table\n")
	cat("[2] unstable.hist \t Making Histograms of Unstable Processes\n")
	cat("[3] strat.hist\t\t Making Stratified Histograms\n")
	cat("[4] corr.plot6\t\t Making Scatter Plots of Six Cases\n")
	cat("[5] scat.lm\t\t Making a Scatter Plot with a Regression Line\n")
	cat("[6] location.est \t Calculating Measures of Central Location\n")
	cat("[7] spread.est\t\t Calculating Measures of Dispersion\n")
    }
    if (1 %in% fn) {
	cat("[1] Making a Frequency Table\n")
	cat("freq.table(x, cuts, dig=4, mp=FALSE, ...)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x  \t Data vector\n")
	cat("[Optional Input]--------------------------\n")
	cat("cuts\t Vector of class limits (default=built-in values)\n")
	cat("dig \t Number of digits below decimal point (default=4)\n")
	cat("mp \t Plot histogram? (default=FALSE)\n")
	cat("...   \t Graphic parameter (default=built-in values)\n")
    }
    if (2 %in% fn) {
	cat("[2] Making Histograms of Unstable Processes\n")
	cat("unstable.hist(N=200, m1=10, s1=1, m2=6, s2=0.5, a=8, b=9, c=9, vc=rep(\"cyan\", 4))\n")
	cat("[Optional Input]--------------------------\n")
	cat("N  \t Number of data for each histogram (default=200)\n")
	cat("m1 \t Mean the main distribution (default=10)\n")
	cat("s1 \t Standard deviation of the main distribution (default=1)\n")
	cat("m2 \t Mean of the contaminated distribution (default=6)\n")
	cat("s2 \t Standard deviation of the contaminated distribution (default=0.5)\n")
	cat("a  \t Lower limit of the missing values (default=8)\n")
	cat("b  \t Upper limit of the missing values (default=9)\n")
	cat("c  \t Lower limit of the cliff type range (default=9)\n")
	cat("vc \t Color vector of histograms (default=\"cyan\")\n")
    }
    if (3 %in% fn) {
	cat("[3] Making Stratified Histograms\n")
	cat("strat.hist(ng=3, n=200, m=c(6,10,14), s=1, sp=c(4, 16), vc, prob=FALSE)\n")
	cat("[Optional Input]--------------------------\n")
	cat("ng \t Number of groups (default=3)\n")
	cat("n  \t Number of data for each histogram (default=200)\n")
	cat("m  \t Vector of mean for each group (default=c(6,10,14))\n")
	cat("s  \t Vector of standard deviation or each group (default=1)\n")
	cat("sp \t Specification limits (default=c(4, 16))\n")
	cat("vc \t Color vector(1+ng) of histograms (default=\"orange\"(total), \"cyan\"(sub))\n")
	cat("prob\t Logical value for selecting density instead of frequency (default=FALSE)\n")
    }
    if (4 %in% fn) {
	cat("[4] Making Scatter Plots of Six Cases\n")
	cat("corr.plot6(m1 = 60, s1=10, m2=60, s2=10, r=0.7, r2=0.8, n=50)\n")
	cat("[Optional Input]--------------------------\n")
	cat("m1 \t Mean of x (default= 60)\n")
	cat("s1  \t Standard deviation of x (default=10)\n")
	cat("m2 \t Mean of y (default=60)\n")
	cat("s2 \t Standard deviation of y (default=10)\n")
	cat("r   \t Correlation coefficient of x and y (default=0.7)\n")
	cat("r2  \t Correlation coefficient of the stratified sample (default=0.8)\n")
	cat("n   \t Number of data pairs (default=50)\n")
    }
    if (5 %in% fn) {
	cat("[5] Making a Scatter Plot with a Regression Line\n")
	cat("scat.lm(x, y, mt, xl, yl, w=c(7, 5), ...)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x  \t Data vector for x-axis\n")
	cat("y  \t Data vector for y-axis\n")
	cat("[Optional Input]--------------------------\n")
	cat("mt \t Title of the scatter plot\n")
	cat("xl \t Label of x-axis\n")
	cat("yl \t Label of y-axis\n")
	cat("w  \t Size of the graphic window (default=c(7, 5))\n")
	cat("...  \t Graphic parameters \n")
    }
    if (6 %in% fn) {
	cat("[6] Calculating Measures of Central Location\n")
	cat("location.est(x, tr=0.1, detail=FALSE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x  \t Data vector or matrix\n")
	cat("[Optional Input]--------------------------\n")
	cat("tr \t Trim proportion of the trimmed mean (default=0.1)\n")
	cat("detail\t Logical value for printing detailed output (default=FALSE)\n")
    }
    if (7 %in% fn) {
	cat("[7] Calculating Measures of Dispersion\n")
	cat("spread.est(x, detail=FALSE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x  \t Data vector or matrix\n")
	cat("[Optional Input]--------------------------\n")
	cat("detail\t Logical value for printing detailed output (default=FALSE)\n")
    }
}

# [2-1] Making a Frequency Table

#' Frequency Table
#' 
#' To create a frequency table for a vector (or matrix) x
#' @param x Data vector (or matrix).
#' @param cuts Breaks (or number) of intervals.
#' @param dig Number of digits. Defaults to 4.
#' @param mp Logical variable for making histogram. Defaults to FALSE.
#' @param ... Other graphic parameters
#' @keywords Ch2 Descriptive Statistics 
#' @return xtab Frequency Table
#' @examples
#' freq.table(rnorm(100)) 
#' @export
freq.table = function(x, cuts, dig=4, mp=FALSE, ...) {
	x = as.vector(x)
	n = length(x)
    # class limits
	if (missing(cuts)) cuts = hist(x, plot=F)$breaks
    # statistics
	xh = hist(x, breaks=cuts, plot=mp, ...)
	xcf = cumsum(xh$counts)
	xrf = xh$counts/n
	xrcf = xcf/n
    # frequency table
	ng = length(cuts)
	xclass = paste0("(", xh$breaks[-ng], ", ", xh$breaks[-1], "]")
	xtab = cbind(xh$mids, xh$counts, xcf, round(xrf, dig), round(xrcf, dig))
	rownames(xtab) = xclass
	colnames(xtab) = c("Center", "Freq", "Cum-Fr", "Rel-Fr", "Rel-CFr")
	return(xtab)
}

# [2-2] Making Histograms of Unstable Processes

#' Unstable Histograms
#' 
#' To create 4 types of unstable histograms
#' @param N Number of random variates. Defaults to 200. 
#' @param m1,m2 Means of two normal distributions. 
#' @param s1,s2 Standard deviations of two normal distributions.
#' @param mp Logical variable for making histogram. Defaults to FALSE.
#' @param ... Other graphic parameters
#' @keywords Ch2 Descriptive Statistics
#' @return None
#' @examples 
#' unstable.hist()
#' unstable.hist(m1=15, s1=2, m2=8, s2=1, a=12, b=13, c=10)
#' @export 
unstable.hist = function(N=200, m1=10, s1=1, m2=6, s2=0.5, 
		a=8, b=9, c=9, vc=rep("cyan", 4)) {
    # creating uniform probability vectors (0.9N, 0.5N, 0.1N)
	p1 = (1:(0.9*N))/(0.9*N+1)
	p2 = (1:(0.5*N))/(0.5*N+1)
	p3 = (1:(0.1*N))/(0.1*N+1)
    # creating quantiles of N(m1, s1^2) distribution (0.9N, 0.5N)
	x1 = qnorm(p1, mean=m1, sd=s1)
	x2 = qnorm(p2, mean=m1, sd=s1)
    # creating quantiles of N(m2, s1^2) and N(m2, s2^2) distribution (0.5N, 0.1N)
	y1 = qnorm(p2, mean=m2, sd=s1)
	y2 = qnorm(p3, mean=m2, sd=s2)
    # (a) Iland type : 90%(x1) + 10%(y2)
	da = c(x1, y2)
    # (b) Camel type : 50%(x2) +50%(y1)
	db = c(x2, y1)
    # (c) Sink type : remove values of x1 in [a, b]
	dc = x1[(x1>b) | (x1<a)]
    # (d) Cliff type : emove values of x1 below c
	dd = x1[x1>=c]
    # making histograms
	win.graph(7, 6)
	par(mfrow=c(2,2))
	hist(da, breaks=15, main="Type-A", xlab="(a)", col=vc[1])
	hist(db, breaks=15, main="Type-B", xlab="(b)", col=vc[2])
	hist(dc, breaks=12, main="Type-C", xlab="(c)", col=vc[3])
	hist(dd, breaks=12, main="Type-D", xlab="(d)", col=vc[4])
}

# [2-3] Making Stratified Histograms

#' Stratified Histograms
#' 
#' To create Stratified Histograms from a mixed data set
#' @param ng Number of classes. Defaults to 3.
#' @param n Number of samples. Defaults to 200.
#' @param m Vector of means. Defaults to c(6,10,14)
#' @param s Standard deviations. Default: 1
#' @param sp Specification limits. Default: c(4, 16)
#' @param vc Colors of histograms.
#' @param prob Density scale? Default: FALSE
#' @keywords Ch2 Descriptive Statistics
#' @return None
#' @examples 
#' strat.hist()
#' strat.hist(ng=4, m=c(6,10,14,18), sp=c(4,20))
#' @export 
strat.hist = function(ng=3, n=200, m=c(6,10,14), s=1, sp=c(4, 16), vc, prob=FALSE) {
	if (ng >=9) stop("Number of class should be less than 9!")
	if (length(s)==1) s=rep(s, ng)
    # creating uniform probability vector (n)
	p = (1:n)/(n+1)
    # creating quantiles of N(m, s^2) distribution (n each)
	x = vector("list", ng)
	for (k in 1:ng) x[[k]] = qnorm(p, mean=m[k], sd=s[k])
    # combining data
	xd = x[[1]]
	for (k in 2:ng) xd = c(xd, x[[k]])
	nb1 = ceiling(sqrt(n*ng))
	nb2 = ceiling(sqrt(n))
    # adjusting graphic parameters
	nr = ifelse(ng<=5, 2, 3)
	nc = ceiling((ng+1)/nr)
	h = ifelse(nr>2, 9, 6)
	w = ifelse(nc>2, 9, 7)
	mt = paste0("group-", LETTERS[1:ng])
	xl = c(min(m[1]-3*s[1], sp[1]-2*s[1]), max(m[ng]+3*s[ng], sp[2]+2*s[ng]))
	if (missing(vc)) vc = c("orange", rep("cyan", ng))
    # making histograms
	win.graph(w, h)
	par(mfrow=c(nr, nc))
	par(mar=c(3,3,4,1))
	xh = hist(xd, breaks=nb1, main="All Data", probability=prob,
		ylab="", xlab="", xlim=xl, col=vc[1])
	ym = ifelse(prob, max(xh$density), max(xh$counts))
	brk = xh$breaks
	segments(sp, 0, sp, ym/2, lwd=2, col=2)
	text(sp, c(ym/2,ym/2), c("SL", "SU"), col=2, pos=3)
	for (k in 1:ng) {hist(x[[k]], breaks=brk, main=mt[k], probability=prob,
		ylab="", xlab="", ylim=c(0,ym), xlim=xl, col=vc[k+1])
		segments(sp, 0, sp, ym/2, lwd=2, col=2)
		text(sp, c(ym,ym)/2, c("SL", "SU"), col=2, pos=3) }
}

# [2-4] Making Scatter Plots of Six Cases
# creating random variables of the bivariate normal distribution
rbivariate = function(m1, s1, m2, s2, r, n) {
    z1 = rnorm(n)
    z2 = rnorm(n)
    x = sqrt(1-r^2)*s1*z1 + r*s1*z2 + m1
    y = s2*z2 + m2
    return(list(x,y))
}

# making 6 types of scatter plots

#' @title Six Scatter Plots
#' @description Create 6 types of scatter plots.
#' @param m1 Mean 1. Default: 60
#' @param s1 Standard deviation 1. Default: 10
#' @param m2 Mean 2. Default: 60
#' @param s2 Standard deviation 2. Default: 10
#' @param r Correlation Coefficient 1. Default: 0.7
#' @param r2 Correlation Coefficient 2. Default: 0.8
#' @param n Number of samples. Default: 50
#' @return None
#' @keywords Ch2. Descriptive Statistics 
#' @examples 
#' corr.plot6()
#' corr.plot6(r=0.6, r2=0.9, n=100)
#' @export
corr.plot6 = function(m1 = 60, s1=10, m2=60, s2=10, r=0.7, r2=0.8, n=50) {
    # adjusting graphic parameters
	x1 = floor(m1-3*s1)
	x2 = ceiling(m1+3*s1)
	y1 = floor(m2-3*s2)
	y2 = ceiling(m2+3*s2)
	xa = seq(m1-2.5*s1, m1+2.5*s1, length=n)
    # setting seed
	set.seed(9857)
    # positive correlation
	d1 = rbivariate(m1, s1, m2, s2, r, n)
    # negative correlation
	d2 = rbivariate(m1, s1, m2, s2, -r, n)
    # quadratic relation
	d3 = list(xa, (m2+2*s2)-0.05*(xa-m1)^2+rnorm(n, 0, s2*0.6))
    # little correlation
	d4 = list(rnorm(n, m1, s1), rnorm(n, m2, s2))
	d81 = rbivariate(m1, s1, m2-1.5*s2, s2, r, n/2)
	d82 = rbivariate(m1, s1, m2+1.5*s2, s2, -r2, n/2)
	d8 = list(c(d81[[1]], d82[[1]]), c(d81[[2]], d82[[2]]))
    # open graphic window
	win.graph(9,6)
	par(mfrow=c(2,3))
    # positive correlation
	plot(d1[[1]],d1[[2]], pch=19, cex=1.2, xlab="(a) Positive Correlation", cex.lab=1.5, 
		ylab="", xlim=c(x1, x2), ylim=c(y1, y2))
	abline(lm(d1[[2]]~d1[[1]]), lwd=2, lty=2, col=2)
    # negative correlation
	plot(d2[[1]],d2[[2]], pch=19, cex=1.2, xlab="(b) Negative Correlation", cex.lab=1.5,
		ylab="", xlim=c(x1, x2), ylim=c(y1, y2))
	abline(lm(d2[[2]]~d2[[1]]), lwd=2, lty=2, col=2)
    # little correlation
	plot(d4[[1]],d4[[2]], pch=19, cex=1.2, xlab="(c) Little Correlation", cex.lab=1.5,
		ylab="", xlim=c(x1, x2), ylim=c(y1, y2))
	abline(lm(d4[[2]]~d4[[1]]), lwd=2, lty=2, col=2)
    # quadratic relation
	plot(d3[[1]],d3[[2]], pch=19, cex=1.2, xlab="(d) Quadratic Relation", cex.lab=1.5,
		ylab="", xlim=c(x1, x2), ylim=c(y1, y2))
	abline(lm(d3[[2]]~d3[[1]]), lwd=2, lty=2, col=2)
    # outlier
	o1 = c(d1[[1]][1:(n-2)], m1-2*s1, m1+2*s1)
	o2 = c(d1[[2]][1:(n-2)], m2+2*s2, m2-2*s2)
	plot(o1, o2, pch=19, cex=1.2, xlab="(e) Outlier", cex.lab=1.5,
		ylab="", xlim=c(x1, x2), ylim=c(y1, y2))
	points(c(m1-2*s1, m1+2*s1), c(m2+2*s2, m2-2*s2), pch=0, cex=2, col=2)
	abline(lm(o2~o1), lwd=2, lty=2, col=2)
    # stratification
	plot(d8[[1]],d8[[2]], pch=19, cex=1.2, xlab="(f) Stratification", cex.lab=1.5,
		ylab="", xlim=c(x1, x2), ylim=c(y1, y2))
	abline(lm(d8[[2]]~d8[[1]]), lwd=2, lty=2, col=2)
	abline(lm(d81[[2]]~d81[[1]]), lwd=2, lty=2, col=4)
	abline(lm(d82[[2]]~d82[[1]]), lwd=2, lty=2, col=4)
}

# [2-5] Making a Scatter Plot with a Regression Line

#' Scatter Plot with a Regression Line 
#' 
#' To create a scatter plot with a linear regression line.
#' @param x Vector of x variable.
#' @param y Vector of y variable.
#' @param mt Title of the plot.
#' @param xl Label of x-axis.
#' @param yl Label of y-axis.
#' @param w Size of graphic window. Default: c(7, 5)
#' @param ... Other graphic parameters.
#' @return None.
#' @keywords Ch2. Descriptive Statistics 
#' @examples 
#' scat.lm(mtcars$wt, mtcars$mpg, mt="Weight vs. MPG")
#' @export 
scat.lm = function(x, y, mt, xl, yl, w=c(7, 5), ...) {
	if (missing(xl)) xl = deparse(substitute(x))
	if (missing(yl)) yl = deparse(substitute(y))
	if (missing(mt)) mt = paste(xl, ":", yl, "scatter plot")
    # size of the graphic window
	win.graph(w[1], w[2])
    # simple scatter plot --> plot(x, y) function
	plot(x, y, main=mt, xlab=xl, ylab=yl, pch=19, cex=1.2, ...)
	grid(col=3)
    # fitted simple regression line --> lm(y ~ x) function & abline( ) function
	sr = lm(y ~ x)
	ssr = summary(sr)
	abline(sr, lty=2, lwd=2, col=2)
    # formula of fitted simple regression equation
	b = sr$coef[[2]]
	a = sr$coef[[1]]
	sign = ifelse(b < 0, "", "+")
	pos = ifelse(b <0, "topright", "topleft")
	pv = 1-pf(ssr$f[1], ssr$f[2], ssr$f[3])
	legend(pos, legend=c(paste("Y =", round(a, 4), sign, round(b, 4), "X"), 
		paste("R-sq =", round(ssr$r.sq, 4)), 
		paste("P-v =", format(pv, digits=3, scientific=T)) ),
		text.col=c(2, 4, 1), cex=1)
}

# [2-6] Calculating Measures of Central Location

#' Estimation of Location
#' 
#' To calculate location estimates. 
#' @param x Vector of input data.
#' @param tr Treaming ratio. Default: 0.1
#' @param detail Print detailed output? Default: FALSE
#' @return None
#' @keywords Ch2. Descriptive Statistics 
#' @examples 
#' location.est(mtcars$mpg, detail=T)
#' @export 
location.est = function(x, tr=0.1, detail=FALSE) {
	n = length(x)
    # mean --> mean( ) function
	xmean = mean(x)
    # median --> median( ) function
	xmed = median(x)
    # frequency --> table( ) function
	tabx = table(x)
    # mode --> value with maximum frequency in table
	xmode = as.numeric(names(tabx[tabx==max(tabx)]))
    # define function for geometric mean
	gm_mean = function(a) {prod(a)^(1/length(a))}
    # geometric mean --> use previously defined gm_mean( ) function
	gmean = gm_mean(x)
    # define function for harmonic mean
	hmean = 1/mean(1/x)
    # trimmed mean (10%) --> mean(x, trim) function
	tmean = mean(x, trim=tr)
    # display output --> cat( ) function
	if (detail==FALSE) {
	cat("Mean=", xmean, "\t Median=", xmed, "\t Mode=", xmode, "\n")
	cat("Geom. Mean=", gmean, "\t Harm. Mean=", hmean, 
		paste0("\t Trim. Mean(",tr,")="), tmean, "\n")
	}
	if (detail==TRUE) {
	nt = floor(n*tr)
	sumt = sum(sort(x)[(nt+1):(n-nt)])
	n2 = n - 2*nt
	cat("Calculation in Detail -----------------------------------------------------",
		"\n(1) Mean =", paste0(sum(x), "/", n), "=", xmean, 
		"\n(2) Median =", paste0("x(", (n+1)/2, ") ="), xmed, 
		"\n(3) Mode =", xmode, paste0("(", max(tabx), "times)"),
		"\n(4) Geom. Mean =", paste0(format(prod(x), digits=7, scientific=T), 
				"^(1/", n, ") ="), gmean, 
		"\n(5) Harm. Mean =", paste0("1/", format(mean(1/x), digits=7), " ="), hmean, 
		paste0("\n(6) Trim. Mean(",tr,") = ", sumt, "/", n2), "=", tmean, "\n")
	}
}

# [2-7] Calculating Measures of Dispersion

#' Spread Estimation
#' 
#' To calculate spread estimates. 
#' @param x Vector of input data.
#' @param detail Print detailed output? Default: FALSE
#' @return None
#' @keywords Ch2. Descriptive Statistics
#' @examples 
#' spread.est(mtcars$mpg, detail=T)
#' @export 
spread.est = function(x, detail=FALSE) {
	if (is.matrix(x)) x = as.vector(x)
	n = length(x)
    # sample variance --> var( ) function
	xvar = var(x)
    # standard deviation --> sd( ) function
	xsd = sd(x)
    # range --> max( )-min( ) function
	xrng = max(x) - min(x)
    # inter-quartile range --> IQR( ) function
	xiqr = IQR(x)
    # coefficient of variation --> sd( ) / mean( ) function
	xcv = xsd / mean(x)
    # display output
	if (detail==FALSE) {
	cat("Variance=", xvar, "\t Stand. Dev.=", xsd, 
		"\nRange=", xrng, "\t IQR=", xiqr, "\t CoV=", xcv, "\n")
	}
	if (detail==TRUE) {
	cat("Calculation in Detail -----------------------------------------------------",
	"\n(1) Variance =", paste0(sum(x^2)," - ", sum(x), "^2 / ", n), "=", xvar, 
	"\n(2) Stand. Dev. =", paste0("sqrt(", round(xvar, 7), ") ="), xsd, 
	"\n(3) Range =", max(x), "-", min(x), "=", xrng, 
	"\n(4) IQR =", quantile(x, 0.75), "-", quantile(x, 0.25), "=", xiqr, 
	"\n(5) CoV =", xsd, "/", mean(x), "=", xcv, "\n")
	}
}