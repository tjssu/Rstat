# [Ch-8 Functions] ----------------------------------------------------------------------------------
# [Ch-8 Function Manual] -----------------------------------------
# source("E:/R-stat/Digeng/ch8-function.txt")
#' @title Manual for Ch.8
#' @description Ch8. Normal and Related Distributions
#' @param fn Function number (0~11), Default: 0
#' @return None.
#' @examples 
#' ch8.man()
#' ch8.man(1)
#' @rdname ch8.man
#' @export 
ch8.man = function(fn=0) {
    if (0 %in% fn) {
	cat("[1] cont.spdf\t\tPlot the PDF of Continuous Random Variables (separate)\n")
	cat("[2] norm.trans\t\tCheck Probability Conservation in Standardizing the Normal Distribution\n")
	cat("[3] snorm.cdf\t\tPlot Standard Normal Cumulative Probability P(Z<z)\n")
	cat("[4] snorm.prob\t\tCentral Probability of the Standard Normal Distribution\n")
	cat("[5] snorm.quant \tQuantile Plot of the Standard Normal Distribution\n")
	cat("[6] chi.prob\t\tCumulative Probability of the Chi-square Distribution\n")
	cat("[7] chi.quant\t\tQuantile Plot of the Chi-square Distribution\n")
	cat("[8] tnorm.comp\t\tPlot Central Probability P(-k<X<k)\n")
	cat("[9] fdist.sim\t\tSimulation of the F-distribution\n") 
	cat("[10] f.prob\t\tCumulative Probability of the F-distribution\n")   
	cat("[11] f.quant\t\tQuantile Plot of the F-distribution\n")
    }
    if (1 %in% fn) {
	cat("[1] Plot the PDF of Continuous Random Variables (separate)\n")
	cat("cont.spdf(dist, lo, up, para, para2, ymax, xl, yl, dcol, np=100, xp)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("dist\t Distribution name ")
	cat("(\"exp\", \"gamma\", \"weibull\", \"beta\", \"norm\", \"t\", \"chisq\", \"f\")\n")
	cat("lo\t Lower limit of x-axis\n")
	cat("up\t Upper limit of x-axis\n")
	cat("para\t First parameter vector of the distribution\n")
	cat("para2\t Second parameter vector (except \"exp\", \"t\", \"chisq\")\n")
	cat("[Optional Input]--------------------------\n")
	cat("ymax\t Upper limit of y-axis\n")
	cat("xl\t Label vector of x-axis\n")
	cat("yl\t Label vector of y-axis\n")
	cat("dcol\t Graph color vector\n")
	cat("np\t Number of plot points(default=100)\n")
	cat("xp\t Location vector for vertical lines\n")
    }
    if (2 %in% fn) {
	cat("[2] Check Probability Conservation in Standardizing the Normal Distribution\n")
	cat("norm.trans(mu, sig, a, b, mt1, mt0, dig=4, span=3, np=100)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("mu\t Mean of the normal distribution\n")
	cat("sig\t Standard deviation of the normal distribution\n")
	cat("a\t Lower limit of X for calculating probability P(a<X<b)\n")
	cat("b\t Upper limit of X for calculating probability P(a<X<b)\n")
	cat("[Optional Input]--------------------------\n")
	cat("mt1\t Title of the normal distribution probability plot\n")
	cat("mt0\t Title of the standard normal distribution probability plot\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("span\t Range of x-axis (mu \U00B1 span \U00D7 sig) (default=3)\n")
	cat("np\t Number of plotting points (default=100)\n")
    }
    if (3 %in% fn) {
	cat("[3] Plot Standard Normal Cumulative Probability P(Z<z)\n")
	cat("snorm.cdf(zp, lo=-4, up=4, mt, dig=4)\n")
	cat("[Optional Input]--------------------------\n")
	cat("zp\t Vector of z-axis values (default=-2:2)\n")
	cat("lo\t Lower limit of z-axis (default=-4)\n")
	cat("up\t Upper limit of z-axis (default=4)\n")
	cat("mt\t Graph title (default=\"Cumulative Probabilities of the Standard Normal Distribution\")\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (4 %in% fn) {
	cat("[4] Central Probability of the Standard Normal Distribution\n")
	cat("snorm.prob(zp, lo=-4, up=4, mt, dig=4)\n")
	cat("[Optional Input]--------------------------\n")
	cat("zp\t Vector of z-axis values (default=1:4)\n")
	cat("lo\t Lower limit of z-axis (default=-4)\n")
	cat("up\t Upper limit of z-axis (default=4)\n")
	cat("mt\t Graph title (default=\"Central Probability of the Standard Normal Distribution\")\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (5 %in% fn) {
	cat("[5] Quantile Plot of the Standard Normal Distribution\n")
	cat("snorm.quant(pv, pv2, mt, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("pv\t Vector of probability values\n")
	cat("pv2\t Vector of specific probability values\n")
	cat("[Optional Input]--------------------------\n")
	cat("mt\t Graph title (default=\"Quantiles of the Standard Normal Distribution\")\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (6 %in% fn) {
	cat("[6] Cumulative Probability of the Chi-square Distribution\n")
	cat("chi.prob(nu, xp, pup=0.995, mt, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("nu\t Degree of freedom of the chi-square distribution\n")
	cat("xp\t Vector of specific x-axis values\n")
	cat("[Optional Input]--------------------------\n")
	cat("pup\t Upper limit of probabilities for quantiles (default=0.995)\n")
	cat("mt\t Graph title\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (7 %in% fn) {
	cat("[7] Quantile Plot of the Chi-square Distribution\n")
	cat("chi.quant(nu, pv, pv2=pv, pup=0.999, mt, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("nu\t Degree of freedom of the chi-square distribution\n")
	cat("pv\t Vector of probabilities for quantiles\n")
	cat("[Optional Input]--------------------------\n")
	cat("pv2\t Vector of probabilities for specific quantiles (default=pv)\n")
	cat("pup\t Upper limit of probabilities for quantiles (default=0.999)\n")
	cat("mt\t Graph title\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (8 %in% fn) {
	cat("[8] Compare T-Dist. with the Standard Normal\n")
	cat("tnorm.comp(nu=c(10, 30), lo=-3.5, up=3.5, dig=4, dcol)\n")
	cat("[Optional Input]--------------------------\n")
	cat("nu\t Degree of freedom for the chi-sq. dist. (default=c(10,30))\n")
	cat("lo\t Lower limit of x-axis (default=-3.5)\n")
	cat("up\t Upper limit of x-axis(default=3.5)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("dcol\t Color of plot lines\n")
    }
    if (9 %in% fn) {
	cat("[9] Simulation of the F-distribution\n")
	cat("fdist.sim(nu1=5, nu2=5, N=10000, ng=250, seed=9857, xp=1:9, dig=4)\n")
	cat("[Optional Input]--------------------------\n")
	cat("nu1\t Numerator degree of freedom (default=5)\n")
	cat("nu2\t Denominator degree of freedom (default=5)\n")
	cat("N\t Number of random values (default=10000)\n")
	cat("ng\t Number of classes in histogram (default=250)\n")
	cat("seed\t Seed value for random number generator (default=9857)\n")
	cat("xp\t Vector of x-axis values (default=1:9)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (10 %in% fn) {
	cat("[10] Cumulative Probability of the F-distribution\n")
	cat("f.prob(nu1, nu2, xp, mt, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("nu1\t Numerator degree of freedom (default=5)\n")
	cat("nu2\t Denominator degree of freedom (default=5)\n")
	cat("xp\t Vector of x-axis values\n")
	cat("[Optional Input]--------------------------\n")
	cat("mt\t Graph title\n")
	cat("dig=4\t Number of digits below the decimal point (default=4)\n")
    }
    if (11 %in% fn) {
	cat("[11] Quantile Plot of the F-distribution\n")
	cat("f.quant(nu1, nu2, pv, pv2=pv, pup=0.995, mt, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("nu1\t Numerator degree of freedom (default=5)\n")
	cat("nu2\t Denominator degree of freedom (default=5)\n")
	cat("pv\t Vector of probabilities for quantiles\n")
	cat("[Optional Input]--------------------------\n")
	cat("pv2\t Vector of probabilities for specific quantiles (default=pv)\n")
	cat("pup\t Upper limit of probabilities for quantiles (default=0.995)\n")
	cat("mt\t Graph title\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
}

# [8-1] Plot the PDF of Continuous Random Variables (separate)
# Get the PDF of continuous random variables
getpdf = function(dist, xa, para, para2) {
	np = length(xa)
	N = max(length(para), length(para2))
    # PDF name
	dpdf = paste0("d", dist)
	pdf = matrix(NA, nrow=np, ncol=N)
    # Vector of the PDF
	if (dist %in% c("exp", "t", "chisq")) { 
		for (k in 1:N) {
			pdf[, k] = do.call(dpdf, list(xa, para[k]))
		}
	} else if (dist == "gamma") { 	
		for (k in 1:N) {
			pdf[, k] = do.call(dpdf, list(xa, para[k], 1/para2[k]))
		}
	} else { 	for (k in 1:N) {
			pdf[, k] = do.call(dpdf, list(xa, para[k], para2[k]))
		}
	}
	invisible(pdf)
}
# Plot (separately) the PDF of Continuous Random Variables
#' @title Plot PDF of Continuous Random Variables
#' @description Plot (separately) the PDF of Continuous Random Variables
#' @param dist Distribution name ("exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f")
#' @param lo Lower limit of x-axis
#' @param up Upper limit of x-axis
#' @param para First parameter vector of the distribution
#' @param para2 Second parameter vector (except "exp", "t", "chisq")
#' @param ymax Upper limit of y-axis
#' @param xl Label vector of x-axis
#' @param yl Label vector of y-axis
#' @param dcol Graph color vector
#' @param np Number of plot points, Default: 100
#' @param xp Location vector for vertical lines
#' @return None.
#' @examples 
#' mu = c(0,0,2,2)
#' sig=c(1,2,1,2)
#' cont.spdf("norm", -7, 7, mu, sig, xp=mu)
#' @rdname cont.spdf
#' @export
cont.spdf = function(dist, lo, up, para, para2, ymax, xl, yl, dcol, np=100, xp) {
    # Number of parameters and their names
	N=length(para)
	pn=deparse(substitute(para))
    # Require para2 except the exponential distribution
	if (missing(para2)) {
		para2=para
	} else {	pn2 = deparse(substitute(para2))
		N2 = length(para2)
		if (N==1 & N2>1) {
			para = rep(para, N2)
			pn=deparse(substitute(para2))
		}
		if (N>1 & N2==1) para2=rep(para2, N)
		N = max(N,N2)
	}
    # Probability distribution name
    # ("Exponential", "Gamma", "Weibull", "Beta", "normal", "T-", "Chi-square", "F-")
	dlist=c("exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f")
	if (missing(dist)) {
		cat(paste(dlist, collapse=", "), "\n")
		stop("In put one of the distribution names above....")
	}
	distnum = grep(dist, dlist)
	if (length(distnum) != 1) stop("probability distribution name Error...")
	dist = dlist[distnum]
    # Main title label
	lab = list()
	for (k in 1:N) lab[[k]] = switch(distnum, 
		bquote( Exp ( lambda == .(para[k]) ) ),
		bquote( Gamma ( alpha == .(para[k]) , theta == .(para2[k]) ) ), 
		bquote( Weib ( alpha == .(para[k]) , theta == .(para2[k]) ) ), 
		bquote( Beta ( alpha == .(para[k]) , beta == .(para2[k]) ) ), 
		bquote( N ( mu == .(para[k]) , sigma^2 == .(para2[k]^2) ) ), 
		bquote( T ( nu == .(para[k]) ) ), 
		bquote( chi^2 ~ ( nu == .(para[k]) ) ), 
		bquote( F ( nu[1] == .(para[k]) , nu[2] == .(para2[k]) ) )  )
    # Calculate the PDF and the CDF
	xa = seq(lo, up, length=np)
	pdf = getpdf(dist, xa, para, para2)
    # Colors and labels
	if (missing(dcol)) dcol = rep(2, N)
	if (missing(yl)) yl  = rep("f(x)", N)
	if (missing(xl)) xl  = paste0("(", letters[1:N], ")")
    # Divide graphic window (1~9)
	nr = switch(N, 1, 1, 2, 2, 2, 2, 3, 3, 3)
	nc = switch(N, 1, 2, 2, 2, 3, 3, 3, 3, 3)
	win.graph(3.5*nc, 3*nr)
	par(mfrow=c(nr, nc))
    # Plot the PDF
	if (missing(ymax)) ymax = max(pdf)
	for (k in 1:N) {
		plot(xa, pdf[ ,k], type="l", main=lab[[k]], xlim=c(lo,up), ylim=c(0, ymax), 
			lwd=2, col=dcol[k], ylab=yl[k], xlab=xl[k] )
		if (!missing(xp)) abline(v=xp[k], lty=2, col=4)
	}
}

# [8-2] Check Probability Conservation in Standardizing the Normal Distribution
#' @title Standardization of the Normal Distribution
#' @description Check Probability Conservation in Standardizing the Normal Distribution
#' @param mu Mean of the normal distribution
#' @param sig Standard deviation of the normal distribution
#' @param a Lower limit of X for calculating probability P(a<X<b)
#' @param b Upper limit of X for calculating probability P(a<X<b)
#' @param mt1 Title of the normal distribution probability plot
#' @param mt0 Title of the standard normal distribution probability plot
#' @param dig Number of digits below the decimal point, Default: 4
#' @param span Range of x-axis in [mu-span*sig, mu+span*sig], Default: 3
#' @param np Number of plotting points, Default: 100
#' @return None.
#' @examples 
#' norm.trans(2, 2, -1, 4)
#' @rdname norm.trans
#' @export
norm.trans = function(mu, sig, a, b, mt1, mt0, dig=4, span=3, np=100) {
    # Calculate the original probability
	px = pnorm(b, mu, sig) - pnorm(a, mu, sig)
	cat(paste0("Pr(", a, " < X < ", b, ") = "), px, "\n")
    # Calculate the standardized probability
	c = (a-mu)/sig
	d = (b-mu)/sig
	pz = pnorm(d) - pnorm(c)
	cat(paste0("Pr(", round(c, dig), " < Z < ", round(d, dig), ") = "), pz, "\n")
    # Calculate the normal PDF
	lo = mu - span*sig
	up = mu + span*sig
	x1 = seq(lo, up, length=np)
	x0 = seq(lo-mu, up-mu, length=np)
	fx = matrix(c(dnorm(x0, 0, 1), dnorm(x1, mu, sig)), ncol=2, byrow=F)
	ymax = max(fx)
    # Set the title and the graphic window
	if (missing(mt1)) mt1 = bquote(N( mu == .(mu) ,  sigma^2 == .(sig^2) ) )
	if (missing(mt0)) mt0 = bquote(N( mu == 0 ,  sigma^2 == 1 ) )
	win.graph(7,6)
	par(mfrow=c(2,1))
	par(mar=c(3,4,3,1))
    # Plot the normal PDF
	plot(x1, fx[,2], type="n", main=mt1, ylim=c(0, ymax), ylab="f(x)", xlab="")
	cord.x = c(a, seq(a, b, 0.01), b) 
	cord.y = c(0, dnorm(seq(a, b, 0.01), mu, sig), 0) 
    # Plot polygon by using polygon( ) function
	polygon(cord.x, cord.y, col='lightcyan')
	ab = (a+b)/2
	text(ab, 0.4*dnorm(ab,mu,sig), labels=paste0("P(",a,"<X<",b,")\n=",round(px, dig)))
	lines(x1, fx[,2], lwd=2, col=2)
    # Plot the standard normal PDF
	plot(x0, fx[,1], type="n", main=mt0, ylim=c(0, ymax), ylab="f(x)", xlab="")
	cord.x = c(c, seq(c, d, 0.01), d) 
	cord.y = c(0, dnorm(seq(c, d, 0.01)), 0) 
	polygon(cord.x, cord.y, col='lightcyan')
	cd = (c+d)/2
	text(cd, 0.4*dnorm(cd), labels=paste0("P(",round(c, dig),"<Z<",round(d, dig),")\n=",round(pz, dig)))
	lines(x0, fx[,1], lwd=2, col=2)
}

# [8-3] Plot Standard Normal Cumulative Probability P(Z<z)
#' @title Plot Standard Normal Cumulative Probability
#' @description Plot Standard Normal Cumulative Probability P(Z<z)
#' @param zp Vector of z-axis values (default=-2:2)
#' @param lo Lower limit of z-axis, Default: -4
#' @param up Upper limit of z-axis, Default: 4
#' @param mt Graph title, Default: 'Cumulative Probabilities of the Standard Normal Distribution'
#' @param dig Number of digits below the decimal point (default=4), Default: 4
#' @return None.
#' @examples 
#' zp = seq(-2, 2, by=0.5)
#' snorm.cdf(zp)
#' @rdname snorm.cdf
#' @export
snorm.cdf = function(zp, lo=-4, up=4, mt, dig=4) {
    # Set the title and axis
	if (missing(mt)) mt = "Cumulative Probabilities of the Standard Normal Distribution"
	x = seq(lo, up, length=100)
	lo1 = lo - 0.12*(up-lo)
	lo2 = (lo1*2 + lo)/3
    # Plot the cumulative probability
	win.graph(7, 6)
	plot(x, pnorm(x), type ="n", main=mt, 
		ylim=c(-0.05, 1), xlim=c(lo1, up), ylab=bquote(Phi(z)), xlab="z")
	abline(h=0, col="green2")
	lines(x, pnorm(x), type="l", lwd=2, col=2)
    #  Display the cumulative probabilities up to zp
	if (missing(zp)) zp = -2:2
	yp = pnorm(zp)
	segments(zp, 0, zp, yp, lty=2, col=4)
	segments(zp, yp, lo, yp, lty=2, col=4)
	text(zp, 0, labels=zp, pos=1, cex=0.8)
	text(lo2, yp, labels=format(yp, digits=dig), cex=0.8)
}

# [8-4] Central Probability of the Standard Normal Distribution
#' @title Central Probability of the Standard Normal Distribution
#' @description Central Probability of the Standard Normal Distribution
#' @param zp Vector of z-axis values (default=1:4)
#' @param lo Lower limit of z-axis, Default: -4
#' @param up Upper limit of z-axis, Default: 4
#' @param mt Graph title (default="Central Probability of the Standard Normal Distribution")
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples 
#' zp = 1:3
#' snorm.prob(zp)
#' @rdname snorm.prob
#' @export
snorm.prob = function(zp, lo=-4, up=4, mt, dig=4) {
    # Set the title and axis
	if (missing(mt)) mt = "Central Probability of the Standard Normal Distribution"
	x = seq(lo, up, length=100)
	ymax = max(dnorm(x))
	nzp = length(zp)
	y1 = 0.11*nzp*ymax
   # Plot the central probability
	win.graph(7, 5)
	plot(x, dnorm(x), type ="n", main=mt, 
		ylim=c(-y1, ymax), xlim=c(lo, up), ylab=bquote(phi(z)), xlab="z")
	abline(h=0, col="green4")
	lines(x, dnorm(x), type="l", lwd=2, col=2)
    # Display the central probability P(-zp<Z<zp)
	if (missing(zp)) zp = 1:4
	prz = pnorm(zp)-pnorm(-zp)
	abline(v=c(-zp, zp), lty=2, col=4)
	yp = -0.11*ymax*(1:nzp)
	arrows(-zp, yp, zp, yp, length=0.1, code=3, col=4)
	text(0, yp, labels=paste0("P(",-zp,"<Z<",zp,")=", format(prz, digits=dig)), pos=3, cex=0.8)
}

# [8-5] Quantile Plot of the Standard Normal Distribution
#' @title Quantile Plot of the Standard Normal Distribution
#' @description Quantile Plot of the Standard Normal Distribution
#' @param pv Vector of probability values
#' @param pv2 Vector of specific probability values
#' @param mt Graph title (default="Quantiles of the Standard Normal Distribution")
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples 
#' pv = c(0.005, 0.01, 0.025, 0.05, 1:9/10, 0.95, 0.975, 0.99, 0.995)
#' snorm.quant(pv, pv)
#' @rdname snorm.quant
#' @export 
snorm.quant = function(pv, pv2, mt, dig=4) {
	if (missing(mt)) mt = "Quantiles of the Standard Normal Distribution"
      # Quantiles
	zv = qnorm(pv)
	names(zv) = pv
	print(round(zv, dig))
	zv2 = qnorm(pv2)

      # Quantiles Graph
	x1 = min(-3, qnorm(min(pv)))
	x2 = max(3, qnorm(max(pv)))
	x = seq(x1, x2, length=100)
	ymax = max(dnorm(x))
	nzp = min(4, ceiling(length(pv2)/4))
	y1 = 0.1*nzp*ymax
	win.graph(7, 5)
	plot(x, dnorm(x), type ="n", main=mt, 
		ylim=c(-y1, ymax*1.2), xlim=c(x1, x2), ylab=bquote(phi(z)), xlab="z")
	abline(h=0, col="green4")
	lines(x, dnorm(x), type="l", lwd=2, col=2)

      # Display Quantiles
	fzv2 = format(zv2, digits=dig)
	yp = -0.08*ymax*(1:nzp)
	yp2 = ymax*1.2+yp
	segments(zv2, yp, zv2, yp2, lty=2, col=4)
	text(zv2, yp2, pv2, cex=0.8, pos=3, col=2)
	text(zv2, yp, fzv2, pos=1, cex=0.8)
}

# Old Version
snorm.quant0 = function(pv, pv2, mt, dig=4) {
	if (missing(mt)) mt = "Quantile Plot of the Standard Normal Distribution"
    # Quantiles of the standard normal distribution
	zv = qnorm(pv)
	names(zv) = pv
	print(round(zv, dig))
    # Plot quantiles
	p = 1:199/200
	win.graph(7, 6)
	plot(p, qnorm(p), type ="l", lwd=2, col=2, ylim=c(-3, 2.7), xlim=c(-0.1, 1),
		ylab=bquote(Phi^-1 ~ (p)), xlab="p", main=mt)
	abline(v=0.5, h=0, lty=1, col=3)
   # Display major quantiles
	segments(pv, -2.7, pv, zv, lty=2, col=4)
	arrows(pv, zv, -0.02, zv, length=0.07, lty=2, col=4)
	text(-0.08, zv, labels=format(zv, digits=3), cex=0.9)
	text(pv2, -3, labels=pv2, cex=0.9)
}

# [8-6] Cumulative Probability of the Chi-square Distribution
#' @title Cumulative Probability of the Chi-square Distribution
#' @description Cumulative Probability of the Chi-square Distribution
#' @param nu Degree of freedom of the chi-square distribution
#' @param xp Vector of specific x-axis values
#' @param pup Upper limit of probabilities for quantiles, Default: 0.995
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @details DETAILS
#' @examples 
#' k= 1:10; nu= 5
#' chi.prob(nu, k)
#' @rdname chi.prob
#' @export
chi.prob = function(nu, xp, pup=0.995, mt, dig=4) {
    # Set the title and axis
	if (missing(mt)) mt = bquote("Cumulative Probabilities " ~ F(x) == P(chi[.(nu)]^2 < x))
	up = qchisq(pup, nu)
	x = seq(0, up, length=100)
	ymax = max(dchisq(x, nu))
	nxp = length(xp)
    # Print the cumulative probabilities
	prx = pchisq(xp, nu)
	names(prx) = xp
	print(round(prx, dig))
    # Plot the chi-square distribution PDF
	y1 = 0.1*nxp*ymax
	wc = ifelse(nxp > 5, 6, 5)
	win.graph(7, wc)
	plot(x, dchisq(x, nu), type ="n", main=mt, 
		ylim=c(-y1, ymax), xlim=c(0, up), ylab="f(x)", xlab="x")
	abline(h=0, col="green4")
	lines(x, dchisq(x, nu), type="l", lwd=2, col=2)
    # Display specific cumulative probabilities
	abline(v=c(0, nu), col=3)
	abline(v=xp, lty=2, col=4)
	yp = -0.1*ymax*(1:nxp)
	arrows(0, yp, xp, yp, length=0.1, code=2, col=4)
	text(xp, yp, labels=paste0("F(", xp, ")=", round(prx, dig)), pos=4, cex=0.8)
}

# [8-7] Quantile Plot of the Chi-square Distribution
#' @title Quantile Plot of the Chi-square Distribution
#' @description Quantile Plot of the Chi-square Distribution
#' @param nu Degree of freedom of the chi-square distribution
#' @param pv Vector of probabilities for quantiles
#' @param pv2 Vector of probabilities for specific quantiles, Default: pv
#' @param pup Upper limit of probabilities for quantiles, Default: 0.999
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples 
#' nu = 5
#' pv = c(0.005, 0.01, 0.025, 0.05, 1:9/10, 0.95, 0.975, 0.99, 0.995)
#' pv2 = c(0.005, 0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99, 0.995)
#' chi.quant(nu, pv, pv2)
#' @rdname chi.quant
#' @export
chi.quant = function(nu, pv, pv2=pv, pup=0.999, mt, dig=4) {
	if (missing(mt)) mt = bquote("Quantiles of the Chi-square Distribution"~chi[p~","~.(nu)]^2 )
    # Quantiles of the chi-square distribution
	cv = qchisq(pv, nu)
	names(cv) = pv
	print(round(cv, dig))
    # Set axis
	up = qchisq(pup, nu)
	x = seq(0, up, length=100)
	pdf = dchisq(x, nu)
	ymax = max(pdf)
	npv = length(pv2)
	y1 = 0.1*npv*ymax
	wc = ifelse(npv > 5, 6, 5)
	win.graph(7, wc)
    # Plot quantiles
	plot(x, pdf, type ="n", ylim=c(-y1, ymax), xlim=c(-0.1*up, up),
		ylab="f(x)", xlab="x", main=mt)
	abline(h=0, col="green4")
	lines(x, pdf, type="l", lwd=2, col=2)
    # Display major quantiles
	cv2 = qchisq(pv2, nu)
	abline(v=c(0, nu), col=3)
	abline(v=cv2, lty=2, col=4)
	yp = -0.1*ymax*(1:npv)
	arrows(0, yp, cv2, yp, length=0.1, code=2, col=4)
	text(0, yp, paste0("p=", pv2), pos=2, col=4, cex=0.8)
	text(cv2, yp, round(cv2, dig), pos=4, cex=0.8)
}

# [8-8] Compare T-Dist. with the Standard Normal
#' @title Compare T-Dist. with the Standard Normal
#' @description Compare T-Dist. with the Standard Normal
#' @param nu Degree of freedom for the chi-sq. dist. Default: c(10, 30)
#' @param lo Lower limit of x-axis, Default: -3.5
#' @param up Upper limit of x-axis, Default: 3.5
#' @param dig Number of digits below the decimal point, Default: 4
#' @param dcol Color of plot lines
#' @return None.
#' @examples 
#' nu = c(1, 5, 10, 30)
#' tnorm.comp(nu)
#' @rdname tnorm.comp
#' @export 
tnorm.comp = function(nu=c(10, 30), lo=-3.5, up=3.5, dig=4, dcol) {
	# Set x-axis
	x = seq(lo, up, length=100)
	x1 = lo*1.1
	x2 = up*1.1
	if (missing(dcol)) dcol=c(1, 2, 4, "green2", "purple", "pink", "cyan", "orange")
	# Set graphic window
	win.graph(7,6)
	par(mfrow=c(2,1))
	par(mar=c(4,4,4,2))
	# Plot PDF => Fig (a)
	plot(x, dnorm(x), type="l", main="N(0,1) & T-dist.", lwd=2, xlim=c(x1,x2), ylab="f(x)", xlab="(a)")
	abline(v=0, lty=2, col=3)

	for (i in 1:4) lines(x, dt(x, nu[i]), lwd=1, col=dcol[i+1])
	legend("topright", c("N(0,1)", paste0("t(", nu, ")")), lwd=2, cex=0.8, col=dcol)
	# PDF in log-scale => Fig (b)
	plot(x, dnorm(x), type="l", log="y", main="N(0,1) & T-dist. (Log scale)", 
		lwd=2, xlim=c(x1,x2), ylab="log[f(x)]", xlab="(b)")
	abline(v=0, lty=2, col=3)
	for (i in 1:4) lines(x, dt(x, nu[i]), lwd=1, col=dcol[i+1])
}

# Old Version [8-8] Plot Central Probability P(-k<X<k)
tnorm.centp = function(kp, nu, lo=0, up=4, y1=0, y2=1, mt, dig=4, dcol) {
	if (missing(mt)) mt = "Central Probability P(-k<X<k)"
	N = length(nu)
	k = seq(lo, up, length=100)
    # Calculate central probability
	prob = pnorm(k)-pnorm(-k)
	nu2 = sort(nu, decreasing = TRUE)
	for (i in 1:N) prob = rbind(prob, pt(k, nu2[i]) - pt(-k, nu2[i]))
	N2 = N+1
     # Plot central probability
	if (missing(dcol)) dcol = rainbow(N2)
	up1 = up + 0.12*(up-lo)
	up2 = (up1*2 + up)/3
	win.graph(7, 6)
	plot(k, prob[1, ], type ="n", main=mt, ylim=c(y1, y2),
		xlim=c(lo, up1), ylab="P(-k<X<k)", xlab="k")
	abline(h=0, col="green2")
	abline(v=up, col="green2")
	for (i in 1:N2) lines(k, prob[i, ], type="l", lwd=2, col=dcol[i])
    # Display specific probability
	nk = length(kp)
	yp = pnorm(kp)-pnorm(-kp)
	for (i in 1:N) yp = rbind(yp, pt(kp, nu[i]) - pt(-kp, nu[i]))
	abline(v=kp, lty=2, col=4)
	for (j in 1:nk) segments(kp[j], yp[,j], up, yp[,j], lty=2, col=4)
	labd = "N(0,1)"
	for (i in 1:N) labd=c(labd, paste0("t(",nu[i],")"))
	for (j in 1:nk) text(up, yp[,j], labels=format(yp[,j], digits=dig), pos=4, cex=0.8)
	for (j in 1:nk) text(kp[j], yp[,j], labels=labd, pos=2, cex=0.8)
}

# [8-9] Simulation of the F-distribution
#' @title Simulation of the F-distribution
#' @description Simulation of the F-distribution
#' @param nu1 Numerator degree of freedom, Default: 5
#' @param nu2 Denominator degree of freedom, Default: 5
#' @param N Number of random values, Default: 10000
#' @param ng Number of classes in histogram, Default: 250
#' @param seed Seed value for random number generator, Default: 9857
#' @param xp Vector of x-axis values (default=1:9), Default: 1:9
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples 
#' fdist.sim(nu1=8, nu2=5)
#' @rdname fdist.sim
#' @export 
fdist.sim=function(nu1=5, nu2=5, N=10000, ng=250, seed=9857, xp=1:9, dig=4) {
    # Generate random numbers
	set.seed(seed)
	dat1 = rchisq(N, nu1)
	dat2 = rchisq(N, nu2)
	fs1 = (dat1/nu1)/(dat2/nu2)
    # Define the F-distribution function
	fd1 = function(x) df(x, nu1, nu2)
	xmax = max(xp, qf(0.99, nu1, nu2))
    # Expected value and standard deviation
	Ex1 = round(mean(fs1), dig)
	Dx1 = round(sd(fs1), dig)
	Ex2 = ifelse(nu2>2, round(nu2/(nu2-2), dig), Inf)
	Dx2 = ifelse(nu2>4, round(sqrt(2*nu2^2*(nu1+nu2-2)/nu1/(nu2-2)^2/(nu2-4)), dig), 
		ifelse(nu2>2, Inf, NA) )
    # Plot histogram
	win.graph(7, 5)
	hist(fs1, breaks=ng, prob=T, 
		xlim=c(0,xmax), col=7, 
		main=bquote("("~chi[.(nu1)]^2~"/"~.(nu1)~") / ("~chi[.(nu2)]^2~"/"~.(nu2)~
			")  ~  F("~.(nu1)~","~.(nu2)~")" ), ylab="f(x)", xlab="x")
	curve(fd1, 0, xmax, lwd=2, col=2, add=T)
	legend("topright", c("Para.  Exact   Simul.", 
			paste("E(X) ", Ex2, Ex1), paste("D(X) ", Dx2, Dx1)),
		text.col=c(1,4,4) )
    # Compare the cumulative probabilities F(1), F(2), ...
	Theory = pf(xp, nu1, nu2)
	Simula = sapply(xp, function(x) sum(fs1<x))/N
	cdf = rbind(Theory, Simula)
	colnames(cdf)=paste0("F(", xp, ")")
	print(round(cdf, dig))
}

# [8-10] Cumulative Probability of the F-distribution
#' @title Cumulative Probability of the F-distribution
#' @description Cumulative Probability of the F-distribution
#' @param nu1 Numerator degree of freedom, Default: 5
#' @param nu2 Denominator degree of freedom, Default: 5
#' @param xp Vector of x-axis values
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples 
#' k= 1:7
#' nu1 = 8; nu2= 5
#' f.prob(nu1, nu2, k)
#' @rdname f.prob
#' @export
f.prob = function(nu1=5, nu2=5, xp, mt, dig=4) {
    # Set the title and axis
	if (missing(mt)) mt = bquote("Cumulative Probabilities" ~ F(x) == P(F[.(nu1)~ ","~ .(nu2)] < x))
	up = qf(0.99, nu1, nu2)
	x = seq(0, up, length=100)
	ymax = max(df(x, nu1, nu2))
	nxp = length(xp)
    # Print the cumulative probability
	prx = pf(xp, nu1, nu2)
	names(prx) = xp
	print(round(prx, dig))
    # Pot the PDF of the F-distribution
	y1 = 0.1*nxp*ymax
	wc = ifelse(nxp > 5, 6, 5)
	win.graph(7, wc)
	plot(x, df(x, nu1, nu2), type ="n", main=mt, 
		ylim=c(-y1, ymax), xlim=c(0, up), ylab="f(x)", xlab="x")
	abline(h=0, col="green4")
	lines(x, df(x, nu1, nu2), type="l", lwd=2, col=2)
    # Display specific cumulative probabilities
	abline(v=0, col="green4")
	abline(v=xp, lty=2, col=4)
	yp = -0.1*ymax*(1:nxp)
	arrows(0, yp, xp, yp, length=0.1, code=2, col=4)
	text(xp, yp, labels=paste0("F(", xp, ")=", round(prx, dig)), pos=4, cex=0.8)
}

# [8-11] Quantile Plot of the F-distribution
#' @title Quantile Plot of the F-distribution
#' @description Quantile Plot of the F-distribution
#' @param nu1 Numerator degree of freedom (default=5)
#' @param nu2 Denominator degree of freedom (default=5)
#' @param pv Vector of probabilities for quantiles
#' @param pv2 Vector of probabilities for specific quantiles, Default: pv
#' @param pup Upper limit of probabilities for quantiles, Default: 0.995
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples 
#' nu1 = 8; nu2 = 5
#' pv = c(0.005, 0.01, 0.025, 0.05, 1:9/10, 0.95, 0.975, 0.99, 0.995)
#' pv2 = c(0.05, 0.5, 0.95, 0.975, 0.99, 0.995)
#' f.quant(nu1, nu2, pv, pv2)
#' @rdname f.quant
#' @export
f.quant = function(nu1=5, nu2=5, pv, pv2=pv, pup=0.995, mt, dig=4) {
	if (missing(mt)) mt = bquote("Quantiles "~F[p~";("~.(nu1)~","~ .(nu2)~")"] )
    # Quantiles of the F-distribution
	cv = qf(pv, nu1, nu2)
	names(cv) = pv
	print(round(cv, dig))
    # Set axis
	up = qf(pup, nu1, nu2)*1.1
	x = seq(0, up, length=100)
	pdf = df(x, nu1, nu2)
	ymax = max(pdf)
	npv = length(pv2)
	y1 = 0.1*npv*ymax
	wc = ifelse(npv > 5, 6, 5)
	win.graph(7, wc)
    # Plot quantiles
	plot(x, pdf, type ="n", ylim=c(-y1, ymax), xlim=c(-0.1*up, up),
		ylab="f(x)", xlab="x", main=mt)
	abline(h=0, col="green4")
	lines(x, pdf, type="l", lwd=2, col=2)
    # Display major quantiles
	cv2 = qf(pv2, nu1, nu2)
	abline(v=0, col=3)
	abline(v=cv2, lty=2, col=4)
	yp = -0.1*ymax*(1:npv)
	arrows(0, yp, cv2, yp, length=0.1, code=2, col=4)
	text(0, yp, paste0("p=", pv2), pos=2, col=4, cex=0.8)
	text(cv2, yp, round(cv2, dig), pos=4, cex=0.8)
}