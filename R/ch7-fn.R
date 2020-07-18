# [Ch-7 Functions] ----------------------------------------------------------------------------------
# [Ch-7 Function Manual] -----------------------------------------
# source("E:/R-stat/Digeng/ch7-function.txt")

#' @title Manual for Ch.7 Functions
#' @description Ch.7 Continuous Random Variables
#' @param fn Function number (0~2), Default: 0
#' @return None
#' @examples 
#' ch7.man()
#' ch7.man(1:2)
#' @rdname ch7.man
#' @export
ch7.man = function(fn=0) {
    if (0 %in% fn) {
	cat("[1] cont.exp\t\tExpected Values of Continuous Random Variables\n")
	cat("[2] cont.mpdf\t\tPDF and CDF plots for Continuous Random Variables\n")
    }
    if (1 %in% fn) {
	cat("[1] Expected Values of Continuous Random Variables\n")
	cat("cont.exp(FUN, lo, up, mt, dig=3, xn=\"x\", prt=FALSE, plot=FALSE, pos=\"center\")\n")
	cat("[Mandatory Input]------------------------------\n")
	cat("FUN\t Continuous probability density function\n")
	cat("lo\t Lower limit of x-axis\n")
	cat("up\t Upper limit of x-axis\n")
	cat("[Optional Input]------------------------------\n")
	cat("mt\t Graph title\n")
	cat("dig\t Number of digits below the decimal point (default=3)\n")
	cat("xn\t Random variable name (default=\"x\")\n")
	cat("prt\t Logical value for printing expected values and variances (default=FALSE)\n")
	cat("plot\t Logical value for plotting probability density function (default=FALSE)\n")
	cat("pos\t Legend location (default=\"center\")\n")
    }
    if (2 %in% fn) {
	cat("[2] Probability Density Function and CDF for Continuous Random Variables\n")
	cat("cont.mpdf(dist, lo, up, para, para2, ymax, mt, dcol, np=100, \n")
	cat("\t pos1=\"topright\", pos2=\"bottomright\", xp1, xp2)\n")
	cat("[Mandatory Input]------------------------------\n")
	cat("dist\t Name of continuous probability distribution (one of the follows)\n")
	cat("\t (\"exp\", \"gamma\", \"weibull\", \"beta\", \"norm\", \"t\", \"chisq\", \"f\")\n")
	cat("lo\t Lower limit of x-axis\n")
	cat("up\t Upper limit of x-axis\n")
	cat("para\t First parameter vector of probability density function\n")
	cat("para2\t Second parameter vector of probability density function (if necessary)\n")
	cat("[Optional Input]------------------------------\n")
	cat("ymax\t Upper limit of y-axis\n")
	cat("mt\t Graph title\n")
	cat("dcol\t Graph color vector (default as follows)\n")
	cat("\t c(\"red\", \"blue\", \"orange2\", \"green4\", \"purple\", \"cyan2\")\n")
	cat("np\t Number of plot points (default=100)\n")
	cat("pos1\t Legend location of probability density function (default=\"topright\")\n")
	cat("pos2\t Legend location of cumulative distribution function (default=\"bottomright\")\n")
	cat("xp1\t Vector of specific x values for probability density function (ignore legend)\n")
	cat("xp2\t Vector of specific x values for cumulative distribution function (ignore legend)\n")
    }
}

# [7-1] Expected Values of Continuous Random Variables
#' @title Expected Values of Continuous Random Variables
#' @description Expected Values of Continuous Random Variables
#' @param FUN Continuous probability density function
#' @param lo Lower limit of x-axis
#' @param up Upper limit of x-axis
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 3
#' @param xn Random variable name, Default: 'X'
#' @param prt Print expected values and variances? Default: FALSE
#' @param plot Plot probability density function? Default: FALSE
#' @param pos Legend location, Default: 'center'
#' @return list(ev=Ex, std=Dx)
#' @examples 
#' fx = function(x) dnorm(x,10,2)
#' cont.exp(fx, 0, 20, prt=T, plot=T)
#' @rdname cont.exp
#' @export 
cont.exp = function(FUN, lo, up, mt, dig=3, xn="X", prt=FALSE, plot=FALSE, pos="center") {
	Xn = toupper(xn)
	if (missing(mt)) mt = paste0("PDF and Expected Value of ", Xn)
    # Define expected values
	ex = function(x) x*FUN(x)
	ex2 = function(x) x^2*FUN(x)
	Ex = integrate(ex, lo, up)[[1]]
	Ex2 = integrate(ex2, lo, up)[[1]]
	Vx = Ex2 - Ex^2
	Dx = sqrt(Vx)
    # Print expected values
     if (prt==TRUE) {
		cat(paste0("E(",Xn,") = ",round(Ex, dig)), "\t ")
		cat(paste0("V(",Xn,") = ",round(Vx, dig)), "\t ")
		cat(paste0("D(",Xn,") = ",round(Dx, dig)), "\n")
     }
    # Plot the pdf and the expected values
     if (plot==TRUE) {
    	# Set the range of x-axis
	xa = seq(lo, up, length=200)
    	# Plot the probability distribution function f(x)
	plot(xa, FUN(xa), type="l", main=mt, ylim=c(0, max(FUN(xa))*1.1),
		xlab="", ylab=paste0("f(", xn,")"), lwd=2, col=2)
	abline(v=Ex, lty=2, col=4)
      # Display legend
	legend(pos, 
		c(paste0("E(",Xn,")=",round(Ex,dig)), 
		paste0("D(",Xn,")=", round(Dx,dig))), 
		bg="white")
     }
	invisible(list(ev=Ex, std=Dx))
}

# [7-2] Probability Density Function and CDF for Continuous Random Variables
# Get the pdf and the CDF vectors of single x-vector
getdf = function(dist, xa, para, para2) {
	np = length(xa)
	N = max(length(para), length(para2))
    # Probability distribution function name
	dpdf = paste0("d", dist)
	dcdf = paste0("p", dist)
	pdf = cdf = matrix(NA, nrow=np, ncol=N)
    # Call the pdf by assigning the parameter
	if (dist %in% c("exp", "t", "chisq")) { 
		for (k in 1:N) {
			pdf[, k] = do.call(dpdf, list(xa, para[k]))
			cdf[, k] = do.call(dcdf, list(xa, para[k]))
		}
	} else if (dist == "gamma") { 	
		for (k in 1:N) {
			pdf[, k] = do.call(dpdf, list(xa, para[k], 1/para2[k]))
			cdf[, k] = do.call(dcdf, list(xa, para[k], 1/para2[k]))
		}
	} else { 	for (k in 1:N) {
			pdf[, k] = do.call(dpdf, list(xa, para[k], para2[k]))
			cdf[, k] = do.call(dcdf, list(xa, para[k], para2[k]))
		}
	}
    # Return the pdf and the CDF
	invisible(list(pdf=pdf, cdf=cdf))
}
# Get the pdf and the CDF vectors of multiple x-vector
getdf2 = function(dist, xa, para, para2) {
	np = length(xa)
	N = max(length(para), length(para2))
	if (N != np) stop("Number of x-vectors must be same as the number of parameters!")
    # Probability distribution function name
	dpdf = paste0("d", dist)
	dcdf = paste0("p", dist)
	pdf = cdf = rep(NA, N)
    # Call the pdf by assigning the parameter
	if (dist %in% c("exp", "t", "chisq")) { 
		for (k in 1:N) {
			pdf[k] = do.call(dpdf, list(xa[k], para[k]))
			cdf[k] = do.call(dcdf, list(xa[k], para[k]))
		}
	} else if (dist == "gamma") { 	
		for (k in 1:N) {
			pdf[k] = do.call(dpdf, list(xa[k], para[k], 1/para2[k]))
			cdf[k] = do.call(dcdf, list(xa[k], para[k], 1/para2[k]))
		}
	} else { 	for (k in 1:N) {
			pdf[k] = do.call(dpdf, list(xa[k], para[k], para2[k]))
			cdf[k] = do.call(dcdf, list(xa[k], para[k], para2[k]))
		}
	}
    # Return the pdf and the CDF
	invisible(list(pdf=pdf, cdf=cdf))
}
# PDF and CDF for Continuous Random Variables
#' @title PDF and CDF for Continuous Random Variables
#' @description PDF and CDF for Continuous Random Variables
#' @param dist Name of continuous probability distribution (one of the follows)
#'                   ("exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f")
#' @param lo Lower limit of x-axis
#' @param up Upper limit of x-axis
#' @param para First parameter vector of PDF
#' @param para2 Second parameter vector of PDF (if necessary)
#' @param ymax Upper limit of y-axis
#' @param mt Graph title
#' @param dcol Graph color vector (default as follows)
#'                   c("red", "blue", "orange2", "green4", "purple", "cyan2")
#' @param np Number of plot points, Default: 100
#' @param pos1 Legend location of PDF, Default: 'topright'
#' @param pos2 Legend location of CDF, Default: 'bottomright'
#' @param xp1 Vector of specific x values for PDF (ignore legend)
#' @param xp2 Vector of specific x values for CDF (ignore legend)
#' @return None.
#' @examples 
#' lamb = 1:5
#' cont.mpdf("exp", 0, 3, para=lamb, ymax=5)
#'
#' alp = c(0.5, 1, 2, 3); rate = 1
#' cont.mpdf("gamma", 0, 8, para=alp, para2=rate, ymax=1.2)
#'
#' th = 1; alp = c(0.5, 1, 2, 3)
#' cont.mpdf("weibull", 0, 5, para=alp, para2=th, ymax=1.2)
#' @rdname cont.mpdf
#' @export
cont.mpdf = function(dist, lo, up, para, para2, ymax, mt, dcol, np=100, 
	pos1="topright", pos2="bottomright", xp1, xp2) {

    # Probability distribution names
	dlist=c("exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f")
	dlist2=c("exponential", "gamma", "weibull", "beta", "normal", "tdist", "chisquare", "fdist")
	if (missing(dist)) {
		cat(paste(dlist, collapse=", "), "\n")
		stop("Input one of the distribution name above ....")
	}
	# Check whether the distribution name is in the list
	dist = tolower(dist)
	distnum = which(dlist %in% dist)

	# Check also the extended list
	if (length(distnum)==0) distnum = grep(dist ,dlist2)
	if (length(distnum)==0) {
		cat(paste(dlist, collapse=", "), "\n")
		stop("Input one of the distribution name above ....")
	}

	# Correct the distribution name, if necessary
	if (!(dist %in% dlist)) dist = dlist[distnum]

	#  Graph title
	dname=paste0(c("Exponential", "Gamma", "Weibull", "Beta", "Normal", "T-", "Chi-square", "F-"), 
		" Dist.")
	distname = dname[which(dlist %in% dist)]
	mt1 = paste0("PDF of ", distname)
	mt2 = paste0("CDF of ", distname)

    # Number of parameters and their names
	if (missing(para)) stop("Input the distribution parameter ....")
	N=length(para)
	pn=deparse(substitute(para))
	varypn=12

    # Require para2 except the exponential, t, chisquare distribution
	if (missing(para2)) {para2=para
		if (!(dist %in% c("exp", "t", "chisq"))) { 
			cat("The second parameter is required for the", dist, "distribution.\n",
			     "It was set to the same as the first parameter...\n") }
	} else { pn2 = deparse(substitute(para2))
		N2 = length(para2)
		if (N==1 & N2>1) { varypn=2
			para = rep(para, N2)
			pn=deparse(substitute(para2))
		}
		if (N>1 & N2==1) { varypn=1
			para2=rep(para2, N) }
		if (N>1 & N2>1) varypn=12
		N = max(N,N2)
	}
    # Check the parameter names
	# exp(rate=lambda), gamma(shape=alpha, rate=1/theta)
	# weibull(shape=alpha, scale=theta), beta(shape1=alpha, shape2=beta)
	# norm(mean=mu, sd= sigma), t(df=nu), chisq(df=nu), f(df1=nu1, df2=nu2)
    # Legend Labels
	lab = list()
	if (varypn==1) {
	    for (k in 1:N) lab[[k]] = switch(distnum, bquote(lambda == .(para[k])),
		bquote(alpha == .(para[k])), bquote(alpha == .(para[k])),
		bquote(alpha == .(para[k])), bquote(mu == .(para[k])),
		bquote(nu == .(para[k])), bquote(nu == .(para[k])), bquote(nu[1] == .(para[k])))
	} else if (varypn==2) {
	    for (k in 1:N) lab[[k]] = switch(distnum, bquote(lambda == .(para[k])),
		bquote(theta == .(para2[k])), bquote(theta == .(para2[k])),
		bquote(beta == .(para2[k])), bquote(sigma == .(para2[k])),
		bquote(nu == .(para[k])), bquote(nu == .(para[k])), bquote(nu[2] == .(para2[k])))
	} else if (varypn==12) {
	    for (k in 1:N) lab[[k]] = switch(distnum, bquote(lambda == .(para[k])),
		bquote(alpha == .(para[k]) ~ theta == .(para2[k])), 
		bquote(alpha == .(para[k]) ~ theta == .(para2[k])),
		bquote(alpha == .(para[k]) ~ beta == .(para2[k])), 
		bquote(mu == .(para[k]) ~ sigma == .(para2[k])),
		bquote(nu == .(para[k])), bquote(nu == .(para[k])), 
		bquote(nu[1] == .(para[k]) ~ nu[2] == .(para2[k])))
	} 
	leg = lab[[1]]
	if (N >= 2) for (i in 2:N) leg=c(leg, lab[[i]])
    # Get the vector of PDF and CDF
	xa = seq(lo, up, length=np)
	dum = getdf(dist, xa, para, para2)
	pdf = dum$pdf
	cdf = dum$cdf
    # Set colors
	if (missing(dcol)) {
		if (N<=6) {dcol = c("red", "blue", "orange2", "green4", "purple", "cyan2")
		} else {dcol= rainbow(N)}
	}
    # Divide graphic window
	win.graph(9, 5); par(mfrow=c(1,2))
    # Plot the probability density function
	if (missing(ymax)) ymax = max(pdf)
	plot(xa, pdf[ ,1], type="l", main=mt1, 
		lwd=2, col=dcol[1], ylab="f(x)", xlab="(a)", ylim=c(0, ymax))
	grid(col=3)
	if (N>=2) for (i in 2:N) lines(xa, pdf[ ,i], lwd=2, col=dcol[i])
	if (!missing(xp1)) {
		yp1 = getdf2(dist, xp1, para, para2)$pdf
		text(xp1, yp1, paste0("(",para, ",", para2, ")"))
	} else if (N>=2) {
		legend(pos1, sapply(leg, as.expression), lwd=2, col=dcol[1:N])
	} else {legend(pos1, as.expression(leg), lwd=2, col=dcol[1]) }
    # Plot the cumulative distribution function
	plot(xa, cdf[ ,1], type="l", main=mt2,
		lwd=2, col=dcol[1], ylim=c(0,1), ylab="F(x)", xlab="(b)")
	grid(col=3)
	if (N>=2) for (i in 2:N) lines(xa, cdf[ ,i], lwd=2, col=dcol[i])
	if (!missing(xp2)) {
		yp2 = getdf2(dist, xp2, para, para2)$cdf
		text(xp2, yp2, paste0("(",para, ",", para2, ")"))
	} else if (N>=2) {
		legend(pos2, sapply(leg, as.expression), lwd=2, col=dcol[1:N])
	} else {legend(pos2, as.expression(leg), lwd=2, col=dcol[1]) }
}