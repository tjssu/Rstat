# [Ch-4 Functions] ----------------------------------------------------------------------------------
# source("E:/R-stat/Digeng/ch4-function.txt")

# [Ch-4 Function Manual] -----------------------------------------
#' @title Manual for Ch4. Functions
#' @description Ch4. Random Variables and Probability Distributions
#' @param fn Function Number (0-14). Default: 0
#' @return None.
#' @examples 
#' ch4.man()
#' ch4.man(7:8)
#' @rdname ch4.man
#' @export
ch4.man = function(fn=0) {
    if (0 %in% fn) {
	cat("[1] rolldie.sum\tPDF of the Sum of n Dice \n")
	cat("[2] hyp.sample\tPDF of the Number of Successes from Finite Population\n") 
	cat("[3] disc.cdf\tCDF of a Discrete Random Variable\n")
	cat("[4] cont.cdf\tCDF of a Continuous Random Variable\n")
	cat("[5] disc.joint2\tJoint PDF of Two Discrete Random Variables\n")
	cat("[6] cont.jcdf\tJoint CDF of Two Continuous Random Variables\n")
	cat("[7] cont.jcdfp\tJoint CDF Plot of Two Continuous Random Variables\n")
	cat("[8] disc.marg2\tMarginal PDF of Two Discrete Random Variables\n")
	cat("[9] cont.marg2\tMarginal PDF Plot of Two Continuous Random Variables\n")
	cat("[10] disc.cond2\tConditional PDF of Discrete Random Variables\n")
	cat("[11] cont.cond2\tConditional PDF Plot of Two Continuous Random Variables\n")
	cat("[12] disc.ind2\tIndependence of Two Discrete Random Variables\n")
	cat("[13] cont.ind2\tIndependence of Two Continuous Random Variables\n")
	cat("[14] cont.trans\tTransforming PDF of a Continuous Random Variable\n")
    }
    if (1 %in% fn) {
	cat("[1] PDF of the Sum of n Dice \n")
	cat("rolldie.sum(n, cex=1)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("n\t Number of dice\n")
	cat("[Optional Input]--------------------------\n")
	cat("cex\t Text size (default=1)\n")
    }
    if (2 %in% fn) {
	cat("[2] PDF of the Number of Successes from Finite Population\n")
	cat("hyp.sample(npop, ndef, nsamp, cex=0.8, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("npop\t Population size\n")
	cat("ndef\t Number of success items in the population\n")
	cat("nsamp\t Sample size\n")
	cat("[Optional Input]--------------------------\n")
	cat("cex\t Text size (default=0.8)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (3 %in% fn) {
	cat("[3] CDF of a Discrete Random Variable\n")
	cat("disc.cdf(xv, xp, mt, cpt=1.2, cex=1, dig=3)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("xv\t Vector of values of the discrete random variable\n")
	cat("xp\t Vector of the discrete probability(or frequency)\n")
	cat("[Optional Input]--------------------------\n")
	cat("mt\t Graph Title of the CDF \n")
	cat("cpt\t Text size of the limit points (default=1.2)\n")
	cat("cex\t Text size of the probability (default=1)\n")
	cat("dig\t Number of digits below the decimal point (default=3)\n")
    }
    if (4 %in% fn) {
	cat("[4] CDF of a Continuous Random Variable\n")
	cat("cont.cdf(FUN, low, up, xs, mt, dig=4, pos=\"bottomright\")\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("FUN\t Probability density function\n")
	cat("low\t Lower limit of x-axis\n")
	cat("up \t Upper limit of x-axis\n")
	cat("[Optional Input]--------------------------\n")
	cat("xs\t Specific values of X for displaying the probability\n")
	cat("mt\t Graph Title of the CDF \n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("pos\t Legend location (default=\"bottomright\")\n")
    }
    if (5 %in% fn) {
	cat("[5] Joint PDF of Two Discrete Random Variables\n")
	cat("require(scatterplot3d)\n")
	cat("disc.joint2(X, Y, prt=TRUE, plot=FALSE, dig=4, dig2=3, ep=0)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("X\t Sample space vector of the first random variable\n")
	cat("Y\t Sample space vector of the second random variable\n")
	cat("[Optional Input]--------------------------\n")
	cat("prt\t Logical value for printing the joint frequency and probability (default=TRUE)\n")
	cat("plot\t Logical value for plotting the joint PDF (default=FALSE)\n")
	cat("dig\t Number of digits below the decimal point in the console (default=4)\n")
	cat("dig2\t Number of digits below the decimal point in the graph (default=3)\n")
	cat("ep \t Minimum value for displaying the joint probability (default=0)\n")
    }
    if (6 %in% fn) {
	cat("[6] Joint CDF of Two Continuous Random Variables\n")
	cat("cont.jcdf(FUN, xs, ys, lo1=-Inf, lo2=-Inf)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("FUN\t Continuous joint PDF function\n")
	cat("xs\t Specific values of X for displaying the cumulative probability\n")
	cat("ys\t Specific values of Y for displaying the cumulative probability\n")
	cat("[Optional Input]--------------------------\n")
	cat("lo1\t Lower limit of X (default=-Inf)\n")
	cat("lo2\t Lower limit of Y (default=-Inf)\n")
    }
    if (7 %in% fn) {
	cat("[7] Joint CDF Plot of Two Continuous Random Variables\n")
	cat("require(scatterplot3d)\n")
	cat("cont.jcdfp(FUN, lo1, up1, lo2, up2, mt)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("FUN\t Continuous joint PDF function\n")
	cat("lo1\t Lower limit of x-axis\n")
	cat("up1\t Upper limit of x-axis\n")
	cat("lo2\t Lower limit of y-axis\n")
	cat("up2\t Upper limit of y-axis\n")
	cat("[Optional Input]--------------------------\n")
	cat("mt\t Title of the joint PDF plot\n")
    }
    if (8 %in% fn) {
	cat("[8] Marginal PDF of Two Discrete Random Variables\n")
	cat("disc.marg2(tabXY, Xn, Yn, prt=TRUE, plot=FALSE, dig=5, dig2=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("tabXY\t Joint frequency table of two random variables\n")
	cat("[Optional Input]--------------------------\n")
	cat("Xn\t Name of the first random variable (default=\"X\")\n")
	cat("Yn\t Name of the second random variable (default=\"Y\")\n")
	cat("prt\t Logical value for printing the marginal frequency and probability (default=TRUE)\n")
	cat("plot\t Logical value for plotting the marginal PDF (default=FALSE)\n")
	cat("dig\t Number of digits below the decimal point in the console (default=5)\n")
	cat("dig2\t Number of digits below the decimal point in the graph (default=4)\n")
    }
    if (9 %in% fn) {
	cat("[9] Marginal PDF Plot of Two Continuous Random Variables\n")
	cat("cont.marg2(FUN, lo1, up1, lo2, up2, xs, ys)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("FUN\t Continuous joint PDF function\n")
	cat("lo1\t Lower limit of x-axis\n")
	cat("up1\t Upper limit of x-axis\n")
	cat("lo2\t Lower limit of y-axis\n")
	cat("up2\t Upper limit of y-axis\n")
	cat("[Optional Input]--------------------------\n")
	cat("xs\t Specific value of X for displaying the probability density\n")
	cat("ys\t Specific value of Y for displaying the probability density\n")
    }
    if (10 %in% fn) {
	cat("[10] Conditional PDF of Discrete Random Variables\n")
	cat("disc.cond2(tabXY, Xs, Ys, prt=TRUE, plot=FALSE, dig=5, dig2=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("tabXY\t Joint frequency table of two random variables\n")
	cat("[Mandatory Input (either one of the follows)]\n")
	cat("Xs\t Conditioning value of X\n")
	cat("Ys\t Conditioning value of Y\n")
	cat("[Optional Input]--------------------------\n")
	cat("prt\t Logical value for printing the marginal frequency and probability (default=TRUE)\n")
	cat("plot\t Logical value for plotting the marginal PDF (default=FALSE)\n")
	cat("dig\t Number of digits below the decimal point in the console (default=5)\n")
	cat("dig2\t Number of digits below the decimal point in the graph (default=4)\n")
    }
    if (11 %in% fn) {	
	cat("[11] Conditional PDF Plot of Two Continuous Random Variables\n")
	cat("cont.cond2(FUN, xc, yc, xs, ys, lo, up)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("FUN\t Continuous joint PDF function\n")
	cat("lo\t Lower limit of the conditioned random variable\n")
	cat("up\t Upper limit of the conditioned random variable\n")
	cat("[Mandatory Input (either one of the follows)]\n")
	cat("xc\t Conditioning value of X\n")
	cat("yc\t Conditioning value of Y\n")
	cat("[Optional Input]--------------------------\n")
	cat("xs\t Specific value of X for displaying the density (given yc)\n")
	cat("ys\t Specific value of Y for displaying the density (given xc)\n")
    }
    if (12 %in% fn) {
	cat("[12] Determine Independence of Two Discrete Random Variables\n")
	cat("disc.ind2(X, Y, prt=TRUE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("X\t Sample space vector of X\n")
	cat("Y\t Sample space vector of Y\n")
	cat("[Optional Input]--------------------------\n")
	cat("prt\t Logical value for printing detailed output (default=TRUE)\n")
    }
    if (13 %in% fn) {
	cat("[13] Determine Independence of Two Continuous Random Variables\n")
	cat("cont.ind2(FUN, lo1, up1, lo2, up2, n=11, ep = 1E-6, prt=FALSE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("FUN\t Continuous joint PDF\n")
	cat("lo1\t Lower limit of X\n")
	cat("up1\t Upper limit of X\n")
	cat("lo2\t Lower limit of Y\n")
	cat("up2\t Upper limit of Y\n")
	cat("[Optional Input]--------------------------\n")
	cat("n \t Number of checking points between lower and upper limit (default=11)\n")
	cat("ep\t Error bound for comparing probablities (default=1E-6)\n")
	cat("prt\t Logical value for printing detailed output (default=FALSE)\n")
    }
    if (14 %in% fn) {
	cat("[14] Transformed PDF of a Continuous Random Variable\n")
	cat("cont.trans(fx, TF, FTF, a, b, lo=0, up=1, plot=FALSE, ...)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("fx\t PDF of the original random variable\n")
	cat("TF\t List of transform functions (1~8)\n")
	cat("FTF\t List of transformed PDF (1~8)\n")
	cat("a\t Lower limit of the original random variable for calculating P(a<X<b)\n")
	cat("b\t Upper limit of the original random variable for calculating P(a<X<b)\n")
	cat("[Optional Input]--------------------------\n")
	cat("lo\t Lower limit of the original random variable (default=0)\n")
	cat("up\t Upper limit of the original random variable(default=1)\n")
	cat("plot\t Logical value for plotting the PDF (default=FALSE)\n")
	cat("... \t Graphic parameters\n")
    }
}

# [4-1] Probability Distribution of the Sum of n Dice
#' @title PDF of the Sum of n Dice
#' @description Probability Distribution of the Sum of n Dice
#' @param n Number of dice.
#' @param cex  Default: 1
#' @return None.
#' @examples 
#' rolldie.sum(4)
#' @rdname rolldie.sum
#' @export 
rolldie.sum = function(n, cex=1) {
     # Create sample space
	S = rolldie2(n)
	N = nrow(S)
     # Create random variables X
	X = apply(S, 1, sum)
	X.freq = table(X)
	print(addmargins(X.freq))
	X.prob = X.freq / length(X)
	print(round(addmargins(X.prob), 4))
     # Mean and variance of random variables X 
	X.val = as.numeric(names(X.freq))
	EX = sum(X.val * X.prob)
	EX2 = sum(X.val^2 * X.prob)
	VX = EX2 - EX^2
	DX = sqrt(VX)
	Xmin = min(X.val)
	Xmax = max(X.val)
     # Distribution graph of random variables X
	win.graph(7, 5)
	plot(X.prob, type="h", col="red", 
		main=paste0("Probability Distribution of the Sum of ", n, " Dice"),
		lwd=4, ylim=c(0, max(X.prob)+0.01))
	fitnorm = function(x) dnorm(x, EX, DX)
	curve(fitnorm, Xmin, Xmax, add=T, col=4)
     # Display probability(frequency)
	text(Xmin:Xmax, X.prob, labels=X.freq, pos=3, col=4, cex=cex)
	legend("topright", c(paste("S-S.Size =",N), paste("E(X) =",EX), 
		paste("D(X) =", round(DX,4))), bg="white")
}

# [4-2] Probability Distribution of the Number of Successes from Finite Population
#' @title PDF of Successes from Finite Population
#' @description Probability Distribution of the Number of Successes from Finite Population
#' @param npop Population size
#' @param ndef Number of success items in the population
#' @param nsamp Sample size
#' @param cex Text size, Default: 0.8
#' @param dig Number of digits below the decimal point, Default: 4
#' @return the PDF
#' @examples 
#'  hyp.sample(50, 8, 10)
#' @rdname hyp.sample
#' @export
hyp.sample = function(npop, ndef, nsamp, cex=0.8, dig=4) {
     # Calculate frequency by using choose( ) function
	denom = choose(npop, nsamp)
	freq = choose(ndef, 0:nsamp) * choose(npop-ndef, nsamp-(0:nsamp))
	names(freq) = 0:nsamp
	print(freq)
	cat("sum(freq) =", sum(freq),"\n")
     # Calculate probability
	fx = freq / denom
	print(round(fx, dig))
	cat("sum(f(x)) =", sum(fx),"\n")
     # Mean and variance of random variable X
	X.val = 0:nsamp
	EX = sum(X.val * fx)
	EX2 = sum(X.val^2 * fx)
	VX = EX2 - EX^2
	DX = sqrt(VX)
	Xmin = min(X.val)-1
	Xmax = max(X.val)+1
     # Distribution graph of random variable X
	win.graph(7, 5)
	plot(X.val, fx, type="h", col="red", lwd=4, xlim=c(Xmin, Xmax), ylim=c(0, max(fx)+0.05), 
		main=paste0("Prob. Distn. of Successes in ", nsamp, " Samples out of (", 
			ndef, "/", npop, ")"), xlab="Number of Successes", ylab="f(x)")
     # Display probability(frequency)
	    text(X.val, fx, labels=round(fx, dig), pos=3, cex=cex, col=4)
	    legend("topright", c(paste("E(X) =",EX), 
		paste("D(X) =", round(DX,4))), bg="white")
	invisible(fx)
}

# [4-3] CDF of a Discrete Random Variable
#' @title CDF of a Discrete Random Variable
#' @description Cumulative Distribution Function of a Discrete Random Variable
#' @param xv Vector of values of the discrete random variable
#' @param xp Vector of the discrete PDF
#' @param mt Graph Title of the CDF
#' @param cpt Text size of the limit points, Default: 1.2
#' @param cex Text size of the probability, Default: 1
#' @param dig Number of digits below the decimal point, Default: 3
#' @return None.
#' @examples 
#' disc.cdf(0:3, choose(3, 0:3))
#' @rdname disc.cdf
#' @export
disc.cdf = function(xv, xp,mt, cpt=1.2, cex=1, dig=3) {
    # Assign the name of random variable
	xname = deparse(substitute(xv))
    # Check the sum of probabilities
	if (sum(xp) > 1) xp = xp/sum(xp)
    # Define the CDF F(x)
	xcdf = c(0, cumsum(xp))
	sf = stepfun(xv, xcdf)
     # Plot the CDF F(x)
	if (missing(mt)) mt = paste0("Cumulative distribution function(CDF) of ", xname)
	win.graph(7, 5)
	plot(sf, main=mt, verticals=F, pch=19, lwd=2, cex=1.2,
		col=2, xlab="x", ylab="F(x)")
	grid(col=3)
	points(xv, xcdf[-length(xcdf)], col=2, cex=cpt)
     # Display the probability
	text(xv, xcdf[-1], labels=round(xcdf[-1], dig), cex=cex, col=4, pos=2)
     # Mean and variance of random variable X
	EX = sum(xv * xp)
	EX2 = sum(xv^2 * xp)
	VX = EX2 - EX^2
	DX = sqrt(VX)
     # Display legend
	legend("bottomright", c(paste("E(X) =",round(EX,4)), 
		paste("D(X) =", round(DX,4))), bg="white")
}

# [4-4] CDF of a Continuous Random Variable
#' @title CDF of a Continuous Random Variable
#' @description Cumulative Distribution Function of a Continuous Random Variable
#' @param FUN Probability density function
#' @param low Lower limit of x-axis
#' @param up Upper limit of x-axis
#' @param xs Specific values of X for displaying the probability
#' @param mt Graph Title of the CDF
#' @param dig Number of digits below the decimal point, Default: 4
#' @param pos Legend location, Default: 'bottomright'
#' @return None.
#' @examples 
#' pdf = function(x) 2*exp(-2*x)*(x>0)
#' cont.cdf(pdf, low=-1, up=3, xs=c((1:5)*0.2, 2))
#' @rdname cont.cdf
#' @export
cont.cdf = function(FUN, low, up, xs, mt, dig=4, pos="bottomright") {
	if (missing(mt)) mt = "Continuous Cumulative Distribution Function(CDF)"
    # Define the CDF F(x)
	Fx = function(x) integrate(FUN, low, x)$value
    # Vectorize the CDF 
	VFx = Vectorize(Fx, "x")
    # Set the range of X
	xrange = seq(low, up, length=100)
    # Plot the CDF F(x)
	win.graph(7, 5)
	plot(xrange, VFx(xrange), type="l", lwd=3, main=mt, 
		col=2, xlab="x", ylab="F(x)")
    # Display the CDF F(xs)
	grid(col=3)
	abline(h=0)
	if (!missing(xs)) {
		n = length(xs)
		lp = low + (up-low)*0.2
		segments(lp, VFx(xs), xs, VFx(xs), lty=2, col=4) 
		segments(xs, 0, xs, VFx(xs), lty=2, col=4) 
		text(rep(low, n), VFx(xs), labels=paste0("F(", xs, ")=", 
			round(VFx(xs),dig)), cex=0.8, col=1, pos=4)
	}

    # Mean and variance of random variable X
	xfx = function(x) x*FUN(x)
	x2fx = function(x) x^2*FUN(x)
	Ex = integrate(xfx, low, Inf)$value
	Ex2 = integrate(x2fx, low, Inf)$value
	Vx = Ex2 - Ex^2
	Dx = sqrt(Vx)
    # Display legend
	legend(pos, c(paste("E(X) =",round(Ex, dig)), 
		paste("D(X) =", round(Dx, dig))), bg="white")
}

# [4-5] Joint Probability Distribution of Two Discrete Random variable
#' @title Joint PDF of Two Discrete Random variable
#' @description Joint Probability Distribution of Two Discrete Random variable
#' @param X Sample space vector of the first random variable
#' @param Y Sample space vector of the second random variable 
#' @param prt Print the joint frequency and probability? Default: TRUE
#' @param plot Plot the joint PDF? Default: FALSE
#' @param dig Number of digits below the decimal point in the console, Default: 4
#' @param dig2 Number of digits below the decimal point in the graph, Default: 3
#' @param ep Minimum value for displaying the joint probability, Default: 0
#' @return Joint PDF
#' @examples 
#' S = rolldie2(4)
#' X = apply(S, 1, max)
#' Y = apply(S, 1, min)
#' disc.joint2(X, Y)
#' library(scatterplot3d)
#' disc.joint2(X, Y, prt=FALSE, plot=TRUE)
#' @rdname disc.joint2
#' @export
disc.joint2 = function(X, Y, prt=TRUE, plot=FALSE, dig=4, dig2=3, ep=0) {
	Xn = deparse(substitute(X))
	Yn = deparse(substitute(Y))
	N = length(X)
    # Joint and marginal frequency table
	tabXY = table(X, Y)
	mtabXY = addmargins(tabXY)
    # Joint and marginal probability
	ptabXY = tabXY/N
	mptabXY = addmargins(ptabXY)
	if (prt==TRUE) {
		cat(paste0(Xn, " & ", Yn, " joint/marginal frequency distribution"), "\n")
		print(mtabXY)
		cat(paste0(Xn, " & ", Yn, " joint/marginal probability distribution"), "\n")
		print(round(mptabXY, dig))
	}
    # Plot Joint PDF
	if (plot==TRUE) {
	  xa = as.numeric(names(table(X)))
	  nx = length(xa)
	  ya = as.numeric(names(table(Y)))
	  ny = length(ya)
	  xa = rep(xa, ny)
	  ya = rep(ya, each=nx)
	  fxy = as.vector(as.matrix(ptabXY))
    	# Set colors
 	  dc = rank(fxy)
	  dp = sort(unique(dc))
	  nc = length(dp)
	  for (k in 1:nc) dc[dc==dp[k]] = nc+1-k
	  dcol = heat.colors(nc) 
    	# Open graphic window
	  win.graph(7, 5)
	 s3d = scatterplot3d(xa, ya, fxy, type="h", 
		main=paste0("Joint Probability Distribution of ", Xn, " & ", Yn), 
		xlab=Xn, ylab=Yn, zlab="f(x, y)", pch=16, lwd=5, color=dcol[dc])
	  s3d.coords = s3d$xyz.convert(xa, ya, fxy)
	  text(s3d.coords$x[fxy>ep], s3d.coords$y[fxy>ep], labels = round(fxy[fxy>ep], dig2),
		pos = 3, offset = 0.3, col=4, cex=0.8)
	}
    # X, Y joint PDF Return
	invisible(list(freq=tabXY, prob=ptabXY))
}

# [4-6] Joint CDF of Two Continuous Random variable
# Define function Pr(x1<X<x2, y1<Y<y2) by double integration
Pr = function(FUN, x1, x2, y1, y2) {
	integrate(function(y) { 
	    sapply(y, function(y) { 
	        integrate(function(x) { 
	            sapply(x, function(x) FUN(x, y)) }, x1, x2)$value 
	    }) 
	  }, y1, y2) }

#' @title Joint CDF of Two Continuous Random Variables
#' @description Joint Cumulative Distribution Function of Two Continuous Random Variables
#' @param FUN Continuous joint PDF function
#' @param xs Specific values of X for displaying the cumulative probability
#' @param ys Specific values of Y for displaying the cumulative probability
#' @param lo1 Lower limit of X, Default: -Inf
#' @param lo2 Lower limit of Y, Default: -Inf
#' @return Cumulative probability
#' @examples 
#' pdf = function(x, y) (x+y)*(x>0 & x<1)*(y>0 & y<1)
#' cont.jcdf(pdf, Inf, Inf)
#' cont.jcdf(pdf, 0.5, 0.5)
#' @rdname cont.jcdf
#' @export
cont.jcdf = function(FUN, xs, ys, lo1=-Inf, lo2=-Inf) {
    # Define CDF
	Fxy = function(x, y) Pr(FUN, lo1, x, lo2, y)[[1]]
    # Vectorize CDF
	VFxy = Vectorize(Fxy, c("x", "y"))
	prob=VFxy(xs, ys)
	names(prob) = paste0("F(",xs,",",ys,")")
    # Return cumulative probability F(xs, ys)
	return(prob)
}

# [4-7] Joint CDF Plot of Two Continuous Random variable
#' @title Joint CDF Plot of Two Continuous Random Variables
#' @description Joint CDF Plot of Two Continuous Random Variables
#' @param FUN Continuous joint PDF function
#' @param lo1 Lower limit of x-axis
#' @param up1 Upper limit of x-axis
#' @param lo2 Lower limit of y-axis
#' @param up2 Upper limit of y-axis
#' @param mt Title of the joint PDF plot
#' @return None.
#' @examples 
#' pdf = function(x, y) (x+y)*(x>0 & x<1)*(y>0 & y<1)
#' library(scatterplot3d)
#' cont.jcdfp(pdf, 0, 1, 0, 1)
#' @rdname cont.jcdfp
#' @export
cont.jcdfp = function(FUN, lo1, up1, lo2, up2, mt) {
	if (missing(mt)) mt = "Continuous Cumulative Probability Distribution Function(CDF)"
    # Define CDF
	Fxy = function(x, y) Pr(FUN, lo1, x, lo2, y)[[1]]
    # Vectorize CDF
	VFxy = Vectorize(Fxy, c("x", "y"))
    # Plot CDF F(x, y)
	  # Set plot area of X and Y
	  xa = seq(lo1, up1, length=20)
	  ya = seq(lo2, up2, length=20)
	  xa = rep(xa, 20)
	  ya = rep(ya, each=20)
	  # Display graph
	  win.graph(7, 5)
	  s3d = scatterplot3d(xa, ya, VFxy(xa, ya), highlight.3d=TRUE,
		main="Joint Cumulative Distribution of X and Y", 
		xlab="x", ylab="y", zlab="F(x, y)", pch=20)
}

# [4-8] Marginal PDF of Two Discrete Random variables
#' @title Marginal PDF of Two Discrete Random variables
#' @description Marginal PDF of Two Discrete Random variables
#' @param tabXY Joint frequency table of two random variables
#' @param Xn Name of the first random variable (default="X")
#' @param Yn Name of the second random variable (default="Y")
#' @param prt Print the marginal frequency and probability? Default: TRUE
#' @param plot Plot the marginal PDF? Default: FALSE
#' @param dig Number of digits below the decimal point in the console, Default: 5
#' @param dig2 Number of digits below the decimal point in the graph, Default: 4
#' @return Marginal Probabilities
#' @examples 
#' fxy = with(mtcars, table(cyl, carb))
#' disc.marg2(fxy)
#' disc.marg2(fxy, "Cylinder", "Carbrator", prt=FALSE, plot=TRUE)
#' @rdname disc.marg2
#' @export
disc.marg2 = function(tabXY, Xn, Yn, prt=TRUE, plot=FALSE, dig=5, dig2=4) {
	if (missing(Xn)) Xn = "X"
	if (missing(Yn)) Yn = "Y"
    # Marginal frequency table of X and Y
	N = sum(tabXY)
	tabX = apply(tabXY, 1, sum)
	tabY = apply(tabXY, 2, sum)
    # Marginal probability of X and Y
	ptabX = tabX/N
	ptabY = tabY/N
    # Print marginal probability of X and Y
	if (prt==TRUE) {
		cat("Marginal Frequency Distribution of", Xn, "\n")
		print(tabX)
		cat("Marginal probability distribution of", Xn, "\n")
		print(round(ptabX, dig))
		cat("Marginal Frequency Distribution of", Yn, "\n")
		print(tabY)
		cat("Marginal probability distribution of", Yn, "\n")
		print(round(ptabY, dig))
	}
    # Plot marginal probability of X and Y
	if (plot==TRUE) {
	  xa = as.numeric(names(tabX))
	  nx = length(xa)
	  ya = as.numeric(names(tabY))
	  ny = length(ya)
	  # Display graph
	  win.graph(7, 6)
	  par(mfrow=c(2, 1))
	  par(mar=c(3,4,4,2))
	  plot(xa, ptabX, type="h", main=paste("Marginal Probability Distribution of", Xn), 
		ylim=c(0, max(ptabX)*1.1), xlab="", ylab="f(x)", pch=16, lwd=5, col=2)
	  text(xa, ptabX, paste0(tabX, "/", N), pos=3, col=4, cex=0.8)
	  plot(ya, ptabY, type="h", main=paste("Marginal Probability Distribution of", Yn), 
		ylim=c(0, max(ptabY)*1.1), xlab="", ylab="f(y)", pch=16, lwd=5, col=2)
	  text(ya, ptabY, paste0(tabY, "/", N), pos=3, col=4, cex=0.8)
	}
	invisible(list(fx=ptabX, fy=ptabY))
}

# [4-9] Marginal PDF Plot of Two Continuous Random Variables
#' @title Marginal PDF Plot of Two Continuous Random Variables
#' @description Marginal Probability Distribution Plot of Two Continuous Random Variables
#' @param FUN Continuous joint PDF function
#' @param lo1 Lower limit of x-axis
#' @param up1 Upper limit of x-axis
#' @param lo2 Lower limit of y-axis
#' @param up2 Upper limit of y-axis
#' @param xs Specific value of X for displaying the probability density
#' @param ys Specific value of Y for displaying the probability density
#' @return Marginal PDF
#' @examples 
#' pdf = function(x, y) 2/7*(2*x+5*y)*(x>=0 & x<=1)*(y>=0 & y<=1)
#' cont.marg2(pdf, -0.2, 1.2, -0.2, 1.2, 0:2/2, 0:2/2)
#' @rdname cont.marg2
#' @export
cont.marg2 = function(FUN, lo1, up1, lo2, up2, xs, ys) {
    # Define marginal PDF f(x)
	fx = function(x) {integrate(function(y) { 
		sapply(y, function(y) FUN(x, y))}, lo2, up2)$value}
    # Define marginal PDF f(y)
	fy = function(y) {integrate(function(x) { 
		sapply(x, function(x) FUN(x, y))}, lo1, up1)$value}
    # Vectorize marginal PDF
	Vfx = Vectorize(fx, "x")
	Vfy = Vectorize(fy, "y")
    # Set plot area of X and Y
	xa = seq(lo1, up1, length=500)
	ya = seq(lo2, up2, length=500)
    # Plot marginal PDF f(x) and f(y)
	win.graph(7, 6)
	par(mfrow=c(2, 1))
	par(mar=c(3,4,4,2))
	plot(xa, Vfx(xa), type="l", main="Marginal Probability Density Function of X", 
		ylim=c(0, max(Vfx(xa))*1.1), xlab="", ylab="f(x)", lwd=3, col=2)
	if (!missing(xs)) {
		fxv = Vfx(xs)
		segments(lo1+0.07*(up1-lo1), fxv, xs, fxv, lty=2, col=4)
		text(rep(lo1, length(xs)), fxv, round(fxv, 4), col=4)
	}
	plot(ya, Vfy(ya), type="l", main="Marginal Probability Density Function of Y", pch=16, lwd=3, 
		ylim=c(0, max(Vfy(ya))*1.1), xlab="", ylab="f(y)", col=2)
	if (!missing(ys)) {
		fyv = Vfy(ys)
		segments(lo2+0.07*(up2-lo2), fyv, ys, fyv, lty=2, col=4)
		text(rep(lo2, length(ys)), fyv, round(fyv, 4), col=4)
	}
    # Return marginal PDF f(x) and f(y)
	invisible(list(fx=Vfx(xa), fy=Vfy(ya)))
}

# [4-10] Conditional PDF of Discrete Random Variables
#' @title Conditional PDF of Discrete Random Variables
#' @description Conditional Probability Distribution of Discrete Random Variables
#' @param tabXY Joint frequency table of two random variables
#' @param Xs Conditioning value of X
#' @param Ys Conditioning value of Y
#' @param prt Print the marginal frequency and probability? Default: TRUE
#' @param plot Plot the marginal PDF? Default: FALSE
#' @param dig Number of digits below the decimal point in the console, Default: 5
#' @param dig2 Number of digits below the decimal point in the graph, Default: 4
#' @return Conditional Frequency and PDF
#' @examples 
#' fxy = with(mtcars, table(cyl, carb))
#' disc.cond2(fxy, Ys=1:4, plot=TRUE)
#' @rdname disc.cond2
#' @export
disc.cond2 = function(tabXY, Xs, Ys, prt=TRUE, plot=FALSE, dig=5, dig2=4) {
    # Marginal frequency table of X and Y
	N = sum(tabXY)
	tabX = apply(tabXY, 1, sum)
	tabY = apply(tabXY, 2, sum)
    # Extract values of X and Y
	xa = as.numeric(names(tabX))
	nx = length(xa)
	ya = as.numeric(names(tabY))
	ny = length(ya)
    # List of conditional probabilities
	nc = ifelse (missing(Xs), length(Ys), length(Xs))
	cpdf = vector("list", nc)
	cfreq = vector("list", nc)
    # Case when the conditioning variable = Y
	if (missing(Xs)) {Cs = Ys
		Cn = "Y"
		Dn = "X"
		da = xa
		yla = "f(x|y)"
		for (k in 1:nc) {Cv = as.character(Cs[k])
			cfreq[[k]] = tabXY[ , Cv]
			cpdf[[k]] = cfreq[[k]] / sum(cfreq[[k]]) }
	}
    # Case when the conditioning variable = X
	if (missing(Ys))  {Cs = Xs
		Cn = "X"
		Dn = "Y"
		da = ya
		yla = "f(y|x)"
		for (k in 1:nc) {Cv = as.character(Cs[k])
			cfreq[[k]] = tabXY[Cv, ]
			cpdf[[k]] = cfreq[[k]] / sum(cfreq[[k]]) }
	}
    # Print conditional PDF
	if (prt==TRUE) {
	    for (k in 1:nc) {
		cat(paste0("Conditional prob. dist. of ", Dn, " (", Cn, " = ", Cs[k], ")"), "\n")
		if (N<2) {print(cpdf[[k]])
		} else {cat(paste(names(cpdf[[k]]), collapse="\t "), "\n")
			cat(paste(paste0(cfreq[[k]],"/",sum(cfreq[[k]])), collapse="\t "), "\n") }
	    }
	}
    # Plot conditional PDF
	if (plot==TRUE) {
	    dr = switch(nc, 1, 2, 2, 2, 2, 2, 3, 3, 3)
	    dc = switch(nc, 1, 1, 2, 2, 3, 3, 3, 3, 3)
	    ww = switch(nc, 7, 7, 8, 8, 9, 9, 9, 9, 9)
	    wh = switch(nc, 3, 6, 6, 6, 6, 6, 9, 9, 9)
	    win.graph(ww, wh)
	    par(mfrow=c(dr,dc))
	    par(mar=c(3,4,4,2))
	  for (k in 1:nc) {
	    plot(da, cpdf[[k]], type="h", 
		main=paste0("Cond. Prob. Dist. of ", Dn, " | ", Cn, "=", Cs[k]), 
		ylim=c(0, max(cpdf[[k]])*1.15), xlab="", ylab=yla, pch=16, lwd=5, col=2)
	    text(da, cpdf[[k]], paste0(cfreq[[k]], "/", sum(cfreq[[k]])), pos=3, col=4, cex=0.8)
	  }
	}
	invisible(list(freq=cfreq, pdf=cpdf))
}

# [4-11] Conditional PDF Plot of Two Continuous Random Variables
#' @title Conditional PDF Plot of Two Continuous Random Variables
#' @description Conditional Probability Distribution Plot of Two Continuous Random Variables
#' @param FUN Continuous joint PDF function
#' @param xc Conditioning value of X
#' @param yc Conditioning value of Y
#' @param xs Specific value of X for displaying the density (given yc)
#' @param ys Specific value of Y for displaying the density (given xc)
#' @param lo Lower limit of the conditioned random variable
#' @param up Upper limit of the conditioned random variable
#' @return Conditional PDF
#' @examples 
#' pdf = function(x, y) (x+y)*(x>=0 & x<=1)*(y>=0 & y<=1)
#' cont.cond2(pdf, yc=0.1, xs=0:2/2, lo=-0.2, up=1.2)
#' @rdname cont.cond2
#' @export
cont.cond2 = function(FUN, xc, yc, xs, ys, lo, up) {
    # Define marginal PDF f(x)
	fx = function(x) {integrate(function(y) { 
		sapply(y, function(y) FUN(x, y))}, lo, up)$value}
    # Define marginal PDF f(y)
	fy = function(y) {integrate(function(x) { 
		sapply(x, function(x) FUN(x, y))}, lo, up)$value}
    # Vectorize marginal PDF
	Vfx = Vectorize(fx, "x")
	Vfy = Vectorize(fy, "y")
	Vfxy = Vectorize(FUN, c("x", "y"))
    # Conditional PDF
	if (missing(xc)) {cpdf = function(d) Vfxy(d, yc)/fy(yc)
			Cs = yc
			Cn = "Y"
			Dn = "X"
			yla = "f(x|y)"
		} else {cpdf = function(d) Vfxy(xc, d)/fx(xc)
			Cs = xc
			Cn = "X"
			Dn = "Y"
			yla = "f(y|x)" }
	  # Set plot area of X and Y
	  da = seq(lo, up, length=500)
	  # Plot conditional PDF f(d | c)
	  win.graph(7, 4)
	  par(mar=c(3,4,4,2))
	  plot(da, cpdf(da), type="l", 
		main=paste0("Conditional PDF of ", Dn, " | ", Cn, "=", Cs),  
		ylim=c(0, max(cpdf(da))*1.1), xlab="", ylab=yla, lwd=3, col=2)
	if (!missing(xs)) {
		fxv = cpdf(xs)
		segments(lo+0.07*(up-lo), fxv, xs, fxv, lty=2, col=4)
		text(rep(lo, length(xs)), fxv, round(fxv, 3), col=4)
	}
	if (!missing(ys)) {
		fyv = cpdf(ys)
		segments(lo+0.07*(up-lo), fyv, ys, fyv, lty=2, col=4)
		text(rep(lo, length(ys)), fyv, round(fyv, 3), col=4)
	}
    # Return the CDF
	invisible(cpdf(da))
}

# [4-12] Determine Independence of Two Discrete Random Variables
#' @title Independence of Two Discrete Random Variables
#' @description Determine Independence of Two Discrete Random Variables
#' @param X Sample space vector of X
#' @param Y Sample space vector of Y
#' @param prt Print detailed output? Default: TRUE
#' @return Joint PDF
#' @examples 
#' S = rolldie2(4)
#' sum3 = function(x) sum(x>=3)
#' X = apply(S, 1, sum3)
#' even = function(x) sum(x %% 2 ==0)
#' Y = apply(S,1, even)
#' disc.ind2(X, Y)
#' @rdname disc.ind2
#' @export
disc.ind2 = function(X, Y, prt=TRUE) {
	Xn = deparse(substitute(X))
	Yn = deparse(substitute(Y))
	N = length(X)
    # Joint and marginal frequency table of X and Y
	tabXY = table(X, Y)
	mtabXY = addmargins(tabXY)
    # Product of marginal probabilities
	frX = table(X)
	frY = table(Y)
	frXY = (frX %o% frY)/N
	mfrXY = addmargins(as.table(frXY))
    # Compare the joint probability and the product of marginal probabilities
	diffXY = mtabXY - mfrXY
	value = c(as.numeric(names(frX)), NA)
	dfXY = cbind(mtabXY, value, diffXY)

	if (prt==TRUE) {
		cat(paste0("Joint PDF: f(x,y)\U00D7",N, " \U21D2 [f(x,y)-f(x)f(y)]\U00D7",N), "\n")
		print(dfXY)
	}
    # Determine independence
	err = abs(max(tabXY - frXY))
	if (err == 0) {cat("f(x,y) = f(x)f(y) \U21D2 Independent\n")
	} else {cat("max|f(x,y)-f(x)f(y)| =", err, "/", N, "\U21D2 Not Independent\n") }
    # Return the joint probability and the product of marginal probabilities
	invisible(list(freq=mtabXY, prod=mfrXY))
}

# [4-13] Determine Independence of Two Continuous Random variable
#' @title Independence of Two Continuous Random Variables
#' @description Determine Independence of Two Continuous Random Variables
#' @param FUN Continuous joint PDF
#' @param lo1 Lower limit of X
#' @param up1 Upper limit of X
#' @param lo2 Lower limit of Y
#' @param up2 Upper limit of Y
#' @param n Number of checking points between lower and upper limit, Default: 11
#' @param ep Error bound for comparing probablities, Default: 1e-06
#' @param prt Print detailed output? Default: FALSE
#' @return Joint PDF
#' @examples 
#' pdf = function(x, y) (x+y)*(x>=0 & x<=1)*(y>=0 & y<=1)
#' cont.ind2(pdf, lo1=0, up1=1, lo2=0, up2=1)
#' @rdname cont.ind2
#' @export
cont.ind2 = function(FUN, lo1, up1, lo2, up2, n=11, ep = 1E-6, prt=FALSE) {
    # Define marginal PDF f(x)
	fx = function(x) {integrate(function(y) { 
		sapply(y, function(y) FUN(x, y))}, -Inf, Inf)$value}
    # Define marginal PDF f(y)
	fy = function(y) {integrate(function(x) { 
		sapply(x, function(x) FUN(x, y))}, -Inf, Inf)$value}
    # Vectorize marginal PDF
	Vfx = Vectorize(fx, "x")
	Vfy = Vectorize(fy, "y")
	Vfxy = FUN
    # Set plot area of X and Y
	xa = seq(lo1, up1, length=n)
	ya = seq(lo2, up2, length=n)
	xa2 = rep(xa, n)
	ya2 = rep(ya, each=n)
    # Compare joint PDF and the product of marginal PDF
	jpdf = matrix(Vfxy(xa2, ya2), n, n)
	ppdf = Vfx(xa) %o% Vfy(ya)
	rownames(jpdf) = rownames(ppdf) = xa
	colnames(jpdf) = colnames(ppdf) = ya
    # Print joint PDF and the difference
	if (prt==TRUE) {
		cat("Joint probability density f(x,y) ---------\n")
		print(jpdf)
		cat("|f(x,y)-f(x)f(y)| ---------------------\n")
		print(abs(jpdf-ppdf))
	}
    # Determine independence
	err = abs(max(jpdf - ppdf))
	errf = format(err, scientific = TRUE, digits=4)
	if (err < ep) {cat("f(x,y)=f(x)f(y) \U21D2 Independent. (Error bound =", errf, ")\n")
	} else {cat("max|f(x,y)-f(x)f(y)| =", errf, "\U21D2 Not Independent.\n") }
    # Return joint PDF and the product of marginal PDF
	invisible(list(jpdf=jpdf, ppdf=ppdf))
}

# [4-14] Transformed PDF of a Continuous Random Variable
#' @title Transformed PDF of a Continuous Random Variable
#' @description Transformed PDF of a Continuous Random Variable
#' @param fx PDF of the original random variable
#' @param TF List of transform functions (1~8)
#' @param FTF List of transformed PDF (1~8)
#' @param a Lower limit of the original random variable for calculating P(a<X<b)
#' @param b Upper limit of the original random variable for calculating P(a<X<b)
#' @param lo Lower limit of the original random variable, Default: 0
#' @param up Upper limit of the original random variable, Default: 1
#' @param plot Plot the PDF? Default: FALSE
#' @param ... Graphic parameters
#' @return None.
#' @examples 
#' fx = function(x) 2*x*(x>=0 & x<=1)
#' ty = function(x) 10*x - 4
#' tw = function(x) -10*x + 4
#' fy = function(y) fx((y+4)/10)/10
#' fw = function(w) fx((-w+4)/10)/10
#' cont.trans(fx, list(y=ty, w=tw), list(fy, fw), 0.3, 0.7, plot=TRUE, cex=1.3)
#' @rdname cont.trans
#' @export
cont.trans = function(fx, TF, FTF, a, b, lo=0, up=1, plot=FALSE, ...) { 
    # Number of transformed variable
	N = length(FTF)
    # Transformed variable names
	vn = names(TF)
	Vn = toupper(vn)
    # Calculate probability
	px = integrate(fx, a, b)[[1]]
	cat(paste0("Pr(", a, " < X < ", b, ") ="), px, "\n")
	py = rep(NA, N)
	for (k in 1:N) {a2 = min(TF[[k]](c(a,b)))
		b2 = max(TF[[k]](c(a,b)))
		py[k] = integrate(FTF[[k]], a2, b2)[[1]] 
		cat(paste0("Pr(", a2, " < ", Vn[k], " < ", b2, ") ="), py[k], "\n") }
     # Plot ---(N <= 8) --------------------------------------
     if (plot==TRUE) {
	divw = switch(N, c(2,1), c(3,1), c(2,2), c(2,3), c(2,3), c(3,3), c(3,3), c(3,3))
	dimw = switch(N, c(7,5), c(7,7), c(8,5), c(8,7), c(8,7), c(9,7), c(9,7), c(9,7))
	win.graph(dimw[1], dimw[2])
	par(mfrow=divw)
    # Display the pdf of X and P(a<X<b)
	x1 = lo - 0.2*(up-lo)
	x2 = up + 0.2*(up-lo)
	k1 = x1*200
	k2 = x2*200
	xm = (a+b)/2
	ym = fx(xm)*0.5
	xa =k1:k2/200
	plot(xa, fx(xa), type="n", las=1, ylab="f(x)", xlab="", main="pdf of X")
	cord.x = c(a, seq(a, b, 0.01), b) 
	cord.y = c(0, fx(seq(a, b, 0.01)), 0) 
	polygon(cord.x, cord.y, col='lightcyan')
	text(xm, ym, labels=paste0("P(",a,"<X<",b,")\n=",round(px, 4)), ...)
	lines(xa, fx(xa), lwd=2, col=2)
    # Display the pdf of Y and P(a2<Y<b2)
	for (k in 1:N) {a2 = min(TF[[k]](c(a,b)))
		b2 = max(TF[[k]](c(a,b)))
		lo2 = min(TF[[k]](c(lo,up)))
		up2 = max(TF[[k]](c(lo,up)))
		# Set plot range
		x1 = lo2 - 0.2*(up2-lo2)
		x2 = up2 + 0.2*(up2-lo2)
		k1 = x1*200
		k2 = x2*200
		xm = (a2+b2)/2
		ym = FTF[[k]](xm)*0.5
		xa =k1:k2/200
		# Plot the pdf and the probabilities
		plot(xa, FTF[[k]](xa), type="n", las=1, ylab=paste0("f(", vn[k],")"), xlab="",
				main=paste0("pdf of ",Vn[k]))
		cord.x = c(a2, seq(a2, b2, 0.01), b2) 
		cord.y = c(0, FTF[[k]](seq(a2, b2, 0.01)), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		text(xm, ym, labels=paste0("P(",a2,"<", Vn[k],"<",b2,")\n=",round(py[k],4)), ...)
		lines(xa, FTF[[k]](xa), lwd=2, col=2)
	}
     }
}