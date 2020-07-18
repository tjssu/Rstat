# [Ch-5 Functions] ----------------------------------------------------------------------------------
# [Ch-5 Function Manual] -----------------------------------------
# source("E:/R-stat/Digeng/ch5-function.txt")

#' @title Manual for Ch5
#' @description Ch5. Expected Values of Random Variables
#' @param fn Function number, Default: 0
#' @return None.
#' @examples 
#' ch5.man()
#' ch5.man(2)
#' @rdname ch5.man
#' @export
ch5.man = function(fn=0) {
    if (0 %in% fn) {
	cat("[1] disc.exp\tExpected Value of a Discrete Random Variable\n")
	cat("[2] disc.jexp\tJoint Probabilities and Expected Values of Two Discrete Random Variables\n")
	cat("[3] cont.jexp\tJoint pdf and Expected Value of Two Continuous Random Variables\n")
	cat("[4] corr.plot\tCorrelation Coefficients and Scatter Plots of Discrete Random variables\n")
    }
    if (1 %in% fn) {
	cat("[1] Expected Value of a Discrete Random Variable\n")
	cat("disc.exp(xv, xf, mt, dig=3, prt=TRUE, plot=FALSE, pos=\"topright\")\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("xv\t Vector of random variable values\n")
	cat("xf\t Vector of PDF(or frequency distribution)\n")
	cat("[Optional Input]--------------------------\n")
	cat("mt\t graph title\n")
	cat("dig\t Number of digits below the decimal point (default=3)\n")
	cat("prt\t Logical value for printing detailed output (default=TRUE)\n")
	cat("plot\t Logical value for plotting the PDF (default=FALSE)\n")
	cat("pos\t Legend location (default=\"topright\")\n")
	cat("[Returned Object]--------------------------\n")
	cat("list(Ex=expected value, Dx=standard deviation, Vx=variance)\n")
    }
    if (2 %in% fn) {
	cat("[2] Joint Probabilities and Expected Values of Two Discrete Random Variables\n")
	cat("disc.jexp(tabXY, Xn=\"X\", Yn=\"Y\", prt=\"exp\", pprt=FALSE, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("tabXY\t Joint frequency (or probability) table\n")
	cat("[Optional Input]--------------------------\n")
	cat("Xn\t Name of X (default=\"X\")\n")
	cat("Yn\t Name of Y (default=\"Y\")\n")
	cat("prt\t Option for detailed output in c(\"\", \"exp\", \"cov\", \"cor\") (default=\"exp\")\n")
	cat("pprt\t Logical value for plotting the joint & marginal PDF (default=FALSE)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("[Returned Object]--------------------------\n")
	cat("list(Ex=E(X), Dx=D(X), Ey=E(Y), Dy=D(Y), Vxy=Cov(X,Y), Cxy=Corr(X,Y))\n")
    }
    if (3 %in% fn) {
	cat("[3] Joint pdf and Expected Value of Two Continuous Random Variables\n")
	cat("cont.jexp(FUN, lo1=-Inf, up1=Inf, lo2=-Inf, up2=Inf, dig=4, prt=\"exp\")\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("FUN\t Continuous joint probability density function\n")
	cat("[Optional Input]--------------------------\n")
	cat("lo1\t Lower limit of X (default=-Inf)\n")
	cat("up1\t Upper limit of X (default=Inf)\n")
	cat("lo2\t Lower limit of Y (default=-Inf)\n")
	cat("up2\t Upper limit of Y (default=Inf)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("prt\t Option for detailed output in c(\"\", \"exp\", \"cov\", \"cor\") (default=\"exp\")\n")
	cat("[Returned Object]--------------------------\n")
	cat("list(Ex=E(X), Dx=D(X), Ey=E(Y), Dy=D(Y), Vxy=Cov(X,Y), Cxy=Corr(X,Y))\n")
    }
    if (4 %in% fn) {
	cat("[4] Correlation Coefficients and Scatter Plots of Discrete Random variables\n")
	cat("corr.plot(X, Mt, item, dig=4, prt=TRUE, pprt=FALSE, plot=FALSE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("X\t Sample space vector of X\n")
	cat("item\t Names of random variables\n")
	cat("[Optional Input]--------------------------\n")
	cat("Mt\t Plot title\n")
	cat("dig\t Number of effective digits (default=5)\n")
	cat("prt\t Logical value for printing the result (default=TRUE)\n")
	cat("pprt\t Logical value for printing frequency tables (default=FALSE)\n")
	cat("plot\t Logical value for plotting the PDF and scatter plots (default=FALSE)\n")
    }
}

# [5-1] Expected Value of a Discrete Random Variable
#' @title Expected Value of a Discrete Random Variable
#' @description Expected Value of a Discrete Random Variable
#' @param xv Vector of random variable values
#' @param xf Vector of probability (or frequency) distribution
#' @param mt graph title
#' @param dig Number of digits below the decimal point (default=3)
#' @param prt Print detailed output? Default: FALSE
#' @param plot Plot the PDF? Default: FALSE
#' @param pos Legend location, Default: 'topright'
#' @return list(Ex=expected value, Dx=standard deviation, Vx=variance)
#' @examples 
#' p = c(1, 3, 3, 1)
#' x = c(-3, -1, 1, 3)*100
#' disc.exp(x, p)
#' @rdname disc.exp
#' @export 
disc.exp = function(xv, xf, mt, dig=3, prt=TRUE, plot=FALSE, pos="topright") {
    # random variable name
	Xn = toupper(deparse(substitute(xv)))
     # Calculate the expected value
	N = sum(xf)
	xp = xf/N
	ex = sum(xv*xp)
    # Calculate variance & standard deviation
	ex2 = sum(xv^2*xp)
	vx = ex2 - ex^2
	dx = sqrt(vx)
     if (prt) {
	if (N<1.01) {
	cat(paste0("E(",Xn,") = ", round(ex, dig)), "\n")
	cat(paste0("V(",Xn,") = ", round(ex2, dig), " - ", round(abs(ex), dig), "\U00B2 = ", round(vx, dig)), "\n")
	} else {
	cat(paste0("E(",Xn,") = ", sum(xv*xf), "/", N, " = ", round(ex, dig)), "\n")
	cat(paste0("V(",Xn,") = ", sum(xv^2*xf), "/", N, " - ", round(abs(ex), dig), "\U00B2 = ", round(vx, dig)), "\n")
	}
	cat(paste0("D(",Xn,") = \U221A(", round(vx, dig), ") = ", round(dx, dig)), "\n")
     }
    # Plot PDF f(x)
     if (plot) {
	if (missing(mt)) mt =paste("Probability Distribution of", Xn)
	win.graph(7, 5)
	x1 = min(xv)
	x2 = max(xv)
	xr = x2 - x1
	plot(xv, xp, type="h", main=mt, pch=19, lwd=5, ylim=c(0, max(xp)*1.1),
		xlim=c(x1-0.1*xr, x2+0.1*xr), col=2, xlab="x", ylab="f(x)")
	grid(col=3)
	# Display probability
	text(xv, xp, round(xp, dig), pos=3, col=4, cex=0.8)
	# Display the expected value
	ym = 0.5*max(xp)
	segments(ex, 0, ex, ym, lty=2, col=4)
	text(ex, ym, paste0("E(", Xn, ")=",round(ex, dig)), pos=3, col=4)
	# Display thestandard deviation
	x1 = ex - dx
	x2 = ex + dx
	arrows(ex, ym, x2, ym, length=0.1, lty=2, col=4)
	arrows(ex, ym, x1, ym, length=0.1, lty=2, col=4)
	# Display legend
	legend(pos, c(paste0("E(", Xn, ") = ",round(ex,dig)), 
		paste0("D(", Xn, ") = ", round(dx,dig))), bg="white")
     }
	invisible(list(Ex=ex, Dx=dx, Vx=vx))
}

# [5-2] Joint Probabilities and Expected Values of Two Discrete Random Variables
#' @title Expected Values of Two Discrete Random Variables
#' @description Joint Probabilities and Expected Values of Two Discrete Random Variables
#' @param tabXY Joint frequency (or probability) table
#' @param Xn Name of X, Default: 'X'
#' @param Yn Name of Y, Default: 'Y'
#' @param prt Option for detailed output in c("", "exp", "cov", "cor"), Default: 'exp'
#' @param pprt Plot the joint & marginal PDF? Default: FALSE
#' @param dig Number of digits below the decimal point, Default: 4
#' @return list(Ex=E(X), Dx=D(X), Ey=E(Y), Dy=D(Y), Vxy=Cov(X,Y), Cxy=Corr(X,Y))
#' @examples 
#' S = rolldie2(2)
#' X = apply(S, 1, max)
#' Y = apply(S, 1, min)
#' tXY = table(X, Y)
#' disc.jexp(tXY, prt="cor", pprt=TRUE)
#' @rdname disc.jexp
#' @export
disc.jexp = function(tabXY, Xn="X", Yn="Y", prt="exp", pprt=FALSE, dig=4) {
    # Marginal frequency distribution of X and Y 
	N = sum(tabXY)
	mtabXY = addmargins(tabXY)
    # Joint and marginal probability of X and Y 
	ptabXY = tabXY/N
	mptabXY = addmargins(ptabXY)
    # Expected value of X and Y
  	Xv = as.numeric(rownames(tabXY))
	Yv = as.numeric(colnames(tabXY))
	nv = dim(mtabXY)
	Xf = as.vector(mtabXY[, nv[2]])[-nv[1]]
	Yf = as.vector(mtabXY[nv[1], ])[-nv[2]]
	Sx = (Xv %*% Xf)[1,1]
	Ex = Sx/N
	Sy = (Yv %*% Yf)[1,1]
	Ey = Sy/N
    # Variances of X and Y
	Sx2 = (Xv^2 %*% Xf)[1,1]
	Vx = Sx2/N - Ex^2
	Dx = sqrt(Vx)
	Sy2 = (Yv^2 %*% Yf)[1,1]
	Vy = Sy2/N - Ey^2
	Dy = sqrt(Vy)
    # Covariance of X and Y
	XY = Xv %o% Yv
	Sxy = (as.vector(XY) %*% as.vector(tabXY))[1,1]
	Exy = Sxy/N
	Vxy = Exy - Ex*Ey
	Cxy = Vxy/(Dx*Dy)
    # Print the joint probabilities
	if (pprt) {
	     if (N>1.1) {
		cat(paste0("Joint & Marginal Frequency Distribution f(x,y)=n/", N), "\n")
		print(mtabXY)
	      } else {
		cat(paste0("Joint & Marginal Probability Distribution f(x,y)"), "\n")
		print(ptabXY)
	      }
	}
    # Print the expected values
	if (prt %in% c("exp", "cov", "cor")) {
		cat(paste0("E[X] = ", Sx, "/", N, " = ", round(Ex, dig)), "\n")
		cat(paste0("E[Y] = ", Sy, "/", N, " = ", round(Ey, dig)), "\n")
		cat(paste0("E[XY] = ", Sxy, "/", N, " = ", round(Exy, dig)), "\n")
	} 
	if (prt %in% c("cov", "cor")) {
		cat(paste0("Var(X) = ", Sx2, "/", N, " - ", round(abs(Ex),dig), "\U00B2 = ", round(Vx,dig)), "\n")
		cat(paste0("Var(Y) = ", Sy2, "/", N, " - ", round(abs(Ey),dig), "\U00B2 = ", round(Vy,dig)), "\n")
		cat(paste0("Cov(X,Y) = ", round(Exy,dig), " - ", round(Ex,dig), " \U00D7 ",
			round(Ey,dig), " = ", round(Vxy,dig)), "\n")
	} 
	if (prt=="cor") {
		cat(paste0("Corr(X,Y) = ", round(Vxy,dig), " / \U221A(", round(Vx,dig), "\U00D7",
			round(Vy,dig), ") = ", round(Cxy,dig)), "\n")
	}
    # Return the results
	out = list(Ex=Ex, Dx=Dx, Ey=Ey, Dy=Dy, Exy=Exy, Vxy=Vxy, Cxy=Cxy)
	invisible(out)
}

# [5-3] Joint pdf and Expected Value of Two Continuous Random Variables
# Define the expected value P(x1<X<x2, y1<Y<y2) (double integration)
Exp = function(FUN, x1, x2, y1, y2) {
	integrate(function(y) { 
	    sapply(y, function(y) { 
	        integrate(function(x) { 
	            sapply(x, function(x) FUN(x, y)) }, x1, x2)$value 
	    }) 
	  }, y1, y2) }

#' @title Expected Value of Two Continuous Random Variables
#' @description Joint pdf and Expected Value of Two Continuous Random Variables
#' @param FUN Continuous joint probability density function
#' @param lo1 Lower limit of X, Default: -Inf
#' @param up1 Upper limit of X, Default: Inf
#' @param lo2 Lower limit of Y, Default: -Inf
#' @param up2 Upper limit of Y, Default: Inf
#' @param dig Number of digits below the decimal point, Default: 4
#' @param prt Option for detailed output in c("", "exp", "cov", "cor"), Default: ''
#' @return list(Ex=E(X), Dx=D(X), Ey=E(Y), Dy=D(Y), Vxy=Cov(X,Y), Cxy=Corr(X,Y))
#' @examples 
#' pdf = function(x, y) 0.5*(x+3*y)*(x>0 & x<1)*(y>0 & y<1)
#' cont.jexp(pdf, prt="cor")
#' @rdname cont.jexp
#' @export 

cont.jexp = function(FUN, lo1=-Inf, up1=Inf, lo2=-Inf, up2=Inf, dig=4, prt="exp") {
    # Mean and variance of random variable X
	ex1 = function(x, y) x*FUN(x, y)
	ex2 = function(x, y) x^2*FUN(x, y)
	Ex = Exp(ex1, lo1, up1, lo2, up2)[[1]]
	Ex2 = Exp(ex2, lo1, up1, lo2, up2)[[1]]
	Vx = Ex2 - Ex^2
	Dx = sqrt(Vx)
    # Mean and variance of random variable X
	ey1 = function(x, y) y*FUN(x, y)
	ey2 = function(x, y) y^2*FUN(x, y)
	Ey = Exp(ey1, lo1, Inf, lo2, Inf)[[1]]
	Ey2 = Exp(ey2, lo1, Inf, lo2, Inf)[[1]]
	Vy = Ey2 - Ey^2
	Dy = sqrt(Vy)
    # Covariance and correlation of random variable X and Y
	exy = function(x, y) x*y*FUN(x, y)
	Exy = Exp(exy, lo1, Inf, lo2, Inf)[[1]]
	Vxy = Exy - Ex*Ey
	Cxy = Vxy/sqrt(Vx*Vy)
    # Display output
	if (prt %in% c("exp", "cov", "cor")) {
		cat(paste0("E(X) = ", round(Ex, dig)), "\n")
		cat(paste0("E(Y) = ", round(Ey, dig)), "\n")
		cat(paste0("E(XY) = ", round(Exy, dig)), "\n")
	} 
	if (prt %in% c("cov", "cor")) {
		cat(paste0("Var(X) = ", round(Ex2,dig), " - ", round(abs(Ex),dig), "\U00B2 = ",
			round(Vx,dig)), "\n")
		cat(paste0("Var(Y) = ", round(Ey2,dig), " - ", round(abs(Ey),dig), "\U00B2 = ",
			round(Vy,dig)), "\n")
		cat(paste0("Cov(X,Y) = ", round(Exy,dig), " - ", round(Ex,dig), " \U00D7 ",
			round(Ey,dig), " = ", round(Vxy,dig)), "\n")
	} 
	if (prt=="cor") {
		cat(paste0("Corr(X,Y) = ", round(Vxy,dig), " / \U221A(", round(Vx,dig), "\U00D7",
			round(Vy,dig), ") = ", round(Cxy,dig)), "\n")
	}
    # Return the results
	out = list(Ex=Ex, Dx=Dx, Ey=Ey, Dy=Dy, Exy=Exy, Vxy=Vxy, Cxy=Cxy)
	invisible(out)
}

# [5-4] Correlation Coefficients and Scatter Plots of Discrete Random variables
#' @title Correlation Coefficients and Scatter Plots
#' @description Correlation Coefficients and Scatter Plots of Discrete Random variables
#' @param X Sample space vector of X
#' @param Mt Plot title
#' @param item Names of random variables
#' @param dig Number of effective digits, Default: 5
#' @param prt Print the result? Default: TRUE
#' @param pprt Print frequency tables? Default: FALSE
#' @param plot Plot the PDF and scatter plots? Default: FALSE
#' @return None.
#' @examples 
#' S = rolldie2(4)
#' item = c("Sum", "Max", "Min", "Range")
#' X = list()
#' X[[1]] = apply(S, 1, sum)
#' X[[2]] = apply(S, 1, max)
#' X[[3]] = apply(S, 1, min)
#' X[[4]] = X[[2]]-X[[3]]
#' Mt = paste("PDF of", item, "in 4 Dice")
#' corr.plot(X, Mt, item, pprt=T, plot=T)
#' @rdname corr.plot
#' @export 
corr.plot = function(X, Mt, item, dig=4, prt=TRUE, pprt=FALSE, plot=FALSE) {
    # Number of random variables
	nv = length(X)
    # List of frequency distributions
	Xf = list()
	for (k in 1:nv) Xf[[k]] = table(X[[k]])
    # Print the frequency distribution
	if (pprt) {
		for (k in 1:nv) {
			cat(paste0(item[k], "(X", k, ") frequency distribution"))
			print(Xf[[k]])
		}
	}
    # List of random variable values
	Xv = list()
	for (k in 1:nv) Xv[[k]] = as.numeric(names(Xf[[k]]))
    # Probability distributions
	Xp = list()
	N = length(X[[1]])
	for (k in 1:nv) Xp[[k]] = Xf[[k]] / N
    # Calculate expected values
	SX = EX = rep(NA, nv)
	for (k in 1:nv) {
		SX[k] = (Xv[[k]] %*% Xf[[k]])[1,1]
		EX[k] = SX[k]/N
	}
    # Calculate variances
	SX2 = EX2 = VX = DX = rep(NA, nv)
	for (k in 1:nv) {
		SX2[k] = (Xv[[k]]^2 %*% Xf[[k]])[1,1]
		EX2[k] = SX2[k]/N
		VX[k] = EX2[k] - EX[k]^2
		DX[k] = sqrt(VX[k])
	}
    # Calculate covariance & correlation coefficient
	SXY = EXY = VXY = CXY = matrix(NA, nv, nv)
	colnames(VXY)=colnames(CXY)=rownames(VXY)=rownames(CXY)=paste0("X",1:nv)
	for (k in 1:nv) for (m in 1:nv) {
		XYf = table(X[[k]], X[[m]])
		SXY[k,m] = (as.vector(Xv[[k]] %o% Xv[[m]]) %*% as.vector(XYf))[1,1]
		XYp = XYf / N
		EXY[k,m] = SXY[k,m] / N
		VXY[k,m] = EXY[k,m] -EX[k]*EX[m]
		CXY[k,m] = VXY[k,m] / (VX[k] * VX[m])^0.5
	}
    # Display output
      if (prt) {
	cat("Expected Values and Variances ------------------------\n")
	for (k in 1:nv) cat(paste0("E(X",k,") ="), format(EX[k], digits=(dig+1)), "\t")
	cat("\n")
	for (k in 1:nv) cat(paste0("Var(X",k,") ="), format(VX[k], digits=(dig+1)), "\t")
	cat("\nVariance-Covariance Matrix ------------------------\n")
	print(VXY)
	cat("Correlation Coefficient Matrix ------------------------\n")
	print(CXY)
      }
    # Display plots -----------------------------------------
      if (plot) {
	if (missing(Mt)) Mt = paste(item, "probability distribution")
    # Display bar charts
	nc = ifelse(nv<=5, 2, 3)
	nr = ceiling(nv/nc)
	h = ifelse(nr>2, 9, 6)
	w = ifelse(nc>2, 9, 7)
	win.graph(w, h)
	par(mfrow=c(nr, nc))
	for (k in 1:nv) plot(Xp[[k]], type="h", col="red", main=Mt[k], ylab="f(x)", xlab="", lwd=3)

    # Scatter plots
	St = matrix("character", nv, nv)
	for (k in 1:nv) for (m in 1:nv) St[k, m] = paste(item[m], ":", item[k])
	np = nv*(nv-1)/2
	nc = ifelse(np<=5, 2, 3)
	nr = ceiling(np/nc)
	h = ifelse(nr>2, 9, 6)
	w = ifelse(nc>2, 9, 7)
	win.graph(w, h)
	par(mfrow=c(nr, nc))
	for (k in 1:(nv-1)) for (m in (k+1):nv) {
		plot(X[[m]], X[[k]], pch=19, col=4, main=St[k, m], 
		xlab=item[m], ylab=item[k])
		abline(lm(X[[k]]~X[[m]]), col=2)
	}
      }
}