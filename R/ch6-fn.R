# [Ch-6 Functions] ----------------------------------------------------------------------------------
# [Ch-6 Function Manual] -----------------------------------------
# source("E:/R-stat/Digeng/ch6-function.txt")

#' @title Manual for Ch.6
#' @description Ch.6 Probability Distributions of Discrete Random Variables
#' @param fn Function number, Default: 0
#' @return None.
#' @examples 
#' ch6.man()
#' ch6.man(1:2)
#' @rdname ch6.man
#' @export
ch6.man = function(fn=0) {
    if (0 %in% fn) {
	cat("[1] disc.mexp\t\tExpected Values of Discrete Random Variables\n")
	cat("[2] multinorm.plot \tGraphic Display of Multinomial Probability Distribution Function\n")
    }
    if (1 %in% fn) {
	cat("[1] Expected Values of Discrete Random Variables\n")
	cat("disc.mexp(xv, fx, fx2, fx3, mt, dig=3, del=0.2, prt=TRUE, plot=TRUE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("xv\t Vector of random variable values\n")
	cat("fx\t List of probability distributions (2~9)\n")
	cat("[Optional Input]--------------------------\n")
	cat("fx2\t Second list of probability distributions (same number of fx)\n")
	cat("fx3\t Third list of probability distributions (same number of fx)\n")
	cat("mt\t Vector of plot titles\n")
	cat("dig\t Number of digits below decimal point (default=3)\n")
	cat("del\t Distance between the probability distribution plots (default=0.2)\n")
	cat("prt\t Logical value for printing the expected values and variances (default=TRUE)\n")
	cat("plot\t Logical value for plotting the probability distributions (default=TRUE)\n")
    }
    if (2 %in% fn) {
	cat("[2] Graphic Display of Multinomial Probability Distribution Function\n")
	cat("multinorm.plot(ps, size)\n")
	cat("require(prob)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("ps\t Probability matrix with column for each group\n")
	cat("\t (relative group sizes may substitute for the probabilities)\n")
	cat("size\t Sample size\n")
    }
}

# [6-1] Expected Values of Discrete Random Variables
#' @title Expected Values of Discrete Random Variables
#' @description Expected Values of Discrete Random Variables
#' @param xv Vector of random variable values
#' @param fx List of probability distributions (2~9)
#' @param fx2 Second list of probability distributions (same number of fx)
#' @param fx3 Third list of probability distributions (same number of fx)
#' @param mt Vector of plot titles
#' @param dig Number of digits below decimal point, Default: 3
#' @param del Distance between the probability distribution plots, Default: 0.2
#' @param prt Print the expected values and variances? Default: TRUE
#' @param plot Plot the probability distributions? Default: TRUE
#' @return list(Ex, Vx, Dx)
#' @examples 
#' # Binomial distributions
#' n = 10;  p=c(0.2, 0.5, 0.8); x = 0:n
#' fx1 = fx2 = list()
#' for (i in 1:3) fx1[[i]] = dbinom(x, n, p[i])
#' mt1 = paste0("B(10,", p, ")")
#' disc.mexp(x, fx1, mt=mt1)
#' # Binomial vs. Hypergeometric
#' N = 50;  S =c(10, 25, 40); n = 10; x = 0:n
#' for (i in 1:3) fx2[[i]] = dhyper(x, S[i], N-S[i], n)
#' mt12 = paste0("HG(10,50,", S,"):B(10,", p,")")
#' disc.mexp(x, fx2, fx1, mt=mt12)
#' @rdname disc.mexp
#' @export
disc.mexp = function(xv, fx, fx2, fx3, mt, dig=3, del=0.2, prt=TRUE, plot=TRUE) {
    # Number of probability distributions
	ng = length(fx)
	if (ng>9) stop("Number of probability distribution must be in 2~9!")
	Add2 = ifelse (missing(fx2), FALSE, TRUE)
	Add3 = ifelse (missing(fx3), FALSE, TRUE)
    # Random variable name
	Xn = toupper(deparse(substitute(xv)))
    # Calculate expected values, variances, and standard deviations
	Ex = Exs = Vx = Dx = list()
	for (k in 1:ng) {
		Ex[[k]] = sum(xv*fx[[k]])
		Exs[[k]] = sum(xv^2*fx[[k]])
		Vx[[k]] = Exs[[k]] - Ex[[k]]^2
		Dx[[k]] = sqrt(Vx[[k]])
	}
	if (Add2) prt = FALSE
	if (Add3) prt = FALSE
    # Print expected values, variances, and standard deviations
      if (prt==TRUE) {
	for (k in 1:ng) {
		cat(paste0("E(",Xn,") = ", round(Ex[[k]], dig)), "\t ")
		cat(paste0("V(",Xn,") = ", round(Exs[[k]], dig), " - ", 
			round(Ex[[k]], dig), "\U00B2 = ", round(Vx[[k]], dig)), "\t ")
		cat(paste0("D(",Xn,") = \U221A(", round(Vx[[k]], dig), ") = ", round(Dx[[k]], dig)), "\n")
	}
     }
    # Plot probability distribution function f(x)
     if (plot==TRUE) {
	if (missing(mt)) mt =paste0("Probability Distribution of ", "X", 1:ng)
	nc = switch(ng, 1, 2, 3, 2, 3, 3, 3, 3, 3)
	nr = switch(ng, 1, 1, 1, 2, 2, 2, 3, 3, 3)
	win.graph(3*nc, 3*nr)
	par(mfrow=c(nr, nc))
	for (k in 1:ng) {
		plot(xv, fx[[k]], type="h", main=mt[k], ylab="f(x)", xlab="x", lwd=3, col=2)
		if (Add2) lines(xv-del, fx2[[k]], type="h", lwd=3, col=4)
		if (Add3) lines(xv+del, fx3[[k]], type="h", lwd=3, col="green3")
	}
     }
    # Return the results
	invisible(list(Ex, Vx, Dx))
}

# [6-2] Graphic Display of Multinomial Probability Distribution Function
#' @title Multinomial PDF Plot
#' @description Graphic Display of Multinomial Probability Distribution Function
#' @param ps Probability (or proportion) matrix with column for each group
#' @param size Sample size
#' @return Matrix of multinomial PDF
#' @examples 
#' library(scatterplot3d)
#' ps = matrix(c(1,1,8, 1,5,4, 4,4,2, 1,1,1), nrow=4, ncol=3, byrow=T)
#' multinorm.plot(ps, 5)
#' @rdname multinorm.plot
#' @export 
multinorm.plot = function(ps, size) {
    # Number of variables (nc) and Number of  distributions (ng)
	nc = 3
	ng = nrow(ps)
    # Create sample space using urnsample2( ) function
	xr = urnsample2(1:nc, size = size, replace = TRUE, ordered = FALSE)
	nr = nrow(xr)
	cat("Number of Possible Combinations =", nr, "\n")
    # Sum frequencies corresponding to each of 1, 2, ..., nc
	x = list()
	for (i in 1:nc) x[[i]] = apply(xr, 1, function(x) sum(x==i))
	cat("Combinations of Random Variable Vectors -------------\n")
	for (i in 1:nc) print(x[[i]])
    # Calculate probability distribution function using dmultinom() function
	xm = x[[1]]
	for (i in 2:nc) xm = cbind(xm, x[[i]])
	fx6 = matrix(NA, nr, ng)
	for (j in 1:ng) {for (i in 1:nr) {
		fx6[i, j] = dmultinom(xm[i, ], size=size, prob=ps[j, ]) } }
	colnames(fx6) = paste0("P",1:ng)
    # Confirm the sum of probabilities
	print(apply(fx6, 2, sum))
    # Display probability distribution function
	probs=rep("", ng)
	if (sum(ps[1,])>1) {
		for (j in 1:ng) probs[j]=paste(paste0(ps[j,],"/",sum(ps[j,])), collapse=",")
	} else {	for (j in 1:ng) probs[j]=paste(ps[j,], collapse=",")
	}
	mt6=paste0("Multinom(", probs, ")")
	wc=c(1,2,3,2,3,3,4,4,3,4)
	wr=c(1,1,1,2,2,2,2,2,3,3)
	ww=c(4,6,9,7,9,9,9,9,9,9)
	wl=c(3,3,3,6,6,6,6,6,9,9)
	win.graph(ww[ng], wl[ng])
	par(mfrow=c(wr[ng], wc[ng]))
	for (k in 1:ng) scatterplot3d(x[[1]], x[[2]], fx6[,k], type="h", main=mt6[k], 
		zlab="f(x1,x2,x3)", xlab="x1", ylab="x2", pch=16, lwd=5, color=2)
	dum = matrix(NA, nc, nr)
	for (i in 1:nc) dum[i,] = x[[i]]
	rownames(fx6) = paste0("p(", apply(dum, 2, paste, collapse=","), ")")
	invisible(fx6)
}
