# [Ch-12 Functions] ----------------------------------------------------------------------------------
# [Ch-12 Function Manual] -----------------------------------------
# source("E:/R-stat/Digeng/ch12-function.txt")
#' @title Manual for Ch12. Functions
#' @description Ch12. Analysis of Categorical Data
#' @param fn Function number, Default: 0
#' @return None.
#' 
#' @examples 
#' ch12.man()
#' ch12.man(1)
#' @rdname ch12.man
#' @export 
ch12.man = function(fn=0) {
    if (0 %in% fn) {
	cat("[1] chi.gof1\t\tGoodness of Fit Test from a Table\n")
	cat("[2] chi.gof2\t\tGoodness of Fit Test for Poisson/Binomial\n")
	cat("[3] cross.test     \tTest of Homogeniety/Independence\n")
	cat("[4] chitest.plot2  \tPlot the Result of Chi-square Test\n")
	cat("[5] mosaic2\t\tMosaic Plot\n")
    }
    if (4 %in% fn) {
	cat("[4] Plot the Result of Chi-square Test\n")
	cat("chitest.plot2(stat, df, alp=0.05, side=\"two\", pup=0.999, dig=4, ppt=20)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("stat\t Chi-square test statistic\n")
	cat("df\t Degree of freedom\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("side\t Type of the alternative hypothesis (default=\"two\")\n")
	cat("pup\t Maximum probability for the range of x-axis (default=0.999)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("ppt\t Number of plot points in the critical region (default=20)\n")
    }
    if (5 %in% fn) {
	cat("[5] Mosaic Plot\n")
	cat("require(vcd)\n")
	cat("mosaic2(tab, mt, dig=4, resid=TRUE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("tab\t Data table \n")
	cat("mt\t Graph title\n")
	cat("[Optional Input]--------------------------\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("resid\t Logical value for displaying Pearson residuals (default=TRUE)\n")
    }
    if (1 %in% fn) {
	cat("[1] Goodness of Fit Test from a Table\n")
	cat("chi.gof1(x, p, alp=0.05, dig=2, dig2=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Table of frequency\n")
	cat("[Optional Input]--------------------------\n")
	cat("p\t Probability vector\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point for the table (default=2)\n")
	cat("dig2\t Number of digits below the decimal point for the results (default=4)\n")
    }
    if (2 %in% fn) {
	cat("[2] Goodness of Fit Test for Poisson/Binomial\n")
	cat("chi.gof2(tab, para, para2, dist=\"pois\", mc1, mc2, alp=0.05, dig=2, dig2=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("tab\t Table of frequency\n")
	cat("[Optional Input]--------------------------\n")
	cat("para\t First parameter (Poisson: mean, Binomial: sample size)\n")
	cat("para2\t Second parameter (Poisson: none, Binomial: probability)\n")
	cat("dist\t Distribution name ('pois' or 'binom') (default='pois')\n")
	cat("mc1\t Sequential column numbers to merge (first set, lower side)\n")
	cat("mc2\t Sequential column numbers to merge (second set, upper side)\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point for the table (default=2)\n")
	cat("dig2\t Number of digits below the decimal point for the results (default=4)\n")
    }
    if (3 %in% fn) {
	cat("[3] Test of Homogeniety/Independence\n")
	cat("cross.test(x, v1, v2, alp=0.05, dig=2, dig2=4, prt=TRUE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Cross Table of frequency\n")
	cat("[Optional Input]--------------------------\n")
	cat("v1\t Row variable name\n")
	cat("v2\t Column variable name\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point for the table (default=2)\n")
	cat("dig2\t Number of digits below the decimal point for the results (default=4)\n")
	cat("prt\t Logical value for printing tables in detail (default=TRUE)\n")
    }
}

# [12-4] Plot the Result of Chi-square Test
#' @title Plot the Chi-square Test
#' @description Plot the Result of Chi-square Test
#' @param stat Chi-square test statistic
#' @param df Degree of freedom
#' @param alp Level of significance, Default: 0.05
#' @param side Type of the alternative hypothesis, Default: 'two'
#' @param pup Maximum probability for the range of x-axis, Default: 0.999
#' @param dig Number of digits below the decimal point, Default: 4
#' @param ppt Number of plot points in the critical region, Default: 20
#' @return None.
#' 
#' @examples 
#' # Goodness-of-fit Test
#' x = c(31,26,22,18,13,10)
#' (ct = chisq.test(x))
#' chitest.plot2(stat=ct$stat, df=ct$para, side="up")
#' # Test of Homogeneity
#' x = c(20,16,29,21,14,  14,22,26,25,13,  18,24,32,18, 8,  8,18,33,16,25)
#' x = matrix(x, nrow=4, ncol=5, byrow=TRUE)
#' (ct = chisq.test(x))
#' chitest.plot2(stat=ct$stat, df=ct$para, side="up")
#' @rdname chitest.plot2
#' @export
chitest.plot2 = function(stat, df, alp=0.05, side="two", pup=0.999, dig=4, ppt=20) {
    # Set the critical value and plot range
	rej = qchisq(1-alp, df)
	prng=c(0,qchisq(pup, df))
	xa = seq(prng[1], prng[2], length.out=101)
    # Plot the PDF
	ymax=max(dchisq(xa, df))
	win.graph(7, 5)
	plot(xa, dchisq(xa, df), type="n", xlab="Chi-square Statistic", ylab="pdf", 
		ylim=c(0, ymax*1.1),
		main=bquote(bold("Distribution of the Chi-square Statistic under H0: ")~chi^2 ~( .(df)) ))
	abline(h=0)
    # P-value and the critical region
	plow = pchisq(stat, df)
	if (side=="up" | grepl(side, "greater")) {
		pv = 1-plow
		cat("P-v =", round(pv, dig), "\n")
		cord.x = c(stat, seq(stat, prng[2], length.out=ppt), prng[2]) 
		cord.y = c(0, dchisq(seq(stat, prng[2], length.out=ppt), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
	    # Display the critical value
		rejy = dchisq(rej, df)
		rejy = (rejy+ymax)/2
		staty = (dchisq(stat, df)+ymax)/2
		segments(stat, 0, stat, staty, lwd=1, col=4)
		xpv = ifelse(stat>qchisq(0.5, df), (stat+prng[2])/2, stat)
		# [corr] ypv = ifelse(stat>qchisq(0.5, df), dchisq(stat, df), dchisq(stat, df)/2)
		ypv = ifelse(stat>qchisq(0.5, df), dchisq(xpv, df), dchisq(stat, df)/2)
		pospv = ifelse(stat>qchisq(0.5, df), 3, 4)
		text(xpv, ypv, round(pv, dig), pos=pospv, col=2)
	    # text(stat, 0, round(stat, dig), pos=1, col=4)
		segments(rej, 0, rej, rejy, lwd=1, col=2)
		ry0 = ifelse (abs(rej-stat)<2, rejy, 0)
	    # text(rej, ry0, labels=round(rej, dig), pos=1, col=4)
		# [corr] postat = ifelse(stat>qchisq(0.5, df), 4, 3)
		postat = ifelse(stat>rej, 4, 2)
		text(stat, staty, labels=bquote(chi[0]^2 == .(round(stat, dig))), pos=postat, col=4, cex=1)
		text(rej, rejy, labels=bquote(chi[.(paste(1-alp,";",df))]^2 == .(round(rej, dig))), pos=3, col=2, cex=1)
	} else if (side=="low" | grepl(side, "less")) {
		pv = plow
		cat("P-v =", round(pv, dig), "\n")
		cord.x = c(prng[1], seq(prng[1], stat, length.out=ppt), chis) 
		cord.y = c(0, dchisq(seq(prng[1], stat, length.out=ppt), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(stat, 0, stat, dchisq(stat, df), lwd=2, col=2)
		text(stat, dchisq(stat, df), round(pv, dig), pos=2, col=2)
		text(stat, 0, round(stat, dig), pos=1, col=4)
	} else if (grepl(side, "two sided")) {
		pv = 2*min(plow, 1-plow)
		cat("P-v =", round(pv, dig), "\n")
		mlow =qchisq(pv/2, df)
		mup =qchisq(1-pv/2, df)
		cord.x = c(mup, seq(mup, prng[2], length.out=ppt), prng[2]) 
		cord.y = c(0, dchisq(seq(mup, prng[2], length.out=ppt), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		cord.x = c(prng[1], seq(prng[1], mlow, length.out=ppt), mlow) 
		cord.y = c(0, dchisq(seq(prng[1], mlow, length.out=ppt), df), 0) 
		polygon(cord.x, cord.y, col='lightcyan')
		segments(c(mlow, mup), 0, c(mlow, mup), dchisq(c(mlow, mup), df), lwd=2, col=2)
		text(c(mlow, mup), dchisq(c(mlow, mup), df), round(pv/2, dig), pos=c(2,4), col=2)
		text(c(mlow, mup), 0, round(c(mlow, mup), dig), pos=1, col=4)
	}
	abline(v=qchisq(0.5, df), lty=2, lwd=2, col="green3")
	lines(xa, dchisq(xa, df), type="l", lwd=2, col=4)

}

# [12-5] Mosaic Plot
#' @title Mosaic Plot
#' @description Mosaic Plot
#' @param tab Data table
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @param resid Display Pearson residuals? Default: TRUE
#' @return Object from chisq.test()
#' 
#' @examples 
#' require(vcd)
#' x = c(39,18,12,31,14, 35,23,18,35,13,  27,16,17,24,8,  9,12,8,19,22)
#' x = matrix(x, nrow=4, ncol=5, byrow=TRUE)
#' Subject = c("Kor", "Eng", "Math", "Etc")
#' Hope = c("Sam", "Pub", "Exp", "Sal", "Etc")
#' t1 = as.table(x)
#' dimnames(t1) = list(Subject=Subject, Hope=Hope)
#' win.graph(7, 6)
#' mosaic2(tab=t1, mt="Students' Favorite Subject and Hope")
#' @rdname mosaic2
#' @export 
mosaic2 = function(tab, mt, dig=4, resid=TRUE) {
	mosaic(tab, shade=T, pop=F, main=mt,
        		labeling_args=list(offset_varnames=c(top=1), offset_labels=c(top=0.3)))
	ct = chisq.test(tab)
	cat("Test statistic =", round(ct$stat, dig), "\t df =", ct$para, "\t P-value =", round(ct$p.val, dig), "\n")
    # Display the Pearson residual in each cell
	if (resid) labeling_cells(text=round(ct$res, 1), clip=FALSE)(tab)

	invisible(ct)
}

# [12-1] Goodness of Fit Test from a Table
#' @title Goodness of Fit Test from a Table
#' @description Goodness of fit test from a table, when the probability is given.
#' @param x Table of frequency
#' @param p Probability vector
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point for the table, Default: 2
#' @param dig2 Number of digits below the decimal point for the results, Default: 4
#' @return list(stat=test stat, df=degree of freedom, crv=critical value, pv=p-value, tab=table)
#' 
#' @examples 
#' # Goodness-of-fit Test
#' x = c(31,26,22,18,13,10)
#' ans = chi.gof1(x)
#' @rdname chi.gof1
#' @export 
chi.gof1 = function(x, p, alp=0.05, dig=2, dig2=4) {
      # Number of Classes and Probability
	k = length(x)
	if (missing(p)) p = rep(1/k, k)
       # Test Statistics
	if (is.null(names(x))) names(x) = 1:k
	n = sum(x)
	np = n*p
	ress = (x-np)^2 / np
	stat = sum(ress)
	crv = qchisq(1-alp, k-1)
	pv = pchisq(stat, k-1, lower=F)
		
       # Print Results
	tab = rbind(x, np, ress)
	rownames(tab) = c("Freq", "n*p", "Resq")
	print(round(addmargins(tab, 2), dig))
	if (stat > crv) {sign = ">"
		ans = "Reject H0" 
	} else {sign = "<"
		ans = "Accept H0"}
	cat("---------------------------------------------------\n")
	cat("GoF Stat =", round(stat, dig2), sign, round(crv, dig2), "\U21D2", ans, "\n")
	cat("alpha =", alp, "\tP-value =", round(pv, dig2), "\n")

	invisible(list(stat=stat, df=k-1, crv=crv, pv=pv, tab=tab))
}

# [12-2] Goodness of Fit Test for Poisson/Binomial from a Frequency Table
#' @title Goodness of Fit Test for Poisson/Binomial
#' @description Goodness of Fit Test for Poisson/Binomial from a Frequency Table
#' @param tab Table of frequency
#' @param para First parameter (Poisson: mean, Binomial: sample size)
#' @param para2 Second parameter (Poisson: none, Binomial: probability)
#' @param dist Distribution name ('pois' or 'binom'), Default: 'pois'
#' @param mc1 Sequential column numbers to merge (first set, lower side)
#' @param mc2 Sequential column numbers to merge (second set, upper side)
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point for the table, Default: 2
#' @param dig2 Number of digits below the decimal point for the results, Default: 4
#' @return list(stat=test stat, df=degree of freedom, crv=critical value, pv=p-value, tab=table)
#' 
#' @examples 
#' # Goodness-of-fit Test (Poisson/Binomial)
#' set.seed(1234)
#' x=rpois(100,5)
#' mytab=table(x)
#' chi.gof2(mytab, 5)
#' chi.gof2(mytab)
#' chi.gof2(mytab, 10, 0.5, dist="binom")
#' chi.gof2(mytab, 10, dist="binom")
#' chi.gof2(mytab, mc1=8:9)
#' @rdname chi.gof2
#' @export 
chi.gof2 = function(tab, para, para2, dist="pois", mc1, mc2, alp=0.05, dig=2, dig2=4) {
      # Frequency Table
	y = as.numeric(names(tab))
	f = as.numeric(tab)
	N = sum(f)
      # Number of Classes and Probability
	k = length(tab)
	miss.para = FALSE

	if (dist=="pois") {
		if (missing(para)) {para=sum(y*f)/N
			miss.para = TRUE}
		p = dpois(y, para)
		p[1] = ppois(y[1], para)
		p[k] = 1 - ppois(y[k-1], para)
	} else if (grepl(dist, "binomial")) {
		if (missing(para)) stop("The number of trials (n) is required for the binomial distribution")
		if (missing(para2)) {para2=sum(y*f)/(para*N)
			miss.para = TRUE}
		p = dbinom(y, para, para2)
		p[1] = pbinom(y[1], para, para2)
		p[k] = 1 - pbinom(y[k-1], para, para2)
	}
	np = N*p
	if (y[1]>0) names(tab)[1]=paste0("~",y[1])
	names(tab)[k]=paste0(y[k],"~")
	zz = names(tab)
	tab = rbind(tab, p, np)

	tab1 = tab
      # Merge Classes ------------------------------------------
	if (!missing(mc1)) {
       	  # Print Initial Table
	  rownames(tab) = c("freq", "p", "n*p")
	  print(round(addmargins(tab, 2), dig))
	  cat("---------------------------------------------------\n")

	  mc11=min(mc1)
	  mc12=max(mc1)
	  rc1=(mc11+1):mc12
	  # Merge
	  tab1[1,mc11] = sum(tab[1,mc1])
	  tab1[2,mc11] = sum(tab[2,mc1])
	  tab1[3,mc11] = sum(tab[3,mc1])
	  colnames(tab1)[mc11] = paste(zz[mc11], zz[mc12], sep=":")
	}
	if (!missing(mc2)) {
	  mc21=min(mc2)
	  mc22=max(mc2)
	  rc2=(mc21+1):mc22
	  # Merge
	  tab1[1,mc21] = sum(tab[1,mc2])
	  tab1[2,mc21] = sum(tab[2,mc2])
	  tab1[3,mc21] = sum(tab[3,mc2])
	  colnames(tab1)[mc21] = paste(zz[mc21], zz[mc22], sep=":")
	}
	# Remove Backward
	if (!missing(mc2))  tab1=tab1[, -rc2]
	if (!missing(mc1))  tab1=tab1[, -rc1]

       # Test Statistics
	ress = (tab1[1, ] - tab1[3, ])^2 / tab1[3, ]
	stat = sum(ress)
	lt = ncol(tab1)
	df = lt - 1
	if (miss.para) df = df - 1
	crv = qchisq(1-alp, df)
	pv = pchisq(stat, df, lower=F)
       # Print Results
	tab1 = rbind(tab1, ress)
	rownames(tab1) = c("Freq", "p", "n*p", "Resq")
	print(round(addmargins(tab1, 2), dig))
	if (stat > crv) {sign = ">"
		ans = "Reject H0" 
	} else {sign = "<"
		ans = "Accept H0"}
	cat("---------------------------------------------------\n")
	if (dist=="pois") {	
		cat("Mean =", round(para, dig2), "\tDf =", df, "\n")
	} else if (grepl(dist, "binomial")) {
		cat("n =", para, "\tp =", round(para2, dig2), "\tDf =", df, "\n")
	}
	cat("GoF Stat =", round(stat, dig2), sign, round(crv, dig2), "\U21D2", ans, "\n")
	cat("alpha =", alp, "\tP-value =", round(pv, dig2), "\n")

	invisible(list(stat=stat, df=df, crv=crv, pv=pv, tab=tab1))
}

# [12-3] Test of Homogeniety/Independence from a Cross Table
#' @title Test of a Cross Table
#' @description Test of Homogeniety/Independence from a Cross Table
#' @param x Cross Table
#' @param v1 Row variable name (optional)
#' @param v2 Column variable name (optional)
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point for the table, Default: 2
#' @param dig2 Number of digits below the decimal point for the results, Default: 4
#' @param prt Logical, whether to print tables in detail, Default: TRUE
#' @return list(stat=test stat, df=df, crv=critical value, pv=p-value, exp=exp table, ress=residual square)
#' 
#' @examples 
#' data(exa12_7)
#' x=exa12_7
#' rownames(x)=c("Kor", "Eng", "Math", "Etc")
#' colnames(x)=c("Sam", "Pub", "Exp", "Sal", "Etc")
#' cross.test(x)
#' @rdname cross.test
#' @export 
cross.test = function(x, v1, v2, alp=0.05, dig=2, dig2=4, prt=TRUE) {
      # Number of Classes and Expectation
	r = nrow(x)
	c = ncol(x)
	if (is.null(rownames(x))) {
		if (missing(v1)) {rownames(x) = 1:r
		} else rownames(x) = paste0(v1, 1:r)
	}
	if (is.null(colnames(x))) {
		if (missing(v2)) {colnames(x) = 1:c
		} else colnames(x) = paste0(v2, 1:c)
	}
	tab0 = addmargins(x)
       # Test Statistics
	ex = outer(tab0[1:r, c+1], tab0[r+1, 1:c], "*")/tab0[r+1,c+1]
	ress = (x-ex)^2 / ex
	stat = sum(ress)
	df = (r-1)*(c-1)
	crv = qchisq(1-alp, df)
	pv = pchisq(stat, df, lower=F)
       # Print Results
	rownames(ex) = rownames(ress) = rownames(x)
	colnames(ex) = colnames(ress) = colnames(x)
	# print(tab0)
       if (prt) {
	cat("[Expected Frequency] --------------------------\n")
	print(round(addmargins(ex, 2), dig))
	cat("[Residual Square] -------------------------------\n")
	print(round(addmargins(ress), dig))
       }
	cat("[Test Summary] ---------------------------------\n")
	if (stat > crv) {sign = ">"
		ans = "Reject H0" 
	} else {sign = "<"
		ans = "Accept H0"}
	cat("GoF Stat =", round(stat, dig2), sign, round(crv, dig2), "\U21D2", ans, "\n")
	cat("Df =", df, "alpha =", alp, "\tP-value =", format(pv, scientific = TRUE, digits=dig2), "\n")

	invisible(list(stat=stat, df=df, crv=crv, pv=pv, exp=ex, res=ress))
}