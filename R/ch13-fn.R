# [Ch-13 Functions] ----------------------------------------------------------------------------------
# [Ch-13 Function Manual] -----------------------------------------
# source("E:/R-stat/test/Rstat/R/ch13-fn.R")
#' @title Manual for Ch13. Functions
#' @description Ch13. Analysis of Variance
#' @param fn Function number, Default: 0
#' @return None.
#' 
#' @examples 
#' ch13.man()
#' ch13.man(1)
#' @rdname ch13.man
#' @export
ch13.man = function(fn=0) {
    if (0 %in% fn) {
	cat("[1] anova1\t\tOne-way Analysis of Variance\n")
	cat("[2] anova2\t\tTwo-way Analysis of Variance\n")
    }
    if (1 %in% fn) {
	cat("[1] One-way Analysis of Variance\n")
	cat("anova1(y, f, xl=\"Factor\", yl=\"Response Variable\", step=0:7, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("y\t Response variable data\n")
	cat("f\t Factors (same length as y)\n")
	cat("[Optional Input]--------------------------\n")
	cat("xl\t Label of x-axis (default=\"Factor\")\n")
	cat("yl\t Label of y-axis (default=\"Response Variable\")\n")
	cat("step\t Steps of the analysis of variance (default=0:7)\n")
	cat("\t 0\t Calculate the mean of response variable for each factor level\n")
	cat("\t 1\t Box plot of response data for prior investigation\n")
	cat("\t 2\t One-way analysis of variance\n")
	cat("\t 3\t Diagnostic plot of one-way analysis of variance\n")
	cat("\t 4\t Confidence interval of the response mean for each factor level\n")
	cat("\t 5\t Plot confidence interval of the response mean for each factor level\n")
	cat("\t 6\t Confidence intervals of the response mean differences\n")
	cat("\t 7\t Plot confidence intervals of the response mean differences\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (2 %in% fn) {
	cat("[2] Two-way Analysis of Variance\n")
	cat("anova2(y, f1, f2, xl1, xl2, yl, step=0:7, alp=0.05,\n")
	cat("\t inter=TRUE, maxim=TRUE, nb=4, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("y\t Response variable data\n")
	cat("f1\t Levels of factor1 (same length as y)\n")
	cat("f2\t Levels of factor2 (same length as y)\n")
	cat("[Optional Input]--------------------------\n")
	cat("xl1\t Label of x-axis (default=name of f1)\n")
	cat("xl2\t Legend label (default=name of f2)\n")
	cat("yl\t Label of y-axis (default=name of y)\n")
	cat("step\t Steps of the analysis of variance (default=0:7)\n")
	cat("\t 0\t Calculate the mean of response variable for each factor level\n")
	cat("\t 1\t Interaction plot for prior investigation of data\n")
	cat("\t 2\t Two-way analysis of variance\n")
	cat("\t 3\t Diagnostic plot of two-way analysis of variance\n")
	cat("\t 4\t Confidence interval of the response mean for each factor level\n")
	cat("\t 5\t Plot confidence interval of the response mean for each factor level\n")
	cat("\t 6\t Confidence intervals of the response mean differences of best nb levels\n")
	cat("\t 7\t Plot confidence intervals of the response mean differences of best nb levels\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("inter\t Logical value for including interaction (default=TRUE)\n")
	cat("\t\t (Set inter=FALSE for the case with no replication\n")
	cat("maxim\t Logical value for maximization problem (default=TRUE)\n")
	cat("\t\t (Set maxim=FALSE for minimization problem\n")
	cat("nb\t Number of best level to be compared (default=4)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
}

# [13-1] One-way analysis of variance
#' @title One-way analysis of variance
#' @description One-way analysis of variance
#' @param y Response variable data
#' @param f Factors (same length as y)
#' @param xl Label of x-axis, Default: 'Factor'
#' @param yl Label of y-axis, Default: 'Response Variable'
#' @param step Steps of the analysis of variance, Default: 0:7
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' y = c(79,83,88,78,75, 81,89,91,84,86,82, 86,91,93,90,89, 76,81,82,79)
#' f = c(rep(150,5), rep(200,6), rep(250,5), rep(300,4))
#' anova1(y, f, xl="Temp", yl="Yield")
#' @rdname anova1
#' @export 
anova1 = function(y, f, xl="Factor", yl="Response Variable", step=0:7, alp=0.05, dig=4) {
    # Number of factor levels & data 
	m1 = length(unique(f))
	nn = length(y)
    # Transform factor levels into factors by as.factor( ) function
	af = as.factor(f)
    # Calculate the mean of response variable for each factor level
	ym = tapply(y, af, mean)
	if (0 %in% step) {
	  cat(paste0("[Step 0] Mean of ", yl, " for each level of ", xl), "---------\n")
	  print(round(ym, dig))
	}
    # Prior investigation of data
	if (1 %in% step) {
	  cat(paste0("[Step 1] Box Plot of ", yl, " for each level of ", xl), "---------\n")
	  win.graph(7,5)
	  boxplot(y ~ af, col=7, main=paste0("Box Plot of ", yl, " for each level of ", xl), 
		xlab=paste("Level of", xl), ylab=yl)
	  points(af, y, pch=19, col=2, cex=1.2)
	  lines(1:m1, ym, type="b", lty=2, pch=17, col=4, cex=1.2)
	}
    # Analysis of variance
	an1 = aov(y ~ af)
	ans1 = summary(an1)[[1]]
	if (2 %in% step) {
	  SSt = sum(y^2) - sum(y)^2/nn
	  antab = matrix(NA, 3, 5)
	  colnames(antab) = c("Sum Sq.", "df", "Mean Sq.", "F0", "P-value")
	  rownames(antab) = c(xl, "Error", "Total")
	  antab[1:2,] = cbind(ans1$Sum, ans1$Df, ans1$Mean, ans1$F, ans1$Pr)
	  antab[3,1] = SSt
	  antab[3,2] = nn-1
	  dum=round(antab, dig)
	  dum[is.na(dum)]=""
	  cat(paste0("[Step 2] ANOVA Table of ", yl, " w.r.t. ", xl), "---------\n")
	  print(as.data.frame(dum))
	}
    # Diagnostic plot
	if (3 %in% step) {
	  cat(paste0("[Step 3] ANOVA Diagnostic Plot of ", yl, " w.r.t. ", xl), "---------\n")
	  win.graph(7,4)
	  par(mfrow=c(1,2))
	  plot(an1, which=1:2)
	}
    # Confidence interval of the response mean for each factor level
	ni = tapply(y, af, length)
    # Mean square error and the tolerance of confidence intervals
	mse = summary(an1)[[1]]$Mean[2]
	se = sqrt(mse/ni)
	tol = qt(1-alp/2, nn-m1)*se
    # Confidence limits
	lcl = ym-tol
	ucl = ym+tol
	if (4 %in% step) {
	  cat(paste0("[Step 4] ", (1-alp)*100, "% CI for the Mean of ", yl, " w.r.t. ", xl), "---------\n")
	  cat("MSE =", round(mse, dig), "\n")
	  print(round(rbind(ym, tol, lcl, ucl), dig))
	}
    # Confidence interval plot
	if (5 %in% step) {
	  cat(paste0("[Step 5] ", (1-alp)*100, "% CI Plot for the Mean of ", yl, " w.r.t. ", xl), "---------\n")
	  width = 7+min(8, max(0, m1-6)*0.5)
	  win.graph(width, 5)
	  fnum = as.numeric(af)
	  lev=1:m1
	  x1 = 0.5
	  x2 = m1+0.5
	  ymin = min(y, lcl)
	  ymax = max(y, ucl)
	  y1 = ymin - 0.1*(ymax-ymin)
	  y2 = ymax + 0.1*(ymax-ymin)

	# Remove xlim and ylim !!
	  plot(fnum, y, pch=19, col=3, cex=1.2, xaxt ="n",
		main=paste0((1-alp)*100, "% CI for the Mean of ", yl, " w.r.t. ", xl), 
		ylab=yl, xlab=xl, xlim=c(x1, x2), ylim=c(y1, y2))
	  grid(col=3)
	  axis(1, at=1:m1, labels=levels(af))

    	# Lines connecting the means, the upper and lower limits
	  lines(lev, ym, type="b", lty=2, pch=17, col=2, cex=1.2)
	  lines(lev, lcl, type="b", lty=4, pch=18, col=4, cex=1.2)
	  lines(lev, ucl, type="b", lty=4, pch=18, col=4, cex=1.2)

    	# Confidence intervals
	  arrows(lev, lcl, lev, ucl, lwd=2, length=0.1, code=3, angle=90)

    	# Values of the means, the upper and lower limits
	  text(lev, ym, labels=round(ym, 3), cex=0.9, col=2, pos=4)
	  text(lev, lcl, labels=round(lcl, 3), cex=0.9, col=4, pos=4)
	  text(lev, ucl, labels=round(ucl, 3), cex=0.9, col=4, pos=4)
	}

    # Mean difference vector
	if (6 %in% step | 7 %in% step) {
	  ym2 = ym[1]-ym[-1]
	  for (k in 2:(m1-1)) ym2 = c(ym2, ym[k]-ym[-(1:k)])
	  ylv = paste0("A", 1:m1)
	  nylv = paste0(ylv[1], "-", ylv[-1])
	  for (k in 2:(m1-1)) nylv = c(nylv, paste0(ylv[k], "-", ylv[-(1:k)]))
	  names(ym2) = nylv
        # Tolerance of the confidence intervals for mean differences
	  env = sqrt(1/ni[1]+1/ni[-1])
	  for (k in 2:(m1-1)) env=c(env, sqrt(1/ni[k]+1/ni[-(1:k)]))
	  tol2 = qt(1-alp/2, nn-m1)*sqrt(mse)*env
	  names(tol2) = names(ym2)
        # Confidence limits for mean differences
	  lcl2 = ym2-tol2
	  ucl2 = ym2+tol2
	  cat(paste0("[Step 6] ", (1-alp)*100, "% CI for the Mean Differences of ", yl, " w.r.t. ", xl), 
		"---------\n")
	  print(round(rbind(ym2, tol2, lcl2, ucl2), dig))
	}
    # Confidence interval plots
	if (7 %in% step) {
	cat(paste0("[Step 7] ", (1-alp)*100, "% CI Plot for the Mean Differences of ", yl, " w.r.t. ", xl), 
		"---------\n")
	n2 = length(ym2)
	y1 = min(lcl2) - 0.1*(max(ucl2)-min(lcl2))
	y2 = max(ucl2) + 0.05*(max(ucl2)-min(lcl2))
	width = 7+min(8, max(0, m1-4))
	win.graph(width, 5)
	plot(ym2, type="n", xlim=c(0.5, n2+0.5), ylim=c(y1, y2),
		main=paste0((1-alp)*100, "% CI for the Mean Differences of ", yl, " w.r.t. ", xl), 
		ylab=paste("Mean Differences of", yl), xlab=paste("Level Combinations of", xl), xaxt="n")
	grid(col=3)
    	# Horizontal line of zero difference
	abline(h=0, lty=2, col=grey(0.2))

    	# Confidence intervals
	dcol= rep("green2", n2)
	dcol[lcl2>0 | ucl2<0] = "orange"
	arrows(1:n2, lcl2, 1:n2, ucl2, lwd=2, length=0.1, code=3, angle=90, col=dcol)
    	# Lines connecting the mean differences, the upper and lower limits
	lines(1:n2, lcl2, type="b", pch=18, lty=2, col=4)
	lines(1:n2, ucl2, type="b", pch=18, lty=2, col=4)

    	# Values of the mean differences, the upper and lower limits
	text(1:n2, ym2, labels=round(ym2, 2), cex=0.9, col=2, pos=4)
	text(1:n2, lcl2, labels=round(lcl2, 2), cex=0.9, col=1, pos=4)
	text(1:n2, ucl2, labels=round(ucl2, 2), cex=0.9, col=1, pos=4)
	lines(1:n2, ym2, type="b", pch=17, lty=2, col=2, cex=1.2)
	# mtext(names(ym2), side=1, at=1:n2, col=1, line=0.5)
	axis(1, at=1:n2, labels=names(ym2), las=ifelse(m1>5,2,1), cex.axis=ifelse(m1>4,0.8,1))
	}
}

# [13-2] Two-way analysis of variance
#' @title Two-way analysis of variance
#' @description Two-way analysis of variance
#' @param y Response variable data
#' @param f1 Levels of factor1 (same length as y)
#' @param f2 Levels of factor2 (same length as y)
#' @param xl1 Label of x-axis (default=name of f1)
#' @param xl2 Legend label (default=name of f2)
#' @param yl Label of y-axis (default=name of y)
#' @param step Steps of the analysis of variance, Default: 0:7
#' @param alp Level of significance, Default: 0.05
#' @param inter Logical value for including interaction, Default: TRUE
#' @param maxim Logical value for maximization problem, Default: TRUE
#' @param nb Number of best level to be compared, Default: 4
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' Yield = c(76,79,81,79,83,85, 79,81,84,86,89,88, 87,91,91,94,88,86, 79,82,85,84,77,76)
#' f1 = c(rep(100,6), rep(150,6), rep(200,6), rep(250,6))
#' f2 = rep(c(1,1,2,2,3,3), 4)
#' Temp = as.factor(f1)
#' Press = as.factor(paste0(f2, "Psig"))
#' anova2(Yield, Temp, Press)
#' @rdname anova2
#' @export
anova2 = function(y, f1, f2, xl1, xl2, yl, step=0:7, alp=0.05, 
	inter=TRUE, maxim=TRUE, nb=4, dig=4) {
	if (missing(yl)) yl = deparse(substitute(y))
	if (missing(xl1)) xl1 = deparse(substitute(f1))
	if (missing(xl2)) xl2 = deparse(substitute(f2))
    # Number of factor levels & data
	nn = length(y)
	m1 = length(unique(f1))
	m2 = length(unique(f2))
	nr = nn/(m1*m2)
	if (m2<6) {dcol = c(1, 2, 4, "green4", "purple")
	} else dcol = rainbow(m2)
    # Transform factor levels into factors by as.factor( ) function
	if (is.factor(f1)) {af1 = f1
	} else af1 = as.factor(f1)
	if (is.factor(f2)) {af2 = f2
	} else af2 = as.factor(f2)
    # Calculate the mean of response variable for each combination of factor levels
	ym1 = tapply(y, af1, mean)
	ym2 = tapply(y, af2, mean)
	ym = tapply(y, list(af2, af1), mean)
	if (0 %in% step) {	
	  cat(paste("[Step 0] Mean of", yl, "for each combination of", xl1, "&", xl2), "---------\n")
	  ymtab = addmargins(ym, FUN=mean, quiet=TRUE)
	  print(round(ymtab, dig))
	}
    # Prior investigation of data
	if (1 %in% step) {
	  cat(paste("[Step 1] Interaction Plot of", yl, "for each combination of", xl1, "&", xl2), "---------\n")
	  win.graph(7,5)
	  interaction.plot(af1, af2, y, type="b", col=dcol[1:m2], lwd=2, leg.bg="white", leg.bty="o",
		main=paste("Interaction Plot of", yl, "w.r.t.", xl1, "&", xl2),
		xlab=xl1, ylab=paste0(yl, " mean"), trace.label = xl2)
	  grid(col=3)
	}
    # Analysis of variance
	if (inter) {an2 = aov(y ~ af1 * af2)
		nrtab = 5
		rname = c(xl1, xl2, paste(xl1,xl2,sep="*"), "Error", "Total")
	} else {an2 = aov(y ~ af1 + af2)
		nrtab = 4
		rname = c(xl1, xl2, "Error", "Total")
	}
	ans2 = summary(an2)[[1]]
	if (2 %in% step) {
	  SSt = sum(y^2) - sum(y)^2/nn
	  antab2 = matrix(NA, nrtab, 5)
	  colnames(antab2) = c("Sum of Sq.", "df", "Mean Sq.", "F0", "P-value")
	  rownames(antab2) = rname
	  antab2[1:(nrtab-1),] = cbind(ans2$Sum, ans2$Df, ans2$Mean, ans2$F, ans2$Pr)
	  antab2[nrtab,1] = SSt
	  antab2[nrtab,2] = nn-1
	  dum=round(antab2, dig)
	  dum[is.na(dum)]=""
	  cat(paste("[Step 2] ANOVA Table of", yl, "w.r.t.", xl1, "&", xl2), "---------\n")
	  print(as.data.frame(dum))
	}
    # Diagnostic plot
	if (3 %in% step) {
	  cat(paste0("[Step 3] ANOVA Diagnostic plot of ", yl, " w.r.t. ", xl1, " & ", xl2), "---------\n")
	  win.graph(7,4)
	  par(mfrow=c(1,2))
	  plot(an2, which=1:2)
	}
    # Confidence interval of the response mean for each combination of factor levels
    # Mean square error and the tolerance of confidence intervals
	if (inter) {ye = ym
		mse = summary(an2)[[1]]$Mean[4]
		se = sqrt(mse/nr)
		dfe = m1*m2*(nr-1)
		if (maxim) { yes = rev(sort(ye))
		} else yes = sort(ye)
		yopt = yes[1]
	} else {	ye = outer(ym2, ym1, "+") - mean(y)
		mse = summary(an2)[[1]]$Mean[3]
		se = sqrt(mse*(1/m1+1/m2-1/(m1*m2)))
		dfe = (m1-1)*(m2-1)	
		if (maxim) { yes = rev(sort(ye))
		} else yes = sort(ye)
		yopt = yes[1]
	}
	tol = qt(1-alp/2, dfe)*se
    # Confidence limits
	lcl = ye - tol
	ucl = ye + tol
    # Confidence interval for the best combination of factor levels
	lmax = yopt-tol
	umax = yopt+tol
	if (4 %in% step) {
	  cat(paste("[Step 4] CI for the Best Mean of", yl, "w.r.t.", xl1, "&", xl2), "---------\n")
	  cat("MSE =", round(mse, dig), "\n")
	  cat(paste0("[", round(yopt, dig), " \U00B1 ", round(tol, dig), "] = [", 
		round(lmax, dig),", ",round(umax, dig),"]\n"))
	  cat(paste("Means of", yl, "w.r.t.", xl1, "&", xl2), "---------\n")
	  print(round(ye, dig))
	  cat(paste("Lower limits for the Mean of", yl, "w.r.t.", xl1, "&", xl2), "---------\n")
	  print(round(lcl, dig))
	  cat(paste("Upper limits for the Mean of", yl, "w.r.t.", xl1, "&", xl2), "---------\n")
	  print(round(ucl, dig))
	}
    # Confidence interval plot
	if (5 %in% step) {
	  cat(paste0("[Step 5] ", (1-alp)*100, "% CI for the Mean of ", yl, " w.r.t. ", xl1, " & ", xl2), 
		"---------\n")
	  win.graph(7, 5)
	  # lev = sort(unique(af1))
	  # x1 = lev[1]-(lev[2]-lev[1])/2
	  # x2 = lev[m1]+(lev[m1]-lev[m1-1])/2
	  lev = 1:m1
	  x1 = 0.5
	  x2 = m1+0.5
	  ymin = min(y, lcl)
	  ymax = max(y, ucl)
	  y1 = ymin - (ymax-ymin)*0.1
	  y2 = ymax + (ymax-ymin)*0.1
	  plot(unique(af1), rep(-10000, m1), type="n", 
		main=paste0((1-alp)*100, "% CI for the Mean of ", yl, " w.r.t. ", xl1, " & ", xl2), 
		ylab=yl, xlab=xl1, xlim=c(x1, x2), ylim=c(y1, y2))
	  grid(col=3)
        # Lines connecting the means, the upper and lower limits
	  del = (lev[m1]-lev[1])*0.01
	  dv = del*(-(m2-1)/2):((m2-1)/2)
	  for (k in 1:m2) lines(lev+dv[k], ye[k, ], type="b", lty=2, pch=17, col=dcol[k], cex=1.2)
        # Confidence intervals
	  for (k in 1:m2) arrows(lev+dv[k], lcl[k,], lev+dv[k], ucl[k,], col=dcol[k], lwd=2, length=0.05, 
		code=3, angle=90)
        # Display legend
	  legend("topright", as.character(unique(af2)), lwd=2, text.col=dcol[1:m2], col=dcol[1:m2])
 	}
    # Mean differences
	if (6 %in% step | 7 %in% step) {
	  # Select best nb combinations of factor levels
	  mm1=mm2=rep(NA, nb)
	  for (k in 1:nb) mm1[k] = ceiling(which(ye==yes[k])/m2)
	  for (k in 1:nb) mm2[k] = which(ye==yes[k]) %% m2
	  mm2[mm2==0] = m2
	  yeb = yes[1:nb]
	  ye2 = yeb[1]-yeb[-1]
	  for (k in 2:(m1-1)) ye2 = c(ye2, yeb[k]-yeb[-(1:k)])
	  ylv = paste0(mm1, mm2)
	  nylv = paste0(ylv[1], "-", ylv[-1])
	  for (k in 2:(m1-1)) nylv = c(nylv, paste0(ylv[k], "-", ylv[-(1:k)]))
	  names(ye2) = nylv
        # Tolerance of the confidence intervals
	  if (inter) {  env = sqrt(2/nr)
	  } else env = sqrt(2*(1/m1+1/m2-1/(m1*m2)))  
	  tol2 = qt(1-alp/2, dfe)*sqrt(mse)*env
        # Confidence limits
	  lcl2 = ye2-tol2
	  ucl2 = ye2+tol2
	  cat(paste("[Step 6] Mean Differences of", yl, "for best", nb, "Combinations of", xl1, "&", xl2), 
		"---------\n")
	  print(round(rbind(ye2, tol2, lcl2, ucl2), dig))
	}
    # Confidence interval plot
	if (7 %in% step) {
	cat(paste0("[Step 7] ", (1-alp)*100, "% CI for the Mean Differences of ", yl, " w.r.t. ", 
		xl1, " & ", xl2), "---------\n")
	n2 = length(ye2)
	y1 = min(lcl2) - 0.1*(max(ucl2)-min(lcl2))
	y2 = max(ucl2) + 0.05*(max(ucl2)-min(lcl2))
	win.graph(7, 5)
	plot(ye2, type="n", xlim=c(0.5, n2+0.5), ylim=c(y1, y2),
	  main=paste0((1-alp)*100, "% CI for the Mean Differences of ", yl, " w.r.t. ", xl1, " & ", xl2), 
	  ylab=paste("Mean Differences of", yl), 
	  xlab=paste("Best", nb, "Combinations of", xl1, "&", xl2), xaxt="n")
	grid(col=3)

    	# Horizontal line of zero difference
	abline(h=0, lty=2, col=grey(0.2))
    	# Confidence intervals
	dcol= rep("green2", n2)
	dcol[lcl2>0 | ucl2<0] = "orange"
	arrows(1:n2, lcl2, 1:n2, ucl2, lwd=2, length=0.1, code=3, angle=90, col=dcol)
    	# Lines connecting the mean differences, the upper and lower limits
	lines(1:n2, lcl2, type="b", pch=18, lty=2, col=4)
	lines(1:n2, ucl2, type="b", pch=18, lty=2, col=4)
    	# Values of the mean differences, the upper and lower limits
	text(1:n2, ye2, labels=round(ye2, 2), cex=0.9, col=2, pos=4)
	text(1:n2, lcl2, labels=round(lcl2, 2), cex=0.9, col=1, pos=4)
	text(1:n2, ucl2, labels=round(ucl2, 2), cex=0.9, col=1, pos=4)
	lines(1:n2, ye2, type="b", pch=17, lty=2, col=2, cex=1.2)
	mtext(names(ye2), side=1, at=1:n2, col=1, line=0.5)
	}
}