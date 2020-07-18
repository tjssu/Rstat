# [Ch-9 Functions] ----------------------------------------------------------------------------------
# [Ch-9 Function Manual] -----------------------------------------
# source("E:/R-stat/Digeng/ch9-function.txt")
#' @title Manual for Ch9. Functions
#' @description Ch9. Distributions of Sample Statistics
#' @param fn Function number (0~6), Default: 0
#' @return None.
#' 
#' @examples 
#' ch9.man()
#' ch9.man(5)
#' @rdname ch9.man
#' @export
ch9.man = function(fn=0) {
    if (0 %in% fn) {
	cat("[1] norm.sim\t\tSimulation for the Normal Distribution\n")
	cat("[2] norm.spn\t\tMinimum Number of Samples from Normal Population\n")
	cat("[3] tdist.sim\t\tSimulation for the t-distribution\n")
	cat("[4] chi.sim\t\tSimulation for the chi-square Distribution\n")
	cat("[5] fdist.sim2\t\tSimulation for the F-distribution\n")
	cat("[6] clt.plot\t\tDiagnose the Central Limit Theorem\n")
    }
    if (1 %in% fn) {
	cat("[1] Simulation for the Normal Distribution\n")
	cat("norm.sim(ns, mu=0, sig=1, N=10000, ng=50, seed=9857, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("ns\t Sample size\n")
	cat("[Optional Input]--------------------------\n")
	cat("mu\t Expected value (default=0)\n")
	cat("sig\t Standard deviation (default=1)\n")
	cat("N\t Number of iterations (default=10000)\n")
	cat("ng\t Number of classes in histogram (default=50)\n")
	cat("seed\t Seed value for generating random numbers (default=9857)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (2 %in% fn) {
	cat("[2] Minimum Number of Samples from Normal Population\n")
	cat("norm.spn(kp, alp, lo=0.1, up=1, mt, dcol, log=TRUE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("kp\t Error limit in multiples of the standard deviation\n")
	cat("alp\t Level of significance vector\n")
	cat("[Optional Input]--------------------------\n")
	cat("lo\t Lower limit of the error limit (default=0.1)\n")
	cat("up\t Upper limit of the error limit (default=1)\n")
	cat("mt\t Plot title\n")
	cat("dcol\t Line color (default=rainbow())\n")
	cat("log\t Logical value for log-scaling y-axis (default=TRUE)\n")
    }
    if (3 %in% fn) {
	cat("[3] Simulation for the t-distribution\n")
	cat("tdist.sim(ns, mu=0, sig=1, N=10000, ng=100, seed=9857, dig=4, mt)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("ns\t Sample size\n")
	cat("[Optional Input]--------------------------\n")
	cat("mu\t Expected value (default=0)\n")
	cat("sig\t Standard deviation (default=1)\n")
	cat("N\t Number of iterations (default=10000)\n")
	cat("ng\t Number of classes in histogram (default=100)\n")
	cat("seed\t Seed value for generating random numbers (default=9857)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("mt\t Plot title\n")
    }
    if (4 %in% fn) {
	cat("[4] Simulation for the chi-square Distribution\n")
	cat("chi.sim(ns, mu=0, sig=1, N=10000, ng=100, seed=9857, dig=4, muknow=TRUE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("ns\t Sample size\n")
	cat("[Optional Input]--------------------------\n")
	cat("mu\t Expected value (default=0)\n")
	cat("sig\t Standard deviation (default=1)\n")
	cat("N\t Number of iterations (default=10000)\n")
	cat("ng\t Number of classes in histogram (default=100)\n")
	cat("seed\t Seed value for generating random numbers (default=9857)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
	cat("muknow\t Logical value for known expected value (default=TRUE)\n")
    }
    if (5 %in% fn) {
	cat("[5] Simulation for the F-distribution\n")
	cat("fdist.sim2(sig1, sig2, n1, n2, N=10000, ng=300, seed=9857, xp=1:9, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("sig1\t Standard deviation of the first population\n")
	cat("sig2\t Standard deviation of the second population\n")
	cat("n1\t Sample size of the first population\n")
	cat("n2\t Sample size of the second population\n")
	cat("[Optional Input]--------------------------\n")
	cat("N\t Number of iterations (default=10000)\n")
	cat("ng\t Number of classes in histogram (default=300)\n")
	cat("seed\t Seed value for generating random numbers (default=9857)\n")
	cat("xp\t Specific x-values for cumulative probability F(x)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (6 %in% fn) {
	cat("[6] Diagnose the Central Limit Theorem\n")
	cat("clt.plot(dist, para, para2, ns=c(10,30,50), d=rep(0.5, 3), N=10000, seed=9857, 
		sigknow=TRUE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("dist\t Name of population distribution (one of the follows)\n")
	cat("\t (\"exp\",\"gamma\",\"weibull\",\"beta\",\"norm\",
		\"t\",\"chisq\",\"f\",\"pois\",\"binom\")\n")
	cat("para\t Parameter for the first population\n")
	cat("para2\t Parameter for the second population (if necessary)\n")
	cat("[Optional Input]--------------------------\n")
	cat("ns\t Sample size (default=c(10,30,50))\n")
	cat("d\t Group width in histogram (default=rep(0.5, 3))\n")
	cat("N\t Number of iterations (default=10000)\n")
	cat("seed\t Seed value for generating random numbers (default=9857)\n")
	cat("sigknow\t Logical value for known population variance (default=TRUE)\n")
    }
}

# [9-1] Simulation for the Normal Distribution
#' @title Simulation for the Normal Distribution
#' @description Simulation for the Normal Distribution
#' @param ns Sample size
#' @param mu Expected value, Default: 0
#' @param sig Standard deviation, Default: 1
#' @param N Number of iterations, Default: 10000
#' @param ng Number of classes in histogram, Default: 50
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' norm.sim(ns=10, mu=100, sig=10, N=10000)
#' @rdname norm.sim
#' @export 
norm.sim=function(ns, mu=0, sig=1, N=10000, ng=50, seed=9857, dig=4) {
    # Generate ns sample means, iterate N times
	set.seed(seed)
	xb = NULL
	for (k in 1:N) xb = c(xb, mean(rnorm(ns, mu, sig)))
    # Standardization
	zb = (xb-mu)/sig*sqrt(ns)
    # Define the population PDF and sample PDF
	popd = function(x) dnorm(x, mu, sig)
	smd = function(x) dnorm(x, mu, sig/sqrt(ns))
    # Expected value & standard deviation
	Ex1 = round(mean(xb), dig)
	Dx1 = round(sd(xb), dig)
	Ex2 = mu
	Dx2 = round(sig/sqrt(ns), dig)
	Ez = round(mean(zb), dig)
	Dz = round(sd(zb), dig)
    # Compare cumulative probabilities F(1), F(2), ...
	xp = seq(floor(mu-3*sig/sqrt(ns)), ceiling(mu+3*sig/sqrt(ns)), by = 0.5*sig)
	Theory = pnorm(xp, mu, sig/sqrt(ns))
	Simula = sapply(xp, function(x) sum(xb<x))/N
	cdf = rbind(Theory, Simula)
	colnames(cdf)=paste0("F(", xp, ")")
	print(round(cdf, dig))
    # Plot the population PDF and sample PDF
	win.graph(7, 6)
	par(mfrow=c(2,1))
	par(mar=c(3,4,4,2))
	x1 = mu-3*sig
	x2 = mu+3*sig
	hist(xb, breaks=ng, prob=T, col=7, xlim=c(x1, x2), ylab="f(x)", xlab="",
		main=bquote(bold("Distribution of ") ~bar(X)[.(ns)]~~ bold(from) ~~ N( .(mu) , .(sig)^2 ) ))
	curve(popd, x1, x2, col=4, add=T)
	curve(smd, x1, x2, col=2, add=T)
	legend("topright", c("Para.  Exact  Simul.", 
			paste("E(X) ", Ex2, Ex1, sep="  "), paste("D(X)", Dx2, Dx1, sep="  ")),
		text.col=c(1,4,4) )
    # Plot distribution of the standardized statistic
	hist(zb, breaks=2*ng, prob=T, col="cyan", xlim=c(-4, 4), ylab=bquote(phi (z)), xlab="",
		main="Distribution of the Standardized Sample Mean")
	curve(dnorm, -4, 4, col=2, add=T)
	legend("topright", c("Para.  Exact  Simul.",  
			paste("E(Z)    ", 0, "    ", Ez), paste("D(Z)    ", 1, "    ", Dz)),
		text.col=c(1,4,4) )
}
# [9-2] Minimum Number of Samples from Normal Population
#' @title Minimum Number of Samples
#' @description Minimum Number of Samples from Normal Population
#' @param kp Error limit in multiples of the standard deviation
#' @param alp Level of significance vector
#' @param lo Lower limit of the error limit, Default: 0.1
#' @param up Upper limit of the error limit, Default: 1
#' @param mt Plot title
#' @param dcol Line color (default=rainbow())
#' @param log Logical value for log-scaling y-axis, Default: TRUE
#' @return None.
#' 
#' @examples 
#' alp = c(0.01, 0.05, 0.1)
#' dcol = c(2, 4, "green4")
#' norm.spn(kp=0.4, alp, dcol=dcol)
#' @rdname norm.spn
#' @export
norm.spn=function(kp, alp, lo=0.1, up=1, mt, dcol, log=TRUE) {
    # Function for calculating the minimum number of samples
	spn = function(k, alp) ceiling((qnorm(1-alp/2)/k)^2)
    # Get the minimum number of samples
	nalp = length(alp)
	nkp = length(kp)
	if (min(nalp, nkp)==1) {
		mspn = spn(kp, alp)
		if (nalp > 1) names(mspn) = alp
		if (nkp > 1) names(mspn) = kp
	} else if (nkp > nalp) {
		mspn = outer(alp, kp, "spn")
		colnames(mspn) = kp
		rownames(mspn) = alp
	} else {
		mspn = outer(kp, alp, "spn")
		colnames(mspn) = alp
		rownames(mspn) = kp
	}
	print(mspn)
    # Plot title
	if (missing(dcol)) dcol = rainbow(nalp)
	if (missing(mt)) mt = paste0("Minimum Number of Samples for ", 
		paste(kp, collapse="/"), "-sigma Error Limit")
    # Plot in log-scale
	win.graph(7, 6)
	kv = seq(lo, up, length=100)
	if (log) {
	plot(kv, spn(kv, alp[1]), type="n", log="y", ylab="Number of sample", xlab="k",
   		ylim=c(1, spn(kv[1], min(alp))), main=mt)
	} else {
	plot(kv, spn(kv, alp[1]), type="n", ylab="Number of sample", xlab="k",
   		ylim=c(1, spn(kv[1], min(alp))), main=mt)
	}
	grid( )
	for (i in 1:nalp) lines(kv,  spn(kv, alp[i]), lwd=2, col=dcol[i])
    # Display legend
	leg = list()
	for (i in 1:nalp) leg[[i]] = bquote(alpha==.(alp[i]))
	legend("topright", sapply(leg, as.expression), lwd=2, col=dcol)
    # Illustrate specific cases
	segments(kp, 1, kp, spn(kp, min(alp)), lty=2, col=6)
	segments(min(kv), spn(kp, alp), kp, spn(kp, alp), lty=2, col=6)
	text(min(kv), spn(kp, alp), labels=spn(kp, alp), col=4)
}

# [9-3] Simulation for the t-distribution
#' @title Simulation for the t-distribution
#' @description Simulation for the t-distribution
#' @param ns Sample size
#' @param mu Expected value, Default: 0
#' @param sig Standard deviation, Default: 1
#' @param N Number of iterations, Default: 10000
#' @param ng Number of classes in histogram, Default: 100
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param dig Number of digits below the decimal point, Default: 4
#' @param mt Plot title
#' @return None.
#' 
#' @examples 
#' tdist.sim(ns=10, mu=100, sig=10)
#' @rdname tdist.sim
#' @export
tdist.sim=function(ns, mu=0, sig=1, N=10000, ng=100, seed=9857, dig=4, mt) {
	if (missing(mt)) mt = paste0("Distribution of Standardized Sample Mean (n=", ns, ", N=", N, ")")
    # Generate ns sample means, iterate N times
	set.seed(seed)
	xb = ss = NULL
	for (k in 1:N) {
		sam = rnorm(ns, mu, sig)
		xb = c(xb, mean(sam))
		ss = c(ss, sd(sam)) 
	}
    # Standardization
	zb = (xb-mu)/ss*sqrt(ns)
    # Define theoretical PDF
	smd = function(x) dt(x, ns-1)
    # Expected value & standard deviation
	Ez = round(mean(zb), dig)
	Dz = round(sd(zb), dig)
	Dt = ifelse(ns>3, round(sqrt((ns-1)/(ns-3)), dig), Inf)
    # Compare cumulative probabilities F(1), F(2), ...
	zp = -3:3
	Theory = pt(zp, ns-1)
	Simula = sapply(zp, function(x) sum(zb<x))/N
	cdf = rbind(Theory, Simula)
	colnames(cdf)=paste0("F(", zp, ")")
	print(round(cdf, dig))
    # Display graph
	x1 = -5
	x2 = 5
	win.graph(7, 5)
	hist(zb, breaks=ng, prob=T, col=7, xlim=c(x1, x2), ylab="f(t)", xlab="t", main=mt)	
	curve(dnorm, x1, x2, lwd=2, col=4, add=T)
	curve(smd, x1, x2, lwd=2, col=2, add=T)
	legend("topright", c("Para.  Exact  Simul.", 
			paste("E(T)      ", 0, "      ", Ez), paste("D(T) ", Dt, " ", Dz)),
		text.col=c(1,4,4) )
	legend("topleft", c(paste0("t(", ns-1,")"), "N(0,1)"), lwd=c(2,2), col=c(2,4))
}

# [9-4] Simulation for the chi-square Distribution
#' @title Simulation for the chi-square Distribution
#' @description Simulation for the chi-square Distribution
#' @param ns Sample size
#' @param mu Expected value, Default: 0
#' @param sig Standard deviation, Default: 1
#' @param N Number of iterations, Default: 10000
#' @param ng Number of classes in histogram, Default: 100
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param dig Number of digits below the decimal point, Default: 4
#' @param muknow Logical value for known expected value, Default: TRUE
#' @return None.
#' 
#' @examples 
#' chi.sim(ns=10, mu=100, sig=10)
#' @rdname chi.sim
#' @export
chi.sim=function(ns, mu=0, sig=1, N=10000, ng=100, seed=9857, dig=4, muknow=TRUE) {
    # Generate ns sample means, iterate N times
	set.seed(seed)
	cs = NULL
    # Sample statistic with mu known or unknown
	if (muknow) {
		for (k in 1:N) cs = c(cs, sum((rnorm(ns, mu, sig)-mu)^2)/sig^2)
	} else {	for (k in 1:N) {
			sam = rnorm(ns, mu, sig)
			xb = mean(sam)
			cs = c(cs, sum((sam-xb)^2)/sig^2)
		}
	}
    # Degree of freedom
	nu0 = ifelse(muknow, ns, ns-1)
	nu1 = ifelse(muknow, ns - 1, ns)
    # Define the chi-square PDF
	svd0 = function(x) dchisq(x, nu0)
	svd1 = function(x) dchisq(x, nu1)
    # Expected value & standard deviation
	Ec = round(mean(cs), dig)
	Dc = round(sd(cs), dig)
	Dc0 = round(sqrt(2*nu0), dig)
	Dc1 = round(sqrt(2*nu1), dig)
    # Compare cumulative probabilities F(1), F(2), ...
	cp = seq(0, ceiling(max(cs)), by=5)
	Theory = pchisq(cp, nu0)
	Error = pchisq(cp, nu1)
	Simula = sapply(cp, function(x) sum(cs < x))/N
	cdf = rbind(Simula, Theory, Error)
	colnames(cdf)=paste0("F(", cp, ")")
	print(round(cdf, dig))
    # Display graph
	x1 = 0
	x2 = ceiling(max(cs))
	win.graph(7, 5)
	mt = ifelse(muknow, "Distribution of Standardized Sum of Squares", 
		"Dist. of Sum of Squares with Unknown Mean")
	hist(cs, breaks=ng, prob=T, col=7, xlim=c(x1, x2), ylab="f(x)", xlab="x",
		main=paste0(mt, " (n=", ns, ", N=", N, ")"))
	curve(svd0, x1, x2, lwd=2, col=2, add=T)
	curve(svd1, x1, x2, lwd=2, col=4, add=T)
	legend("right", c("Para.   Exact    Error    Simul.", 
			paste("E(X)      ", nu0, "      ", nu1, "     ", Ec), 
			paste("D(X)", Dc0, " ", Dc1, " ", Dc)),
		text.col=c(1,4,4) )
	leg=list()
	leg[[1]] = bquote(chi^2 ~( .(nu0) ))
	leg[[2]] = bquote(chi^2 ~( .(nu1) ))
	legend("topright", sapply(leg, as.expression),
		lwd=c(2,2), col=c(2,4))
}

# [9-5] Simulation for the F-distribution
#' @title Simulation for the F-distribution
#' @description Simulation for the F-distribution
#' @param sig1 Standard deviation of the first population
#' @param sig2 Standard deviation of the second population
#' @param n1 Sample size of the first population
#' @param n2 Sample size of the second population
#' @param N Number of iterations, Default: 10000
#' @param ng Number of classes in histogram, Default: 300
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param xp Specific x-values for cumulative probability F(x), Default: 1:9
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' 
#' @examples 
#' fdist.sim2(sig1=2, sig2=7, n1=8, n2=6)
#' @rdname fdist.sim2
#' @export
fdist.sim2=function(sig1, sig2, n1, n2, N=10000, ng=300, seed=9857, xp=1:9, dig=4) {
    # Generate N statistics from two independent normal population
	set.seed(seed)
	fs = NULL
	vratio = function(n1, n2, s1, s2) {
		var(rnorm(n1, sd=s1))/s1^2/(var(rnorm(n2, sd=s2))/s2^2)}
	for (k in 1:N) fs = c(fs, vratio(n1, n2, sig1, sig2))
    # Define F-PDF
	fd0 = function(x) df(x, n1-1, n2-1)
	fd1 = function(x) df(x, n1, n2)
	xmax = max(xp, qf(0.99, n1-1, n2-1))
	xmod = ifelse(n1>3, (n1-3)/(n1-1)*(n2-1)/(n2+1), 0)
	ymax = ifelse(n1>3, max(fd0(xmod), fd1(xmod)), 1)
    # Expected value & standard deviation
	Ex = mean(fs)
	Dx = sd(fs)
	Ex0 = ifelse(n2>3, (n2-1)/(n2-3), Inf)
	Dx0 = ifelse(n2>5, sqrt(2*(n2-1)^2*(n1+n2-4)/(n1-1)/(n2-3)^2/(n2-5)), 
		ifelse(n2>3, Inf, NA) )
	Ex1 = ifelse(n2>2, n2/(n2-2), Inf)
	Dx1 = ifelse(n2>4, sqrt(2*n2^2*(n1+n2-2)/n1/(n2-2)^2/(n2-4)), 
		ifelse(n2>2, Inf, NA) )
    # Compare cumulative probabilities F(1), F(2), ...
	Theory = pf(xp, n1-1, n2-1)
	Error = pf(xp, n1, n2)
	Simula = sapply(xp, function(x) sum(fs < x))/N
	cdf = rbind(Simula, Theory, Error)
	colnames(cdf)=paste0("F(", xp, ")")
	print(round(cdf, dig))
    # Display graph
	win.graph(7, 5)
	hist(fs, breaks=ng, prob=T, xlim=c(0, xmax), ylim=c(0, ymax), col=7, 
	main=bquote("("~S[1]^2~"/"~sigma[1]^2~ ")/(" ~S[2]^2~"/"~sigma[2]^2~") ~"~ F( .(n1-1) , .(n2-1) ) ), 
		ylab="f(x)", xlab="x")
	curve(fd0, 0, xmax, lwd=2, col=2, add=T)
	curve(fd1, 0, xmax, lwd=2, col=4, add=T)
    # Display legends
	legend("right", c("Para.  Exact   Error  Simul.", 
			paste("E(X)", paste(format(c(Ex0, Ex1, Ex), digits=dig), collapse="  ")), 
			paste("D(X)", paste(format(c(Dx0, Dx1, Dx), digits=dig), collapse="  ")) ), 
		text.col=c(1,4,4) )
	leg=list()
	leg[[1]] = bquote(F( .(n1-1) , .(n2-1) ))
	leg[[2]] = bquote(F( .(n1) , .(n2) ))
	legend("topright", sapply(leg, as.expression),
		lwd=c(2,2), col=c(2,4))
}

# [9-6] Diagnose the Central Limit Theorem
# Generate random variables and standardized statistics
genstat = function(dist, para, para2, ns, N, seed, sigknow) {
    # Probability distribution names and serial numbers
	dlist=c("exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f", "pois", "binom")
	dnum = grep(dist, dlist)
	dnum = dnum[length(dnum)]
	dist = dlist[dnum]
    # Function name for generating random variables
	rdist = paste0("r", dist)
    # Expected value and standard deviation
	Ex = switch(dnum, 1/para, para*para2, para2*gamma(1+1/para), para/(para+para2),
		para, 0, para, ifelse(para2>2, para2/(para2-2), Inf),
		para, ns*para )
	Vx = switch(dnum, 1/para^2, para*para2^2, para2^2*(gamma(1+2/para)-gamma(1+1/para)^2), 
		para*para2/(para+para2)^2/(para+para2+1), para2^2, 
		ifelse(para>2, para/(para-2), ifelse(para>1, Inf, NA)), 2*para, 
		ifelse(para2>4, 2*para2^2*(para+para2-2)/para/(para2-2)^2/(para2-4), ifelse(para2>2, Inf, NA)),
		para, ns*para*(1-para) )
	Dx = sqrt(Vx)
    # Generate random variables standardized statistics
	sgr = rep(1:N, each=ns)
	set.seed(seed)
	if (dist %in% c("exp", "t", "chisq", "pois")) { 
		dat = do.call(rdist, list(N*ns, para))
	} else if (dist == "gamma") { 
		dat = do.call(rdist, list(N*ns, para, 1/para2))
	} else if (dist == "binom") { 
		dat = do.call(rdist, list(N, ns, para))
	} else { 	
		dat = do.call(rdist, list(N*ns, para, para2))
	}
    # Two cases for sigma known or unknown
	if (sigknow) {
		if (dist == "binom") { 
			stat = (dat-Ex)/Dx
		} else {	stat = tapply(dat, sgr, function(x) (mean(x)-Ex)/Dx*sqrt(ns))
		}
	} else	{
		if (dist == "binom") { 
			stat = (dat-Ex)/sqrt(dat*(1-dat/ns))
		} else {	xmean = tapply(dat, sgr, mean)
			xstd =  tapply(dat, sgr, sd)
		    # Remove cases for standard deviation = 0
			stat = ((xmean-Ex)/xstd*sqrt(ns))[xstd>0]
		}
	}
    # Return generated statistics
	invisible(stat)
}

# Histogram of the generated statistics
testplot = function(z, d, mt, n) {
	m = length(n)
	win.graph(3*m, 6)
	par(mfrow=c(2,m))
	for (j in 1:m) { 
    # Set histogram range: centering 0, width d[j], covering all values
	br = c(rev(seq(-d[j]/2, min(z[[j]])-d[j], by=-d[j])), seq(d[j]/2, max(z[[j]])+d[j], by=d[j]))
	dum = hist(z[[j]], breaks=br, plot=FALSE)
	ymax=max(0.4, dum$dens)
    	hist(z[[j]], breaks=br, prob=T, xlim=c(-4,4), ylim=c(0, ymax), col=7, ylab="f(x)",
		main=paste0(mt, "  n=", n[[j]]), xlab=NULL)
    	curve(dnorm, -4, 4, lwd=2, col=2, add=T) }
    # Normal probability plot with selected 100 points, using qqnorm( ) function
  	set.seed(47)
  	for (j in 1:3) { zss = sort(z[[j]])[(0:99)*100+50]
		temp = qqnorm(zss, pch=19, cex=0.8)
		abline(lm(temp$y ~ temp$x), lwd=2, col=2) }
}

# Verify the central limit theorem
#' @title Diagnose the Central Limit Theorem
#' @description Diagnose the Central Limit Theorem
#' @param dist Name of population distribution ("exp","gamma","weibull","beta","norm", "t","chisq","f","pois","binom")
#' @param para Parameter for the first population
#' @param para2 Parameter for the second population (if necessary)
#' @param ns Sample size, Default: c(10, 30, 50)
#' @param d Group width in histogram, Default: rep(0.5, 3)
#' @param N Number of iterations, Default: 10000
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param sigknow Logical value for known population variance, Default: TRUE
#' @return 
#' 
#' @examples 
#' clt.plot("exp", para=5, d=rep(0.4, 3))
#' clt.plot("bin", para=0.1, ns=nv, d=c(1, 0.6, 0.5))
#' @rdname clt.plot
#' @export
clt.plot = function(dist, para, para2, ns=c(10,30,50), d=rep(0.5, 3), N=10000, seed=9857, sigknow=TRUE) {
    # Probability distribution name and graph title
	dlist=c("exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f", "pois", "binom")
	dname=c("Exp", "Gam", "Wei", "Beta", "Norm", "T", "Chisq", "F", "Poi", "Binom")
	if (missing(dist)) {
		cat(paste(dlist, collapse=", "), "\n")
		stop("Input one of the distribution above....")
	}
	dnum = grep(dist, dlist)
	dnum = dnum[length(dnum)]
	mt = ifelse(dnum %in% c(1,6,7,9,10), paste0(dname[dnum], "(", para, ")"),
		paste0(dname[dnum], "(", para, ",", para2, ")")  )
	if (dnum==10) mt = paste0(dname[dnum], "(n,", para, ")")
    # Calculate standardized statistics
	m = length(ns)
	zs = list()
	for (k in 1:m) zs[[k]] = genstat(dist, para, para2, ns[k], N, seed, sigknow)

    # Display graph
	testplot(zs, d, mt, n=ns)
}