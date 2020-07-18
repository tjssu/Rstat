# [Ch-3 Functions] ----------------------------------------------------------------------------------
# [prob function] ----------------------------------------------------------------------------------
#' @title Sample Space of Rolling Dice
#' @description Create Sample Space of Rolling Dice
#' @param times Number of dice
#' @param nsides Number of sides of a die, Default: 6
#' @return Sample space in data frame
#' @examples 
#' rolldie2(2)
#' @export 
rolldie2 <- function (times, nsides = 6) 
{
    temp = list()
    for (i in 1:times) {
        temp[[i]] <- 1:nsides
    }
    res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
    names(res) <- c(paste(rep("X", times), 1:times, sep = ""))
    return(res)
}

#' @title Sample Space of Tossing Coins
#' @description Create Sample Space of Tossing Coins
#' @param times Number of coins
#' @return Sample space in data frame
#' @examples 
#' tosscoin2(4)
#' @export 
tosscoin2 <- function (times) 
{
    temp <- list()
    for (i in 1:times) {
        temp[[i]] <- c("H", "T")
    }
    res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
    names(res) <- c(paste(rep("X", times), 1:times, sep = ""))
    return(res)
}

#' @title Sample Space of Sampling Cards
#' @description Create Sample Space of Sampling Cards
#' @param jokers Include a joker? Default: FALSE
#' @return Sample space in data frame
#' @examples 
#' cards2()
#' @export 
cards2 <- function (jokers = FALSE) 
{
    x <- c(2:10, "J", "Q", "K", "A")
    y <- c("Club", "Diamond", "Heart", "Spade")
    res <- expand.grid(rank = x, suit = y)
    if (jokers) {
        levels(res$rank) <- c(levels(res$rank), "Joker")
        res <- rbind(res, data.frame(rank = c("Joker", "Joker"), 
            suit = c(NA, NA)))
    }
    return(res)
}

# Sampling with replacement, no order
combnWithRepetition <- function(n, k) combn(n+k-1, k) - seq(from=0, len=k)

# Permutation (without replacement, ordered)
permn <- function(x, n) {
    if (n<1) return(vector(class(x)))
    do.call(rbind, lapply(1:length(x), function(i) {
         cbind(x[i], permn(x[-i], n-1))
    })
    )
}

# Urn sampling

#' @title Sample Space of Urn Sampling
#' @description Create Sample Space of Urn Sampling
#' @param x Vector of objects in the urn
#' @param size Number of samples from the urn
#' @param replace Sampling with replacement? Default: FALSE
#' @param ordered Consider the order of samples? Default: FALSE
#' @param probspace Create probablity space? Default: FALSE
#' @param ... Other parameters
#' @return Sample space in data frame
#' @examples 
#' urnsample2(letters[1:5], 3)
#' @export 
urnsample2 <- function (x, size, replace = FALSE, ordered = FALSE, probspace = FALSE, ...)
{
    # x should be a vector of length n
    n = length(x)
    xf = as.factor(x)
    if (replace) {
	if (ordered) {
	    temp <- list()
	    for (i in 1:size) {
        		temp[[i]] <- x
	    }
    	    res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
	} else {
	    dum = combnWithRepetition(n, size)
	    res = as.data.frame(t(matrix(xf[dum], size, ncol(dum))))
	  if (probspace) {
    	    # Merge same elements
    	    dd = apply(res, 1, paste, collapse=":")
    	    # Count frequency
    	    tab = table(dd)
    	    res = data.frame(matrix(unlist(strsplit(names(tab),":")), length(tab), size, byrow=TRUE))
    	    res$freq = as.vector(tab)
	  }
	}
    } else {
	if (ordered) {
	    dum = permn(x, size)
	    res = as.data.frame(matrix(as.factor(dum), nrow(dum), size))
    	    return(res)
	} else {
	    dum = combn(x, size)
	    res = as.data.frame(t(dum))
	  if (probspace) {
    	    # Merge same elements
    	    dd = apply(res, 1, paste, collapse=":")
    	    # Count frequency
    	    tab = table(dd)
    	    res = data.frame(matrix(unlist(strsplit(names(tab),":")), length(tab), size, byrow=TRUE))
    	    res$freq = as.vector(tab)
	  }
	}
    }
    return(res)
}

#' @title Union of Events
#' @description Union of Events in Data frame Format
#' @export
union2 = function(x, y) {
	if (is.vector(x)) res = union(x, y)
	if (is.data.frame(x)) {
		nc = ncol(x)
		x2 = apply(x, 1, paste, collapse=":")
		y2 = apply(y, 1, paste, collapse=":")
		dum = union(x2, y2)
		dd = strsplit(dum, split=":")
		nr = length(dd)
		res = as.data.frame(matrix(unlist(dd), nr, nc, byrow=T))
		names(res) <- c(paste(rep("X", nc), 1:nc, sep = ""))
	}
	return(res)
}

#' @title Intersection of Events
#' @description Intersection of Events in Data frame Format
#' @export
intersect2 <- function(x, y) {
	if (is.vector(x)) res = intersect(x, y)
	if (is.data.frame(x)) {
		nc = ncol(x)
		x2 = apply(x, 1, paste, collapse=":")
		y2 = apply(y, 1, paste, collapse=":")
		dum = intersect(x2, y2)
		if (length(dum)>0) {
		    dd = strsplit(dum, split=":")
		    nr = length(dd)
		    res = as.data.frame(matrix(unlist(dd), nr, nc, byrow=T))
		    names(res) <- c(paste(rep("X", nc), 1:nc, sep = ""))
		} else res=NULL
	}
	return(res)
}

#' @title Set Difference of Events
#' @description Set Difference of Events in Data frame Format
#' @export
setdiff2 <- function(x, y) {
	if (is.vector(x)) res = setdiff(x, y)
	if (is.data.frame(x)) {
		nc = ncol(x)
		x2 = apply(x, 1, paste, collapse=":")
		y2 = apply(y, 1, paste, collapse=":")
		dum = setdiff(x2, y2)
		if (length(dum)>0) {
		    dd = strsplit(dum, split=":")
		    nr = length(dd)
		    res = as.data.frame(matrix(unlist(dd), nr, nc, byrow=T))
		    names(res) <- c(paste(rep("X", nc), 1:nc, sep = ""))
		} else res=NULL
	}
	return(res)
}

# [Ch-3 Function Manual] -----------------------------------------

#' @title Ch3. Probability
#' @description List of Ch3. Functions
#' @param fn Function number to be refered. Default: 0
#' @return None.
#' @examples 
#' ch3.man()
#' @rdname ch3.man
#' @export 
ch3.man = function(fn=0) {
    if (0 %in% fn) {
	cat("[P#] prob package functions replaced ----------------- \n")
	cat("[P1] rolldie2\t\tCreating Sample Space of Rolling Dice\n")
	cat("[P2] tosscoin2\t\tCreating Sample Space of Tossing Coins\n")
	cat("[P3] cards2  \t\tCreating Sample Space of Sampling Cards\n")
	cat("[P4] urnsample2\t\tCreating Sample Space of Urn Sampling\n")
	cat("[P5] union2 \t\tUnion of Events in Data frame Format\n")
	cat("[P6] intersect2\t\tIntersection of Events in Data frame Format\n")
	cat("[P7] setdiff2 \t\tSet Difference of Events in Data frame Format\n\n")

	cat("[#] Function for Chapter 3. Probability ----------------- \n")
	cat("[1] element \t\tDisplaying Elements of an Event\n")
	cat("[2] pprt    \t\tCalculating Probability of an Event\n")
	cat("[3] cprt    \t\tCalculating the Conditional Probability\n")
	cat("[4] indep.event\t\tIndependence of Two Discrete Random Variables\n")
	cat("[5] bayes.plot\t\tDisplaying the Prior and the Posterior Probabilities\n")
    }
    if (1 %in% fn) {
	cat("[1] Displaying Elements of an Event\n")
	cat("element(A, r=10)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("A  \t Data frame with elements of the event for each row\n")
	cat("[Optional Input]--------------------------\n")
	cat("r  \t Maximum number of elements for each Line (default=10)\n")
    }
    if (2 %in% fn) {
	cat("[2] Calculating the Probability of an Event by Counting the Elements\n")
	cat("pprt(x, n, prt=TRUE)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x  \t Data frame with elements of the event for each row\n")
	cat("n  \t Number of elements in the sample space \n")
	cat("[Optional Input]--------------------------\n")
	cat("prt \t Logical value for printing detailed output (default=TRUE)\n")
    }
    if (3 %in% fn) {
	cat("[3] Calculating the Conditional Probability\n")
	cat("cprt(a, b)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("a  \t Data frame of conditioned event \n")
	cat("b  \t Data frame of conditioning event \n")
    }
    if (4 %in% fn) {
	cat("[4] Determining the Independence of Two Discrete Random Variables\n")
	cat("indep.event(X, Y, N, ep=1E-6)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("X  \t First random variable\n")
	cat("Y  \t Second random variable\n")
	cat("N  \t Number of elements in the sample space\n")
	cat("[Optional Input]--------------------------\n")
	cat("ep \t Precision limit for probability calculation (default=1E-6)\n")
    }
    if (5 %in% fn) {
	cat("[5] Displaying the Prior and the Posterior Probabilities\n")
	cat("bayes.plot(prior, post, group, cond, dcol, cex=1, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("prior\t Prior probability distribution vector\n")
	cat("post\t Posterior probability distribution vector\n")
	cat("[Optional Input]--------------------------\n")
	cat("group\t Class name (default=A, B, C, ...)\n")
	cat("cond\t Conditional event name (default=F)\n")
	cat("dcol\t Bar chart colors (default=transparent rainbow colors)\n")
	cat("cex \t Text size of the probability (default=1)\n")
	cat("dig \t Number of digits below the decimal point (default=4)\n")
    }
}

# [3-1] Displaying Elements of an Event

#' @title Elements of an Event
#' @description Displaying Elements of an Event
#' @param A Event in data frame.
#' @param r Maximum number of elements in a line. Default: 10
#' @return Vector of elements of the event.
#' @examples 
#' S = rolldie2(2)
#' B = subset(S2, (X1+X2) >=8)
#' element(B)
#' @rdname element
#' @export
element = function(A, r=10) {
    # A must be a data frame
    # Number of elements in A 
	n = nrow(A)
    # Dimension of each element
	d = ncol(A)
    # Number of lines for print (r elements per line)
	m = ceiling(n/r)
    # Create elements
	elem = "("
	if (d >= 2) for (k in 1:(d-1)) elem = paste0(elem, A[[k]], ",")
	elem = paste0(elem, A[[d]], ")")
    # Print elements
	for (k in 1:m) cat(elem[(r*(k-1)+1):min(r*k,n)], "\n")
	invisible(elem)
} 
	
# [3-2] Calculating the Probability of an Event by Counting the Elements

#' @title Probability of an Event
#' @description Calculating the Probability of an Event
#' @param x Event in data frame.
#' @param n Size of the sample space.
#' @param prt Print output? Default: TRUE
#' @return The probability of an event.
#' @examples 
#' S = rolldie2(2)
#' B = subset(S, (X1+X2) >=8)
#' pprt(B, nrow(S))
#' @rdname pprt
#' @export 
pprt = function(x, n, prt=TRUE) { 
	en = deparse(substitute(x))
	if (prt==TRUE) cat(paste0("P(", en, ") ="), nrow(x), "/", n, "=", nrow(x)/n,"\n")
	invisible(nrow(x)/n)
}

#' @export 
pprt2 = function(x, xn, n, prt=TRUE) {
	if (prt==TRUE) cat(paste0("P(", xn, ") ="), nrow(x), "/", n, 
		"=", nrow(x)/n,"\n")
	invisible(nrow(x)/n)
}

# [3-3] Calculating the Conditional Probability

#' @title Conditional Probability
#' @description Calculating the Conditional Probability
#' @param a Event to be conditioned.
#' @param b Event to be conditioning.
#' @return None.
#' @examples 
#' S = rolldie2(2)
#' A = subset(S, (X1+X2) >=4)
#' B = subset(S, (X1+X2) >=8)
#' cprt(B, A)
#' @rdname cprt
#' @export 
cprt = function(a, b) {
	an = deparse(substitute(a))
	bn = deparse(substitute(b))
	ab = intersect2(a, b)
    	cat(paste0("P(",an,"|",bn,")="), nrow(ab),"/",nrow(b),
		"=", nrow(ab)/nrow(b),"\n")
}

#' @export 
cprt2 = function(a, an, b, bn) {
	ab = intersect2(a, b)
    	cat(paste0("P(",an,"|",bn,") ="), nrow(ab),"/",nrow(b),
		"=", nrow(ab)/nrow(b))
}

# [3-4] Determining the Independence of Two Discrete Random Variables

#' @title Independence of Random Variables
#' @description Determining Independence of Two Discrete Random Variables
#' @param X First random variable.
#' @param Y Second random variable.
#' @param N Size of the sample space.
#' @param ep Precision limit. Default: 1e-06
#' @return Probablities to be compared.
#' @examples 
#' indep.event(A, B, nrow(S))
#' @rdname indep.event
#' @export
indep.event = function(X, Y, N, ep=1E-6) {
	Xn = deparse(substitute(X))
	Yn = deparse(substitute(Y))
    # Probability of X and Y
	px = pprt2(X, Xn, N)
	py = pprt2(Y, Yn, N)
	cat(paste0("P(", Xn, ") \U00D7 P(", Yn, ") ="), px*py, "\n")
    # Probability of the intersection of two events
	XY = intersect2(X, Y)
	XYn = paste0(Xn, Yn)
	pxy = pprt2(XY, XYn, N)
    # Display the result of determining independence 
	err = abs(pxy - px*py)
	if (err < ep) {cat(paste0("P(", XYn, ") = P(", Xn, ") \U00D7 P(", Yn, ") \U21D2 Independent"), "\n")
		cprt2(X, Xn, Y, Yn); cat(" = "); pprt2(X, Xn, N)
		cprt2(Y, Yn, X, Xn); cat(" = "); pprt2(Y, Yn, N)
	} else {cat(paste0("|P(", XYn, ")-P(", Xn, ")xP(", Yn, ")|=", 
		round(err, 4), " --> Not independent"), "\n") 
		cprt2(X, Xn, Y, Yn); cat("<>"); pprt2(X, Xn, N)
		cprt2(Y, Yn, X, Xn); cat("<>"); pprt2(Y, Yn, N)
	}
    # Return the probabilities
	invisible(list(pxpy=px*py, pxy=pxy))
}

# [3-5] Displaying the Prior and the Posterior Probabilities

#' @title Prior and Posterior Probabilities
#' @description Displaying the Prior and the Posterior Probabilities
#' @param prior Prior probability distribution vector.
#' @param post Posterior probability distribution vector.
#' @param group Class names, Default: A, B, C, ...
#' @param cond Conditional event name, Default: F
#' @param dcol Bar chart colors, Default: transparent rainbow colors
#' @param cex Text size of the probability, Default: 1
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples 
#' prior = c(0.2, 0.4, 0.3, 0.1)
#' cond = c(0.04, 0.02, 0.01, 0.05)
#' tot = prior*cond
#' post = tot / sum(tot)
#' bayes.plot(prior, post)
#' @rdname bayes.plot
#' @export 
bayes.plot = function(prior, post, group, cond, dcol, cex=1, dig=4) {
    # Set graphic elements
	n = length(prior)
	if (missing(dcol)) dcol = rainbow(n, alpha=0.3)
	if (missing(group)) group = LETTERS[1:n]
	if (missing(cond)) cond = "F"
    # Display graph
	win.graph(7,4)
	dum = barplot(cbind(prior, post), col=dcol,
		main="Prior Probability vs. Posterior Probability", horiz=T)
    # Calculate central location 
	centprior = cumsum(prior)-prior/2
	centpost = cumsum(post)-post/2
    # Display the probabilities
	text(centprior, dum[1], labels=paste0("P(", group, ")\n", prior), cex=cex)
	text(centpost, dum[2], labels=paste0("P(", group, "|", cond, ")\n", 
		format(post, digits=dig)), cex=cex)
} 