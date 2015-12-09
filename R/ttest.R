#' T-test via sample statistics
#' 
#' Perform a t-test just using sample statistics
#' 
#' @param mean1 Numeric.  The sample mean for the first group.
#' @param mean2 Numeric.  The sample mean for the second group.
#' @param s1 Numeric. The standard deviation for the first group.
#' @param s2 Numeric. The standard deviation for the second group.
#' @param n1 Numeric. The sample size for the first group.
#' @param n2 Numeric. The sample size for the second group.
#' @param var.equal Logical. If true do an equal-variance t-test.
#'  If false use Welch–Satterthwaite to do an unequal variance t-test.
#' @param alternative Character. Either "two.sided", "greater", or "less" specifying
#'  the direction specified in the alternative hypothesis of the t-test.
#' @param conf.level Numeric. Value between 0 and 1 denoting the confidence
#'  level for the test.  This is the same as 1-alpha.
#' @param simulate Logical. If true simulate data that matches the 
#'  sample statistics and use the build in t-test function in R.  
#'  If false just use the sample statistics directly to do the test.
#' @param data.names Character vector. A two element character vector
#'  that gives the names for the two groups.
#' @export
#' @examples
#' x <- rnorm(20)
#' y <- rnorm(20, 0, 5)
#' m1 <- mean(x)
#' m2 <- mean(y)
#' s1 <- sd(x)
#' s2 <- sd(y)
#' n1 <- length(x)
#' n2 <- length(y)
#' o1 <- t.test(x, y, var.equal = TRUE)
#' o2 <- fss_t.test(m1,m2, s1,s2, 20, 20, var.equal = TRUE)
#' all.equal(o1, o2)
fss_t.test <- function(mean1, mean2, s1, s2, n1, n2, 
                    var.equal = FALSE, alternative = "two.sided", 
                    conf.level = .95, simulate = FALSE,
                    data.names = c("x", "y"))
{
    
    if(simulate){
        # Generate normally distributed data with the
        # specified mean and standard deviation
        # for each group
        x <- scale(rnorm(n1))*s1 + mean1
        y <- scale(rnorm(n2))*s2 + mean2
        
        out <- t.test(x, y, 
                      var.equal = var.equal, 
                      alternative = alternative,
                      conf.level = conf.level)
        return(out)
    }
    
    ## Otherwise we'll compute everything ourselves...
    
    if(var.equal){
        se <- sqrt((s1^2*(n1-1) + s2^2*(n2-1))/(n1+n2-2)) * sqrt(1/n1 + 1/n2)
        df <- n1 + n2 - 2
    }else{
        se <- sqrt(s1^2/n1 + s2^2/n2)
        # The Welch–Satterthwaite equation gives us
        # the appropriate df for this variance estimate
        num <- (s1^2/n1 + s2^2/n2)^2
        den <- (s1^4/(n1^2*(n1-1)) + s2^4/(n2^2*(n2-1)))
        df <- num/den
    }
    
    tstat <- (mean1 - mean2)/se
    
    ## TODO: test what happens if given not one of these
    p <- switch(alternative,
                "two.sided" = 2 * pt(-abs(tstat), df),
                "greater"   = 1 - pt(tstat, df),
                "less"      = pt(tstat, df),
                NA
    )
    
    quantile <- 1 - (1 - conf.level)/2
    confint <- (mean1 - mean2) + c(-1, 1) * qt(quantile, df) * se

    data.name <- paste(data.names[1], data.names[2], sep = " and ")

    # Match the output from t.test
    out <- list(statistic = c(t = tstat),
                parameter = c(df = df),
                p.value = p,
                conf.int = structure(confint, conf.level = conf.level),
                estimate = c("mean of x" = mean1, "mean of y" = mean2),
                null.value = c("difference in means" = 0),
                alternative = alternative,
                method = ifelse(var.equal, 
                                " Two Sample t-test", 
                                "Welch Two Sample t-test"),
                data.name = data.name)
    
    class(out) <- "htest"
    
    return(out)
}

