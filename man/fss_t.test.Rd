\name{fss_t.test}
\alias{fss_t.test}
\title{T-test via sample statistics}
\usage{
fss_t.test(mean1, mean2, s1, s2, n1, n2, var.equal = FALSE,
  alternative = "two.sided", conf.level = 0.95, simulate = FALSE)
}
\arguments{
  \item{mean1}{Numeric.  The sample mean for the first
  group.}

  \item{mean2}{Numeric.  The sample mean for the second
  group.}

  \item{s1}{Numeric. The standard deviation for the first
  group.}

  \item{s2}{Numeric. The standard deviation for the second
  group.}

  \item{n1}{Numeric. The sample size for the first group.}

  \item{n2}{Numeric. The sample size for the second group.}

  \item{var.equal}{Logical. If true do an equal-variance
  t-test.  If false use Welch–Satterthwaite to do an
  unequal variance t-test.}

  \item{alternative}{Character. Either "two.sided",
  "greater", or "less" specifying the direction specified
  in the alternative hypothesis of the t-test.}

  \item{conf.level}{Numeric. Value between 0 and 1 denoting
  the confidence level for the test.  This is the same as
  1-alpha.}

  \item{simulate}{Logical. If true simulate data that
  matches the sample statistics and use the build in t-test
  function in R.  If false just use the sample statistics
  directly to do the test.}
}
\description{
Perform a t-test just using sample statistics
}
\examples{
x <- rnorm(20)
y <- rnorm(20, 0, 5)
m1 <- mean(x)
m2 <- mean(y)
s1 <- sd(x)
s2 <- sd(y)
n1 <- length(x)
n2 <- length(y)
o1 <- t.test(x, y, var.equal = TRUE)
o2 <- fss_t.test(m1,m2, s1,s2, 20, 20, var.equal = TRUE)
all.equal(o1, o2)
}

