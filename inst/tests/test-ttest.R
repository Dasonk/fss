context("Checking fss_t.test")

test_that("fss_test - equal variance, no simulate",{

    
    x <- rnorm(20)
    y <- rnorm(20, 0, 5)
    m1 <- mean(x)
    m2 <- mean(y)
    s1 <- sd(x)
    s2 <- sd(y)
    n1 <- length(x)
    n2 <- length(y)
    o.R <- t.test(x, y, var.equal = T)
    o.fss <- fss_t.test(m1,m2, s1,s2, 20, 20, var.equal = T)

    expect_equal(o.fss, o.R)

})

test_that("fss_test - equal variance, simulate",{
    
    
    x <- rnorm(20)
    y <- rnorm(20, 0, 5)
    m1 <- mean(x)
    m2 <- mean(y)
    s1 <- sd(x)
    s2 <- sd(y)
    n1 <- length(x)
    n2 <- length(y)
    o.R <- t.test(x, y, var.equal = T)
    o.fss <- fss_t.test(m1,m2, s1,s2, 20, 20, var.equal = T, simulate = TRUE)
    
    expect_equal(o.fss, o.R)
    
})

test_that("fss_test - unequal variance, no simulate",{
    
    
    x <- rnorm(20)
    y <- rnorm(20, 0, 5)
    m1 <- mean(x)
    m2 <- mean(y)
    s1 <- sd(x)
    s2 <- sd(y)
    n1 <- length(x)
    n2 <- length(y)
    o.R <- t.test(x, y, var.equal = F)
    o.fss <- fss_t.test(m1,m2, s1,s2, 20, 20, var.equal = F)
    
    expect_equal(o.fss, o.R)
    
})

test_that("fss_test - unequal variance, simulate",{
    
    
    x <- rnorm(20)
    y <- rnorm(20, 0, 5)
    m1 <- mean(x)
    m2 <- mean(y)
    s1 <- sd(x)
    s2 <- sd(y)
    n1 <- length(x)
    n2 <- length(y)
    o.R <- t.test(x, y, var.equal = F)
    o.fss <- fss_t.test(m1,m2, s1,s2, 20, 20, var.equal = F, simulate = TRUE)
    
    expect_equal(o.fss, o.R)
    
})


