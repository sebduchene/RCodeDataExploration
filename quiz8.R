#x

set.seed(3343)
pValues <- rep(NA, 100)
for(i in 1:100){
    z <- rnorm(20)
    x <- rnorm(20)
    y <- rnorm(20, mean = 0.5*x)
    pValues[i] <- summary(lm(y ~ x))$coef[2, 4]
}

alpha <- 0.1

sum(p.adjust(pValues, method = "bonferroni") < 0.1)

sum(pValues < 0.1)

sum(p.adjust(pValues, method = "BH") < 0.1q)


# Q4

# y = 1 + 2*x + e

x <- rnorm(100)
e <- rnorm(100)

y <- 1 + 2*x + e

lmy <- lm(y ~x)

# CASE 1


fun.na <- function(x){
    if(x > 0){
        m <- sample(c(NA,1), 1, prob = c(0.8, 0.2))
    }else{
        m <- 1
    }
    return(m)
}

lm1 <- vector()
for(i in 1:100){
    x.1 <- vector()
    for(j in 1:length(x)){
        x.1[j] <- x[j]*fun.na(x[j])
    }
    lm1[i] <- lm(y ~ x.1)$coef[2]
}

fun.na.2 <- function(x){
    if(x > mean(y)){
        m <- sample(c(NA,1), 1, prob = c(0.8, 0.2))
    }else{
        m <- 1
    }
    return(m)
}

lm2 <- vector()
for(i in 1:100){
    y.2 <- vector()
    for(j in 1:length(y)){
        y.2[j] <- y[j]*fun.na.2(y[j])
    }
    lm2[i] <- lm(y.2 ~ x)$coef[2]
}


library(MASS)

rlm1 <- vector()
for(i in 1:100){
    x.1 <- vector()
    for(j in 1:length(x)){
        x.1[j] <- x[j]*fun.na(x[j])
    }
    rlm1[i] <- rlm(y ~ x.1, na.action = na.omit)$coef[2]
}

rlm2 <- vector()
for(i in 1:100){
    y.2 <- vector()
    for(j in 1:length(y)){
        y.2[j] <- y[j]*fun.na.2(y[j])
    }
    rlm2[i] <- rlm(y.2 ~ x, na.action = na.omit)$coef[2]
}

summary(rlm1)

summary(rlm2)


