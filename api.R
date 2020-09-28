k <- 0.1 # weighting factor, must be less than unity
d <- 3 # days
n <- length(temp) # enter daily precip here, may need to use nrow
API <- array(0, dim = c(n))
for (i in 1:(d-1)) {
    API[i] <- temp[i] # this fills the days prior to the start with the current precip, not an API
}
for (i in d:n) {
    for (j in 1:d) {
        t <- j-1
        API[i] <- API[i]+temp[i-t]*(k^t)
    }
}

