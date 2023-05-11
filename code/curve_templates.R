get_template <- function(param, type) {

    if (type == 1) {

        a <- param[1]   #final size
        d <- param[4]   #number of time steps

        out <- a * rep(1 / d, length.out = d)

    }


    if (type == 2) {

        a <- param[1]   #final size
        b <- param[2]   #peak time
        c <- param[3]   #peak width
        d <- param[4]   #number of time steps

        t <- 1:d

        out <- a / sqrt(2 * pi * c) * exp(-(t - b)^2 / (2 * c))

    }

    return(out)

}
