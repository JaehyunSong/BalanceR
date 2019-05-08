SB_Calc_C  <- function(x, y) {
    mx <- mean(x, na.rm = TRUE)
    my <- mean(y, na.rm = TRUE)
    vx <- var(x, na.rm = TRUE)
    vy <- var(x, na.rm = TRUE)

    return(100 * (mx - my) / sqrt(0.5 * (vx + vy)))
}
