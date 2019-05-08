SB_Calc_B  <- function(x, y) {
    mx <- mean(x, na.rm = TRUE)
    my <- mean(y, na.rm = TRUE)

    return(100 * (mx - my) / sqrt(0.5 * (mx * (1 - mx) + my * (1 - my))))
}
