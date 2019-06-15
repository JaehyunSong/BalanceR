#' @import dplyr
#' @import forcats
#' @import tidyr
#' @import ggplot2
#' @import rlang
#' @importFrom stats var
#' @importFrom utils combn
#' @export BalanceR
#' @export print.BalanceR
#' @export plot.BalanceR
#' @export SB_Calc_B
#' @export SB_Calc_C

library(dplyr)
library(ggplot2)

SB_Calc_C  <- function(x, y) {
    mx <- mean(x, na.rm = TRUE)
    my <- mean(y, na.rm = TRUE)
    vx <- var(x, na.rm = TRUE)
    vy <- var(x, na.rm = TRUE)

    return(100 * (mx - my) / sqrt(0.5 * (vx + vy)))
}

SB_Calc_B  <- function(x, y) {
    mx <- mean(x, na.rm = TRUE)
    my <- mean(y, na.rm = TRUE)

    return(100 * (mx - my) / sqrt(0.5 * (mx * (1 - mx) + my * (1 - my))))
}

BalanceR <- function(data, group, cov) {

    if ("tbl_df" %in% class(data) == TRUE) {
        data <- as.data.frame(data)
    }

    if (prod(class(data) == "data.frame") == 0) {
        stop("Only data.frame class is supported.")
    }

    if (sum(cov %in% names(data)) != length(cov)) {

        notList_i <- which(cov %in% names(data))
        notList   <- cov[-notList_i]

        warning(paste0("Some covariates are not in the data: ",
                       paste0(notList, collapse = ", ")),
                "\nCovariates not listed in the data are excluded.")

        cov <- cov[notList_i]
    }
    Group    <- as.character(sort(unique(data[, group])))
    GroupV   <- group
    NGroup   <- length(Group)
    GrpComb  <- combn(Group, 2)
    NComb    <- ncol(GrpComb)
    NCov     <- length(cov)

    CombName <- apply(combn(Group, 2), 2, paste, collapse = "-")

    Result.df <- as.data.frame(matrix(rep(NA,
                                          NCov * (NGroup * 2 + NComb) + NCov) ,
                                      nrow = NCov))

    # Descriptive Statistics
    SumText <- paste0(rep(c("mean(", "sd("), NCov),
                      rep(cov, each = 2),
                      rep(")", NCov * 2))

    DS <- data %>%
        group_by(Group = eval(parse(text = GroupV))) %>%
        summarise(!!!parse_exprs(SumText)) %>%
        select(-Group) %>%
        as.data.frame()

    DSm <- DS %>%
        select(starts_with("mean")) %>%
        t() %>%
        as.data.frame()
    names(DSm) <- paste0("V", seq(1, NGroup * 2, 2))

    DSs <- DS %>%
        select(starts_with("sd")) %>%
        t() %>%
        as.data.frame()
    names(DSs) <- paste0("V", seq(2, NGroup * 2, 2))

    DS <- cbind(DSm, DSs)

    DS <- DS %>%
        select(!!!parse_exprs(sort(names(DS))))

    Result.df[1:NCov, 2:(NGroup * 2 + 1)] <- DS

    # Standardized Bias
    tmpSB <- as.data.frame(matrix(rep(0, NCov * NComb),
                                  nrow = NCov))

    for (i in 1:NCov) {
        tmpCov <- cov[i]
        for (j in 1:NComb) {
            tmpG1  <- GrpComb[1, j]
            tmpG2  <- GrpComb[2, j]

            G1Text <- paste0("data %>% filter(",
                             group, " == '", tmpG1, "') %>% ",
                             ".$", tmpCov)
            G2Text <- paste0("data %>% filter(",
                             group, " == '", tmpG2, "') %>% ",
                             ".$", tmpCov)

            G1 <- eval(parse(text = G1Text))
            G2 <- eval(parse(text = G2Text))

            if (length(unique(c(G1, G2))) == 2 &
                 (0 %in% c(1, 0)) == TRUE &
                 (1 %in% c(1, 0)) == TRUE) {
                tmpSB[i, j] <- SB_Calc_B(G1, G2)
            }else{
                tmpSB[i, j] <- SB_Calc_C(G1, G2)
            }

        }
    }

    Result.df[1:NCov, (NGroup * 2 + 2):ncol(Result.df)] <- tmpSB

    names(Result.df) <- c("Covariate",
                          paste0(rep(c("Mean:", "SD:"), NGroup),
                                 rep(Group, each = 2)),
                          paste0("SB:", CombName))

    Result.df$Covariate <- cov
    Result.df$Covariate <- fct_rev(fct_inorder(Result.df$Covariate))



    Result <- list(Desc = Result.df[, 1:(NGroup * 2 + 1)],
                   SB   = Result.df[, c(1, (NGroup * 2 + 2):ncol(Result.df))])

    class(Result) <- append("BalanceR", class(Result))

    return(Result)
}

#' @method print BalanceR
#' @export

print.BalanceR <- function(data, only.SB = FALSE, digits = 3) {

    if (only.SB == FALSE) {
        x <- data$Desc
        y <- data$SB
        y$Covariate <- NULL

        x <- cbind(x, y)
    }else{
        x <- data$SB
    }

    x[1:nrow(x), 2:ncol(x)] <- format(round(x[1:nrow(x), 2:ncol(x)], digits),
                                      nsmall = digits)

    print(x)
}

#' @method plot BalanceR
#' @export

plot.BalanceR <- function(data,
                          point.size = 2.5,
                          text.size  = 12,
                          vline      = c(3, 5, 10),
                          color      = TRUE) {
    x <- data$SB
    x <- x %>%
        select(Covariate, starts_with("SB")) %>%
        gather(key = Pair, value = SB, -Covariate) %>%
        separate(Pair, into = c("X", "Pair"), sep = ":") %>%
        select(-X)

    plot_x <- x %>%
        ggplot(aes(x = SB, y = Covariate)) +
        geom_vline(xintercept = c(-vline, vline),
                   linetype = 2) +
        geom_vline(xintercept = 0)

    if (color == TRUE) {
        plot_x <- plot_x +
            geom_point(aes(color = Pair), size = point.size) +
            labs(color = "")
    }else{
        plot_x <- plot_x +
            geom_point(aes(shape = Pair), size = point.size) +
            labs(shape = "")
    }

    plot_x <- plot_x +
        scale_x_continuous(breaks = c(-vline, 0, vline),
                           labels = c(-vline, 0, vline)) +
        labs(x = "Standardized Bias") +
        theme_bw() +
        theme(legend.position = "bottom",
              text = element_text(size = text.size))

    return(plot_x)
}
