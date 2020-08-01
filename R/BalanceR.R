#' @import dplyr
#' @import forcats
#' @import tidyr
#' @import ggplot2
#' @import stringr
#' @import rlang
#' @import magrittr
#' @importFrom stats var
#' @importFrom utils combn
#' @export BalanceR
#' @export print.BalanceR
#' @export plot.BalanceR
#' @export SB_Calc_B
#' @export SB_Calc_C

.onLoad <- function(...) {
    packageStartupMessage("BalanceR 0.5.0")
    library(magrittr)
}

SB_Calc_C  <- function(x, y) {
    mx <- mean(x, na.rm = TRUE)
    my <- mean(y, na.rm = TRUE)
    vx <- var(x, na.rm = TRUE)
    vy <- var(y, na.rm = TRUE)

    return(100 * (mx - my) / sqrt(0.5 * (vx + vy)))
}

SB_Calc_B  <- function(x, y) {
    mx <- mean(x, na.rm = TRUE)
    my <- mean(y, na.rm = TRUE)

    return(100 * (mx - my) / sqrt(0.5 * (mx * (1 - mx) + my * (1 - my))))
}

BalanceR <- function(data, group, cov) {

    if (length(deparse(substitute(group))) != 1) {
        stop("Length of group argument must be 1.")
    }

    if (sum(class(data) == "tbl_df") != 0) {
        data <- as.data.frame(data)
    }

    if (prod(class(data) == "data.frame") == 0) {
        stop("Only data.frame class is supported.")
    }

    data  <- dplyr::select(data, {{ group }}, {{ cov }})
    group <- names(data)[1]
    cov   <- names(data)[-1]

    Group    <- as.character(sort(unique(data[, 1])))
    GroupV   <- group
    NGroup   <- length(Group)
    GrpComb  <- combn(Group, 2)
    NComb    <- ncol(GrpComb)
    NCov     <- ncol(data) - 1

    CombName <- apply(combn(Group, 2), 2, paste, collapse = "-")

    Result.df <- as.data.frame(matrix(rep(NA,
                                          NCov * (NGroup * 2 + NComb) + NCov) ,
                                      nrow = NCov))

    # Descriptive Statistics
    SumText <- paste0(rep(c("mean(", "sd("), NCov),
                      rep(cov, each = 2),
                      rep(", na.rm = TRUE)", NCov * 2))

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

    if (!is.null(names(cov))) {
        names(cov)[names(cov) == ""] <- cov[names(cov) == ""]
        cov <- names(cov)
    }

    Result.df$Covariate <- cov

    Result.df$Covariate <- fct_rev(fct_inorder(Result.df$Covariate))



    Result <- list(Desc    = Result.df[, 1:(NGroup * 2 + 1)],
                   SB      = Result.df[, c(1, (NGroup * 2 + 2):ncol(Result.df))],
                   Data    = data,
                   Group_N = NGroup,
                   Cov_N   = NCov)

    class(Result) <- append("BalanceR", class(Result))

    return(Result)
}

validate_BalanceR <- function(data) {
    if (!("BalanceR" %in% class(data))) {
        stop("Please make sure whether the data has 'BalanceR' class.")
    }

    if (length(data) != 5) {
        stop("Invalid 'BalanceR' class.")
    }

    if (prod(names(data) == c("Desc", "SB", "Data", "Group_N", "Cov_N")) != 1) {
        stop("Invalid 'BalanceR' class.")
    }

    if (class(data$Desc) != "data.frame") {
        stop("Invalid 'BalanceR' class.")
    }

    if (class(data$SB) != "data.frame") {
        stop("Invalid 'BalanceR' class.")
    }

    if (nrow(data$Desc) != nrow(data$SB)) {
        stop("Invalid 'BalanceR' class.")
    }

    if (!("data.frame" %in% class(data$Data))) {
        stop("Invalid 'BalanceR' class.")
    }

    if (!is.numeric(data$Group_N) | !length(data$Group_N)) {
        stop("Invalid 'BalanceR' class.")
    }

    if (!is.numeric(data$Cov_N) | !length(data$Cov_N)) {
        stop("Invalid 'BalanceR' class.")
    }
}

#' @method print BalanceR
#' @export

print.BalanceR <- function(data, only.SB = FALSE, simplify = FALSE, abs = FALSE,
                           digits = 3) {

    validate_BalanceR(data)

    if (!is.logical(simplify)) {
        stop("An argument 'simplify' must have logical value.")
    }

    if (!is.logical(only.SB)) {
        stop("An argument 'only.SB' must have logical value.")
    }

    if (!is.logical(abs)) {
        stop("An argument 'abs' must have logical value.")
    }

    if (abs) {
        data$SB[, -1] <- abs(data$SB[, -1])
    }

    if (simplify) {
        Max_SB_Pos  <- apply(abs(data$SB[, -1]), 1, which.max)
        Max_SB      <- apply(data$SB[, -1], 1, `[`, Max_SB_Pos)
        data$SB     <- data.frame(Maximum_SB = diag(Max_SB))
    }

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
                          color      = TRUE,
                          simpify    = FALSE,
                          abs        = FALSE) {

    validate_BalanceR(data)

    # prod(unlist(lapply(lapply(lapply(list(a, a), class), `%in%`, "BalanceR"), sum)))

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
