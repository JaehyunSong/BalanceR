#' @import dplyr
#' @import forcats
#' @import tidyr
#' @import ggplot2
#' @import stringr
#' @import rlang
#' @import tidyselect
#' @importFrom magrittr `%>%`
#' @importFrom stats var
#' @importFrom utils combn
#' @export BalanceR
#' @export print.BalanceR
#' @export plot.BalanceR
#' @export summary.BalanceR
#' @export validate_BalanceR
#' @export SB_Calc_B
#' @export SB_Calc_C

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

    if (inherits(data, "tbl_df")) {
        data <- as.data.frame(data)
    }

    if (!inherits(data, "data.frame")) {
        stop("Only data.frame or tibble class is supported.")
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
        summarise(!!!parse_exprs(SumText),
                  .groups = "drop") %>%
        select(-Group) %>%
        as.data.frame()

    DSm <- DS %>%
        select(starts_with("mean")) %>%
        t() %>%
        as.data.frame()
    names(DSm) <- paste0("V", sprintf("%04d", seq(1, NGroup * 2, 2)))

    DSs <- DS %>%
        select(starts_with("sd")) %>%
        t() %>%
        as.data.frame()
    names(DSs) <- paste0("V", sprintf("%04d", seq(2, NGroup * 2, 2)))

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
                             "pull(", tmpCov, ")")
            G2Text <- paste0("data %>% filter(",
                             group, " == '", tmpG2, "') %>% ",
                             "pull(", tmpCov, ")")

            G1 <- eval(parse(text = G1Text))
            G2 <- eval(parse(text = G2Text))

            if (length(unique(data[, tmpCov])) == 2) {
                if(min(unique(data[, tmpCov])) != 0 |
                   max(unique(data[, tmpCov])) != 1) {
                    G1 <- ifelse(G1 == min(G1), 0, 1)
                    G2 <- ifelse(G2 == min(G2), 0, 1)
                }
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

validate_BalanceR <- function(x) {
    if (!("BalanceR" %in% class(x))) {
        stop("Please make sure whether the data has 'BalanceR' class.")
    }

    if (length(x) != 5) {
        stop("Invalid 'BalanceR' class.")
    }

    if (prod(names(x) == c("Desc", "SB", "Data", "Group_N", "Cov_N")) != 1) {
        stop("Invalid 'BalanceR' class.")
    }

    if (!inherits(x$Desc, "data.frame")) {
        stop("Invalid 'BalanceR' class.")
    }

    if (!inherits(x$SB, "data.frame")) {
        stop("Invalid 'BalanceR' class.")
    }

    if (nrow(x$Desc) != nrow(x$SB)) {
        stop("Invalid 'BalanceR' class.")
    }

    if (!inherits(x$Data, "data.frame")) {
        stop("Invalid 'BalanceR' class.")
    }

    if (!is.numeric(x$Group_N) | !length(x$Group_N)) {
        stop("Invalid 'BalanceR' class.")
    }

    if (!is.numeric(x$Cov_N) | !length(x$Cov_N)) {
        stop("Invalid 'BalanceR' class.")
    }
}

#' @method print BalanceR
#' @export

print.BalanceR <- function(x,
                           only.SB  = FALSE,
                           simplify = FALSE,
                           abs      = FALSE,
                           digits   = 3,
                           ...) {

    validate_BalanceR(x)

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
        x$SB[, -1] <- abs(x$SB[, -1])
    }

    if (simplify & dim(x$SB)[2] > 2) {
        Max_SB_Pos  <- apply(abs(x$SB[, -1]), 1, which.max)
        Max_SB      <- apply(x$SB[, -1], 1, `[`, Max_SB_Pos)

        x$SB        <- data.frame(Covariate  = x$SB$Covariate,
                                  Maximum_SB = diag(Max_SB))
    }

    if (only.SB == FALSE) {
        x$SB$Covariate <- NULL

        x <- cbind(x$Desc, x$SB)
    }else {
        x <- x$SB
    }

    x[1:nrow(x), 2:ncol(x)] <- format(round(x[1:nrow(x), 2:ncol(x)], digits),
                                      nsmall = digits)

    print(x)
}

#' @method plot BalanceR
#' @export

plot.BalanceR <- function(x,
                          point.size = 2.5,
                          text.size  = 12,
                          vline      = c(10, 25),
                          color      = TRUE,
                          simplify   = FALSE,
                          abs        = FALSE,
                          ...) {

    if (!is.logical(simplify) | length(simplify) != 1) {
        stop("An argument 'simplify' must have logical value.")
    }

    if (!is.logical(abs) | length(abs) != 1) {
        stop("An argument 'abs' must have logical value.")
    }

    if (!is.logical(color) | length(color) != 1) {
        stop("An argument 'color' must have logical value.")
    }

    if (!is.numeric(point.size) | length(point.size) != 1) {
        stop("An argument 'point.size' must have logical value with length of 1.")
    }

    if (!is.numeric(text.size) | length(text.size) != 1) {
        stop("An argument 'text.size' must have logical value with length of 1.")
    }

    if (!is.numeric(vline)) {
        stop("An argument 'vline' must have numeric value.")
    }

    validate_BalanceR(x)

    if (abs == TRUE) {
        x$SB[, -1] <- abs(x$SB[, -1])

        breaklines <- c(0, vline)
        xlab       <- "Absolute Values of "
    } else if (abs == FALSE) {
        breaklines <- c(-vline, 0, vline)
        xlab       <- ""
    }

    if (simplify == TRUE) {
        if (dim(x$SB)[2] > 2) {
            Max_SB_Pos  <- apply(abs(x$SB[, -1]), 1, which.max)
            Max_SB      <- apply(x$SB[, -1], 1, `[`, Max_SB_Pos)
            x$SB        <- data.frame(Covariate  = x$SB$Covariate,
                                      Pair       = "Maximum SB",
                                      SB         = diag(Max_SB))
        }

        x    <- x$SB
        xlab <- paste0(xlab, "Maximum Standardized Biases")

    } else if (simplify == FALSE) {
        x <- x$SB

        x <- x %>%
            dplyr::select("Covariate", starts_with("SB")) %>%
            tidyr::pivot_longer(cols = starts_with("SB"),
                                names_to = "Pair", values_to = "SB") %>%
            tidyr::separate("Pair", into = c("X", "Pair"), sep = ":") %>%
            dplyr::select(-"X")

        xlab <- paste0(xlab, "Standardized Biases")
    }

    plot_x <- ggplot(data = x, aes(x = .data$SB, y = .data$Covariate)) +
        geom_vline(xintercept = breaklines,
                   linetype = 2) +
        geom_vline(xintercept = 0)

    if (color == TRUE) {
        plot_x <- plot_x +
            geom_point(aes(color = .data$Pair), size = point.size) +
            labs(color = "")

        if (simplify == TRUE) {
            plot_x <- plot_x +
                scale_color_manual(values = c("Maximum SB" = "black"))
        }
    }else{
        plot_x <- plot_x +
            geom_point(aes(shape = .data$Pair), size = point.size) +
            labs(shape = "")
    }

    plot_x <- plot_x +
        scale_x_continuous(breaks = breaklines,
                           labels = breaklines) +
        labs(x = xlab, y = "Covariates") +
        theme_bw() +
        theme(legend.position = ifelse(simplify == FALSE, "bottom", "none"),
              text = element_text(size = text.size))

    plot_x
}

#' @method summary BalanceR
#' @export

summary.BalanceR <- function(object, digits = 3, ...) {

    validate_BalanceR(object)

    x <- object$SB

    if (!is.numeric(digits) | length(digits) != 1) {
        stop("An argument 'digits' must have numeric value with length of 1.")
    }

    x[, -1]    <- abs(x[, -1])

    if (dim(x)[2] > 2) {
        Max_SB_Pos <- apply(x[, -1], 1, which.max)
        Max_SB     <- apply(x[, -1], 1, `[`, Max_SB_Pos)
        x          <- data.frame(Covariate      = x$Covariate,
                                 Abs_Maximum_SB = diag(Max_SB))
    }

    x[1:nrow(x), 2:ncol(x)] <- format(round(x[1:nrow(x), 2:ncol(x)], digits),
                                      nsmall = digits)

    print(x)
}
