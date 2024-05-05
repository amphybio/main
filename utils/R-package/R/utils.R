# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, you can obtain one at https://mozilla.org/MPL/2.0/
#
# Copyright 2023-2024 Alexandre Ramos, AMPhyBio Laboratory <alex.ramos@usp.br>
#
# Project:  AMPhyBio Utilities
# Version:  0.1
# Created:  06-12-2020
# Authors:
#   - Leonardo Gama <leonardo.gama@usp.br> <leogama@github>


# @export
#`%||%` <- utils:::`%||%`


#' Apply a function to one variable versus a list of variables
#'
#' From a `data.frame`, apply the same (statistical) function between
#' one of its variables against a list of other variables.
#'
#' Using `stat_list()` allows the substitution of the following pattern:
#'
#' res <- dat |>
#'     select(all_of(y_vars)) |>
#'     lapply(cor.test, dat[[x_var]], method='spearman')
#'
#' by a call like this:
#'
#' res <- dat |> stat_list(cor.test, x_var, y_vars, method='spearman')
#'
#' @param DATA  A `data.frame`-like object
#' @param FUN  A function that accepts two vectors as its first arguments
#' @param X  A variable (column index) from `data`
#' @param Y  A vector of variables from `data` (optional)
#' @param ...  Optional arguments to `FUN`
#' @returns A list with the individual results named after the second
#'   variables
#' @seealso [get_table()] for a convenient way of extracting specific
#'   statistics from the results.
#' @export

stat_list <- function(DATA, FUN, X, Y=NULL, ...) {
    if (!is.vector(X) || length(X) != 1L)
        stop("Argument 'X' must be a vector of length 1")
    if (is.null(Y))
        Y <- setdiff(colnames(DATA), X)
    res <- list()
    x <- DATA[, X]
    for (y in Y)
        res[[y]] <- FUN(DATA[, y], x, ...)
    res
}

#' Apply a function to a list of variables versus another list of variables
#'
#' From a `data.frame`, apply the same (statistical) function between
#' each variable of the first list against each variable of the other
#' list.
#'
#' @param X  row variables
#' @param Y  column variables
#'
#' @export

stat_matrix <- function(DATA, FUN, ROWS=NULL, COLS=ROWS, ...) {

    FUN <- match.fun(FUN)
    stopifnot(is.atomic(ROWS), is.atomic(COLS))
    if (is.null(ROWS)) {
        if (is.null(COLS))
            ROWS <- COLS <- colnames(DATA)
        else
            stop("Pass both 'ROWS' and 'COLS' or only 'ROWS'.")
    }
    if (!is.data.frame(DATA) && !is.matrix(DATA))
        DATA <- as.data.frame(DATA)
    symmetric <- identical(ROWS, COLS)

    res <- length(COLS) |> replicate(list()) |> setNames(COLS)  # list of lists

    for (col in seq_along(COLS)) for (row in seq_along(ROWS)) {
        x <- ROWS[row]
        y <- COLS[col]
        if (!symmetric || col <= row) {
            res[[y]][[x]] <- FUN(DATA[, x], DATA[, y], ...)
        } else {
            stat <- res[[x]][[y]]
            if (hasName(stat, 'x') && hasName(stat, 'y')) {
                tmp <- stat$x
                stat$x <- stat$y
                stat$y <- tmp
            }
            res[[y]][[x]] <- stat
        }
    }

    # Mark as data.frame.
    class(res) <- 'data.frame'
    rownames(res) <- names(res[[1L]])
    res
}

#' Extract a table of test statistics from a list of test results
#'
#' Build a `data.frame` with the specified statistics from a list of
#' statistical test results.
#'
#' @export

get_table <- function(results_list, ...) {
    what <- c(...)
    wnames <- names(what)
    names(what) <- if (is.null(wnames)) what else ifelse(wnames == '', what, wnames)

    if (is.data.frame(results_list)) {
        dn <- dimnames(results_list)
        symmetric <- identical(dn[[1L]], dn[[2L]])
        res <- if (symmetric) {
            dn[[1L]] |> combn(2L) |> t() |> as.data.frame()
        } else {
            expand.grid(dn, KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)[2L:1L]
        }
        colnames(res) <- c('var1', 'var2')
        for (name in names(what)) {
            value_len <- results_list[[1L]][[1L]] |> getElement(what[name]) |> length()
            if (value_len == 1L) {
                mat <- results_list |> get_matrix(what[name])
                res[[name]] <- if (symmetric) mat[lower.tri(mat)] else as.vector(mat)
            } else for (i in seq_len(value_len)) {
                colname <- paste(name, i, sep='_')
                mat <- results_list |> get_matrix(what[name], i)
                res[[colname]] <- if (symmetric) mat[lower.tri(mat)] else as.vector(mat)
            }
        }
    } else {
        res <- data.frame(var=names(results_list))
        for (name in names(what)) {
            values <- lapply(results_list, getElement, what[name])
            if (length(values[[1L]]) == 1L) {
                res[[name]] <- unlist(values, use.names=FALSE)
            } else for (i in seq_along(values[[1L]])) {
                colname <- paste(name, i, sep='_')
                res[[colname]] <- unlist(lapply(values, getElement, i), use.names=FALSE)
            }
        }
    }
    res
}

#' Extract a matrix of specific test statistic from a list-matrix of test results
#'
#' Build a `matrix` with the specified statistic from a list-matrix of
#' statistical test results.
#'
#' @export

get_matrix <- function(list_matrix, name, index=1L, diag_value=NULL, mode=NULL) {
    if (missing(mode))
        mode <- list_matrix[[1L]][[1L]] |> getElement(name) |> getElement(index) |> typeof()
    value_template <- vector(mode, 1L)

    res <- list_matrix |> sapply(
        vapply,
        function(x, y) getElement(x, y)[[index]],
        value_template,
        name
    )

    if (!is.null(diag_value)) {
        diag_value <- rep(diag_value, nrow(res))
        stopifnot(
            identical(rownames(res), colnames(res)),
            all.equal(unname(diag(res)), diag_value)
        )
        diag(res) <- diag_value
    }

    res
}


#' Fowler-Noll-Vo (FNV-1a) hash function, 32-bit version.
#'
#' Vectorized version of FNV-1a with optional salt string.

hash <- function(x, length = 8L, salt = NULL) {
    rlang::check_installed('bitops', reason = "to use `amphybio::hash()^")
    withr::local_package('bitops')

    if (length < 1L || length > 8L )
        stop("'length' must be an integer between 1 and 8 (inclusive)")

    if (!is.null(salt))
        x <- paste0(salt, x)

    x_bytes <- iconv(x, to = 'UTF-8', toRaw = TRUE)

    hash <- vapply(x_bytes, fnv1a_32_loop, numeric(1))

    hash <- if (length == 8L) {
        #NOTE: 0xFFFFFFFF can't be represented by integer type.
        hash_hi <- hash %>>% 16L
        hash_lo <- hash %&% MASK_16_BITS
        sprintf("%04X%04X", hash_hi, hash_lo)
    } else {
        # Apply XOR-folding to reduce length.
        bit_size <- length * 4L
        size_mask <- (1 %<<% bit_size) - 1
        hash <- ((hash %>>% bit_size) %^% hash) %&% size_mask
        sprintf(glue::glue("%0{length}X"), hash)
    }

    hash
}

fnv1a_32_loop <- function(bytes) {
    hash <- FNV32_BASIS
    for (byte in bytes) {
        hash <- hash %^% byte
        hash_lo <- hash %&% MASK_16_BITS  # avoid overflow
        hash <- hash * FNV32_PRIME_LO + hash_lo * FNV32_PRIME_HI
        hash <- hash %% MASK_32_BITS
    }
    hash
}

FNV32_BASIS <- 0x811C9DC5
FNV32_PRIME_HI <- 0x01000000
FNV32_PRIME_LO <- 0x0193
MASK_16_BITS <- 0xFFFF
MASK_32_BITS <- 2**32  # modulo masking
