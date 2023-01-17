# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, you can obtain one at https://mozilla.org/MPL/2.0/
#
# Copyright 2023 Alexandre Ferreira Ramos - AMPhyBio Laboratory
#
# Project:  AMPhyBio Utilities
# Version:  0.1
# Created:  06-12-2020
# Authors:
#   - Leonardo Gama <leonardo.gama@usp.br> <leogama@github>


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
        res[[y]] <- FUN(x, DATA[, y], ...)
    res
}

#' Apply a function to a list of variables versus another list of variables
#'
#' From a `data.frame`, apply the same (statistical) function between
#' each variable of the first list against each variable of the other
#' list.
#'
#' @export

stat_matrix <- function(DATA, FUN, X=NULL, Y=NULL, ...) {

    if (is.null(X) && is.null(Y))
        X <- Y <- colnames(DATA)
    if (is.null(X) || is.null(Y))
        stop("Pass both 'X' and 'Y' or none of them.")
    symmetric <- identical(X, Y)
    m <- length(X)
    n <- length(Y)

    res <- setNames(replicate(m, list()), X)
    for (i in seq_len(m)) for (j in seq_len(n)) {
        x <- X[i]
        y <- Y[j]
        if (i > j && symmetric) {
            stat <- res[[y]][[x]]
            if (hasName(stat, 'x') && hasName(stat, 'y')) {
                tmp <- stat$x
                stat$x <- stat$y
                stat$y <- tmp
            }
            res[[x]][[y]] <- stat

        } else {
            res[[x]][[y]] <- FUN(DATA[, x], DATA[, y], ...)
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
        res <- expand.grid(dn, KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE) |>
            setNames(c('var1', 'var2'))
        for (name in names(what)) {
            value_len <- results_list[[1L]][[1L]] |> getElement(what[name]) |> length()
            if (value_len == 1L) {
                res[[name]] <- results_list |> get_matrix(what[name]) |> as.vector()
            } else for (i in seq_len(value_len)) {
                colname <- paste(name, i, sep='_')
                res[[colname]] <- results_list |> get_matrix(what[name], i) |> as.vector()
            }
        }
        if (identical(dn[[1L]], dn[[2L]])) {
            res <- res[upper.tri(results_list), ]
            rownames(res) <- NULL
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

get_matrix <- function(list_matrix, name, index=1L) {
    res <- sapply(
            list_matrix,
            sapply,
            function(object, name) unname(getElement(object, name))[index],
            name
    )
    t(res)
}
