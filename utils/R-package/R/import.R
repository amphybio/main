# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, you can obtain one at https://mozilla.org/MPL/2.0/
#
# Copyright 2023 Alexandre Ferreira Ramos - AMPhyBio Laboratory
#
# Project:  AMPhyBio Code Library
# Version:  0.1
# Created:  06-12-2020
# Authors:
#   - Leonardo Gama <leonardo.gama@usp.br> <leogama@github>


EXCEL_COLS <- expand.grid(LETTERS, c('', LETTERS))
EXCEL_COLS <- setNames(1:(26*27), paste0(EXCEL_COLS[[2]], EXCEL_COLS[[1]]))

#' Convert spreadsheet column ranges to numeric indexes
#'
#' Para criar os objetos que serão usados nas análises, é preciso
#' selecionar as colunas desejadas, na ordem desejada. Os editores de
#' planilha indicam as colunas usando índices alfabéticos ("C", "AA").
#' Podemos construir um dicionário (baseado em resposta no site
#' StackOverflow).
#'
#' @export

excel_cols <- function(...) {
    args <- toupper(as.character(list(...)))
    if (length(args) == 1) {
        # Accept indexes as a comma separated list in a character string.
        args <- strsplit(trimws(args), '[[:space:]]*,[[:space:]]*')[[1]]
    }
    if (length(invalid <- grep('^[A-Z]{1,2}(:[A-Z]{1,2})?$', args, invert=TRUE, value=TRUE))) {
        stop("Invalid column index or range:  ", paste(invalid, collapse=", "))
    }

    cols <- integer()
    for (col_index in strsplit(args, ':')) {
        if (length(col_index) == 1) {
            cols <- c(cols, EXCEL_COLS[col_index[[1]]])
        } else {
            # Preserve columns' alphabetical indexes in names.
            range_begin <- EXCEL_COLS[col_index[[1]]]
            range_end <- EXCEL_COLS[col_index[[2]]]
            cols <- c(cols, EXCEL_COLS[range_begin:range_end])
        }
    }
    cols
}
