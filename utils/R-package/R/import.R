# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, you can obtain one at https://mozilla.org/MPL/2.0/
#
# Copyright 2023-2024 Alexandre Ramos, AMPhyBio Laboratory <alex.ramos@usp.br>
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


default_hash <- function(x) vapply(x, rlang::hash, character(1), USE.NAMES = FALSE)

read_sheet <- function(
    path,
    sheet = 1L,
    range = NULL,
    guess_max = Inf,
    check = TRUE,
    checksum = NULL,
    checksum_func = tools::md5sum,
    id_col = NULL,
    id_cast = NULL,
    sensible_cols = NULL,
    hash_func = default_hash,
    rows = NULL

) {
    #'  Read an Excel sheet applying some checks.
    #'
    #'  Read a single sheet from a spreadsheet file in Microsoft Excel's format
    #'  into a data.frame using the readxl package.  Check the file existence,
    #'  its checksum, the number of rows expected to be read and...
    #'
    #'  Parameters:
    #'      path (character): Path to the xls/xlsx file.
    #'      sheet (character|integer):
    #'          Sheet to read.  Either a string (the name of a sheet), or an
    #'          integer (the position of the sheet).
    #'      range (character):
    #'          A cell range to read from, as described in cell-specification.
    #'          Includes typical Excel ranges like "B3:D87" and more.
    #'      guess_max (integer):
    #'          Maximum number of data rows to use for guessing column types.
    #'      check (logical): Whether to apply the checks.
    #'
    #'  Returns:
    #'      data.frame: The read table (sheet).
    #'
    #'  Raises:
    #'      Error if...
    #'
    #'  See Also:
    #'      `vignette('sheet-geometry', package = 'readxl')`
    #'
    #'  Example:
    #'
    #'      sheets <- readxl::read_excel('data/annotation.xlsx, sheet = 'sheets') |>
    #'          column_to_rownames('name')  # also casts to data.frame
    #'
    #'      raw_dat <- list()
    #'
    #'      for (i in seq(nrow(sheets))) {
    #'          name <- rownames(sheets)[i]
    #'          raw_dat[[name]] <- with(sheets[i, ], {
    #'              read_sheet(
    #'                  path = user::data_file(filename),
    #'                  sheet = sheet_name,
    #'                  range = range,
    #'                  guess_max = 10000,
    #'                  checksum = md5sum,
    #'                  id_col = id_col,
    #'                  id_cast = as.integer,
    #'                  sensible_cols = if (!is.na(sensible_cols)) str_split_1(sensible_cols, ';'),
    #'                  rows = rows
    #'              )
    #'          })
    #'      }
    #'
    #'  # Reexport data to tabulated files.
    #'  for (name in names(raw_dat)) {
    #'      filename <- sprintf('data/%s.tsv', name)
    #'      write.table(raw_dat[[name]], filename, sep = '\t', row.names = FALSE)
    #'  }

    is_nil <- function(x) is.null(x) || (length(x) == 1 && is.na(x))

    # Verify data file exists and is readable.
    if (check && file.access(path, mode = 4) != 0) {
        stop(sprintf("file is not accessible: '%s'", path))
    }

    # Check file integrity with checksum.
    if (check && !is_nil(checksum)
            && !identical(checksum, unname(checksum_func(path)))) {
        stop(sprintf("does not match checksum: '%s'", path))
    }

    # Read single sheet.
    cat(sprintf("Reading '%s'\n", path))

    #FIXME: trim_ws from read_excel is ignored (using str_trim)
    dat <- path |>
        readxl::read_excel(sheet, range, guess_max = guess_max) |>
        mutate(across(where(is.character), stringr::str_trim)) |>
        as.data.frame()

    # Remove spurious whitespaces from column headers.
    colnames(dat) <- colnames(dat) |> stringr::str_squish()

    # Manipulate the ID column.
    if (!is_nil(id_col)) {
        # Match exact regex to find the ID column.
        if (is.character(id_col)) {
            id_col <- colnames(dat) |>
                stringr::str_which(stringr::str_c('^', id_col, '$'))
        }

        # Relocate and rename the ID column to 'id'.
        dat <- dat |> relocate(id = id_col, .before = 1)

        # Cast id column and drop invalid/empty entries.
        if (!is.null(id_cast)) {
            dat[['id']] <- suppressWarnings(id_cast(dat[['id']]))
            dat <- dat |> filter(!is.na(id))
        }
    }

    # Calculate hash of sensible (unused) data.
    if (!is_nil(sensible_cols)) for (col in sensible_cols) {
        dat[[col]] <- hash_func(dat[[col]])
    }

    # Check if the number of rows is the expected.
    if (check && !is_nil(rows)) {
        stopifnot(nrow(dat) == rows)
    }

    dat
}


get_annotation <- attr_getter('annotation')
