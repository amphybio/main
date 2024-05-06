# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, you can obtain one at https://mozilla.org/MPL/2.0/
#
# Copyright 2024 Alexandre Ramos, AMPhyBio Laboratory <alex.ramos@usp.br>
#
# Project:  AMPhyBio Utilities
# Version:  0.1
# Created:  06-05-2024
# Authors:
#   - Leonardo Gama <leonardo.gama@usp.br> <leogama@github>


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
