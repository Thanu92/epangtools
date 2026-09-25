## ============================================================
# epang-jplace-tools
#
# Function: get_best_epang_placements
#
# Author: M. A. Thanuja M. Fernando
# Year: 2026
# License: MIT
#
# Description:
# Extracts the highest likelihood EPA-ng placement for each
# ZOTU from an EPA-ng jplace file and identifies the
# corresponding reference name from the tree stored in the
# jplace file.
# ============================================================
#' Extract best EPA-ng placements from a jplace file
#'
#' Reads an EPA-ng jplace file, selects the highest likelihood
#' placement for each ZOTU, and extracts the corresponding
#' reference name from the tree stored in the jplace file.
#'
#' @param jplace_file Path to an EPA-ng jplace file.
#'
#' @return A data.frame containing one best placement per ZOTU,
#' including edge number, likelihood, likelihood weight ratio,
#' distal length, pendant length, and reference name.
#'
#' @importFrom jsonlite fromJSON
#'
#' @export
get_best_epang_placements <- function(jplace_file) {

  jplace <- fromJSON(
    jplace_file,
    simplifyVector = FALSE
  )

  fields <- unlist(jplace$fields)

  best_placements <- do.call(
    rbind,
    lapply(jplace$placements, function(x) {

      zotu <- x$n[[1]]

      p <- do.call(
        rbind,
        lapply(x$p, unlist)
      )

      p <- as.data.frame(p)
      colnames(p) <- fields

      p$edge_num <- as.numeric(p$edge_num)
      p$likelihood <- as.numeric(p$likelihood)
      p$like_weight_ratio <- as.numeric(p$like_weight_ratio)
      p$distal_length <- as.numeric(p$distal_length)
      p$pendant_length <- as.numeric(p$pendant_length)

      best <- p[which.max(p$likelihood), ]

      data.frame(
        ZOTU = zotu,
        edge_num = best$edge_num,
        likelihood = best$likelihood,
        like_weight_ratio = best$like_weight_ratio,
        distal_length = best$distal_length,
        pendant_length = best$pendant_length
      )
    })
  )

  rownames(best_placements) <- NULL

  get_reference_name <- function(edge_num, tree_string) {

    marker <- paste0("{", edge_num, "}")

    pos <- regexpr(
      marker,
      tree_string,
      fixed = TRUE
    )[1]

    if (pos == -1) {
      return(NA_character_)
    }

    before <- substr(
      tree_string,
      1,
      pos - 1
    )

    matches <- gregexpr(
      "([^(),:]+):[0-9eE+.-]+",
      before,
      perl = TRUE
    )[[1]]

    if (matches[1] == -1) {
      return(NA_character_)
    }

    lengths <- attr(
      matches,
      "match.length"
    )

    names_found <- sapply(
      seq_along(matches),
      function(i) {

        piece <- substr(
          before,
          matches[i],
          matches[i] + lengths[i] - 1
        )

        sub(
          ":.*$",
          "",
          piece
        )
      }
    )

    for (x in rev(names_found)) {

      x <- trimws(x)

      if (x == "") next

      if (grepl(
        "^[0-9.]+(/[0-9.]+)?$",
        x
      )) next

      return(x)
    }

    return(NA_character_)
  }

  best_placements$reference_name <- sapply(
    best_placements$edge_num,
    get_reference_name,
    tree_string = jplace$tree
  )

  return(best_placements)
}
