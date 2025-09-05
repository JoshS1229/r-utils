#' Merge two data frames and add a Stata‑style merge flag
#'
#' Creates a flag indicating whether rows came from `x` only (1),
#' `y` only (2), or both (3).  The flag is in the style of Stata's
#' `_merge` variable, with support for both numeric and labelled
#' versions.  A `full`, `left`, `right`, or `inner` join can be
#' selected via the `type` argument.
#'
#' @param x,y Data frames to join.
#' @param by Character vector of join keys passed to dplyr.  If
#'   `NULL` the common column names between `x` and `y` are used.
#' @param type Join type: one of `"full"`, `"left"`, `"right"`, or `"inner"`.
#' @param suffix A length‑two character vector of suffixes to be
#'   appended to overlapping non‑key columns from `x` and `y`.  Passed
#'   to `dplyr` join functions.
#' @param relationship Optional one‑to‑one check for the join.  See
#'   `dplyr::join` for details.  Requires dplyr ≥ 1.1.
#' @param flag_name Name of the flag column to be added to the result.
#'   Defaults to `_merge`.
#' @param flag Should the flag be returned as `"numeric"` (1/2/3) or as
#'   a `"label"` (character vector of `"x_only"`, `"y_only"`, and
#'   `"both"`).
#'
#' @return A data frame resulting from the chosen join with an
#'   additional column named by `flag_name` indicating the origin of each row.
#'   If `flag = "numeric"` the column is an integer; if `flag = "label"`
#'   the column is a factor.
#'
#' @importFrom dplyr full_join left_join right_join inner_join mutate coalesce case_when select
#' @export
merge_with_flag <- function(
    x, y, by = NULL,
    type = c("full", "left", "right", "inner"),
    suffix = c(".x", ".y"),
    relationship = NULL,
    flag_name = "_merge",
    flag = c("numeric", "label")
) {
  # match arguments
  type <- match.arg(type)
  flag <- match.arg(flag)

  # choose appropriate join function
  jf <- switch(type,
               full  = dplyr::full_join,
               left  = dplyr::left_join,
               right = dplyr::right_join,
               inner = dplyr::inner_join)

  # tag rows to mark their origin
  x2 <- dplyr::mutate(x, .__from_x = TRUE)
  y2 <- dplyr::mutate(y, .__from_y = TRUE)

  # perform join and mark presence in each input
  joined <- jf(x2, y2, by = by, suffix = suffix, relationship = relationship) |>
    dplyr::mutate(
      .from_x = dplyr::coalesce(.__from_x, FALSE),
      .from_y = dplyr::coalesce(.__from_y, FALSE)
    )

  # create merge flag based on origin of each row
  joined <- if (flag == "numeric") {
    dplyr::mutate(
      joined,
      !!flag_name := dplyr::case_when(
        .from_x & !.from_y ~ 1L,
        !.from_x & .from_y ~ 2L,
        .from_x &  .from_y ~ 3L,
        TRUE ~ NA_integer_
      )
    )
  } else {
    dplyr::mutate(
      joined,
      !!flag_name := dplyr::case_when(
        .from_x & !.from_y ~ "x_only",
        !.from_x & .from_y ~ "y_only",
        .from_x &  .from_y ~ "both",
        TRUE ~ NA_character_
      ) |>
        factor(levels = c("x_only", "y_only", "both"))
    )
  }

  # clean up temporary columns and return
  dplyr::select(joined, -.__from_x, -.__from_y, -.from_x, -.from_y)
}