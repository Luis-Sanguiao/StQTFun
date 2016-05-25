#' @export
NAfill <- function(...)
  lapply(list(...), function(x) x[!is.na(x)][1])
