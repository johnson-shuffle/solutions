load_arm_cache <- function(labels) {
  f <- list.files(
    path = './arm/arm_cache',
    pattern = str_c("^(", paste(labels, collapse = "|"), ")_[0-9a-f]{32}\\.rdx$"),
    full.names = T
    )
  f <- gsub("\\.rdx$", "", f)
  sapply(f, lazyLoad, envir = parent.frame())
  invisible()
}
