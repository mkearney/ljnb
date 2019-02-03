is_mac <- function() {
  is_unix() && grepl("apple|darwin", R.version$platform)
}

is_unix <- function() .Platform$OS.type == "unix"

gray_col <- function(...) {
  x <- paste0(c(...), collapse = "")
  paste0("\033[38;5;240m", x, "\033[39m")
}

cat_line <- function(..., fill = TRUE) {
  cat(paste0(c(...), collapse = ""), fill = fill)
}

cat_colon <- function(lhs, rhs) {
  if (nchar(rhs) > getOption("width", 80) - 17) {
    rhs <- strwrap(rhs, getOption("width", 80) - 17)
    if (length(rhs) > 1) {
      rhs[-1] <- paste0(paste0(rep(" ", 16), collapse = ""), rhs[-1])
      rhs[-length(rhs)] <- paste0(rhs[-length(rhs)], "\u2026")
    }
    rhs <- paste(rhs, collapse = "\n")
  }
  if (nchar(lhs) > 11) {
    lhs <- paste0(substr(lhs, 1, 10), "\u2026")
  }
  s <- paste(rep(" ", 11 - nchar(lhs)), collapse = "")
  cat_line(paste0("+ ", lhs, s, ":  ", rhs))
}

is_python <- function() !identical("", Sys.which("python"))

is_jupyter <- function() !identical("", Sys.which("jupyter"))

is_jupyter_nb <- function() !identical("", Sys.which("jupyter-notebook"))

is_anaconda <- function() length(anaconda_find()) > 0

anaconda_find <- function() {
  ## all anaconda folders in home
  an <- fml::list_dirs(fml::fp("~"), pattern = "anaconda")
  if (length(an) == 0) return(an)

  ## check if bin folder
  an_bin <- fml::fp(an, "bin")
  has_bin <- dapr::vap_lgl(an_bin, fml::dir_exists)
  has_bin_py <-  dapr::vap_lgl(an_bin,
    ~ length(fml::list_files(.x, pattern = "python")) > 0)
  an <- an[has_bin & has_bin_py]
  if (length(an) < 2) return(an)
  if (any(grepl("anaconda\\d$", an))) {
    an <- sort(grep("anaconda\\d$", an, value = TRUE),
      decreasing = TRUE)[1]
    return(an)
  }
  if (any(grepl("anaconda$", an))) {
    an <- grep("anaconda$", an, value = TRUE)
    return(an)
  }
  an <- an[order(nchar(an), decreasing = TRUE)]
  an[1]
}
