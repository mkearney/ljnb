
#' Launch jupyter
#'
#' Opens terminal and launches a Juypter environment in the system's
#' default browser.
#'
#' @section Requirements:
#' This function assumes the installation of
#' \href{https://www.anaconda.com/distribution/}{Anaconda} (a data science
#' distribution/platform for R and python), which comes bundled with
#' \href{https://www.python.org}{Python} and
#' \href{https://jupyter.org/about}{Jupyter}.
#'
#' For instructions on installing Anaconda see
#' \url{http://docs.anaconda.com/anaconda/install/}.
#'
#'
#' @param path Path to desired default directory for Jupyter environment.
#' @return Opens a new terminal and executes a jupyter command, which should,
#' in turn, open the system's default browser to the launched jupyter server.
#'
#' @export
launch_jupyter <- function(path = ".") {
  ## check OS
  if (!is_unix()) {
    stop("Currently launch_jupyter only works on unix machines",
      call. = FALSE)
  }
  if (!is_anaconda()) {
    stop("Currently launch_jupyter assumes anaconda is installed",
      call. = FALSE)
  }
  cat_colon("platform", gray_col(R.version$platform))

  ## find anaconda distribution
  anaconda <- anaconda_find()
  cat_colon("anaconda", gray_col(anaconda))

  ## determine/set jupyter command
  if (is_mac()) {
    cmd <- sprintf(
      "cd %s ; %s 2>&1",
      path,
      fml::fp(anaconda, "bin", "jupyter_mac.command")
    )
  } else {
    cmd <- sprintf(
      "cd %s ; %s",
      path,
      fml::fp(anaconda, "bin", "jupyter-notebook")
    )
  }

  ## execute new terminal command
  cat_colon("terminal", gray_col(cmd))
  sh <- system(terminal_cmd(cmd), intern = TRUE)
  cat_colon("server", gray_col("http://localhost:8888/tree"))
  cat_colon("path", gray_col(fml::pe(path)))

  ## return any captured terminal output invisibly
  invisible(sh)
}


terminal_cmd <- function(cmd) {
  if (is_mac()) {
    sprintf(
      "osascript -e 'tell application \"Terminal\" to do script \"%s\"'",
      cmd
    )
  } else {
    sprintf(
      paste0("gnome-terminal --tab --title=\"jupyter\" ",
        "--command=\"bash -c '%s; $SHELL'\""),
      cmd
    )
  }
}
