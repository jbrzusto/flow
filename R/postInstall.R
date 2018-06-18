#' create symbolic links etc. after installing the `flow` package
#'
#' This function can only be run by a user with sudo privileges.
#' It creates symlinks in /usr/local/bin to the scripts installed
#' in this package's directory.  This should be run from R after
#' (re-) installing the `flow` package.
#'
#' @return no return value
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

postInstall = function() {
    SU = function(cmd, ...) system(sprintf(paste("sudo", cmd), ...))
    SCR = function(x) SU("ln -s %s /usr/local/bin/%s", x, system.file(x, package="flow"))

    SCR("makepol")        ## script to generate a bz2-compressed .pol file from a sequence of digdar .dat files
    SCR("digdar2jpg")     ## script to generate (and display) a .jpg scan-converted from a single digdar .dat file
}
