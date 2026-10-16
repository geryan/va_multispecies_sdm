#' Run a function from R/ in a fresh R process
#'
#' For targets that call `greta::calculate()` on the ~6.75 GB fit image.
#' targets runs every target in one R process, and one `calculate()` on the
#' loaded image leaves ~5.2 GB of R memory held after the function returns
#' (measured 2026-10-16: not greta's stash, and still held after `gc()`, a
#' python `gc.collect()` and a TensorFlow `clear_session()`). The next target
#' to load the image then goes over R's 16 GB limit. A child process gives all
#' of its memory back when it exits. Same approach as `fit_cv_fold()`.
#'
#' The child attaches the pipeline's packages, sources R/, and sets the
#' calling target's seed, which would not otherwise reach it. terra objects
#' cannot be sent to another process as they are, so SpatRaster and SpatVector
#' arguments are wrapped here and unwrapped in the child.
#'
#' @param fun name of a function defined in R/, as a string
#' @param args named list of arguments to `fun`
#' @return whatever `fun` returns
#' @author geryan
#' @export
in_fresh_process <- function(
    fun,
    args
){

  callr::r(
    func = function(fun, args, packages, seed){

      if (!is.na(seed)) {
        set.seed(seed)
      }

      suppressMessages(
        lapply(
          packages,
          library,
          character.only = TRUE
        )
      )

      targets::tar_source(files = "R")

      args <- lapply(
        args,
        function(arg) {
          if (inherits(arg, c("PackedSpatRaster", "PackedSpatVector"))) {
            terra::unwrap(arg)
          } else {
            arg
          }
        }
      )

      do.call(
        fun,
        args
      )

    },
    args = list(
      fun = fun,
      args = lapply(
        args,
        function(arg) {
          if (inherits(arg, c("SpatRaster", "SpatVector"))) {
            terra::wrap(arg)
          } else {
            arg
          }
        }
      ),
      packages = targets::tar_option_get("packages"),
      seed = targets::tar_seed_get()
    ),
    libpath = .libPaths(),
    wd = getwd(),
    show = TRUE
  )

}
