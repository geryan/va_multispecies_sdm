#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param plots
#' @param dir
#' @param prefix
#' @return
#' @author geryan
#' @export
saveplotlist <- function(
    plots,
    dir,
    prefix = "distribution"
) {

  if(!dir.exists(dir)){
    dir.create(dir)
  }

  pnames <- names(plots)

  mapply(
    FUN = function(x, y, prefix, dir){
      ggsave(
        filename = sprintf(
          "%s/%s_%s.png",
          dir,
          prefix,
          y
        ),
        plot = x,
        width = 3200,
        height = 3200,
        dpi = 300,
        units = "px"
      )
    },
    x = plots,
    y = pnames,
    MoreArgs = list(
      prefix,
      dir
    )
  )

  NULL

}
