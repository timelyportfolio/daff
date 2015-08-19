#' Render a data diff  to html
#'
#' \code{render_diff} will show a diff in html. When \code{view} is \code{TRUE} \code{render_diff} will show
#' the result in a browser.
#'
#' @example ./examples/render-diff.R
#' @param diff object generated with \code{\link{diff_data}}
#' @param file to write to
#' @param view \code{TRUE} or \code{FALSE}, if \code{TRUE} render_diff will show the diff
#' in the browser (only if R is used interactively)
#' @param fragment if \code{FALSE} the generated HTML will be a valid HTML document, otherwise it is a HTML fragment
#' @param pretty if \code{TRUE} use fancy utf8 characters to make arrows prettier.
#' @return generated html
#'
#' @importFrom htmltools HTML html_print attachDependencies htmlDependency
#' @export
render_diff <- function( diff
                       , file=NULL
                       , view=interactive()
                       , fragment=TRUE
                       , pretty=FALSE
                       ){
  ctx <- diff$ctx
  # keep fragment = TRUE despite parameter
  #   we'll add the css and html wrapper with htmltools::save_html
  html <- ctx$call("render_diff", JS(diff$var_name), fragment, pretty)

  # allow for both view and save file
  #   if fragment then leave as is
  if(!is.null(file) ) {
    cat(html, file = file)
  }

  # now get in htmltools HTML form for rmarkdown or Shiny
  #   unfortunately requires us to rerun if fragment before
  #   could avoid by always calling above with fragment = FALSE
  #   and manually piecing together the rest of the HTML
  if( !fragment ){
    html <- HTML( ctx$call("render_diff", JS(diff$var_name), TRUE, pretty) )
    # attach dependency for the CSS
    html <- attachDependencies(
      html
      , htmlDependency(
        name = "daff"
        , version = "1.2.20"
        , src = c( file = (function(){system.file( "css", package = "daff")})()  )
        , stylesheet = "daff.css"
      )
    )
    html <- htmltools::tags$div(class = "highlighter", html)
  } else {
    html <- HTML( html )
  }

  if( view && interactive() ) html_print( html )

  html

}

