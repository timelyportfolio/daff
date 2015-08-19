context("render")

test_that("Render diff fragment results in html",{
  x <- data.frame(a=1:3, b=5:7)
  y <- x
  y$a <- 10
  expect_is(render_diff(diff_data(x,y),fragment=T),c("html"))
})

test_that("Render diff fragment=T results in shiny.tag",{
  x <- data.frame(a=1:3, b=5:7)
  y <- x
  y$a <- 10
  expect_is(render_diff(diff_data(x,y),fragment=F),c("shiny.tag"))
})

test_that("Render diff fragment=T has CSS dependency",{
  x <- data.frame(a=1:3, b=5:7)
  y <- x
  y$a <- 10
  expect_true(renderTags(render_diff(diff_data(x,y),fragment=F))$dependencies[[1]]$name == "daff" )
})

test_that("Render diff saves a file if requested",{
  x <- data.frame(a=1:3, b=5:7)
  y <- x
  y$a <- 10
  tf <- tempfile()
  on.exit(unlink(tf))
  render_diff(diff_data(x,y),fragment=T,file=tf)
  expect_identical( readLines(tf)[1], '<table>' )

})
