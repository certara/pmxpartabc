test_that("test parframe2setup returns correct class objects", {

  suppressWarnings({
  output <- pmxpartabc::parframe2setup(run_dir = file.path(system.file(package = "pmxpartabc"), "examples"),
                           run_prefix = "run",
                           runno = "6",
                           conf.level = 0.95,
                           min_suc = TRUE,
                           yaml.file = TRUE,
                           yaml.file.name = file.path(system.file(package = "pmxpartabc"), "meta.yaml"))

  tab <- pmxpartabc::parframe(out=output[[1]], meta=output[[2]])

  tab1 <- pmxpartabc::pmxpartab(tab, output[[2]], columns=c(value="Estimate", rse="RSE%", ci95="95%CI", shrinkage="Shrinkage"))
  })

  testthat::expect_true(is.data.frame(output[[1]]))
  testthat::expect_true(tibble::is_tibble(output[[2]]))
})
