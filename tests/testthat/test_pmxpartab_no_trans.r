test_that("pmxpartab renders when meta declares no trans attribute", {
  # Reproduces the user-reported error:
  #   Error in `!is.null(trans) && !is.na(trans)`:
  #   ! 'length = 6' in coercion to 'logical(1)'
  # which occurred when a model's parameter comments contained no `trans=`
  # attribute, so the meta data frame had no `trans` column and
  # parameter.estimate.table.row() fell back to a length-6 default.

  out <- data.frame(
    name  = "CL",
    value = 10,
    se    = 1,
    fixed = FALSE,
    stringsAsFactors = FALSE
  )

  meta <- tibble::tibble(
    name  = "CL",
    label = "Clearance",
    units = "L/h",
    type  = "Structural"
  )

  tab <- pmxpartabc::parframe(out = out, meta = meta)

  expect_no_error(
    res <- pmxpartabc::pmxpartab(tab, meta)
  )
  expect_s3_class(res, "pmxpartab")
})
