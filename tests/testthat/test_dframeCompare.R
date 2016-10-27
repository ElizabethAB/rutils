context("dframeCompare")

df_base <- data.frame(id1 = c("First", "Second", "Third"),
                      id2 = c("Primero", "Segundo", "Tercero"),
                      log = c(TRUE, TRUE, FALSE),
                      int = 1:3,
                      num = c(1, 2, 3),
                      cha = letters[1:3],
                      fac = factor(letters[1:3]),
                      date = as.Date(c("1900-01-01",
                                       "2000-01-01",
                                       "2016-07-22")),
                      comp = c(1 + 1i, 2 + 1i, 3))

test_that("Output form", {
  out <- dframeCompare(df_base, df_base, ids = c("id1", "id2"))

  expect_type(out, "list")
  expect_named(out, c("differences_df", "id_overlap", "column_overlap"))

  expect_equal(class(out[["differences_df"]]), "data.frame")

  expect_type(out[["id_overlap"]], "list")
})

test_that("Matching data.frame (columns of all data types)", {
  out <- dframeCompare(df_base, df_base, ids = "id1")
  out$differences_df
})

test_that("Mismatch on single column, all data types", {

})

test_that("Mismatch in multiple columns", {

})

test_that("Mismatch in single row", {

})

test_that("Differing column titles", {

})

test_that("Differing ID values", {

})
