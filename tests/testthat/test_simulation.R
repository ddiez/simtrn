context("Testing TRN simulation")

net <- sim_trn(n = 10, nreg = 2, seed = 123)

test_that("sim_trn returns expected object", {
  expect_type(net, "list")
  expect_length(net, 4)
  expect_equal(names(net), c("mod_info", "reg_info", "reg_value", "gene_value"))
})

test_that("mod_info contains expected information", {
  tmp <- net$mod_info
  expect_equal(dim(tmp), c(2, 3))
  expect_identical(tmp[1, 1], 1)
  expect_identical(tmp[2, 1], 4)
})

test_that("reg_info contains expected information", {
  tmp <- net$reg_info
  expect_equal(dim(tmp), c(2, 3))
  expect_identical(tmp[1, "regulator"], "R1")
  expect_identical(tmp[1, "mean"], 1)
  expect_identical(tmp[2, "sd"], 1)
})

test_that("reg_value contains expected information", {
  tmp <- net$reg_value
  expect_equal(dim(tmp), c(10, 2))
})

test_that("gene_value contains expected information", {
  tmp <- net$gene_value
  expect_equal(dim(tmp), c(10, 3))
})