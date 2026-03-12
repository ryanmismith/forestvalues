test_that("discount computes correct present value", {
  # $100,000 in 30 years at 6% = $17,411.01 (Klemperer Ch. 4)
  expect_equal(round(discount(100000, 0.06, 30), 2), 17411.01)
  # At time 0, PV = FV
  expect_equal(discount(5000, 0.06, 0), 5000)
  # Vectorized
  result <- discount(c(1000, 2000), 0.05, c(10, 20))
  expect_length(result, 2)
})

test_that("compound computes correct future value", {
  # $5,000 compounded 30 years at 6% = $28,717.46
  expect_equal(round(compound(5000, 0.06, 30), 2), 28717.46)
  # At time 0, FV = PV
  expect_equal(compound(5000, 0.06, 0), 5000)
})

test_that("discount and compound are inverses", {
  fv <- compound(1000, 0.08, 25)
  pv <- discount(fv, 0.08, 25)
  expect_equal(round(pv, 2), 1000)
})

test_that("discount validates inputs", {
  expect_error(discount("abc", 0.06, 10), "must be numeric")
  expect_error(discount(1000, "abc", 10), "must be numeric")
  expect_error(discount(1000, 0.06, "abc"), "must be numeric")
  expect_warning(discount(1000, 0.06, -5), "Negative")
})

test_that("compound validates inputs", {
  expect_error(compound("abc", 0.06, 10), "must be numeric")
})
