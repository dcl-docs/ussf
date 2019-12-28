test_that("state 20m albers", {
  expect_identical(
    boundaries(
      geography = "state",
      resolution = "20m",
      projection = "albers"
    ),
    state_20m_albers
  )
})

test_that("state 20m longlat", {
  expect_identical(
    boundaries(
      geography = "state",
      resolution = "20m",
      projection = "longlat"
    ),
    state_20m_longlat
  )
})

test_that("state 5m albers", {
  expect_identical(
    boundaries(
      geography = "state",
      resolution = "5m",
      projection = "albers"
    ),
    state_5m_albers
  )
})

test_that("state 5m longlat", {
  expect_identical(
    boundaries(
      geography = "state",
      resolution = "5m",
      projection = "longlat"
    ),
    state_5m_longlat
  )
})

test_that("state 500k albers", {
  expect_identical(
    boundaries(
      geography = "state",
      resolution = "500k",
      projection = "albers"
    ),
    state_500k_albers
  )
})

test_that("state 500k longlat", {
  expect_identical(
    boundaries(
      geography = "state",
      resolution = "500k",
      projection = "longlat"
    ),
    state_500k_longlat
  )
})

test_that("county 20m albers", {
  expect_identical(
    boundaries(
      geography = "county",
      resolution = "20m",
      projection = "albers"
    ),
    county_20m_albers
  )
})

test_that("county 20m longlat", {
  expect_identical(
    boundaries(
      geography = "county",
      resolution = "20m",
      projection = "longlat"
    ),
    county_20m_longlat
  )
})

test_that("county 5m albers", {
  expect_identical(
    boundaries(
      geography = "county",
      resolution = "5m",
      projection = "albers"
    ),
    county_5m_albers
  )
})

test_that("county 5m longlat", {
  expect_identical(
    boundaries(
      geography = "county",
      resolution = "5m",
      projection = "longlat"
    ),
    county_5m_longlat
  )
})

test_that("county 500k albers", {
  expect_identical(
    boundaries(
      geography = "county",
      resolution = "500k",
      projection = "albers"
    ),
    county_500k_albers
  )
})

test_that("county 500k longlat", {
  expect_identical(
    boundaries(
      geography = "county",
      resolution = "500k",
      projection = "longlat"
    ),
    county_500k_longlat
  )
})

test_that("cz 20m albers", {
  expect_identical(
    boundaries(
      geography = "cz",
      resolution = "20m",
      projection = "albers"
    ),
    cz_20m_albers
  )
})

test_that("cz 20m longlat", {
  expect_identical(
    boundaries(
      geography = "cz",
      resolution = "20m",
      projection = "longlat"
    ),
    cz_20m_longlat
  )
})

test_that("cz 5m albers", {
  expect_identical(
    boundaries(
      geography = "cz",
      resolution = "5m",
      projection = "albers"
    ),
    cz_5m_albers
  )
})

test_that("cz 5m longlat", {
  expect_identical(
    boundaries(
      geography = "cz",
      resolution = "5m",
      projection = "longlat"
    ),
    cz_5m_longlat
  )
})

test_that("cz 500k albers", {
  expect_identical(
    boundaries(
      geography = "cz",
      resolution = "500k",
      projection = "albers"
    ),
    cz_500k_albers
  )
})

test_that("cz 500k longlat", {
  expect_identical(
    boundaries(
      geography = "cz",
      resolution = "500k",
      projection = "longlat"
    ),
    cz_500k_longlat
  )
})
