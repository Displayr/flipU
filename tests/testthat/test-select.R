context("Select")

test_that("Select: vector", {
    z <- 1:5
    expect_error(Select(z, "b"), "There are no names to select from.")
    names(z) <- LETTERS[1:5]
    expect_error(Select(z, "b"), "'b' is not one of the names.")
    expect_error(Select(z, "B"))
    expect_equal(Select(z, "B", "D"), z[2:4])
})

test_that("Select: data frame", {
    z <- as.data.frame(matrix(1:25, 5))
    expect_error(Select(z, "b"), "'b' is not one of the names.")
    names(z) <- LETTERS[1:5]
    expect_error(Select(z, "b"), "'b' is not one of the names.")
    expect_error(Select(z, "B"))
    expect_equal(Select(z, "B", "D"), z[, 2:4])
})


test_that("Select: matrix", {
    z <- matrix(1:25, 5)
    expect_error(Select(z, "b"),
                 "'MARGIN needs to be specified. A 1 for rows and 2 for columns.")
    expect_error(Select(z, "b", MARGIN = 1),
                 "There are no names to select from.")
    expect_error(Select(z, "b", MARGIN = 12),
                 "'MARGIN' is invalid (not compatible with dimensions of the array).",
                 fixed = TRUE)
    dimnames(z) <- list(LETTERS[1:5], letters[1:5])
    expect_error(Select(z, "b", MARGIN = 1),
                 "'b' is not one of the names.")
    expect_error(Select(z, "B", MARGIN = 1))
    expect_equal(Select(z, "B", "D", MARGIN = 1),
                 z[2:4, ])
    expect_error(Select(z, "B", "D", MARGIN = 2))
    expect_equal(Select(z, "a", "d", MARGIN = 2),
                 z[, 1:4, drop = FALSE])
    expect_equal(Select(z, "a", "d", MARGIN = 2, drop = TRUE),
                 z[, 1:4, drop = TRUE])
})
