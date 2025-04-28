# Testing RemoveByName

test_that("RemoveByName works with vector names",
{

    x <- c(a = 1, b = 2, c = 3)
    expect_error(RemoveByName(x, names(x)),
                 "Removing entries gives empty vector.")
    expect_equal(x[-1], RemoveByName(x, "a"))
})

test_that("RemoveByName works with comma-separated names",
{
    x <- c(a = 1, aa = 2, aaa = 3)
    expect_error(RemoveByName(x, "a; aa;aaa  "),
                 "Removing entries gives empty vector.")
    expect_equal(x[-3], RemoveByName(x, "aaa"))
})

test_that("RemoveByName works with list of names",
{
    x <- c(a = 1, b = 2, c = 3, d = 4, e = 5)
    expect_equal(x[5], RemoveByName(x, list(c("a", "b"), " c,  d")))
    expect_error(RemoveByName(x, as.list(names(x))))
    expect_error(RemoveByName(x, list(names(x))))
})

test_that("RemoveByName data.frame",
{
    x <- data.frame(a = 1, b = 2, c = 3, d = 4, e = 5)
    expect_equal(x[, c("d", "e")], RemoveByName(x, list(c("a", "b"), "c")))
    expect_error(RemoveByName(x, as.list(names(x))),
                 "Removing entries gives empty vector.")
    expect_error(RemoveByName(x, list(names(x))),
                 "Removing entries gives empty vector.")
})

test_that("RemoveByName remove list elements",
{
    x <- list(a = 1, b = 2, c = 3, d = 4, e = 5)
    expect_equal(x[c("d", "e")], RemoveByName(x, list(c("a", "b"), "c")))

    attr(x[["e"]], "foo") <- "bar"
    expect_equal(attr(x[["e"]], "foo"),
                 attr(RemoveByName(x, list(c("a", "b"), "c"))[["e"]], "foo"))
    expect_error(RemoveByName(x, as.list(names(x))),
                 "Removing entries gives empty vector.")
    expect_error(RemoveByName(x, list(names(x))),
                 "Removing entries gives empty vector.")
})

test_that("RemoveByName: list of vectors",
{
    x <- c(a = 1, aa = 2, aaa = 3)
    x <- list(x, x)
    expect_error(RemoveByName(x, "a; aa;aaa  "),
                 "Removing entries gives empty vector.")
    expect_equal(x[[1L]][-3], RemoveByName(x, "aaa")[[1L]])
    expect_equal(x[[2L]][-3], RemoveByName(x, "aaa")[[2L]])
})
