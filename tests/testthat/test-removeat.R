context("RemoveAt")

# This is also tested extensively in flipTables:RemoveAtRowsAndOrColumns
test_that("RemoveAt: Invalid 'at'", {
    expect_error(RemoveAt(1:5, NA),
                 "'at' must contain character (string) or integer values.",
                 fixed = TRUE)
    expect_error(RemoveAt(1:5, c(3, NA)),
                 "'at' must contain character (string) or integer values.",
                 fixed = TRUE)
    expect_error(RemoveAt(1:5, c(3, -1)),
                 "'at' must contain positive integers.",
                 fixed = TRUE)
    expect_error(RemoveAt(1:5, c(3, 10)),
                 "'at' contains a value of 10 which is bigger than the length of 'x'.",
                 fixed = TRUE)
    expect_equal(RemoveAt(1:5, c(3, 1)), c(2, 4, 5))
    expect_error(RemoveAt(1:5, 1.3, NA),
                 "'at' must contain character (string) or integer values.",
                 fixed = TRUE)
})


test_that("RemoveAt.default: Un-named vector with character 'at'", {
    z <- 1:5
    expect_equal(RemoveAt(z, "A"), z)
    expect_equal(length(RemoveAt(z, names(z))), 5)
    expect_equal(RemoveAt(z, "2,3", split = ","), z[-(2:3)])
})


test_that("RemoveAt.default: Un-named vector with integer 'at'", {
    z <- 1:5
    expect_equal(RemoveAt(z, 1), z[-1])
    expect_equal(length(RemoveAt(z, 1:5)), 0)
})


test_that("RemoveAt.default: Named vector", {
    z <- 1:5
    names(z) = LETTERS[1:5]
    expect_equal(RemoveAt(z, "A"), z[-1])
    expect_equal(length(RemoveAt(z, names(z))), 0)
    expect_equal(RemoveAt(z, "2,3", split = ","), z[-(2:3)])
})

test_that("RemoveAt.default: list", {
    z <- as.list(1:5)
    names(z) = LETTERS[1:5]
    expect_equal(RemoveAt(z, "A"), z[-1])
    expect_equal(length(RemoveAt(z, names(z))), 0)
    expect_equal(RemoveAt(z, "2,3", split = ","), as.list(z[-(2:3)]))

  x = list(1:10, 1:10)
  expect_equal(RemoveAt(x[[1]], at = "sdfds"), x[[1]])

})


test_that("RemoveAt.default: array", {
    z <- array(1:8, dim = c(2,2,2), dimnames = list(LETTERS[1:2], LETTERS[3:4], LETTERS[5:6]))
    expect_equal(RemoveAt(z, "A"), z[-1,,, drop = FALSE])
    expect_equal(RemoveAt(z, "C"), z[, -1,, drop = FALSE])
    expect_equal(RemoveAt(z, MARGIN = 1, "C"), z)
    expect_equal(RemoveAt(z, MARGIN = 2, "C"), z[, -1,, drop = FALSE])
    expect_equal(RemoveAt(z, MARGIN = 2, "1"), z[, -1,, drop = FALSE])

    z <- array(1:8, dim = c(2,2,2), dimnames = list(LETTERS[1:2], LETTERS[3:4], NULL))
    expect_equal(RemoveAt(z, "A"), z[-1,,, drop = FALSE])
    expect_equal(RemoveAt(z, "C"), z[, -1,, drop = FALSE])
    expect_equal(RemoveAt(z, MARGIN = 1, "C"), z)
    expect_equal(RemoveAt(z, MARGIN = 2, "C"), z[, -1,, drop = FALSE])

    z <- array(1:8, dim = c(2,2,2), dimnames = list(NULL, LETTERS[3:4], NULL))
    expect_equal(RemoveAt(z, "A"), z)
    expect_equal(RemoveAt(z, "C"), z[, -1,, drop = FALSE])
    expect_equal(RemoveAt(z, "C", MARGIN = 1), z)
    expect_equal(RemoveAt(z, MARGIN = 2, "C"), z[, -1,, drop = FALSE])

})

test_that("RemoveAt: matrix", {
    z <- matrix(1:4, 2, dimnames = list(LETTERS[1:2], LETTERS[3:4]))
    expect_equal(RemoveAt(z, "A"), z[-1,, drop = FALSE])
    expect_equal(RemoveAt(z, "A", MARGIN = 1), z[-1,, drop = FALSE])
    expect_equal(RemoveAt(z, "A", MARGIN = 2), z)


    x <- matrix(NA, 3, 3, dimnames = list(LETTERS[1:3],LETTERS[1:3]))
    z <- RemoveAt(x, list("A", c("C","A")))
    expect_equal(z, x[2:3, 2, drop = FALSE])
    z <- RemoveAt(x, list("A", c("C","A")), MARGIN = 1:2)
    expect_equal(z, x[2:3, 2, drop = FALSE])

    dat <- structure(c(39.9370629370629, 45.9459459459459, 43.1311475409836,
        54.2222222222222, 43.7954545454545, 42, 40.2748538011696, 44.6754966887417,
        42.3385093167702), .Dim = c(9L, 1L), .Dimnames = list(c("Coca Cola ",
        "Diet Coke", "Coke Zero", "Pepsi Light ", "Pepsi Max", "Pepsi ",
        "NET Sugarred", "NET Sugarless", "NET"), "Age in years"), statistic = "Average", name = "Q3. Age in years by Preferred cola", questions = c("Q3. Age in years",
        "Preferred cola"))

    z <- RemoveAt(dat, at = list("Pepsi", c("SUM", "NET")))
expect_equal(nrow(z), 8)
          })

test_that("RemoveAt: data.frame", {
    z <- data.frame(C = 1:2, D = 3:4)
    rownames(z) = LETTERS[1:2]
    expect_equal(RemoveAt(z, "C"), z[, -1, drop = FALSE])
    expect_equal(RemoveAt(z, "A", MARGIN = 1), z[-1,])
    expect_equal(RemoveAt(z, "A", MARGIN = 2), z)
    expect_equal(RemoveAt(z, list(NULL, 2)), z[,-2, drop = FALSE])
    expect_equal(RemoveAt(z, list(NULL, "1"), split = TRUE), z[,-1, drop = FALSE])


    x <- matrix(NA, 3, 3, dimnames = list(LETTERS[1:3],LETTERS[1:3]))
    z <- RemoveAt(x, list("A", c("C","A")))
    expect_equal(z, x[2:3, 2, drop = FALSE])
    z <- RemoveAt(x, list("A", c("C","A")), MARGIN = 1:2)
    expect_equal(z, x[2:3, 2, drop = FALSE])
    z <- RemoveAt(x, list(c(1,3), 2))
    expect_equal(z, structure(c(NA, NA), .Dim = 1:2,
        .Dimnames = list("B", c("A", "C"))))
    dat <- structure(list(Q6_A = structure(c(3L, 5L, 5L, 6L, 4L, 1L, 3L,
        6L, 5L, 6L, 6L, 5L, 5L, 4L, 3L, 6L, 6L, 5L, 5L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Coca Cola", .Names = "Q6_A")),
        Q6_B = structure(c(5L, 2L, 6L, 3L, 6L, 1L, 4L, 3L, 5L, 6L,
        2L, 3L, 3L, 3L, 6L, 5L, 5L, 3L, 3L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Diet Coke", .Names = "Q6_B")),
        Q6_C = structure(c(3L, 5L, 3L, 3L, 4L, 1L, 5L, 5L, 1L, 6L,
        2L, 3L, 3L, 5L, 3L, 5L, 5L, 3L, 5L, 6L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Coke Zero", .Names = "Q6_C")),
        Q6_D = structure(c(4L, 5L, 4L, 3L, 4L, 1L, 3L, 4L, 5L, 5L,
        6L, 5L, 4L, 4L, 5L, 5L, 3L, 5L, 4L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Pepsi", .Names = "Q6_D")),
        Q6_E = structure(c(2L, 4L, 2L, 3L, 6L, 6L, 3L, 3L, 5L, 5L,
        2L, 3L, 3L, 4L, 6L, 1L, 2L, 3L, 4L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Pepsi Light", .Names = "Q6_E")),
        Q6_F = structure(c(6L, 6L, 2L, 3L, 3L, 6L, 3L, 5L, 4L, 4L,
        2L, 3L, 3L, 5L, 3L, 1L, 2L, 3L, 4L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Pepsi Max", .Names = "Q6_F"))), .Names = c("Q6_A",
        "Q6_B", "Q6_C", "Q6_D", "Q6_E", "Q6_F"), row.names = c(NA, 20L
                                                           ), class = "data.frame")
        attr(dat, "statistic") <- "means"

    out <- RemoveAt(dat, at = list(NULL, c("Q6_B", "Q6_F", "Q6_C", "Q6_D", "Q6_A")))
    expect_equal(as.vector(attributes(out[[1]])$label), "Q6. Pepsi Light" )
    expect_equal(dim(out), c(nrow(dat), 1L))
    expect_equal(attr(out, "statistic"), attr(dat, "statistic"))

    dat2 <- structure(list(Income = structure(c(6L, 6L, 2L, 3L, 3L, 6L, 6L),
            .Label = c("Less than $15,000", "$15,001 to $30,000",
            "$30,001 to $45,000", "$60,001 to $90,000", "$90,001 to $120,000",
            "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more"),
            class = "factor", label = structure("Income", .Names = "d2"))), .Names = "Income",
            row.names = sprintf("Num %d", 1:7),
            class = "data.frame", scatter.variable.indices = structure(c(NA, 1, NA, NA),
            .Names = c("x", "y", "sizes", "colors")))
    out2 <- RemoveAt(dat2, list(c("NET", "SUM"), c("NET")), 1:2)
    expect_true(is.data.frame(out2))
})



test_that("RemoveAt: another vector and a list",
{
    dat <- structure(c(39.9370629370629, 45.9459459459459, 43.1311475409836,
        54.2222222222222, 43.7954545454545, 42, 40.2748538011696, 44.6754966887417,
        42.3385093167702), .Dim = c(9L, 1L), .Dimnames = list(c("Coca Cola ",
        "Diet Coke", "a", "Pepsi Light ", "Pepsi Max", "Pepsi ",
        "NET Sugarred", "NET Sugarless", "NET"), "Age in years"), statistic = "Average", name = "Q3. Age in years by Preferred cola", questions = c("Q3. Age in years",
        "Preferred cola"))

    x <- list(dat, c(a = 1, aa = 2, aaa = 3))

    expect_equal(dim(RemoveAt(x[[1]], at = "a; aa ", split = "[;,]")), dim(dat) - c(1, 0))
    expect_equal(RemoveAt(x[[2]], at = "a; aa ", split = "[;,]"), x[[2L]][3])

    res <- RemoveAt(dat, at = list(1:3, NULL))
    expect_equal(dimnames(res), list(c("Pepsi Light ", "Pepsi Max", "Pepsi ",
            "NET Sugarred", "NET Sugarless", "NET"), "Age in years"))
})

# test_that("RemoveAtCharacterElements", # This is more extensively tested in flipTables via the functions for removing rows and columns
#           {
#               expect_equal(RemoveCharacterElements(LETTERS[1:5]), LETTERS[1:5])
#               expect_equal(RemoveCharacterElements(LETTERS[1:5], names.to.remove = "C"), LETTERS[1:5][-3])
#           })
#
