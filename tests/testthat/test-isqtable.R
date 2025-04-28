# Testing ability to identify QTables

not.q.table <- array(1:12, dim = 3:4, dimnames = list(LETTERS[1:3], letters[1:4]))
legacy.q.table <- not.q.table
attr(legacy.q.table, "questions") <- c("foo", "bar")
attr(legacy.q.table, "name") <- "cool.q.table"
classed.q.table <- not.q.table
class(classed.q.table) <- "QTable"

test_that("Legacy Q Tables identified", {
    expect_true(IsQTable(legacy.q.table))
})

test_that("Other types not identified as Q Table", {
    expect_false(IsQTable(not.q.table))
    not.q.tables <- list(runif(5), data.frame(runif(5)), 1:5, letters, as.logical(0:1))
    for (input in not.q.tables)
        expect_false(IsQTable(input))
})

test_that("Classed Q Tables identified", {
    expect_true(IsQTable(classed.q.table))
})
