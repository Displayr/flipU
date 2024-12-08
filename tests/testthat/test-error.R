
test_that("Errors are constructed correctly", {
    # Error condition has the correct structure
    user.error <- Stop("Incorrect selection") |> capture_error()
    user.error |> inherits(what = "UserError") |> expect_true()
    user.error |> inherits(what = "error") |> expect_true()
    user.error |> inherits(what = "condition") |> expect_true()
    user.error[["message"]] |> expect_equal("Incorrect selection")
    # Ellipsis works as expected
    user.error <- Stop("Incorrect selection ", "used over ", "the dots") |> capture_error()
    user.error[["message"]] |> expect_equal("Incorrect selection used over the dots")
    inherits(user.error, "UserError") |> expect_true()
    foo <- function(x) {
        Stop("Incorrect selection ", x, " used over ", "the dots")
    }
    foo.error <- foo(1) |> capture_error()
    foo.error[["message"]] |> expect_equal("Incorrect selection 1 used over the dots")
})
