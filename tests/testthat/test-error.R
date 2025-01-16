
test_that("Errors are constructed correctly", {
    # Error condition has the correct structure
    user.error <- StopUserError("Incorrect selection") |> capture_error()
    user.error |> inherits(what = "UserError") |> expect_true()
    user.error |> inherits(what = "error") |> expect_true()
    user.error |> inherits(what = "condition") |> expect_true()
    user.error[["message"]] |> expect_equal("Incorrect selection")
    # Ellipsis works as expected
    user.error <- StopUserError("Incorrect selection ", "used over ", "the dots") |> capture_error()
    user.error[["message"]] |> expect_equal("Incorrect selection used over the dots")
    inherits(user.error, "UserError") |> expect_true()
    foo <- function(x) {
        StopUserError("Incorrect selection ", x, " used over ", "the dots")
    }
    dots.working <- foo(1) |> capture_error()
    dots.working[["message"]] |> expect_equal("Incorrect selection 1 used over the dots")
    # Warning if using call.
    warning.option <- getOption("warn")
    options(warn = 2L)
    on.exit(options(warn = warning.option))
    StopUserError("Incorrect selection", call. = FALSE) |> expect_error("call. argument not supported in Stop")
})
