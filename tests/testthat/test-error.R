
test_that("Errors are constructed correctly", {
    # Error condition has the correct structure
    user.error <- Stop("Incorrect selection") |> capture_error()
    user.error |> inherits(what = "UserError") |> expect_true()
    user.error |> inherits(what = "error") |> expect_true()
    user.error |> inherits(what = "condition") |> expect_true()
    user.error[["message"]] |> expect_equal("Incorrect selection")
})
