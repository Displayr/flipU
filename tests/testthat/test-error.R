
test_that("Errors are constructed correctly", {
    # Basic error condition constructed
    Stop("This can't be done") |>
        expect_condition(
            class = "userError",

        )
    # Error condition has the correct structure
    user.error <- Stop("Incorrect selection") |> capture_error()
    user.error |> inherits(what = "UserError") |> expect_true()
    user.error |> inherits(what = "error") |> expect_true()
    user.error |> inherits(what = "condition") |> expect_true()
    user.error[["message"]] |> expect_equal("Incorrect selection")
})
