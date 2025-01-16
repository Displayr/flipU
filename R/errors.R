#' @title CheckVariableLengths
#' @param variable.list A list of variables from a form.dropBox control with
#'     multiple inputs.
#' @param list.name The name used to refer to the variable list in the error
#'     message.
#' @return Nothing is returned, but an error is thrown if the variables have
#'     differing lengths.
#' @export
CheckVariableLengths <- function(variable.list, list.name)
{
    list.lengths <- lengths(variable.list)
    if (any(list.lengths != list.lengths[1])) {
        msg <- paste(
            gettextf("%s have different lengths .", list.name),
            gettextf("Please ensure that they are from the same data set or have the same length.")
        )
        StopUserError(msg, domain = NA)
    }
}

#' Error condition with special class to identify user errors
#'
#' The inputs are not offered for translation, as they are assumed to be constructed using
#' \code{gettextf} or similar functions. That is, the inputs have already been offered for
#' translation before used as arguments.
#' @param call. Unused parameter, used to absorb usage from previous calls using stop
#' @inheritParams base::stop
#' @export
StopUserError <- function(..., call. = FALSE) {
    # Using a custom error condition and throwing using `stop` assumes the message
    # is already constructed from the custom error condition, so call. parameter is redundant
    call.used <- match.call()[["call."]]
    if (!is.null(call.used)) {
        gettextf("call. argument not supported in Stop, the call is never part of the error message") |>
            enc2utf8() |>
            warning(domain = NA)
    }
    .makeMessage(..., domain = NA) |>
        enc2utf8() |>
        errorCondition(class = "UserError") |>
        stop()
}
