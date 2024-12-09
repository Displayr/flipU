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
    if (length(unique(sapply(variable.list, length))) > 1)
        stop(list.name , " have differing lengths. Please ensure ",
             "that they are from the same data set or have the same length.")
}

#' Error condition with special class to identify user errors
#' @param call. Unused parameter, used to absorb usage from previous calls using stop
#' @inheritParams base::stop
#' @export
Stop <- function(..., call. = FALSE, domain = NULL) {
    # Using a custom error condition and throwing using `stop` assumes the message
    # is already constructed from the custom error condition, so call. parameter is redundant
    call.used <- match.call()[["call."]]
    if (is.logical(call.used)) {
        warning("call. argument not supported in Stop, the call is never part of the error message")
    }
    message <- .makeMessage(..., domain = domain)
    errorCondition(message, class = "UserError") |> stop()
}
