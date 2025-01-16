#' Collect warnings and just warn once.
#'
#' @param expr R expression
#' @param return.list When \code{TRUE} return a list with
#'     list(object,warnings) instead of issuing the warnings.
#'     Otherwise, when \code{FALSE} issue the warnings and return the
#'     object.
#' @return The value of the expression or a list with the value of
#'     the expression and a list of warning messages
#' @export
CollectWarnings <- function(expr, return.list = FALSE){
    ws <- c()
    this.env <- environment()
    result <- suppressWarnings(withCallingHandlers(expr,
                warning = function(w){assign("ws", unique(c(w$message, ws)), this.env)}))
    if (return.list)
        return(list(result, ws))
    else
    {
        for (w in ws)
            warning(w)
        return(result)
    }
}

#' @title ExpectWarning
#' @description Checks to see if the printing of an object causes warnings to
#' appear.
#' @param code The code to evaluate for warnings.
#' @param string The string of text to be searched for in the warnings.
#' @return Returns NULL. Throws an exception if the code is evaluated and no
#' warning is thrown or the string does not appear in the warning.
#' @examples
#' throwsAWarning <- function()
#' {
#'     warning("This is a warning")
#' }
#' ExpectWarning(throwsAWarning(), "a warning")
#' @export
ExpectWarning <- function(code, string)
{
    stopifnot(containsWarning(code, string))
}

#' @title ExpectNoWarning
#' @description Checks to see if the printing of an object does not cause a
#' warning to appear.
#' @param code The code to evaluate for warnings.
#' @param string The string of text to be searched for in the warnings
#' @return Returns NULL. Throws an exception if the code is evaluated and
#' the string appears in the warning.
#' @examples
#' throwsAWarning <- function()
#' {
#'     warning("This is a warning")
#' }
#' ExpectNoWarning(throwsAWarning(), "different warning")
#' @export
ExpectNoWarning <- function(code, string)
{
    stopifnot(!containsWarning(code, string))
}

#' @importFrom utils capture.output
#' @noRd
containsWarning <- function(code, string)
{
    my.warnings <- NULL
    capture.output(withCallingHandlers(print(code), warning = function(e){
        my.warnings <<- c(my.warnings, e$message)
        invokeRestart("muffleWarning")
        }))
    any(grepl(string, my.warnings))
}

containsWarning2 <- function(code, string)
{
    my.warnings <- NULL
    withCallingHandlers(code, warning = function(e){
e$message
        })
    any(grepl(string, my.warnings))
}


#' @title InterceptExceptions
#' @description This function intercepts warning and error messages produced
#' from running \code{expr} and passes them to \code{warning.handler} and
#' \code{error.handler}. Note that execution of expr is stopped after the
#' first error.
#' @param expr The expression whose warnings are to be intercepted.
#' @param warning.handler The function that handles intercepted warnings.
#' @param error.handler The function that handles intercepted errors.
#' @return The value from evaluating \code{expr}.
#' @examples
#' addExclamationMark <- function(warn)
#' {
#'     warning(warn$message, "!")
#' }
#' ## Intercepts a warning and rethrows it with an
#' ## exclamation mark.
#' InterceptExceptions(warning("This is a warning"),
#'                     warning.handler = addExclamationMark)
#'
#' addQuestionMark <- function(error)
#' {
#'     warning(error$message, "?")
#' }
#' ## Intercepts the error from throwsAWarning and rethrows it with an
#' ## exclamation mark.
#' InterceptExceptions(stop("Stop"), error.handler = addQuestionMark)
#' @export
InterceptExceptions <- function(expr, warning.handler = NULL,
                                error.handler = NULL)
{
    withCallingHandlers(withRestarts(expr, muffleStop = function() NULL),
                        warning = function(w) {
                            if (!is.null(warning.handler))
                            {
                                warning.handler(w)
                                invokeRestart("muffleWarning")
                            }
                        },
                        error = function(e) {
                            if (!is.null(error.handler))
                            {
                                error.handler(e)
                                invokeRestart("muffleStop")
                            }
                        })
}

#' @title WarnIfVariablesSelectedFromMultipleDataSets
#' @description Check the contents of the global environment for values of
#' the \code{dataset} attribute attached to any object. This will typically
#' be the environment of a Standard R output in Displayr or Q. Warn if there
#' is more than one data set. The purpose of this is to warn a Displayr
#' user if they have accidentally supplied data from multiple files. Most
#' analysis methods assume the data comes from a single file. Using multiple
#' files is dangerous because the number of cases may not match, and even if
#' the number of cases does match, there is no guarantee that the cases are
#' in the same order.
#' @export
WarnIfVariablesSelectedFromMultipleDataSets <- function() {
    all.data.sets.in.environment <- eapply(.GlobalEnv, FUN = attr, which = "dataset")
    all.data.sets.referenced <- unique(unlist(all.data.sets.in.environment))
    if (length(all.data.sets.referenced) > 1) {
        paste(
            gettextf("The selected data come from more than one Data Set."),
            gettextf("The data sets may have different lengths, and the cases may not be in the same order."),
            gettextf("The data sets used are: %s", paste0(all.data.sets.referenced, collapse = ", "))
        ) |>
            enc2utf8() |>
            warning()
    }
}
