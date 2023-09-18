#' \code{RemoveAt}
#'
#' @description Removes one or more elements from an object. Dimensions are never
#'              If all elements are removed from a vector, then a 0-length vector is returned.
#' @param x The object from which the elements are to be removed.
#' @param at A \code{\link{vector}} or indicating which elements are to be removed.
#' When \code{at} is \code{\link{character}} elements with this name are removed. When \code{at}
#' is \code{\link{integer}}, elements with this position are removed. If \code{MARGIN} contains two
#' or more values, \code{at} may be a list, where each element of the list contains a vector.
#' @param MARGIN An \code{\link{vector}} of integers giving the dimensions from which elements should be removed.
#' @param split Character delimiter to split \code{at} when \code{at} is a
#' \code{\link{character}}. Default is NULL which means that no splitting
#' occurs. See \code{\link{strsplit}}.
#' @param ignore.case Ignores case if and when removing elements based on their names.
#' @return An object of the same class as \code{x}.
#' @export
RemoveAt <- function(x, at = NULL, MARGIN = NULL, ignore.case = TRUE, split = NULL)
{
    UseMethod("RemoveAt")
}

#' @export
RemoveAt.default <- function(x, at = NULL, MARGIN = NULL, ignore.case = TRUE, split = NULL)
{
    if (is.null(at))
        return(x)
    at <- parseIndex(unlist(at), split = split)
    if (is.character(at) && (is.null(names(x)) || all(!nzchar(at))))
        return(x)
    out <- x[indicesToRetain(names(x), at, length(x), ignore.case = ignore.case, split = split)]
    # Subscripting QTables (verbs:::`[.QTable`) already updates attributes
    if (!inherits(x, "QTable")) out <- CopyAttributes(out, x)
    out
}



#' @describeIn RemoveAt Where only a \code{at} is provided, it is assumed
#' to be the variables of a data frame. Otherwise, the first \code{MARGIN}
#' is assumed the rows and the second the columns.
#' @export
RemoveAt.data.frame <- function(x, at = NULL, MARGIN = NULL, ignore.case = TRUE, split = NULL)
{
    if (!is.list(at) && is.null(MARGIN))
        return(RemoveAt.default(x, at, MARGIN, ignore.case, split))
    RemoveAt.array(x, at, MARGIN, ignore.case, split)
}

#' @describeIn RemoveAt If \code{MARGIN} is not provided, the \code{at} argument
#'                      is used along all margins. If \code{at} is a list, then each
#'                      list element is sequentially used along each \code{MARGIN}
#'                      If \code{at} is not a list, then it all elements inside is
#'                      are removed across all margins.
#' @export
RemoveAt.array <- function(x, at = NULL, MARGIN = NULL, ignore.case = TRUE, split = NULL)
{
    if (is.null(MARGIN))
         MARGIN <- 1:length(dim(x))
    if (removeArrayInputsBad(x, at, MARGIN))
        return(x)
    out <- x
    for (m in seq_along(MARGIN))
    {
        a <- if (is.list(at)) at[[m]] else at
        if (length(a) == 0)
            out <- out
        else
            out <- removeFromDimension(out, a, MARGIN[m], ignore.case, split)
    }
    # Subscripting QTables (verbs:::`[.QTable`) already updates attributes
    if (!inherits(x, "QTable")) out <- CopyAttributes(out, x)
    out
}

#' @describeIn RemoveAt Applies the array method
#' @export
RemoveAt.matrix <- function(x, at = NULL, MARGIN = NULL, ignore.case = TRUE, split = NULL)
{
    RemoveAt.array(x, at, MARGIN, ignore.case, split)
}

#' @describeIn RemoveAt If any characters are used in \code{at} but the \code{ftable}
#'                      doesn't have dimnames then the original \code{ftable} is returned.
#'                      If integer references are used, then the appropriate dim is
#'                      removed using the array method.
#'                      If the \code{ftable} has dimnames and characters are used \code{at}
#'                      then array method is used so that the dimnames of \code{ftable}
#'                      are used to determine the rows or columns in the removal.
#'                      In all cases, the \code{row.var} and \code{col.var} attributes
#'                      are not retained in the output.
#' @export
RemoveAt.ftable <- function(x, at = NULL, MARGIN = NULL, ignore.case = TRUE, split = NULL)
{
    character.requests <- is.character(at) ||
                          (is.list(at) && any(vapply(at, is.character, logical(1L))))
    no.dimnames <- is.null(dimnames(x))
    if (no.dimnames && character.requests) return(x)
    out <- RemoveAt.array(x, at, MARGIN, ignore.case, split)
    attr(out, "row.vars") <- attr(out, "col.vars") <- NULL
    out
}

removeArrayInputsBad <- function(x, at, MARGIN)
{
    if (is.null(at))
        return(TRUE)
    if (is.list(at) && length(MARGIN) != length(at))
        stop("If 'at' is a list, it must have the same number of elements as MARGIN.")
    dimnames <- dimnames(x)
    is.character(at) && all(is.null(dimnames[MARGIN]))
}

#' @inherit RemoveAt
removeFromDimension <- function(x, at = NULL, MARGIN = 1L, ignore.case = TRUE, split = NULL)
{
    at <- parseIndex(at)
    names <- dimnames(x)[[MARGIN]]
    if (is.character(at) && is.null(names))
        return(x)
    dims <- dim(x)
    args <- c(list(x), rep(alist(, )[1L], length(dims)), drop = FALSE)
    # Indices or Logical
    args[[MARGIN + 1L]] <- indicesToRetain(names, at, dims[MARGIN], ignore.case, split)
    do.call(`[`, args)
}

determineIndicesFromChar <- function(at, names, ignore.case, split, trim.whitespace = TRUE)
{
    if (trim.whitespace)
        at <- TrimWhitespace(at)
    if (ignore.case)
    {
        at <- tolower(at)
        names <- tolower(names)
    }
    if (!is.null(split))
        at <- ConvertCommaSeparatedStringToVector(at, split)
    !names %in% at
}

#' indicesToRetain
#'
#' Worker function for \code{RemoveAt}.
#' @inherit RemoveAt
#' @param length.x The length of the object that is to be subscripted.
#' @param names The names of the elements.
indicesToRetain <- function(names, at, length.x, ignore.case = TRUE, split = NULL)
{
    names <- trimws(names)
    if (is.null(names))
        return(1:length.x)
    # 'at' is character and able to represent a variable
    if (is.character(at))
        return(determineIndicesFromChar(at, names, ignore.case, split))
    # 'at' is numeric
    if (anyNA(at) || !AllIntegers(at))
        stop("'at' must contain character (string) or integer values.")
    if (any(at < 1))
        stop("'at' must contain positive integers.")
    if (max(at) > length.x)
        stop("'at' contains a value of ", max(at), " which is bigger than the length of 'x'.")
    -at
}


parseIndex <- function(index, split = NULL)
{
    if (!is.character(index))
        return(index)
    if (!is.null(split))
        index <- ConvertCommaSeparatedStringToVector(index, split)
    tmp <- suppressWarnings(as.numeric(index))
    if (all(!is.na(tmp))) tmp else index
}
