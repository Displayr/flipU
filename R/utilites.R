#' \code{ConvertCommaSeparatedStringToVector}
#'
#' Converts a string containing commas into a vector, trimming whitepaces along
#' the way.
#' @param string A \code{\link{character}} to be converted.
#' @param split A \code{\link{character}} vector containing regular expressions
#'   to be used in splitting. Where multiple entries are in the vector they are
#'   recycled along the vector of \code{string} (i.e., they are not all used as
#'   delimiters).
#' @param text.qualifier A character that is placed at the start and end of
#'   text (after the start split and before the end split), so that it is never
#'   split even if it contains the split character.
#' @return A \code{vector} of \code{character}s.
#' @importFrom utils localeToCharset
#' @export
ConvertCommaSeparatedStringToVector <- function(string, split = ",",
                                                text.qualifier = NULL)
{
    # Substitute smart quotes for normal quotes
    patt <- if ("UTF-8" %in% localeToCharset()) '[\u201C\u201D\u201E]'  # linux (utf-8 encoding)
            else                                '[\x93\x94\x84]'        # windows (latin-1)
    string <- gsub(patt, "\"", string)

    if (getIsStringFromControl(string))
        return(string)

    split.text <- unlist(strsplit(string, split))

    result <- character(0)
    if (!is.null(text.qualifier) && length(split.text) > 0)
    {
        matches <- gregexpr(split, string)
        delim <- sapply(matches[[1]], function (x) {
            substr(string, x, x)
        })

        n.parts <- length(split.text)
        open.quote.index <- NA
        for (i in 1:n.parts)
        {
            t <- TrimWhitespace(split.text[i])
            nc <- nchar(t)
            if (is.na(open.quote.index))
            {
                if (substr(t, 1, 1) == "\"")
                {
                    if (nc > 1 && substr(t, nc, nc) == "\"")
                        result <- c(result, substr(t, 2, nc - 1))
                    else
                        open.quote.index <- i
                }
                else
                    result <- c(result, t)
            }
            else if (substr(t, nc, nc) == "\"")
            {
                composite <- paste0(split.text[open.quote.index:i],
                                    c(delim[open.quote.index:(i - 1)], ""),
                                    collapse = "")
                composite <- TrimWhitespace(composite)
                composite <- substr(composite, 2, nchar(composite) - 1)
                result <- c(result, composite)
                open.quote.index <- NA
            }
        }
        if (!is.na(open.quote.index)) # quotes never closed
            result <- c(result, split.text[open.quote.index:n.parts])
    }
    else
        result <- split.text

    return(TrimWhitespace(result))
}

#' \code{ParseTextList}
#'
#' Parse a string of the form "pet: cat, dog, rat", i.e., list name followed
#' by a colon and a comma-separated list of elements. Double quotes are used
#' to escape : and ,
#'
#' @param string A \code{\link{character}} to be converted.
#' @return A list containing the name and elements.
#' @export
ParseTextList <- function(string)
{
    matches <- gregexpr("^\\s*\".*?\"\\s*:", string, perl = TRUE)
    if (matches[[1]] != -1)
    {
        match.length <- attr(matches[[1]], "match.length")
        name <- TrimWhitespace(substr(string, 1, match.length - 1))
        name <- substr(name, 2, nchar(name) - 1) # remove double quotes
        element.string <- substr(string, match.length + 1, nchar(string))
    }
    else
    {
        matches <- gregexpr("^.*?:", string)
        if (matches[[1]] != -1)
        {
            match.length <- attr(matches[[1]], "match.length")
            name <- TrimWhitespace(substr(string, 1, match.length - 1))
            element.string <- substr(string, match.length + 1, nchar(string))
        }
        else
        {
            name <- character(0)
            element.string <- character(0)
        }
    }

    elements <- ConvertCommaSeparatedStringToVector(element.string,
                                                    text.qualifier = "\"")
    list(name = name, elements = elements)
}

#' \code{UniquePlaceholders}
#'
#' Generates a vector of unique alphanumeric characters, which can be used as
#' placeholders in text manipulation.
#' @param n.placeholders The length of the vector.
#' @param n.characters The number of characters in each entry.
#' @param padding Prefix and suffix characters for the placeholders.
#' @return A vector of unique alphanumeric characters.
#' @export
UniquePlaceholders <- function(n.placeholders, n.characters = 64, padding = "")
{
    if (n.placeholders == 0)
        return(character(0))

    result <- character(0)
    while (length(result) < n.placeholders)
    {
        candidate <- paste0(sample(c(LETTERS, letters, 0:9), 64, TRUE),
                            collapse = "")
        if (!(candidate %in% result)) # unlikely false but check anyway
            result <- c(result, candidate)
    }
    paste0(padding, result, padding)
}

#' \code{TrimLeadingWhitespace}
#'
#' Removes whitespace (e.g.,spaces, tab characters) from the beginning of a string.
#' @param x A \code{\link{character}} vector that may contain whitespace.
#' @return A \code{character}.
#' @export
TrimLeadingWhitespace <- function (x) {
    if (is.list(x))
        x <- unlist(x)
    if (length(x) > 1)
        return(as.vector(unlist(sapply(x, TrimLeadingWhitespace))))

    x <- gsub("\\xA0", " ", x)
    result <- gsub("^\\s+", "", x)
    if (is.null(names(x)))
        names(result) <- NULL
    result
}

#' \code{TrimTrailingWhitespace}
#'
#' Removes whitespace (e.g.,spaces, tab characters) from the end of a string.
#' @param x A \code{\link{character}} vector that may contain whitespace.
#' @return A \code{character}.
#' @export
TrimTrailingWhitespace <- function (x) {
    if (is.list(x))
        x <- unlist(x)
    if (length(x) > 1)
        return(as.vector(unlist(sapply(x, TrimTrailingWhitespace))))

    x <- gsub("\\xA0", " ", x)
    result <- gsub("\\s+$", "", x)
    if (is.null(names(x)))
        names(result) <- NULL
    result
}


#' \code{TrimWhitespace}
#'
#' Removes whitespace (e.g.,spaces, tab characters) from the beginning or end of a string.
#' @param x A \code{\link{character}} vector that may contain whitespace.
#' @return A \code{character}.
#' @export
TrimWhitespace <- function (x){
    if (is.list(x))
        x <- unlist(x)
    if (length(x) > 1)
        return(as.vector(unlist(sapply(x, TrimWhitespace))))

    # Convert non-breaking (hexcode: A0) space to normal space
    # In Windows, this automatically matches white space (\s) but not in Ubuntu
    x <- gsub("\\xA0", " ", x)
    result <- gsub("^\\s+|\\s+$", "", x)
    if (is.null(names(x)))
        names(result) <- NULL
    result
}

#' @title TrimCharacterAndWhitespace
#' @description Trim single occurrances of leading and trailing characters from
#'   the beginning and end of a string. All removes any whitespace.
#' @param x A \code{\link{character}} vector that may contain characters to be trimmed.
#' @param characters A \code{\link{character}} vector of the characters to look for.
#' @return A \code{character}.
#' @export
TrimCharacterAndWhitespace <- function(x, characters)
{
    x <- TrimWhitespace(x)
    ind <- substr(x, 1, 1) %in% characters
    x[ind] <- substr(x[ind], 2, nchar(x[ind]))
    nchar.x <- nchar(x)
    ind <- substr(x, nchar.x, nchar.x) %in% characters
    x[ind] <- substr(x[ind], 1, nchar.x[ind] - 1)
    TrimWhitespace(x)
}

#' De-duplicate names by appending characters
#'
#' This is similar to \code{make.unique} but is useful for making less ugly names
#' @param names A character vector
#' @param suffix A character string to append to de-duplicated name.
#'   If name with the prefix is already used then another copy of the prefix and suffix is added.
#' @param prefix A character string to prepend to de-duplicated name.
#'   If name with the prefix is already used then another copy of the prefix and suffix is added.
#' @export
MakeUniqueNames <- function(names, suffix = " ", prefix = "")
{
    ind.dup <- which(duplicated(names))
    if (nchar(suffix) < 1)
        stop("'suffix' cannot be empty")
    for (i in ind.dup)
    {
        new.name <- paste0(prefix, names[i], suffix)
        while(i > 2 && any(names[-i] == new.name))
            new.name <- paste0(prefix, new.name, suffix)
        names[i] <- new.name
    }
    return(names)
}

#' @title Escape any special regex characters in a character vector
#' @description Used to escape special characters from text before passing it
#'   to regex functions such as gsub.
#' @param text A character vector
#' @export
EscapeRegexSymbols <- function(text)
{
    # backslash needs to be first
    regex.symbols <- c("\\", ".", "|", "(", ")", "[", "]", "{", "}", "^", "$",
                       "*", "+", "?")

    for (symb in regex.symbols)
        text <- gsub(symb, paste0("\\", symb), text, fixed = TRUE)
    text
}

#' @title Determine the dimensions of an array or vector
#' @description Used to give the dimensions of any array or vector.
#' A vector is considered 1 dimensional and this function will return its
#' length.
#' @param x The input to be inspected and have its dimensions returned
#' @return An integer vector containing either the length of the vector
#' or the integer dimensions of the array.
#' @seealso Extension for \code{dim} in the same vein as \code{ncol} to \code{NCOL}.
#' @export
DIM <- function(x)
{
    x.dim <- dim(x)
    if (is.null(x.dim))
        return(length(x))
    x.dim
}

#' Identify Q Table objects
#'
#' Inspects for the qTable class or checks if attributes exist
#' to identify legacy Q Table with the \code{"questions"} and
#' and \code{"name"} attributes
#'
#' @note An attribute \code{"statistic"} is not guaranteed to be present, as
#' the names of the statistics computed may be present in the dimnames
#"
#' @param x Input to check if belonging to Q Table class or containing Q Table attributes
#' @return Logical value \code{TRUE} if the input is identified as being a Q Table,
#'         \code{FALSE} otherwise
#' @export
IsQTable <- function(x)
{
    inherits(x, "qTable") ||
    (!is.null(attr(x, "questions")) && !is.null(attr(x, "name")))
}


#' Check that variables supplied belong to a certain question type.
#'
#' Helper function to use in Standard R items which require the user
#' to select variables of a certain question type / variable set
#' structure.
#'
#' @param variables A list containing the variables (or questions) to check.
#' @param required.type A string indicating the required questiontype value.
#'        Should be one of: "PickAny", "PickOne", "PickOneMulti", "Number",
#'        "NumberMulti", "NumberGrid", "PickAnyGrid", "Text", "TextMulti",
#'        "Ranking", or "Experiment".
#' @param message.prefix Text to display in the error message before the
#'        required question type / structure.
#' @param message.suffix Text to display at the end of the message.
#'
#' @export
RequireQuestionType <- function(variables, required.type, message.prefix, message.suffix) {
    found.question.types <- vapply(variables,
                                   FUN = attr,
                                   FUN.VALUE = character(1),
                                   which = "questiontype",
                                   exact = TRUE)

    if (any(found.question.types != required.type)) {
        # productName is an environment variable present in Q/Displayr
        product.name <- get0("productName", ifnotfound = "Displayr")
        structure.name <- GetTranslatedQuestionType(required.type, product.name)
        structure <- if (product.name == "Q") " question"  else " variable set"
        message.middle <- paste0(structure.name, structure)
        stop(message.prefix, message.middle, message.suffix)
    }
}

#' Get the Q/Displayr name for an entry in a questiontype attribute of a variable.
#'
#' Helper function to use in Standard R items which require the user
#' to select variables of a certain question type / variable set
#' structure.
#'
#' @param type A string indicating the required questiontype value.
#'        Should be one of: "PickAny", "PickOne", "PickOneMulti", "Number",
#'        "NumberMulti", "NumberGrid", "PickAnyGrid", "Text", "TextMulti",
#'        "Ranking", or "Experiment".
#' @param product.name A string, which should be either Displayr or Q
#'
#' @export
GetTranslatedQuestionType <- function(type, product.name) {
    stopifnot("The product name should be either Q or Displayr." = product.name %in% c("Displayr", "Q"))
    question.types <- list(Q = list("PickAny" = "Pick Any",
                                "PickOne" = "Pick One",
                                "PickOneMulti" = "Pick One - Multi",
                                "Number" = "Number",
                                "NumberMulti" = "Number - Multi",
                                "NumberGrid" = "Number - Grid",
                                "PickAnyGrid" = "Pick Any - Grid",
                                "Text" = "Text",
                                "TextMulti" = "Text - Multi",
                                "Ranking" = "Ranking",
                                "Experiment" = "Experiment"),
                       Displayr = list("PickAny" = "Binary - Multi",
                                "PickOne" = "Nominal/Ordinal",
                                "PickOneMulti" = "Nominal/Ordinal - Multi",
                                "Number" = "Numeric",
                                "NumberMulti" = "Numeric - Multi",
                                "NumberGrid" = "Numeric - Grid",
                                "PickAnyGrid" = "Binary - Grid",
                                "Text" = "Text",
                                "TextMulti" = "Text - Multi",
                                "Ranking" = "Ranking",
                                "Experiment" = "Experiment"))
    structure.name <- question.types[[product.name]][[type]]
    if (is.null(structure.name))
        stop(type, " is not a valid Question Type to supply.")
    structure.name
}

#' Specify that a string was generated by a Displayr control
#'
#' Add an attribute to a character vector called is.control
#' which can tell other functions that a string originally
#' came from a combo box or listbox control.
#' @param control.output A character vector to which the attribute
#' should be attached.
#'
#' @export
StringIsFromControl <- function(control.output) {
    if (!is.null(control.output))
        attr(control.output, "is.control") <- TRUE
    control.output
}

#' @export
#' @rdname StringIsFromControl
SetIsStringIsFromControl <- StringIsFromControl

#' Check whether a character vector was generated by a Displayr control.
#' @noRd
getIsStringFromControl <- function(x) {
    isTRUE(attr(x, "is.control"))
}


#' Ensure that variables being returned to Displayr are
#' as small as they can be.
#'
#' Given a vector, matrix or data frame containing variables
#' which are to be added to a Data Set in Displayr,
#' remove any unecessary attributes that would cause
#' the variables to be larger (in bytes) than they
#' need to be. Variable attributes are not accessible
#' once the variable has been added to a data set. The 
#' only exception is attributes for factors, which
#' should be kept. If input is a data frame, return
#' it as a list, since data frames must have row names
#' that do not need to be kept.
#' @param x A vector, matrix, data frame, or list
#' containing variables that are to be added to a
#' Displayr Data Set.
#' @export
TidyDataForRVariables <- function(x) {
    if (is.list(x)) {
        return(lapply(x, tidyVariableForDisplayr))
    }

    tidyVariableForDisplayr(x)
}

tidyVariableForDisplayr <- function(x) {
    if (is.matrix(x)) {
        rownames(x) <- NULL
    }

    if (is.vector(x)) {
        names(x) <- NULL
    }

    x
}