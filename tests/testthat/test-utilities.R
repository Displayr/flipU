
test_that("ConvertCommaSeparatedStringToVector",
          {
              expect_equal(ConvertCommaSeparatedStringToVector("a, a "), c("a","a"))
              expect_equal(ConvertCommaSeparatedStringToVector("a; a ", split = ";"), c("a","a"))
              expect_equal(ConvertCommaSeparatedStringToVector("a; a ;   a ", split = c(";")), c("a","a", "a"))

              # non-breaking white space
              xx <- "NET, Total, SUM, 1901-1910"
              expect_equal(nchar(ConvertCommaSeparatedStringToVector(xx)), c(3,5,3,9))
          })

test_that("ConvertCommaSeparatedStringToVector with text qualifier",
{
    expect_equal(ConvertCommaSeparatedStringToVector("alfa, \"bravo\", charlie",
                                                     text.qualifier = "\""),
                                                     c("alfa", "bravo", "charlie"))
    expect_equal(ConvertCommaSeparatedStringToVector("alfa, \"bravo, char\"lie",
                                                     text.qualifier = "\""),
                                                     c("alfa", "\"bravo", "char\"lie"))
    expect_equal(ConvertCommaSeparatedStringToVector("\",alfa\", \"br , a , vo\", \"char,lie,\"",
                                                     text.qualifier = "\""),
                                                     c(",alfa", "br , a , vo", "char,lie,"))
})

test_that("ConvertCommaSeparatedStringToVector with text qualifier",
{
    expect_equal(ParseTextList("pet: cat, dog, rat"),
                 list(name = "pet", elements = c("cat", "dog", "rat")))
    expect_equal(ParseTextList("\"p: et\": cat, \"d, og\", rat"),
                 list(name = "p: et", elements = c("cat", "d, og", "rat")))
    expect_equal(ParseTextList(":"), list(name = "", elements = character(0)))
    expect_equal(ParseTextList(""), list(name = character(0), elements = character(0)))
})

test_that("Trim white space",
          {
              expect_equal(TrimLeadingWhitespace("        Big dog         "), "Big dog         ")
              expect_equal(TrimTrailingWhitespace("        Big dog         "), "        Big dog")
              expect_equal(TrimWhitespace("        Big dog         "), "Big dog")
          })

test_that("Trim character and white space",
{
    expect_equal(TrimCharacterAndWhitespace(c("/ Excel + ", "&SPSS", "", "R"), c("/", "+", "&")),
                 c("Excel", "SPSS", "", "R"))
})

test_that("MakeUniqueNames",
{
    expect_equal(MakeUniqueNames(rep(letters[1:3], 1:3)), c("a", "b", "b ", "c", "c ", "c  "))
})

test_that("EscapeRegexSymbols",
{
    expect_equal(EscapeRegexSymbols("backslash: \\"), "backslash: \\\\")
    expect_equal(EscapeRegexSymbols("full stop: ."), "full stop: \\.")
    expect_equal(EscapeRegexSymbols("pipe: |"), "pipe: \\|")
    expect_equal(EscapeRegexSymbols("parentheses: ()"), "parentheses: \\(\\)")
    expect_equal(EscapeRegexSymbols("square brackets: []"),
                 "square brackets: \\[\\]")
    expect_equal(EscapeRegexSymbols("braces: {}"),
                 "braces: \\{\\}")
    expect_equal(EscapeRegexSymbols("caret: ^"), "caret: \\^")
    expect_equal(EscapeRegexSymbols("dollar: $"), "dollar: \\$")
    expect_equal(EscapeRegexSymbols("asterisk: *"),"asterisk: \\*")
    expect_equal(EscapeRegexSymbols("plus: +"), "plus: \\+")
    expect_equal(EscapeRegexSymbols("question mark: ?"), "question mark: \\?")
})


test_that("DS-4287: Helper function to check dual-response-none variables in Q/Displayr", {
    fake.data <- data.frame(D1 = c(0,1,0,1), D2 = c(1,1,0,0))
    addQuestionType <- function(x, question.type) {
        attr(x, "questiontype") <- question.type
        x
    }

    drn.good = data.frame(lapply(fake.data, addQuestionType, question.type = "PickAny"))
    drn.bad = data.frame(lapply(fake.data, addQuestionType, question.type = "PickOneMulti"))

    expect_error(RequireQuestionType(drn.good, required.type = "PickAny",
                                     message.prefix = "",
                                     message.suffix = ""), NA)
    expect_error(RequireQuestionType(drn.bad, required.type = "PickAny",
                                     message.prefix = "",
                                     message.suffix = ""), "Binary - Multi")
})

test_that("Don't split text that is marked as having originated from a control", {
    control.string <- StringIsFromControl("This is a comma, separated string")
    expect_true(attr(control.string, "is.control"))
    expect_equal(ConvertCommaSeparatedStringToVector(control.string), control.string)

    #Control in Displayr can be NULL if user not selected anything yet
    expect_null(StringIsFromControl(NULL))
})

test_that("DS-4211: Names attributes are removed and data frames are returned as lists", {

    num <- c(1,2,3,4,5,6,7)
    fac <- factor(num)
    txt <- letters[num]
    setNames(num, txt)
    setNames(fac, txt)
    setNames(txt, txt)

    mat <- matrix(1:12, nrow = 4)
    rownames(mat) <- letters[1:4]
    colnames(mat) <- letters[1:3]

    df <- data.frame(fac, num, txt)

    expect_null(attr(TidyDataForRVariableSet(num), "names"))
    expect_null(attr(TidyDataForRVariableSet(fac), "names"))
    expect_null(attr(TidyDataForRVariableSet(txt), "names"))
    
    tidied.df <- TidyDataForRVariableSet(df)
    expect_false(is.data.frame(tidied.df))
    expect_true(is.list(tidied.df))
    expect_null(attr(tidied.df[[1]], "names"))
    expect_null(attr(tidied.df[[2]], "names"))
    expect_null(attr(tidied.df[[3]], "names"))

});