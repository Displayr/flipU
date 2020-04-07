context("properties")

dat <- data.frame(a = rep((1:10)/10,2),
                  b = rep(1:10,2),
                  c = factor(rep(c(rep("A",5),rep("B",5)),2)),
                  d = ordered(rep(c(rep("A",5),rep("B",5)),2)), e = rep("dog",20), stringsAsFactors = FALSE)
for (i in 1:5)
    dat[i, i] <- NA

test_that("AnyNA",
          {
              expect_true(AnyNA(dat))
              expect_false(AnyNA(dat[11:20,]))
              expect_true(AnyNA(dat[3:20, ], c ~ a + b))
              expect_true(AnyNA(dat[3:20, ], ~ a + b + c + d))
              expect_false(AnyNA(dat[3:20, ], a ~ b))
              expect_false(AnyNA(dat[3:20, ], ~ a + b))
              expect_false(AnyNA(dat[3:20, ], ~ a + b))
          })

test_that("CopyAttributes",
          {
              z <- 1:10
              attr(z, "question") <- "Question"
              q <- 2
              expect_equal(attr(CopyAttributes(q, z), "question"), "Question")

              df <- data.frame(a = 1:10, b = z)
              df1 <- data.frame(a = 1:10, b = 1:10)
              expect_equal(attr(CopyAttributes(df1, df)$b, "question"), "Question")

              expect_equal(attr(CopyAttributes(df1, z)$b, "question"), "Question")
          })

test_that("CopyAttributes: data.frame column attributes",
          {
              dat <- structure(list(foo = structure(c(1,
                                                      2), name = "UniqueID", label = "Unique Identifier", question = "Unique Identifier", questiontype = "Number"),
                                    bar = structure(c(3L,
                                                      5L), .Label = c("19-Dec-11-01-Jan-12",
                                                                      "02-Jan-12-15-Jan-12", "16-Jan-12-29-Jan-12", "30-Jan-12-12-Feb-12",
                                                                      "13-Feb-12-26-Feb-12", "27-Feb-12-11-Mar-12", "12-Mar-12-25-Mar-12",
                                                                      "26-Mar-12-08-Apr-12", "09-Apr-12-22-Apr-12", "23-Apr-12-06-May-12",
                                                                      "07-May-12-20-May-12", "21-May-12-03-Jun-12", "04-Jun-12-17-Jun-12",
                                                                      "18-Jun-12-01-Jul-12", "02-Jul-12-15-Jul-12", "16-Jul-12-29-Jul-12",
                                                                      "30-Jul-12-12-Aug-12", "13-Aug-12-26-Aug-12", "27-Aug-12-09-Sep-12",
                                                                      "10-Sep-12-23-Sep-12", "24-Sep-12-07-Oct-12", "08-Oct-12-21-Oct-12",
                                                                      "22-Oct-12-04-Nov-12", "05-Nov-12-18-Nov-12", "19-Nov-12-02-Dec-12",
                                                                      "03-Dec-12-16-Dec-12", "17-Dec-12-30-Dec-12"), class = c("ordered",
                                                                                                                               "factor"), name = "date", label = "Interview Date", question = "Interview Date", questiontype = "Date")), .Names = c("foo",
                                                                                                                                                                                                                                                    "bar"
                                                                                                                               ), row.names = c(NA, 2L), class = "data.frame")

              dat2 <- dat
              attr(dat2$foo, "question") <- NULL
              expect_identical(dat, CopyAttributes(dat2, dat))

              dat2 <- dat
              attributes(dat2$foo) <- NULL
              expect_identical(dat, CopyAttributes(dat2, dat))

          })

test_that("CopyAttributes: data.frame object attributes",
          {
              dat <- structure(list(foo = structure(c(1,
                                                      2), name = "UniqueID", label = "Unique Identifier", question = "Unique Identifier", questiontype = "Number"),
                                    bar = structure(c(3L,
                                                      5L), .Label = c("19-Dec-11-01-Jan-12",
                                                                      "02-Jan-12-15-Jan-12", "16-Jan-12-29-Jan-12", "30-Jan-12-12-Feb-12",
                                                                      "13-Feb-12-26-Feb-12", "27-Feb-12-11-Mar-12", "12-Mar-12-25-Mar-12",
                                                                      "26-Mar-12-08-Apr-12", "09-Apr-12-22-Apr-12", "23-Apr-12-06-May-12",
                                                                      "07-May-12-20-May-12", "21-May-12-03-Jun-12", "04-Jun-12-17-Jun-12",
                                                                      "18-Jun-12-01-Jul-12", "02-Jul-12-15-Jul-12", "16-Jul-12-29-Jul-12",
                                                                      "30-Jul-12-12-Aug-12", "13-Aug-12-26-Aug-12", "27-Aug-12-09-Sep-12",
                                                                      "10-Sep-12-23-Sep-12", "24-Sep-12-07-Oct-12", "08-Oct-12-21-Oct-12",
                                                                      "22-Oct-12-04-Nov-12", "05-Nov-12-18-Nov-12", "19-Nov-12-02-Dec-12",
                                                                      "03-Dec-12-16-Dec-12", "17-Dec-12-30-Dec-12"), class = c("ordered",
                                                                                                                               "factor"), name = "date", label = "Interview Date", question = "Interview Date", questiontype = "Date")), .Names = c("foo",
                                                                                                                                                                                                                                                    "bar"
                                                                                                                               ), row.names = c(NA, 2L), class = "data.frame")

              dat2 <- dat
              attr(dat, "My Amazing Attribute") <- "Hi!"
              expect_identical(dat, CopyAttributes(dat2, dat))

              dat2 <- dat
              attributes(dat2$foo) <- NULL
              expect_identical(dat, CopyAttributes(dat2, dat))

          })

test_that("CopyAttributes: QTable attributes",
          {
              q5 <- structure(c(1, 0.9375, 0.923547400611621,
                                0.0033112582781457, 0, 0.00611620795107034,
                                2.41721854304636, 2.375, 2.33639143730887,
                                0.149006622516556, 1, 0.146788990825688,
                                0.794701986754967, 0.875, 0.764525993883792,
                                2.98344370860927, 4.14583333333333,
                                2.89296636085627, 1.77814569536424, 2.9375,
                                1.74006116207951, 2.70529801324503, 2.9375,
                                2.64220183486239, 15.9503311258278, 18.625,
                                15.8012232415902, 0.5, 0.96, 0.923547400611621,
                                1, 0, 0.00611620795107034, 5.5, 2.36,
                                2.33639143730887, 0, 0.168, 0.146788990825688,
                                0, 1, 0.764525993883792, 0.5, 3.364,
                                2.89296636085627, 1, 1.84, 1.74006116207951, 1,
                                3.044, 2.64220183486239, 11, 16.832,
                                15.8012232415902, 0, 0.244623027395041,
                                0.266128140495266, 0.0575435337648436, 0,
                                0.0780861484322752, 1.03979052256587,
                                1.14157412784352, 1.12554954797962,
                                0.356686029364813, 0, 0.354437809580216,
                                0.40458962917493, 0.334218682391596,
                                0.42494496767139, 1.44778123122884,
                                1.39892942856289, 1.50790740166423,
                                0.807226006691074, 0.976451456452655,
                                0.800508271775028, 1.13062403487421,
                                1.079918239616, 1.14733301667923,
                                4.27239262713802, 4.3202886034409,
                                4.34835823708049, 0.707106781186548,
                                0.196352277469526, 0.266128140495266, 0, 0,
                                0.0780861484322752, 3.53553390593274,
                                1.03667680362817, 1.12554954797962, 0,
                                0.374616269531358, 0.354437809580216, 0, 0,
                                0.42494496767139, 0.707106781186548,
                                1.22863426969733, 1.50790740166423, 0,
                                0.820422067888176, 0.800508271775028, 0,
                                0.962167900250831, 1.14733301667923,
                                2.82842712474619, 3.99344040459605,
                                4.34835823708049, 327, 327, 327, 327, 327, 327,
                                327, 327, 327, 327, 327, 327, 327, 327, 327,
                                327, 327, 327, 327, 327, 327, 327, 327, 327,
                                327, 327, 327, 327, 327, 327, 327, 327, 327,
                                327, 327, 327, 327, 327, 327, 327, 327, 327,
                                327, 327, 327, 327, 327, 327, 327, 327, 327,
                                327, 327, 327), .Dim = c(3L, 3L, 3L, 2L, 3L),
                              .Dimnames = list(c("Zebra", "Llama", "SUM"),
                                               c("Traditional", "Weight-conscious", "SUM"),
                                               c("Coke", "Diet Coke", "NET"),
                                               c("Traditional", "Weight-conscious"),
                                               c("Average", "Standard Deviation", "Sample Size")),
                              name = "number grid by PickAnyGrid",
                              questions = c("number grid",
                                            "PickAnyGrid [Colas edited]"))

              q5na <- q5
              attributes(q5na) <- NULL
              expect_false(identical(q5, CopyAttributes(q5na, q5)))
              expect_identical(q5, CopyAttributes(q5na, q5, ""))

              q5nd <- q5
              dimnames(q5nd) <- NULL
              expect_identical(q5, CopyAttributes(q5nd, q5, c("names", "row.names", "dim",
                                                              "class", "levels")))

          })

test_that("CopyAttributes: Copy after extract",
          {
              q5 <- structure(c(1, 0.9375, 0.923547400611621,
                                0.0033112582781457, 0, 0.00611620795107034,
                                2.41721854304636, 2.375, 2.33639143730887,
                                0.149006622516556, 1, 0.146788990825688,
                                0.794701986754967, 0.875, 0.764525993883792,
                                2.98344370860927, 4.14583333333333,
                                2.89296636085627, 1.77814569536424, 2.9375,
                                1.74006116207951, 2.70529801324503, 2.9375,
                                2.64220183486239, 15.9503311258278, 18.625,
                                15.8012232415902, 0.5, 0.96, 0.923547400611621,
                                1, 0, 0.00611620795107034, 5.5, 2.36,
                                2.33639143730887, 0, 0.168, 0.146788990825688,
                                0, 1, 0.764525993883792, 0.5, 3.364,
                                2.89296636085627, 1, 1.84, 1.74006116207951, 1,
                                3.044, 2.64220183486239, 11, 16.832,
                                15.8012232415902, 0, 0.244623027395041,
                                0.266128140495266, 0.0575435337648436, 0,
                                0.0780861484322752, 1.03979052256587,
                                1.14157412784352, 1.12554954797962,
                                0.356686029364813, 0, 0.354437809580216,
                                0.40458962917493, 0.334218682391596,
                                0.42494496767139, 1.44778123122884,
                                1.39892942856289, 1.50790740166423,
                                0.807226006691074, 0.976451456452655,
                                0.800508271775028, 1.13062403487421,
                                1.079918239616, 1.14733301667923,
                                4.27239262713802, 4.3202886034409,
                                4.34835823708049, 0.707106781186548,
                                0.196352277469526, 0.266128140495266, 0, 0,
                                0.0780861484322752, 3.53553390593274,
                                1.03667680362817, 1.12554954797962, 0,
                                0.374616269531358, 0.354437809580216, 0, 0,
                                0.42494496767139, 0.707106781186548,
                                1.22863426969733, 1.50790740166423, 0,
                                0.820422067888176, 0.800508271775028, 0,
                                0.962167900250831, 1.14733301667923,
                                2.82842712474619, 3.99344040459605,
                                4.34835823708049, 327, 327, 327, 327, 327, 327,
                                327, 327, 327, 327, 327, 327, 327, 327, 327,
                                327, 327, 327, 327, 327, 327, 327, 327, 327,
                                327, 327, 327, 327, 327, 327, 327, 327, 327,
                                327, 327, 327, 327, 327, 327, 327, 327, 327,
                                327, 327, 327, 327, 327, 327, 327, 327, 327,
                                327, 327, 327), .Dim = c(3L, 3L, 3L, 2L, 3L),
                              .Dimnames = list(c("Zebra", "Llama", "SUM"),
                                               c("Traditional", "Weight-conscious", "SUM"),
                                               c("Coke", "Diet Coke", "NET"),
                                               c("Traditional", "Weight-conscious"),
                                               c("Average", "Standard Deviation", "Sample Size")),
                              name = "number grid by PickAnyGrid",
                              questions = c("number grid",
                                            "PickAnyGrid [Colas edited]"))

              q5sub <- q5[, 1:2, , , ]
              out <- CopyAttributes(q5sub, q5)
              expect_equal(attributes(q5)[c("name", "questions")],
                           attributes(out)[c("name", "questions")])

              q5nd <- q5
              dimnames(q5nd) <- NULL
              expect_identical(q5, CopyAttributes(q5nd, q5, c("names", "row.names", "dim",
                                                              "class", "levels")))

          })

test_that("CopyAttributes with data.frames with different columns",
          {
              df1 <- data.frame(x = 1, y = 1, z = 2)
              df2 <- data.frame(x = 1, y = 2)
              attr(df1$x, "xatt") <- "foo"
              attr(df1$y, "yatt") <- "bar"
              attr(df1$z, "zatt") <- "baz"

              out <- CopyAttributes(df2, df1)
              expect_equal(attributes(out$x), attributes(df1$x))
              expect_equal(attributes(out$y), attributes(df1$y))

              out <- CopyAttributes(df1, df2)
              expect_equal(attributes(out$x), attributes(df1$x))
              expect_equal(attributes(out$y), attributes(df1$y))
          })


test_that("AllVariablesNames when data is NULL",
          {
              expect_equal(AllVariablesNames(`Cola.sav$Variables$Q2` ~ Q3),
                           c("`Cola.sav$Variables$Q2`", "Q3"))
          })

test_that("AllVariablesNames  non-syntactic response,  dot on RHS",
          {
              df1 <- data.frame(x = 1, y = 2, z = 3)
              colnames(df1)[1] <- "`Cola.sav$Variables$Q2`"
              colnames(df1)[2] <- "`Cola.sav$Variables$Q99`"
              expect_equal(AllVariablesNames(`Cola.sav$Variables$Q2` ~ ., data = df1),
                           c("`Cola.sav$Variables$Q2`", "`Cola.sav$Variables$Q99`", "z"))
          })

test_that("AllVariablesNames backticks are added to any non-syntactic variable",
          {
              dat <- data.frame(`a$b$c` = 1, "a?b" = 3, `d$e$f` = 2,
                                check.names = FALSE)
              out <- AllVariablesNames(`a$b$c` ~ ., data = dat)
              expect_equal(out, paste0("`", names(dat), "`"))
          })

test_that("AllVariablesNames non-syntactic response,  dot on RHS",
          {
              ## this fails on old version of AllVariablesNames (flipU <= 1.0.0)
              dat <- data.frame(`a$b$c` = 1, x = 3, "`d$e$f`" = 2,
                                check.names = FALSE)
              out <- AllVariablesNames(`a$b$c` ~ ., data = dat)
              expect_equal(out[1], paste0("`", names(dat)[1], "`"))
              expect_equal(out[2:3], names(dat)[2:3])
          })

test_that("AllVariablesNames backticks in some data names",
          {
              dat <- data.frame(`a$b$c` = 1, x = 3, "`d$e$f`" = 2,
                                check.names = FALSE)
              out <- AllVariablesNames(`a$b$c` ~ x + `d$e$f`, data = dat)
              ## backticks are added to any non-syntactic variable
              expect_equal(out[1], paste0("`", names(dat)[1], "`"))
              expect_equal(out[2:3], names(dat)[2:3])
          })



test_that("AllVariablesNames var in formula not in data",
          {
              dat <- data.frame(`a$b$c` = 1, x = 3, "`d$e$f`" = 2,
                                check.names = FALSE)
              out <- AllVariablesNames(`a$b$c` ~ . +z, data = dat)
              ## backticks are added to any non-syntactic variable
              expect_equal(out[1], paste0("`", names(dat)[1], "`"))
              expect_equal(out[2:3], names(dat)[2:3])
              expect_equal(out[4], "z")
          })

test_that("AllVariablesNames interaction in formula",
          {
              dat <- data.frame(`a$b$c` = 1, x = 3, "`d$e$f`" = 2,
                                check.names = FALSE)
              out <- AllVariablesNames(`a$b$c` ~ x*`d$e$f`, data = dat)
              ## backticks are added to any non-syntactic variable
              expect_equal(out[1], paste0("`", names(dat)[1], "`"))
              expect_equal(out[2:3], names(dat)[2:3])
          })

test_that("AllVariablesNames $ + no backticks in formula, data NULL",
          {
              dat <- data.frame(y = 1, x = 2)
              out <- AllVariablesNames(dat$y ~ dat$x, data = NULL)
              ## backticks are added to any non-syntactic variable
              expect_equal(out, paste0("dat$", names(dat)))
          })

test_that("AllVariablesNames formula with '-' ",
          {
              out <- AllVariablesNames(`a$b$c` ~ a*b - a, data = NULL)
              ## backticks are added to any non-syntactic variable
              expect_equal(out, c("`a$b$c`", "a", "b"))
          })

test_that("AllVariablesNames formula with ':' ",
          {
              dat <- data.frame(y = 1, x = 2)
              out <- AllVariablesNames(~ x:y, data = dat)
              ## backticks are added to any non-syntactic variable
              expect_equal(out, c("x", "y"))
          })

test_that("AllVariablesNames formula with ':' and missing main effect",
          {
              out <- AllVariablesNames(y~a + a:b, data = NULL)
              expect_equal(out, c("y", "a", "b"))
          })


test_that("AllVariablesNames formula with '^' and '(' ",
          {
              out <- AllVariablesNames(y~(`a:b`+c)^2, data = NULL)
              ## backticks are added to any non-syntactic variable
              expect_equal(out, c("y", "`a:b`", "c"))
          })

test_that("AllVariablesNames formula with '*' ",
          {
              out <- AllVariablesNames(~`a$b$c`*c, data = NULL)
              ## backticks are added to any non-syntactic variable
              expect_equal(out, c("`a$b$c`", "c"))
          })

test_that("AllVariablesNames interaction with : in variable name ",
          {
              dat <- data.frame(y = 1, "a:b" = 2, c = 3)
              out <- AllVariablesNames(y~`a:b`+`a:b`:c, data = dat)
              ## backticks are added to any non-syntactic variable
              expect_equal(out, c("y" , "`a:b`", "c"))
          })

test_that("AllVariablesNames functions in formula",
          {
              out <- AllVariablesNames(log(y) ~ I(log(x))+ `a(b`)
              expect_equal(out, c("y", "x", "`a(b`"))
          })

test_that("AllVariablesNames array extraction in formula",
          {
              out <- AllVariablesNames(y ~ x^2+ arr[1, "z", ])
              expect_equal(out, c("y", "x", "arr[1,'z',]"))
          })

test_that("AllVariablesNames list extraction in formula",
          {
              out <- AllVariablesNames(y ~ x + df[["z"]])
              expect_equal(out, c("y", "x", "df[['z']]"))
          })


test_that("OutcomeName function in response",
          {
              out <- OutcomeName(log(response) ~ I(log(x))+ `a(b`)
              expect_equal(out, "response")
          })

test_that("OutcomeName I() in response",
          {
              out <- OutcomeName(I(sin(y)) ~ I(log(x))*z)
              expect_equal(out, "y")
          })

test_that("OutcomeName backticks in response",
          {
              out <- OutcomeName(`dat$var$y` ~ I(log(x))+ `a(b`)
              expect_equal(out, "`dat$var$y`")
          })

test_that("OutcomeName response has $ without backticks",
          {
              out <- OutcomeName(dat$y ~ .)
              expect_equal(out, "dat$y")
          })

test_that("OutcomeName response has [[ without backticks",
          {
              out <- OutcomeName(dat[["y"]] ~ .)
              expect_equal(out, "dat[['y']]")
          })

test_that("OutcomeName: supplied formula is a terms object",
          {
              out <- OutcomeName(terms(y~x))
              expect_equal(out, "y")
          })
