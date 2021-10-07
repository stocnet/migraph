cites <- tibble::tibble(qID1 = c("BNLHPB_2016P:BNLHPB_1970A",
                                 "PARIS_2015A","INOOTO_2015A",
                                 "RUS-USA[UUF]_2015A",
                                 "RUS-USA[UUF]_2015A",
                                 "RUS-USA[UUF]_2015A",
                                 "RUS-USA[UUF]_2015A",
                                 "INECHA_2015O",
                                 "ST04DC_2014P",
                                 "ST04DC_2014P"),
                        qID2 = c("BNLHPB_1977P:BNLHPB_1970A",
                                 "UNFCCC_1992A",
                                 "INOOTO_2005A",
                                 "RUS-USA[MFR]_1988A",
                                 "PS07UF_2009A",
                                 "UNCLOS_1982A",
                                 "UNCLOS_1982A",
                                 "ERECHA_1991O",
                                 "AI07EM_1998A",
                                 "CNEWNH_1979A"))

test_that("Plot lineage function works", {
  testplot <- gglineage(cites)
  expect_true(is.list(testplot))
  expect_length(testplot, 9)
  expect_named(testplot[1], "data")
})
