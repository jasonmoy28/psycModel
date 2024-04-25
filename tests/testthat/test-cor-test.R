testthat::expect_no_error(cor_test(iris, where(is.numeric)))

testthat::expect_no_error(cor_test(iris, where(is.numeric),show_p = T))