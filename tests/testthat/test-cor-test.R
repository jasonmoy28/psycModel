testthat::expect_no_error(cor_test(iris, where(is.numeric),verbose = FALSE))

testthat::expect_no_error(cor_test(iris, where(is.numeric),show_p = T,verbose = FALSE))
