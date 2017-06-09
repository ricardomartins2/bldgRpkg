library(testthat)
library(bldgRpkg)

test_check("bldgRpkg")

expect_error(fars_read("non_existent_file.csv.bz2"))

expect_match(make_filename("2013"), "accident_2013.csv.bz2")
