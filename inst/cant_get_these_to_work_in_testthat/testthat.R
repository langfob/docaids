library(testthat)
library(docaids)

#----------

# expect_equal_files <- function (file1, file2)
#     {
# #browser()
#     file1_lines <- readLines (file1)
#     file2_lines <- readLines (file2)
#
#     return (expect_equal (file1_lines, file2_lines))
#     }

#----------

test_check("docaids")
