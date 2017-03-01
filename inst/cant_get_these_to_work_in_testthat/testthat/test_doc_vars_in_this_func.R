context("Document local variables in function")


#----------

run_bump_global_ctr_for_cur_func <- function (num_times)
    {
    for (iii in 1:num_times)
        ctr_value <- bump_global_ctr_for_cur_func ()

    return (ctr_value)
    }

test_that("bump global counter for current function", {
  expect_equal (5, run_bump_global_ctr_for_cur_func (5))
})

#----------

inner_func <- function (a_in_z=3, run_once=TRUE)
    {
    if (run_once)
        {
        doc_vars_in_this_func_once ()
        } else
        {
        doc_vars_in_this_func()
        }
    }

outer_func <- function (num_times=2, run_once=TRUE, sinkFilePath)
    {
#    tempConsoleOutFile <- file (sinkFilePath, open="wt")
    tempConsoleOutFile <- file (sinkFilePath, open="w+")
    sink (tempConsoleOutFile, split=TRUE)

#cat ("\n\n***********  In outer_func:  sinkFilePath = '",
#     sinkFilePath, "'")

    for (iii in 1:num_times)
        inner_func (a_in_z=3, run_once)

    if (run_once)
        {
        doc_vars_in_this_func_once ()
        } else
        {
        doc_vars_in_this_func()
        }
#browser()
    sink ()
    close (tempConsoleOutFile)
    }

#----------

test_run_once <- function (run_once_outfile)
    {
    outer_func (3, TRUE, run_once_outfile)
    remove_global_ctrs_if_desired ()
    }

#----------

run_once_outfile <- system.file ("tests", "run_once_output.txt", package="docaids")
correct_run_once_outfile <- system.file ("tests", "correct_run_once_output.txt", package="docaids")

test_run_once (run_once_outfile)

file1_lines <- readLines (run_once_outfile)
file2_lines <- readLines (correct_run_once_outfile)

test_that("document variables in this function once", {
    expect_equal (file1_lines, file2_lines)
##  expect_equal_files ("run_once_output.txt", "correct_run_once_output.txt")

  # expect_output_file (test_run_once(),
  #                     "correct_run_once_output.txt")
})

#----------

# test_run_multi <- function ()
#     {
#     outer_func (3, FALSE, "run_multi_output.txt")
#     }
#
#
# file1_lines <- readLines ("/Users/bill/D/Projects/docaids/tests/testthat/run_multi_output.txt")
# file2_lines <- readLines ("/Users/bill/D/Projects/docaids/tests/testthat/correct_run_multi_output.txt")
#
#
# test_that("document variables in this function", {
#     expect_equal (file1_lines, file2_lines)
# #  expect_equal_files ("run_multi_output.txt", "correct_run_multi_output.txt")
#
#   # expect_output_file (test_run_multi(),
#   #                     "correct_run_multi_output.txt")
# })

#----------

#  clean up?  i.e., delete test output files?

