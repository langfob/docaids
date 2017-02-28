context("Generate function variable roxygen comments")

#----------

    #  Load the correct results to compare against.

outfile_roxygen_correct <- "correct_roxygen_comment_outfile.txt"
correct_result_lines <- readLines (outfile_roxygen_correct)

#----------

    #  Test behavior when first line of first block occurs AFTER the
    #  first line of the file, e.g., after a few blank lines.

infile_AFTER_first = "roxygen_generator_input_with_first_block_starting_AFTER_first_line.txt"
outfile_AFTER_first = "roxygen_outfile_AFTER_first_doc.txt"
generate_func_var_roxygen_comments (infile_AFTER_first, outfile_AFTER_first)
result_lines_AFTER_first <- readLines (outfile_AFTER_first)

test_that("correct Roxygen comments are generated when starting AFTER first line", {
  expect_equal(correct_result_lines, result_lines_AFTER_first)
})

#----------

    #  Test behavior when first line of first block occurs ON the
    #  first line of the file.

infile_ON_first = "roxygen_generator_input_with_first_block_starting_ON_first_line.txt"
outfile_ON_first = "roxygen_outfile_ON_first_doc.txt"
generate_func_var_roxygen_comments (infile_ON_first, outfile_ON_first)
result_lines_ON_first <- readLines (outfile_ON_first)

test_that("correct Roxygen comments are generated when starting ON first line", {
  expect_equal(correct_result_lines, result_lines_ON_first)
})

