context("Document local variables in function")

#----------

test_that("bump global counter for current function", {
  expect_equal(correct_result_lines, result_lines_AFTER_first)
})

#----------

test_that("document variables in this function", {
  expect_equal(correct_result_lines, result_lines_ON_first)
})

#----------

test_that("document variables in this function ONCE", {
  expect_equal(correct_result_lines, result_lines_ON_first)
})

#===============================================================================
                                #  TESTS...
#===============================================================================

test_bump_global_ctr_for_cur_func <- function (num_times=5)
    {
    for (kkk in 1:num_times)
        ctr_value <- bump_global_ctr_for_cur_func ()

    return (ctr_value)
    }

#-------------------------------------------------------------------------------

xxx <- function (y)
    {
    cat ("\n\nIn xxx(), y = '", y, sep='')
    bump_global_ctr_for_cur_func ()
    cat ("\nLeaving xxx() now.\n-----------------------\n")
    }

#-------------------------------------------------------------------------------

zzz <- function (a_in_z=3, run_once=TRUE)
    {
    if (run_once)
        {
        doc_vars_in_this_func_once ()
        } else
        {
        doc_vars_in_this_func()
        }
    }

kkk <- function (num_times=2, run_once=TRUE)
    {
    for (iii in 1:num_times)
        zzz (a_in_z=3, run_once)

    if (run_once)
        {
        doc_vars_in_this_func_once ()
        } else
        {
        doc_vars_in_this_func()
        }
    }

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

if (FALSE)
{
cat ("\n\n================  run ONCE  ================\n\n")
kkk (3, TRUE)
cat ("\n\n================  run 3 times  ================\n\n")
kkk (3, FALSE)

temp_vars_to_delete <- ls (pattern="TEMPCTR___RM_THIS_AT_END__*")
cat ("\n\ntemp_vars_to_delete = \n")
print (temp_vars_to_delete)
yes_or_no <- utils::menu(c("Yes", "No"), title="Remove all of these variables?")
if (yes_or_no == 1)
    rm (list=temp_vars_to_delete)
}

