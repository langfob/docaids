#===============================================================================
#
#                       doc_vars_in_this_func.R
#
#===============================================================================

#  Library references:
#      - Uses gtools::odd()
#      - Uses sourcetools::tokenize_string()

#  Part of the mainline was derived by reading:
#  From:  http://www.numbertheory.nl/2013/03/24/parsing-complex-text-files-using-regular-expressions-and-vectorization/

#===============================================================================

#' Document variables and their types that are visibile inside current function
#'
#' Call the str() function for all variables visible inside the current
#' function.  Before doing that, write out a flag line to indicate the start
#' of a section of output for a particular function and write the call made
#' to that function, i.e., function name followed by its argument list.
#' After writing out the structures of all the variables write out a
#' closing line to flag the end of str() output for the current function.
#'
#' @param sys.call_ht negative integer to pass to sys.call() to specify how
#'     high to look on the \code{\link{sys.call}} stack.
#' @param sys.frame_ht negative integer to pass to sys.frame() to specify how
#'     high to look on the \code{\link{sys.frame}} stack.
#'
#' Generally shouldn't need to set either of the arguments since the value
#' either defaults to the correct value or is set correctly inside
#' \code{\link{doc_vars_in_this_func_once}}.
#'
#' @seealso \code{\link{doc_vars_in_this_func_once}}
#' @return Returns nothing
#' @export

doc_vars_in_this_func <- function (sys.call_ht = -1,
                                   sys.frame_ht = -1)
    {
    cat("\n\n>>>>>>>>>>>>>>>>>>>>>>>>  START doc_vars_in_this_func  >>>>>>>>>>>>>>>>>>>>>>>>\n");
    print (sys.call (sys.call_ht))
    print(utils::ls.str(envir = sys.frame (sys.frame_ht))) ## [1] "aa" "t2"
    cat("<<<<<<<<<<<<<<<<<<<<<<<<  END doc_vars_in_this_func  <<<<<<<<<<<<<<<<<<<<<<<<<<\n")
    }

#===============================================================================

#' Increment a global counter specific to the current function
#'
#' This function creates a global counter for a function if that counter
#' doesn't exist yet.  If it already exists, then it increments that counter.
#' This is primarily used to help \code{\link{doc_vars_in_this_func_once}}
#' determine whether to call \code{\link{doc_vars_in_this_func}}.  This is
#' because you usually don't want to call the documenter more than once,
#' no matter how often the function it's documenting is called.
#'
#' This function creates what is hoped to be a unique name for the counter
#' based on the name of the function, i.e., it prepends a copy of the function's
#' name with a strange string.  That prepend string is
#' "TEMPCTR___RM_THIS_AT_END__".  If the function being documented was called
#' func1, then the resulting counter's name would be
#' "TEMPCTR___RM_THIS_AT_END__func1".
#'
#' @section Choice of default sys.call_ht:
#' The default value for sys.call_ht has been set to -7 as a result of much
#' trial and error to find what height landed on the right call in the stack.
#' Be very careful if you give it a different value.
#'
#' @section Possible (though unlikely) problems:
#' This method of creating names for counters is not foolproof, but it's good
#' enough for the quick hacking of documentation aids that this code is
#' intended to support.  There are at least two ways that it might screw up,
#' but each has a fix.
#'
#' \describe{
#'   \item{Name matches a name in user's code}{If the user's code contained
#'       a name matching the very strange generated name, the ctr_name_prefix
#'       can be set in the argument list.}
#'   \item{Generic function}{There may be different versions of the same
#'       generic function in use at the same time and they will all end up
#'       being counted as the same function since the generated name will
#'       be the same.  In this case, one solution is to pass in a different
#'       unique ctr_name_prefix in the call in each of the different uses of the
#'       generic. Depending on how long it takes for the program to run,
#'       it may be simpler to just re-run the whole program with the call
#'       cut and pasted into each different form of the generic each time
#'       the program is re-run; not pretty, but perhaps the least trouble in
#'       the end. }
#' }
#'
#' @inheritParams doc_vars_in_this_func
#' @param ctr_name_prefix character string to prepend to function name when
#'     building a counter name that is supposed to be unique
#'
#' @seealso \code{\link{remove_global_ctrs_if_desired}} for cleanup of counters
#'     at end of run
#' @return integer value of global counter for the current function
#' @export

bump_global_ctr_for_cur_func <- function (sys.call_ht=-7,
                                          ctr_name_prefix =
                                              "TEMPCTR___RM_THIS_AT_END__")
    {
        #--------------------------------------------------------------
        #  Build a name for the counter:
        #  Look up the call stack to find the text of the call to the
        #  function of interest, then extract it from the text.
        #  It's the first token in the parsed call text.
        #--------------------------------------------------------------

    func_call_text <- utils::capture.output (sys.call (sys.call_ht))
    syscall_tokens <- sourcetools::tokenize_string (func_call_text)
    func_name <- syscall_tokens [1, "value"]

        #----------------------------------------------------------------
        #  Build a (hopefully) unique counter name for the function of
        #  interest by prepending its name with a weird prefix.
        #----------------------------------------------------------------

    ctr_name <- paste0 (ctr_name_prefix, func_name)

        #----------------------------------------------------------------
        #  Increment the counter.
        #  If this is the first time the function of interest has been
        #  called in the program, no counter will exist for it yet,
        #  so you need to initialize it before trying to increment it.
        #----------------------------------------------------------------

    ctr <- 0
    if (exists (ctr_name))
        ctr <- get (ctr_name)
    ctr <- ctr + 1

        #----------------------------------------------------------------------
        #  Assign the value to it as a global variable.
        #  This is of course, blasphemous behavior, but it's a very simple
        #  way to solve the problem of explicitly not wanting to pass a
        #  million function-specific counters around the code.
        #  Another alternative might be to create a specific environment
        #  just to hold all the counters, but that would mean requiring
        #  the main program to create that environment.
        #  Maybe that's ok, but the point of all this code is just a one-time
        #  quick and dirty insertion in the program to grab the variable
        #  information and then delete the calls to this function, so
        #  it seems better to have as little overhead as possible to make
        #  it useful.
        #----------------------------------------------------------------------

    assign (ctr_name, ctr, envir = .GlobalEnv)

    return (ctr)
    }

#===============================================================================

#' Remove temporary global counters if desired
#'
#' Running either of the functions \code{\link{bump_global_ctr_for_cur_func}},
#' or \code{\link{doc_vars_in_this_func_once}} will lead to the creation of some
#' temporary global variables used as counters.  As they are of no use outside
#' these routines, they should probably be deleted at the end of the run to
#' avoid cluttering the global environment with meaningless variables.  This
#' function takes care of asking whether to delete them or not or just deleting
#' them without asking, depending on how the arguments are set.  Since you will
#' almost always want to delete these counters, the flags default to deleting
#' without prompting.
#'
#' @section When to run this function:
#'
#' This function is meant to be run on its own, generally from the command line
#' though that's not required.  Because the documenting functions are meant to
#' be injected into other functions as simply as possible, there is no
#' initialization code to add to the start of the program being documented.
#' That means there could be leftover global counts from a previous run if
#' they haven't been cleared.  Consequently, you should clear them before
#' doing another documentation run.
#'
#' You can do this at the command line
#' after a run or you can enforce their clearing by including this clearing
#' action at the start of the program being documented, as a form of
#' initialization.  I've chosen not to require that be done, because you can
#' enforce that yourself if you want and because the consequences of not
#' clearing aren't dire.  If you're using
#' \code{\link{doc_vars_in_this_func_once}}, then the documenting code will
#' just think that all the functions with pre-existing counters in the global
#' environment have already been documented and won't write anything out.
#' This can even be desirable behavior if you're just continually adding new
#' functions to the set you're documenting and you've already grabbed the
#' documentation for the ones already run.
#'
#' @section Ambiguous flag settings:
#'
#' There is one pairing of flag settings that whose settings could lead to
#' confusion about what action will be taken, i.e.,
#' \code{prompt_to_remove_global_ctrs} = TRUE __and__
#' \code{remove_global_ctrs_without_prompt} = TRUE.  This is resolved by
#' choosing the more cautious course and ignoring the value of the second flag
#' and instead, prompting the user about whether to remove the variables.
#'
#' If both flags are FALSE, then nothing will be done, so it's equivalent to
#' not calling this routine at all, other than the side effect of printing
#' out the list of global counters.
#'
#' @param prompt_to_remove_global_ctrs boolean flag indicating whether to prompt
#'     the user to ask whether they want to remove the global counters built by
#'     \code{\link{bump_global_ctr_for_cur_func}}; TRUE implies prompt the user;
#'     FALSE implies don't ask the user
#' @param remove_global_ctrs_without_prompt boolean flag indicating whether to
#'     remove the global counters built by
#'     \code{\link{bump_global_ctr_for_cur_func}} without asking the user
#'     whether they want to do that; TRUE implies remove without asking; FALSE
#'     implies don't remove without asking
#'
#' @seealso \code{\link{doc_vars_in_this_func_once}}
#' @seealso \code{\link{bump_global_ctr_for_cur_func}}
#' @return Nothing
#' @export
#' @examples \dontrun{
#' library (docaids)
#' remove_global_ctrs_if_desired ()    #  Remove ctrs w/o prompting
#' remove_global_ctrs_if_desired (TRUE, FALSE)    #  Remove ctrs if user says yes
#' }

remove_global_ctrs_if_desired <-
    function (prompt_to_remove_global_ctrs = FALSE,
              remove_global_ctrs_without_prompt = TRUE)
    {
        #---------------------------------------------------------------------
        #  Make a list of all the global counter variables and print that
        #  list for the user to check or to simply record what is going to
        #  be deleted if deletion is automatic and a problem is found later.
        #---------------------------------------------------------------------

    temp_ctrs_to_delete <- ls (pattern="TEMPCTR___RM_THIS_AT_END__*",
                               envir=globalenv())  #.GlobalEnv)
    cat ("\n\nglobal counters that were created by bump_global_ctr_for_cur_func() = \n")
    print (temp_ctrs_to_delete)

        #-----------------------------------------------------------------------
        #  Do the prompting and/or removals now.
        #
        #  Note that the arg to rm() must be "envir", not "environ".
        #  ls() uses "environ".
        #  Getting this wrong in the call to rm() gives a misleading error msg:
        #
        #      global counters that were created by bump_global_ctr_for_cur_func() =
        #      [1] "TEMPCTR___RM_THIS_AT_END__inner_func" "TEMPCTR___RM_THIS_AT_END__outer_func"
        #      Error in rm(list = temp_ctrs_to_delete, environ = globalenv()) :
        #        ... must contain names or character strings
        #      Called from: rm(list = temp_ctrs_to_delete, environ = globalenv())
        #-----------------------------------------------------------------------

    if (prompt_to_remove_global_ctrs)
        {
        yes_or_no <- utils::menu(c("Yes", "No"),
                                 title=paste0 ("Remove all of these temporary ",
                                               "counters from the global ",
                                                "environment?"))
        if (yes_or_no == 1)
            rm (list = temp_ctrs_to_delete, environ=globalenv())

        } else if (remove_global_ctrs_without_prompt)  #  No prompting was requested
        {
        rm (list = temp_ctrs_to_delete, envir = globalenv())

#  environ rather than envir gives:
# global counters that were created by bump_global_ctr_for_cur_func() =
# [1] "TEMPCTR___RM_THIS_AT_END__inner_func" "TEMPCTR___RM_THIS_AT_END__outer_func"
# Error in rm(list = temp_ctrs_to_delete, environ = globalenv()) :
#   ... must contain names or character strings
# Called from: rm(list = temp_ctrs_to_delete, environ = globalenv())
#
        }
    }

#===============================================================================

#' Document function's variables a limited number of times
#'
#' This is a wrapper function for \code{\link{doc_vars_in_this_func}}.
#' It makes sure that \code{doc_vars_in_this_func} is only run once
#' in a given invocation of a program, no matter how many times the function
#' containing the \code{doc_vars_in_this_func} call is called.  This is
#' useful in avoiding getting a bunch of duplicate outputs for the same
#' function when it is called more than once in a program.
#'
#'
#' @param max_ct integer maximum number of times to write the documentation
#'     for a function, no matter how many times it's called
#' @inheritParams bump_global_ctr_for_cur_func
#'
#' @seealso \code{\link{doc_vars_in_this_func}} for the main action of this
#'     routine
#' @seealso \code{\link{doc_vars_in_this_func_once}} for explanation about
#'     ctr_name_prefix, which you will probably never touch
#' @return Returns nothing
#' @export

doc_vars_in_this_func_once <- function (max_ct = 1,
                                        ctr_name_prefix =
                                              "TEMPCTR___RM_THIS_AT_END__")
    {
    ctr_for_cur_func <- bump_global_ctr_for_cur_func (sys.call_ht=-7,
                                                      ctr_name_prefix)

    if (ctr_for_cur_func <= max_ct)
        doc_vars_in_this_func (sys.call_ht=-2, sys.frame_ht=-2)
    }

#===============================================================================

