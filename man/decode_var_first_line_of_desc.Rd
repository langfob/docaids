% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_function_documentation.R
\name{decode_var_first_line_of_desc}
\alias{decode_var_first_line_of_desc}
\title{Title}
\usage{
decode_var_first_line_of_desc(data_line, data_line_num, prev_state,
  first_token_of_next_line)
}
\arguments{
\item{data_line}{character string containing one full line from the input
file}

\item{data_line_num}{integer line number of data_line in original input file}

\item{prev_state}{character string indicating the previous state, e.g.,
"state__block_start"}

\item{first_token_of_next_line}{character string containing the first
token type of the next line in the input file, e.g., "whitespace" or
"symbol"}
}
\value{
character string indicating the next state to transition to, e.g.,
    "state__block_start"
}
\description{
Title
}
