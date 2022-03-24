library(tidyverse)
library(assertthat)


gc_content <- function(seq) {

  assert_that(is.character(seq))

  if (any(str_detect(seq,"[^GATC]"))) {
    warning("Non GATC characters found in sequences")
  }

  seq <- toupper(seq)

  str_replace_all(seq,"[^GC]","") -> just_gc

  return(100*(nchar(just_gc)/nchar(seq)))

}

read_fastq <- function(file) {
  assert_that(is.readable(file))
  assert_that(has_extension(file,"fq"))

  scan(file, character()) -> file.lines
  file.lines[c(T,F,F,F)] -> ids
  file.lines[c(F,T,F,F)] -> sequences
  file.lines[c(F,F,F,T)] -> qualities

  if (!all(startsWith(ids,"@"))) {
    stop("Some ID lines didn't start with @")
  }

  str_sub(ids,2) -> ids

  if (!all(nchar(sequences)==nchar(qualities))) {
    stop("Some sequences were a different length to the qualities")
  }

  if (any(duplicated(ids))) {
    stop("Some IDs are duplicated")
  }

  tibble(ID = ids, Bases=sequences, Qualities=qualities, GC=gc_content(sequences)) %>%
    return()

}
