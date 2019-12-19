new_bb <- function(text) {
  stopifnot(!is.numeric(text))
  # replace ((consonants) (1-2 vowels) (consonants)) with
  # ((consonants) (vowels) b (same vowels again) (consonants)):
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  bb_text <- gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text)
  
  structure(
    bb_text,
    class = append(class(text),"bb")
  )
}


# Method dispatch
bb <- function(x) {
  UseMethod("bb")
}  

# Default (character vectors, matrix)
bb.default <- function(my_text){
  new_bb(my_text)
}

# Factors
bb.factor <- function(my_factor){
  temp_str <- as.character(my_factor)
  temp_bb <- new_bb(temp_str)
  bb_factor <- factor(temp_bb, levels = bb(levels(my_factor)))
  class(bb_factor) <- append(class(my_factor),"bb")
  bb_factor
}

# List
bb.list <- function(my_list){
  bb_list <- rapply(my_list, bb.default, how = "list")
  class(bb_list) <- append(class(my_list),"bb")
  bb_list
}



