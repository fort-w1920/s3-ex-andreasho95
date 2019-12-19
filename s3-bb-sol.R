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




# Development --------------------------------------------------------------------------------------

texttest <- "Bedeutet nach jedem Vokal oder Diphtong die Konsonanten..." # default
test_vec <- strsplit(texttest, " ")[[1]] # default
test_matrix <- matrix(test_vec, nrow = 2, ncol = 4, byrow = TRUE) # default
test_array <- array(test_vec, dim = c(2, 2, 2)) # default
test_list <- as.list(test_vec) # list
test_listoflists <- list(as.list(test_vec), list(test_vec)) # list
test_factor <- factor(test_vec) # factor
test_ordered <- ordered(test_vec) # factor

bb(texttest)
bb(test_vec)
bb(test_factor)
bb(test_matrix)
bb(test_array)
bb(test_listoflists)
bb(test_list)

levels(test_factor)
temp_str <- as.character(test_factor)
temp_bb <- new_bb(temp_str)
class(factor(temp_bb))



