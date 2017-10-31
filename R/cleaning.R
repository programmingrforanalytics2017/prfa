#' Column renamer
#'
#' Strip out the punctuation and white space from imported colnames so they can be autocompleted
#' @param x Character vector of column names or data.frame/data.table object
#' @return The same names as input, but lowercase with underscores instead of spaces (multiple spaces are reduced to single)
#' @examples
#' raw_names <- c("  The first column  ", "_second col _ ")
#' name_cleaner(raw_names)
#' # [1] "the_first_column" "second_col"
#'
#' raw_names <- c("A_", "_a", "  A _")
#' name_cleaner(raw_names)
#' # [1] "a_1" "a_2" "a_3"
#'
name_cleaner <- function(x) {
  require(magrittr)
  
  if (!is.vector(x)) {
    if (is.data.frame(x)) {
      x <- colnames(x)
      message("Detected data.frame input")
    } else {
      stop("Expecting a character vector or data.frame")
    }
  }
  
  recurse_spaces <- function(y) {
    # Base case
    if (length(grep("__", y)) == 0) { return(y) }
    
    y <- gsub("__", "_", y)
    
    return(recurse_spaces(y))
  }
  
  clean_names <- x %>%
    tolower %>%
    gsub("[\n\r\t ]", "_", .) %>% # whitespace -> underscore
    gsub("[^[:alnum:]^_^]", "", .) %>% # strip out other punctuation
    recurse_spaces %>%
    gsub("_$", "", .) %>% # strip out trailing underscores
    gsub("^_", "", .) # strip out leading underscores
  
  
  # After cleaning the spaces, we may have repeated names. This appends numbers to repeated names:
  if (length(unique(clean_names)) < length(x)) {
    require(data.table)
    
    name_dt <- data.table(old = x,
                          clean = clean_names,
                          idx = seq_along(x))
    
    
    for (x in unique(clean_names)) {
      name_dt[clean == x, new_name := paste(clean, .I, sep = "_")]
    }
    
    clean_names <- name_dt[order(idx), new_name]
  }
  
  return(clean_names)
}

