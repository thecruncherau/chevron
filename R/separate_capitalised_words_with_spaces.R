separate_capitalised_words_with_spaces <- function(str) {
  # get vector of characters
  str_vec <- strsplit(str, split = "")[[1]]

  # add additional space before any capital letters not at the beginning
  paste0(str_vec[1],
         vapply(str_vec[-1], \(char) {
           if (char %in% base::LETTERS) paste0(" ", char)
           else char
         }, "") |> paste0(collapse = "", sep = "")
  )
}