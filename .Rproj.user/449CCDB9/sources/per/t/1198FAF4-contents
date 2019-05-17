#' Write ATF
#'
#' This function takes a two column dataframe and saves it as an Axon Text File.
#' Ensure that the path ends with a slash.
#'
#' @export

write.to.atf <- function(input.data.frame = as.data.frame(cbind(1:100,sin(1:100))),
                      output.dir.path = "S:/Data_Daniel/test_atf_write/",
                      output.file.name = "short_sine.atf"){
  # this line is causing it to hiccup.
  #output.file.name <- paste(output.dir.path, output.file.name, sep = "")
  output.file.name <- paste(output.dir.path, output.file.name, sep = "")
  #write out header
  cat("ATF\t1.0\n0\t2\n", file = output.file.name)
  cat("\"Time (ms)\"\t\"Trace #1\"\n", file = output.file.name, append=TRUE)
  #write out data
  write.table(input.data.frame, file = output.file.name, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
}

#' Read ATF
#'
#' This function takes an ATF, strips the header and returns a dataframe for easy visualization or manipulaiton.
#' It's simply a wrapper for read.table. Ensure that the path ends with a slash.
#'
#' @export

read.atf <- function(input.file.name = "short_sine.atf",
                     input.dir.path = "S:/Data_Daniel/test_atf_write/"){
  atf.location <- paste0(input.dir.path,input.file.name)
  return(as.data.frame(read.table(file = atf.location, skip = 3)))
}

