#' Return delta delta cq data.frame
#'
#' This function takes a set of cq values and returns a data frame of delta delat cqs..
#'
#' @export


# M <- data.frame(Animal = c(1, 1, 1, 1, 1, 1),
#                 Ganglia = c(1, 2, 3, 1, 2, 3),
#                 Actin = c(0.1, 0.2, 0.3, 0.3, 0.2, 0.1),
#                 Ssh = c(0.2, 0.3, 0.4, 0.6, 0.1, 0.2),
#                 Fsh = c(0.2, 0.3, 0.4, 0.6, 0.1, 0.2))




calc_ddcq <- function(input.df = M,
                      gene.cols = c(4, 5),
                      norm.gene = "Actin",
                      ctrl.col = "Ganglia",
                      ctrl.index = 1){
input.df <- as.data.frame(input.df)
output.df <- input.df
for (i in gene.cols){
  delta.cq <- input.df[, i] - input.df[[norm.gene]]

  temp.ave.delta.cq <- delta.cq[input.df[[ctrl.col]] == ctrl.index]

  temp.ave.delta.cq <- mean(temp.ave.delta.cq)

  output.df[, i] <- 2^(-1*(delta.cq - temp.ave.delta.cq))
}
return(output.df)
}








