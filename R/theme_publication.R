#' Theme Publication
#'
#' This theme to clean up ggplots for presentations.
#'
#' @export

#ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species))+
#  geom_point() +
#  labs(title = "Iris Sepal Spread")+
#  theme_Publication()

#code modified from: https://rpubs.com/Koundy/71792
theme_Publication <- function(base_size=14, base_family="Arial") {
  theme(plot.title = element_text(face = "bold",
                                size = rel(1.2), 
                                hjust = 0.5),
      text = element_text(),
      #panel.background = element_rect(colour = NA), #removed
      panel.background = element_blank(), #changed
      plot.background = element_rect(colour = NA),
      #panel.border = element_rect(colour = NA), #removed
      axis.title = element_text(face = "bold",size = rel(1)),
      axis.title.y = element_text(angle=90,vjust =2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(), 
      axis.line = element_line(colour="black"),
      axis.ticks = element_line(),
      #panel.grid.major = element_line(colour="#f0f0f0"),
      panel.grid.major = element_line(colour = "grey"), #changed
      panel.grid.minor = element_blank(),
      legend.key = element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size= unit(0.2, "cm"),
      legend.spacing = unit(0, "cm"),
      legend.title = element_text(face="italic"),
      plot.margin=unit(c(10,5,5,5),"mm"),
      strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
      strip.text = element_text(face="bold")
  )
}