
tile_size <- 0.1 # m
total_area <- 15 # m2
proportion_blue <- 0.1
area_height <- 3
area_width <- 5

{
  num_tiles <- round((1/(tile_size^2))*total_area,-2)
  tile_vector <- runif(n=num_tiles)
  tile_vector <- ifelse(tile_vector<=proportion_blue,1,0)
  tile_matrix <- matrix(tile_vector,
                      ncol=area_width/tile_size,
                      nrow=area_height/tile_size)
  colnames(tile_matrix) <- c(1:(area_width/tile_size))
  rownames(tile_matrix) <- c(1:(area_height/tile_size))
  plot_matrix <- reshape2::melt(as.matrix(tile_matrix))
}

ggplot(data=as.data.frame(plot_matrix),aes(y=Var1,x=Var2,fill=as.factor(value)))+
  geom_point(shape=22,size=6)+
  theme_void()+
  theme(legend.position="none")+
  scale_fill_manual(values=c("1"="darkblue","0"="white"))
