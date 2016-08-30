path_floor <- function(in_start, in_end, in_floor){
   require(dplyr)
   stopifnot(is.vector(in_start, "numeric"))
   stopifnot(is.vector(in_end, "numeric"))
   stopifnot(is.vector(in_floor, "numeric"))
   
   data_path_points <- read.csv("data_path_points.csv")
   
   data_path_floor <- data_path_points[data_path_points$floor==in_floor,]
   
   which_end <- which(in_end[1]==data_path_floor[,c("x")] & in_end[2]==data_path_floor[,c("y")])
   which_start <- which(in_start[1]==data_path_floor[,c("x")] & in_start[2]==data_path_floor[,c("y")])
   
   data_path <- data_path_floor[- which_start,]
   
   dist_end <- as.matrix(dist(data_path[,1:2], upper = TRUE, diag= TRUE))[,which_end]
   
   data_path <- cbind(data_path, distan = dist_end) 
   
   data_path <- data_path %>% mutate(distan_nor = 1-distan/(max(dist_end)+0.01))
   
   path_length <- sample(5:30,1)
   
   list_path <- vector("list", path_length)
   list_path[[1]] <- in_start
   
   i <- 1
   while (list_path[[i]][1]!=in_end[1] | list_path[[i]][2]!=in_end[2]) {
      
      if (i==path_length) {
         list_path[[i]] <- in_end
         break()
      }
      
      iter_start <- list_path[[i]]
      
      dist_tmp <- as.numeric(as.matrix( dist(rbind(iter_start, data_path[,1:2])) )[,1])
      which_near <- order(dist_tmp)<=20
      data_tmp <- data_path[which_near,]
      point_sampl <- sample(1:nrow(data_tmp), 1, prob=data_tmp$distan_nor)
      
      list_path[[i+1]] <- as.numeric(data_tmp[point_sampl,1:2])
      data_path <- data_path[-as.numeric(rownames(data_tmp)[point_sampl]),]
      i <- i+1
   }
   
   data_last <- matrix(unlist(list_path), byrow = TRUE, ncol=2)
   return(data_last)
}
