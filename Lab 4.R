#--------------------------------------------------------
# Data Analytics 2021 FALL Lab03                        |
# By: Haolin Xiong                                       |
# RCS: xiongh                                            |
# 10/1/2021                                             |
#--------------------------------------------------------

set.seed(12345)
help(par)
par(mar = rep(0.2,4))
data_Matrix <-matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])
help("heatmap")
help(rep)
par(mar=rep(0.2,4))
heatmap(data_Matrix)
help("rbinom")
set.seed(678910)
for(i in 1:40){
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)
  if(coin_Flip){
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each =5)
  }
}
par(mar= rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])
par(mar=rep(0.2, 4))
heatmap(data_Matrix)