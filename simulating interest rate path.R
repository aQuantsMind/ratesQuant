#PRE-REQ. "rates_model.R"
#MOTIVATION. getting a feel for how the short rate might evolve according to our model
maturity_T = 20
rates_sim <- function(int_tree=int_treeM,n=1000){
  montecarlo_df = data.frame("int rate"=int_tree[,maturity_T],frequency=0)
  for (j in 1:n){
    row=1
    for (i in 2:(maturity_T)){
      if(runif(1)>.5){
        row=row+1
      }
    }
    montecarlo_df[row,2] = montecarlo_df[row,2]+1
  }
  return(montecarlo_df)
}

png("short_rate_inT_bar_plot.png")
plot <- ggplot(montecarlo_df, aes(int.rate, frequency))
base <- plot + geom_bar(stat = "identity")
short_rate_inT_bar_plot <- base + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black",size=0.5),axis.text = element_text(size=7),axis.title = element_text(size=10))
short_rate_inT_bar_plot
dev.off()
