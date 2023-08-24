#PRE-REQ. "yieldcurve.R"

#GOAL. Building a model forecasting the short-term rate (6-months rate)
#MOTIVATION. The resulting interest rate tree can be used to price all sorts of rates derivatives


#to easily source R scripts, change the following folder path to your local github repo
setwd("/Users/dominik/R_quant/rates quant_MBS modelling/veronesi/ratesQuant")

#function which returns a bond price (or, alternatively returns, the underlying price tree)
#R convention: num_period refers to bonds' maturity time T. To keep our pricing and interest trees aligned, however, we write the price of a bond (its value today) in node 1,1, not 0,0. In node T,T of our bond price tree, we therefore need to discount by one period (see "if i == num_period" code below) 
#pricing tree with "Nodes_N" nodes (and maturity_T periods); the last node has a column with 0 entries (coupon occurs in that period, but we denote its discounted values in nodes_N-1)
obtain_bond_price <- function(int_tree, maturity_T, coupon=0, par=100, step_size = 0.5, tree=F){
  nodes_N = maturity_T + 1
  price_tree <- matrix(0,nrow=nodes_N,ncol=nodes_N)
  for (j in maturity_T:1){
    for (i in j:1){
      if(j == (maturity_T)){
        price_tree[i,j] = exp(-1*step_size*int_tree[i,j])*(par+coupon)
      }
      else{
        price_tree[i,j]= exp(-1*step_size*int_tree[i,j])*((.5*(price_tree[i,j+1]+price_tree[i+1,j+1]))+coupon)
      }
    }
  }
  if(tree){
    return(price_tree)
  }
  return(price_tree[1,1])
}

#load yield curve data
source("yieldcurve.R")

# solver. find drift term which calibrates interest rate tree in a way that matches (zero bond) pricing tree
#initialize interest rate tree and a vector holding the drift term theta


#idea. to build our interest rate tree we search for a drift terms theta_i which match the term structure
#since we take the first short-term rate r_0.5(0,0.5) as given, we start our for loop at 2
calibrate_rates_tree <- function(maturity_T,step_size=0.5,tree=F){
  nodes_N = maturity_T + 1
  
  int_treeM <- matrix(0,nrow=nodes_N,ncol=nodes_N)
  theta <- c()
  
  #assume our initial short rate is given, and take it as a starting point for the BDT or Ho-Lee model
  ZeroT1 <- yieldcurve[3,1]
  #inexplicitly
  #int_treeM[1,1] <- yieldcurve[4,1]
  #explicitly:
  int_treeM[1,1] <- -1/step_size*log(ZeroT1/100)
  
  for (Zi in 2:maturity_T) {
    #to keep deviations between our model and the observed term structure low, we choose a low target error. The choice is discretionary, but I believe 10^(-10) should do the trick
    tgt_err = 10^(-5)
    error = 0.2
    #choose lower and upper bound for the drift term
    Theta_min = -.5
    Theta_max = .5
    #input the interest rate vol (i.e., computed as the standard deviation of past realized changes in the short term interest rates)
    sigma=0.214190909
    #time interval delta
    delta=0.5
    
    while (abs(error) > abs(tgt_err)) {
      thetaMean = mean(c(Theta_min, Theta_max))
      #to predict interest rate in next period, Zi, use Z_0 as a starting point and forecast according to BDT or Ho-Lee
      Z_0 <-Zi-1
      for (i in 1:Z_0){
        z0log <- log(int_treeM[i,Z_0])
        zUplog <- z0log + thetaMean * delta + sigma * sqrt(delta)
        int_treeM[i,Zi] <- exp(zUplog)
        zDownlog <- z0log + thetaMean * delta - sigma * sqrt(delta)
        int_treeM[i+1,Zi] <- exp(zDownlog)
      }
      
      error = Zeros[Zi] - obtain_bond_price(int_tree=int_treeM,maturity_T=Zi)
      if (error > 0){ 
        Theta_max = thetaMean}
      else{
        Theta_min = thetaMean
      }
    }
    theta[Zi-1] <- thetaMean
  }
  if(tree){
    return(int_treeM)
  }
  else{
  return(theta)
  }
}

#find the drift terms theta_i
theta <- calibrate_rates_tree(maturity_T = 20)
print(theta)

#find interest rate tree
int_treeM <- calibrate_rates_tree(maturity_T = 20,tree=T)
View(int_treeM)


