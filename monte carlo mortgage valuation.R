#Because R is indexed at 1, we need to use (maturity) T + 1 nodes.
#Specifically, see the definition of "disc_coupon". If we would define nodes_N = 10, we wouldn't be capturing the last coupon
find_mortgage_price <- function(mortgage_tree,coupon,principal_schedule, int_tree, tree=F){
  american_tree <- matrix(nrow=maturity_T, ncol=maturity_T)
  exercise_tree <- matrix(0, nrow = nodes_N, ncol = nodes_N)
  for (j in (maturity_T):1){
    for (i in j:1){
      if(j != (maturity_T)){
        if(j!= 1){
          wait = exp(-1*step_size*int_tree[i,j])*((.5*(american_tree[i,j+1]+american_tree[i+1,j+1])))
          exercise = max(mortgage_tree[i,j] - principal_schedule[j],0)
          american_tree[i,j] = max(wait,exercise)
          if(exercise > wait){
            exercise_tree[i,j]=1
          }
        }
        else{
          american_tree[i,j] = exp(-1*step_size*int_tree[i,j])*((0.5*(american_tree[i,j+1]+american_tree[i+1,j+1])))
        }
      }
      else{
        #by definition, wait and exercise value are 0 in the lats period. hence, option's value is 0
        american_tree[i,j] = 0
      }
    }
  }
  if(tree){
    return(exercise_tree)
  }
  return(mortgage_tree[1,1] - american_tree[1,1])
}


simulate_mortgage <- function(rate_mbs, int_tree=int_treeM, n=100000,par=100000, step_size=0.5){
  path <- vector()
  coupon=par*(rate_mbs/2)/(1-1/(1+(rate_mbs/2))^(nodes_N-1))
  L_t <- c()
  L_t <- PrincipalScheduleFunc(coupon=coupon,L_0 = 100000,r=rate_mbs,step_size = 0.5,maturity_T = nodes_N-1)
  mortgage_tree = find_bond_price(int_tree=int_treeM, maturity_T = nodes_N-1,coupon=coupon,par=0,tree=T)
  exercise_tree = find_mortgage_price(mortgage_tree=mortgage_tree,coupon = coupon,principal_schedule=L_t,int_tree = int_treeM,tree = T)
  values_mortgage <- c()
  for(j in 1:n){
    row=1
    path[1]=int_tree[1,1]
    paid=F
    value_mortgage=0
    for (i in 2:nodes_N){
      if(!paid){
        if(runif(1)>.5){
          row=row+1
        }
        path[i]=int_tree[row,i]
        if(exercise_tree[row,i]>0){
          #to exercise option of early repayment in period t, we owe the outstanding principal in t-1 plus the (mortgage) interest accrued during period t
          disc_principal=L_t[i-1]+L_t[i-1]*(rate_mbs/2)
          for(k in (i-1):1){
            disc_principal=exp(-1*step_size*path[k])*disc_principal
          }
          value_mortgage=value_mortgage+disc_principal
          paid=T
        }
        else{
          for (k in (i-1):1){
            if (k == (i-1)){
              disc_coupon = exp(-1 * step_size * path[k]) * coupon
            }
            else{
              disc_coupon = exp(-1 * step_size * path[k]) * disc_coupon
            }
          }
          value_mortgage= value_mortgage+disc_coupon
        }
      }
    }
    values_mortgage[j]=value_mortgage}
  values_mortgage_df <- as.data.frame(values_mortgage)
  mortgage_value_estimate <- mean(values_mortgage_df[,1])
  return(mortgage_value_estimate)
}
  
target_mortgage_value = 100000

diff_func_montecarlo <- function(r){
  mortgage_value_estimate <- simulate_mortgage(rate_mbs = r)
  return(mortgage_value_estimate - target_mortgage_value)
}

source("monte carlo_find mortgage rate_example 12.7.R")

maturity_T = 20
nodes_N = maturity_T + 1

result <- uniroot(diff_func_montecarlo, interval = c(0.001, 0.2))  # You can adjust the interval based on your problem domain

# Check if uniroot was successful
if (result$root == result$root) {
  print(paste("The mortgage rate (r) for our semi-annually compounded 10-year mortgage with a face value of $100,000 is:", result$root))
} else {
  print("Uniroot was not successful. Try adjusting the interval or using a different method.")
}

