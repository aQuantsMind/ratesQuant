#PREQ-REQ.
##PRE-REQ. "rates_model.R"; "find_mortgage_rate.R"; "find_mortgage_rate_Example 12.7.R"; "monte carlo_confidence interval.R"; "monte carlo_find mortgage rate example 12.7.R"
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
    values_mortgage[j]=value_mortgage
  }
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

#re-evaluate mortgage value. in other words, show that the mortgage is issued at par if the mortgage rate amounts to the rate found above.
rate_mbs <- result$root
N=100000
par=100000
source("monte carlo_confidence interval.R")

mortgage_value_point_estimate <- mean(mortgage_value_df[,1])
std_error <- sd(mortgage_value_df[,1])/sqrt(N)
print(paste("the point estimate for our mortgage value is",mortgage_value_point_estimate))
print(paste("confidence interval ranges from",mortgage_value_point_estimate-2*std_error,"to",mortgage_value_point_estimate+2*std_error))
