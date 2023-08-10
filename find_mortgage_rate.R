FindCoupon <- function(r_m,numb_payments_per_year=2,maturity_T=10,present_value=100000){
  Sum = 0
  Periods_N = numb_payments_per_year*maturity_T
  for (t in 1:Periods_N){
    add_to_Sum <- 1/((1+r_m/numb_payments_per_year)^(t))
    Sum <- Sum + add_to_Sum
    }
  coupon = present_value / Sum
  return(coupon)
}

obtain_bond_price <- function(int_tree, num_period, coupon=0, par=100, step_size = 0.5, tree=F){
  price_tree <- matrix(NA,nrow=num_period,ncol=num_period)
  for (j in num_period:1){
    for (i in j:1){
      if(j == (num_period)){
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

obtain_mortgage_price_F <- function(mortgage_tree,coupon,principal_schedule, int_tree, tree=F){
  american_tree <- matrix(nrow=num_periods, ncol=num_periods)
  exercise_tree <- matrix(0, nrow = num_periods, ncol = num_periods)
  for (j in num_periods:1){
    for (i in j:1){
      if(j != num_periods){
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
        wait = 0
        exercise = max(mortgage_tree[i,j] - principal_schedule[j],0)
        american_tree[i,j] = max(wait,exercise)
        }
      }
    }
  if(tree){
      return(exercise_tree)
    }
  return(mortgage_tree[1,1] - american_tree[1,1])
}

PrincipalScheduleFunc <- function(coupon, L_0 = 100000,r=r,step_size=0.5,num_period=num_periods){
  I_paid=c()
  Principal_paid = c()
  L_t = c()
  for (t in 1:num_period){
    if (t==1){
      I_paid[t] = 0
      Principal_paid[t] = 0
      L_t[t] = L_0
    }
    else {
      I_paid[t] = L_t[t-1]*r*step_size
      if (coupon > I_paid[t]){
        Principal_paid[t] = coupon - I_paid[t]
      }
      else{
        Principal_paid[t] = 0
      }
      
      L_t[t] = L_t[t-1] - Principal_paid[t]
    }
  }
  return(L_t)
}

source("find_mortgage_rate_Example 12.7.R")

target_mortgage_value = 100000
num_periods = 20
step_size=0.5

diff_function <- function(r) {
  current_coupon <- FindCoupon(r_m = r)
  L_t <- PrincipalScheduleFunc(coupon=current_coupon,L_0 = 100000,r=r,step_size = 0.5,num_period = num_periods)
  mortgage_tree = obtain_bond_price(int_tree=int_treeM, num_period = num_periods,coupon=current_coupon,par=0,tree=T)
  current_mortgage_value <- obtain_mortgage_price_F(mortgage_tree=mortgage_tree,coupon = current_coupon,principal_schedule=L_t,int_tree = int_treeM,tree = F)
  return(current_mortgage_value - target_mortgage_value)
}

result <- uniroot(diff_function, interval = c(0.001, 0.2))  # You can adjust the interval based on your problem domain

# Check if uniroot was successful
if (result$root == result$root) {
  print(paste("The mortgage rate (r) that gives a mortgage value of $100,000 is:", result$root))
} else {
  print("Uniroot was not successful. Try adjusting the interval or using a different method.")
}

mortgage_tree[1,1]

