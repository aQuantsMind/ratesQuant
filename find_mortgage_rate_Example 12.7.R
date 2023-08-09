target_mortgage_value = 100000
num_periods = 10
step_size=0.5


diff_function <- function(r) {
  current_coupon <- FindCoupon(r_m = r,maturity_T = 5)
  L_t <- PrincipalScheduleFunc(coupon=current_coupon,L_0 = 100000,r=r,step_size = 0.5,num_period = num_periods)
  mortgage_tree = obtain_bond_price(int_tree=int_treeM, num_period = num_periods,coupon=current_coupon,par=0,tree=T)
  current_mortgage_value <- obtain_mortgage_price_F(mortgage_tree=mortgage_tree,coupon = current_coupon,principal_schedule=L_t,int_tree = int_treeM,tree = F)
  return(current_mortgage_value - target_mortgage_value)
}


result <- uniroot(diff_function, interval = c(0.05, 0.1))  # You can adjust the interval based on your problem domain

# Check if uniroot was successful
if (result$root == result$root) {
  print(paste("The mortgage rate (r) that gives a mortgage value of $100,000 is:", result$root))
} else {
  print("Uniroot was not successful. Try adjusting the interval or using a different method.")
}

