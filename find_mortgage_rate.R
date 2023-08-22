#BACKGROUND. We can build pricing models using our interest rate tree model (we built in "rates_model.R")
#GOAL. we're interest in the optimal mortgage rate, which a bank charges to homeowners.
#APPROACH. When setting the mortgage rate, we account not only for the current interest rate environment, but also take into account our view about how the short rate will evolve over time (subsumed by an interest rate tree)

#to find coupon, given mortgage interest rate and other inputs
#alternatively, use pmt() from the FinCal library 
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

#find bond price, given an interest rate tree (see "rates model.R" and a bond's coupon and par)
#to return the bond price only, set tree=False
#to return the whole price tree, set tree=True

#####as a side note,...
#since R is indexed at 1, prices are written in the column 1 (and row 1) in the price tree, and not at column/row 0. However, this also means that the repayment of the face value, together with the last coupon payment, occurs in column/node ((number_payments_per_year)*T)+1, where T is the maturity date 
#(contd.) For example, if we have a 10-year bond with semi-annual coupon payments, the last payment occurs in node (number_of_payments_per_year)*T+1 = 2*10+1 = 21
#(contd.) Instead of having 21 nodes in our price trees, however, we just discount the 21th period back into our node 20, instead of writing the actual payments that occur in node 21 explicitly
find_bond_price <- function(int_tree, maturity_T, coupon=0, par=100, step_size = 0.5, tree=F){
  price_tree <- matrix(NA,nrow=maturity_T,ncol=maturity_T)
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

#to find mortgage price, start with the price of a mortgage without prepayment option (using "mortgage_tree" as an input)
#Then, compute the value of the prepayment option through backward-induction
#In each node t, compute the value of exercising your option by comparing the debt_t homebuyers owe (mortgage value in node t) with the outstanding principal
find_mortgage_price <- function(mortgage_tree,coupon,principal_schedule, int_tree, tree=F){
  american_tree <- matrix(nrow=nodes_N, ncol=nodes_N)
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

#Write a function which computes the outstanding principal in each node.
#motivation. In each node, we want to compare the outstanding principal with the market value of debt (in other words, the mortgage value without prepayment option) which homebuyers owe.
PrincipalScheduleFunc <- function(coupon, L_0 = 100000,r=r,step_size=0.5,maturity_T=maturity_T){
  I_paid=c()
  Principal_paid = c()
  L_t = c()
  for (t in 1:maturity_T){
    if (t==1){
      #initialize the principal schedule in node
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
maturity_T = 20
step_size=0.5

diff_function <- function(r) {
  current_coupon <- FindCoupon(r_m = r)
  L_t <- PrincipalScheduleFunc(coupon=current_coupon,L_0 = 100000,r=r,step_size = 0.5,maturity_T = maturity_T)
  mortgage_tree = find_bond_price(int_tree=int_treeM, maturity_T, coupon=current_coupon,par=0,tree=T)
  current_mortgage_value <- find_mortgage_price(mortgage_tree=mortgage_tree,coupon = current_coupon,principal_schedule=L_t,int_tree = int_treeM,tree = F)
  return(current_mortgage_value - target_mortgage_value)
}

result <- uniroot(diff_function, interval = c(0.001, 0.2))  # You can adjust the interval based on your problem domain

# Check if uniroot was successful
if (result$root == result$root) {
  print(paste("The mortgage rate (r) for our semi-annually compounded 10-year mortgage with a face value of $100,000 is:", result$root))
} else {
  print("Uniroot was not successful. Try adjusting the interval or using a different method.")
}



