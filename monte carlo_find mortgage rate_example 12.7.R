maturity_T = 10
#since R is indexed at 1
nodes_N = maturity_T +1 

result <- uniroot(diff_func_montecarlo, interval = c(0.001, 0.2))  # You can adjust the interval based on your problem domain

# Check if uniroot was successful
if (result$root == result$root) {
  print(paste("The mortgage rate (r) for our semi-annually compounded 5-year mortgage with a face value of $100,000 is:", result$root))
} else {
  print("Uniroot was not successful. Try adjusting the interval or using a different method.")
}