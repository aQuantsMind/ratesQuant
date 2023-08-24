# ratesQuant

This collection of RScripts shows how to use backward induction and monte carlo simuations to price mortgages and MBS. It builds on Pietro Veronesi's "fixed income securities. valuation, risk, and risk management" from 2010. challenge: reverse-engineer example 12.7 and generalize the base case.

Contents. 
Note. "rates model.R", "find_mortgage_rate.R", and "monte carlo mortgage valuation.R" are the main scripts (when sourcing the scripts, one after another (in the order they're being mentioned), they'll load dependencies automatically)

- "rates model.R" forecasts interest rates using the BDT model (which uses log interest rates) to construct an interest rates. We use the observed yield curve to construct our interest rate tree, thereby incorporating the market's current view of interest rates. We then define drift and diffusion terms to model our view of the future interest rate path
- "find_mortgage_rate.R" uses backward induction to find the optimal mortgage rate at which one might optimmaly issue the specified mortgage
- "monte carlo mortgage valuation.R" shows an alternative way to find the mortgage rate. It should yield the same result as "find_mortgage_rate.R". The script also loads the "monte carlo_confidence interval.R" script and re-evaluates the mortgage contract using the mortgage rate found previously in the script. The point estimate should be around 100k (as the mortgage is issued at par).
