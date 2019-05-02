# Workout 3: binomial package

### Summary
This is a package that allows the you to get information related to a binomial distributed variables. It has the following main functions. 

- `bin_choose()` gives you the binomial coefficients. 
- `bin_probability()` gives you the probabilities corresponding to successes. 
- `bin_distribution()` gives you the distribution table and outputs an object of class `bindis`. 
- `bin_cumulative()` gives you cumulative distribution table and outputs and object class `bincum`.
- `bin_variable()` gives you an object of class `binvar`.
- `plot()` can be used to retrieve graphs related to distributions.
- `summary()` can be used to see detailed information about your binomial variable.
- Specific measures can be retrieved using functions like `bin_mean()`, etc. 

### Examples 

Plot an object of class `bindis`.
```r
bindis <- bin_distribution(trials, prob)
plot(bindis)
```

Plot an object of class `bincum`.
```r
bincum <- bin_cumulative(trials, prob)
plot(bincum)
```

Print the summary of a binomial variable
```r
binvar <- bin_variable(trials, prob)
summary(binvar)
```
