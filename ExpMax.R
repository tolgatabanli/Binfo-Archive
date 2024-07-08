# Expectation Maximization

em = function(tetA, tetB, heads, total = 10, reps = 1) {
  # take guessed parameters from global
  parA <- tetA
  parB <- tetB
  for (i in 1:reps) {
    
    # Form first matrix for probabilities of observation for each parameter value (actually likelihoods)
    probs = matrix(c(choose(total, heads)*parA^heads*(1-parA)^(total-heads), 
                     choose(total, heads)*parB^heads*(1-parB)^(total-heads)),
                   nrow = length(heads), ncol = 2)
    
    # Form a second matrix for likelihood odds
    likelihoods = t(cbind(probs[,] / rowSums(probs)))
    
    # Compute the expectation matrix, that is, the expected observation of heads
    # proportional to each parameter's likelihood, here only calculated for heads.
    expectations = (likelihoods %*% heads)
    
    # Lastly, the next estimation is given by summing up expected numbers and dividing by
    # the total likelihoods, like a weighted average, and then by 10 (total coins).
    results = expectations / rowSums(likelihoods) / total
    parA <- results[1,1]
    parB <- results[2,1]
  }
  return(c(parA, parB))
}
trueab = c(0.6, 0.5)
heads = c(5,9,8,4,7)
total = 10
print(em(0.6, 0.5, heads, total, reps = 10))

# Grid search

gridResult = data.frame()
for (i in seq(0.05, 0.95, by = 0.05)) {
  for (k in seq(0.05, 0.95, by = 0.05)) {
    result = em(i, k, heads, reps = 5)
    parA = result[1]
    parB = result[2]
    probs = matrix(c(choose(total, heads)*parA^heads*(1-parA)^(total-heads), 
                     choose(total, heads)*parB^heads*(1-parB)^(total-heads)),
                   nrow = length(heads), ncol = 2)
    maxes = apply(probs, 1, max)
    max_indices = apply(probs, 1, which.max)
    difs = trueab[max_indices] - maxes
    sumdif = sum(difs)
    gridResult = rbind(gridResult, c(i, k, sumdif))
  }
}
colnames(gridResult) = c("x", "y", "mu")

x <- seq(0.05, 0.95, by = 0.05)
y <- seq(0.05, 0.95, by = 0.05)

par(mfrow = c(1,2))

# This plot should show with which starting parameters we lie close to
# the real parameters. NOT FINISHED
lattice::wireframe(mu ~ x * y, gridResult)


