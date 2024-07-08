dinuc <- matrix(c(0.09, 0.09, 0.07, 0.05, 0.11, 0.08, 0.04, 0.04, 0.07, 0.06, 0.03, 0.05, 0.06,0.06,0.06, 0.04),
                4,4)
dimnames(dinuc) <- list(c("a", "t","c","g"), c("a", "t","c","g"))
trans <- t(apply(dinuc, 1, function(x) x / sum(x))) # transition matrix
dimnames(trans) = dimnames(dinuc)

seq <- "ctattaaga"
seq <- strsplit(seq, split = "")[[1]]

prob = sum(dinuc[3,])

for(k in seq((length(seq[]) - 1))) {
  print(k)
  prob = prob * trans[seq[k],seq[k+1]]
  print(prob)
}

prob

# Viterbi

states <- matrix(c(-2.322,-1.737, -1.737, -2.322, -1.737, -2.322, -2.322, -1.737), 4, 2)
colnames(states) <- c("H", "L")
rownames(states) <- c("A", "C", "G", "T")

transState <- matrix(c(-1, -1.322, -1, -0.737), 2,2,dimnames = list(c("H","L"),c("H","L")))
startToH <- -1
startToL <- -1

seq <- strsplit("GGCACTGAA", split = "")[[1]]
viterbi <- matrix(double(length(seq) * 2 + 2), nrow = 2)
viterbi[,1] = -1
viterbi[,2] = viterbi[,1] + states[seq[1],]
colnames(viterbi) <- c("start", seq)

viterbi

findPossibles <- function(mat, now, state) {
  return(mat[, now - 1] + t(transState[, state]))
}
nstates <- ncol(states)
# B?? SORUN VAR AMA NE
for (k in seq(from = 2, to = length(seq))) {
  possibles <- sapply(seq(nstates), function(x) findPossibles(viterbi, k + 1, x)) # vectors for each next state
  maxes <- apply(possibles, 2, which.max)
  print(possibles)
  viterbi[, k + 1] <- possibles[matrix(c(seq(nstates), maxes), nstates, nstates)] + states[seq[k],]
}
viterbi
  