### SAR model with independent normal-gamma prior and beta prior for rho ###

# install MASS package for multivariate normal random number
if (!require("MASS")) install.packages("MASS")
require(psych)
# contains function to contruct n-nearest neighbour spatial weight matrix
source("knn.R")
# Fast logdet approximation function
source("lndet.R")

### first let's construct our SAR DGP
W <- kronecker(diag(12), w_queen)

# 7 nearest neighbour W construction from random pattern
# xy <- cbind(runif(n),runif(n))
# W<-getWknn(,7)
#X <- cbind( 1, rnorm(n), rnorm(n) )
X <- bayes[, -1]
Y <- bayes[, 1]
# X <- cbind( rnorm(n), rnorm(n), rnorm(n) )
#X = scale(X,scale = FALSE,center = TRUE)
VV = kronecker(diag(12), matrix(1, nrow(X) / 12, 1))
X = cbind(1, X, W %*% X, VV[, -1])
# RHO = .35
# BETA <- c(0, 5, -2 , 2.5, 1,  -1.2, .03)
# SIGMA = .1
# AI_ = solve(diag(n) - RHO * W)
# 
# Y = AI_ %*% (X %*%  BETA + rnorm(n, mean = 0,sd = SIGMA) )
# 
# DIRECT = sum(diag(AI_))/n * BETA[1:4] + c(0,sum(diag(AI_ %*% W))/n * BETA[5:7])
# TOTAL = sum(AI_)/n * BETA[1:4] + c(0,sum(AI_ %*% W)/n * BETA[5:7])
# INDIRECT =TOTAL - DIRECT

k = ncol(X)
n=nrow(X)
smallk = ncol(bayes[, -1])

### let us assign prior values
# beta mean and variance are proper, but with high variance (= low prior information)
beta_prior_mean = matrix(0,k,1)
beta_prior_var = diag(k) * 10^8
# sigma rate and shape are also proper but non-informative
sigma_a = .001
sigma_b = .001
# rho prior is beta, with 
beta_prob = function(rho,a) 1/beta(a,a) * ((1+rho)^(a-1) *(1-rho)^(a-1))/(2^(2*a - 1))
rho_a = 1.01

### calibration parameters for rho sampling
cc = 1 #scaling of rho proposals
c_adjust = 1.1 #proposal distribution adjustment
rho_accept = 0 #counter for rho acceptance rates

### set-up the gibbs sampler
# total number of draws
niter = 2000
# retain only S2 and discard S1, with S = S1 + S2
nretain = 1000
ndiscard = niter - nretain
# save the posterior draws here
postb = matrix(0,k,nretain)
posts = matrix(0,1,nretain)
postr = matrix(0,1,nretain)
rtmp = matrix(0,1,niter) # rho acceptance rates
post.direct = matrix(0,smallk + 1,nretain)
post.indirect = matrix(0,smallk + 1,nretain)
post.total = matrix(0,smallk + 1,nretain)

# set-up for griddy gibbs
griddy_n = 100
logdets = lndetPaceBarry(as.matrix(W),length.out = griddy_n+2)
logdets = logdets[-c(1,griddy_n + 2),]
rrhos = logdets[,2]
AYs = array(0,c(n,griddy_n))
## storage for efficient partial derivatives
ai_diags = rep(0,griddy_n)
ai_tots = rep(0,griddy_n)
aiW_diags = rep(0,griddy_n)
aiW_tots = rep(0,griddy_n)
cat("Pre-calculate griddy GIBBS...")
for (ii in 1:griddy_n) {
  A = (.sparseDiagonal(n) - rrhos[ii] * W)
  AYs[,ii] = as.matrix(A %*% Y)
  AI = solve(A)
  ai_diags[ii] = sum(diag(AI))
  ai_tots[ii] = sum(AI)
  aiW_diags[ii] = sum(diag(AI %*% W))
  aiW_tots[ii] = sum(AI %*% W)
}
cat("Done!\n")

# starting values (won't matter after sufficient draws)
curr.beta = mvrnorm(1,beta_prior_mean,beta_prior_var)
curr.sigma = 1/rgamma(1,sigma_a/2,sigma_b/2)
curr.rho = 0

# pre-calculate some terms for faster draws
beta_prior_var_inv = solve(beta_prior_var)
XpX = t(X) %*% X
WY = W %*% Y
curr.Ay = Y - curr.rho*WY

### Gibbs sampling
for (iter in 1:niter) {
  cat("iter:",iter,"curr.rho:",curr.rho,"\n")
  
  # draw beta
  V = solve(beta_prior_var_inv + 1/curr.sigma * XpX )
  b = V %*% (beta_prior_var_inv%*%beta_prior_mean + 1/curr.sigma * t(X) %*% curr.Ay )
  curr.beta = mvrnorm(1,b,V)
  
  # draw sigma
  curr.xb = X %*% curr.beta
  curr.ESS = t(curr.Ay - curr.xb) %*% (curr.Ay - curr.xb)
  curr.sigma = 1/rgamma(1, sigma_a + n/2, sigma_b + as.double(curr.ESS) / 2)
  
  ## Griddy-Gibbs step for rho
  V = solve(beta_prior_var_inv + 1/curr.sigma * XpX )
  b0 = V %*% (beta_prior_var_inv%*%beta_prior_mean + 1/curr.sigma * t(X) %*% Y )
  bd = V %*% (beta_prior_var_inv%*%beta_prior_mean + 1/curr.sigma * t(X) %*% WY)
  e0 = Y - X %*% b0
  ed = WY - X %*% bd
  epe0 = as.double(t(e0) %*% e0)
  eped = as.double(t(ed) %*% ed)
  epe0d = as.double(t(ed) %*% e0)
  z = epe0  - 2 * rrhos * epe0d + rrhos^2 * eped
  z = -(n-k)/2 * log(z)
  den = logdets[,1] + z + log(beta_prob(rrhos,rho_a))
  y = rrhos
  adj = max(den)
  den = den - adj
  x = exp(den)
  isum = sum((y[-1] + y[-length(y)])*(x[-1]  - x[-length(x)])/2)
  z = abs(x/isum)
  den = cumsum(z)
  rnd = runif(1) * sum(z)
  ind = max(c(1,which(den <= rnd)))
  curr.rho = rrhos[ind]
  curr.Ay = AYs[,ind]
  curr.ai_diag = ai_diags[ind]
  curr.ai_tot = ai_tots[ind]
  curr.aiW_diag = aiW_diags[ind]
  curr.aiW_tot = aiW_tots[ind]
  
  # ## Metropolis-Hastings step for rho
  # # logdet calculation costly, so do it as few times as possible
  # if (iter == 1) {curr.logdet = log(det(diag(n) - curr.rho*W))}
  # curr.llh = curr.logdet - as.double(curr.ESS)/ (2*curr.sigma) + beta_prob(curr.rho,rho_a)
  # accept = 0; 
  # while (accept!=1) {
  #   prop.rho = curr.rho + cc*rnorm(1,0,1) 
  #   if (prop.rho<1 && prop.rho>-1) {
  #     accept = 1 
  #   }
  # }    
  # prop.Ay = Y - prop.rho*WY
  # prop.ESS = t(prop.Ay - curr.xb) %*% (prop.Ay - curr.xb)
  # prop.logdet = log(det(diag(n) - prop.rho*W))
  # prop.llh =  prop.logdet - as.double(prop.ESS)/ (2*curr.sigma) + beta_prob(prop.rho,rho_a)
  # acc_prob = min(1,exp(prop.llh - curr.llh))
  # if (rbinom(1,1,acc_prob) == 1) {      
  #   curr.rho = prop.rho
  #   rho_accept = rho_accept + 1
  #   curr.logdet = prop.logdet
  #   curr.Ay = prop.Ay
  # }
  # # adjust candidate distribution based on acceptance probability
  # rtmp[iter] = rho_accept/iter
  # if (iter < ndiscard/2) {
  #   if (rtmp[iter]<.4) {
  #     cc <- cc/c_adjust
  #   } else if (rtmp[iter]>.6) {
  #     cc <- cc*c_adjust
  #   }
  # }
  
  # we are past the burn-in, save the draws
  if (iter > ndiscard) {
    s = iter - ndiscard
    postb[,s] = as.matrix(curr.beta)
    posts[s] = curr.sigma
    postr[s] = curr.rho
    
    # calculate summary spatial effects
    post.direct[,s] = curr.ai_diag/n * curr.beta[1:(smallk + 1)] + 
      c(0, curr.aiW_diag / n * curr.beta[(smallk + 2):(2*smallk + 1)]) #
    post.total[,s] = curr.ai_tot/n * curr.beta[1:(smallk + 1)] + 
      c(0, curr.aiW_tot / n * curr.beta[(smallk + 2):(2*smallk + 1)])
    post.indirect[,s] = post.total[,s] - post.direct[,s]
    #AI = solve(diag(n) - curr.rho * W)
    # for (rr in 1:k) {
    #   SW = AI %*% (diag(n) * curr.beta[rr])
    #   post.direct[rr,s] = sum(diag(SW))/n
    #   post.total[rr,s] = sum(SW)/n
    #   post.indirect[rr,s] = post.total[rr,s] - post.direct[rr,s]
    # }
  }
}

### calculate posterior mean of beta, sigma and rho
beta_post_mean = apply(postb,c(1),mean)
sigma_post_mean = mean(posts)
rho_post_mean = mean(postr)

# spatial effects estimates
direct_post_mean = apply(post.direct,c(1),mean)
indirect_post_mean = apply(post.indirect,c(1),mean)
total_post_mean = apply(post.total,c(1),mean)
direct_post_sd = apply(post.direct,c(1),sd)
indirect_post_sd = apply(post.indirect,c(1),sd)
total_post_sd = apply(post.total,c(1),sd)

# output as table, post_mean/post_sd ~ bayesian t-values
cbind(c("constant", colnames(bayes)[-1]),
      apply(
        data.frame(
          Variable = c("constant", colnames(bayes)[-1]),
          Direct = direct_post_mean,
          Direct_t = direct_post_mean / direct_post_sd,
          Indirect = indirect_post_mean,
          Indirect_t = indirect_post_mean / indirect_post_sd)[-1]
        , 2, round, 5)
)

# plots
par(mfrow=c(2,2))
plot(c(postr),main="Rho post. draws",xlab="Retained draws",ylab="Rho")
plot(c(posts),main="Sigma post. draws",xlab="Retained draws",ylab="Sigma")
plot(Y,solve(diag(n)- rho_post_mean*W) %*% X %*% beta_post_mean,
     main="Posterior vs simulated",xlab="Simulated",ylab="Posterior")
psych::violinBy(t(postb),xlab = "Coefficients",ylab = "Density")
par(mfrow=c(1,1))
