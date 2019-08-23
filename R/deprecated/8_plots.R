
c(plm_pred <- oos[, -1] %*% out$coefficients + plm::fixef(out))
y_pred_mean
oos[, 1]

op <- par(mfrow = c(1, 2))
plot(oos[, 1], y_pred_mean)
lines(x = -1:1, y = -1:1)
plot(oos[, 1], plm_pred)
lines(x = -1:1, y = -1:1)
par(op)

op <- par(mfrow = c(1, 2))
plot(oos[, 1] - y_pred_mean)
abline(h = 0)
plot(oos[, 1] - plm_pred)
abline(h = 0)
par(op)

boxplot(oos[, 1], y_pred_mean)

png("plots/rho_densities.png", width = 800, height = 400, pointsize = 18)
op <- par(mar = c(2, 2, 2, 0.5))
plot(density(results_qu[[1]]$rho_post), xlim = c(0.4, 1), ylim = c(0, 15), 
     col = "darkgreen", main = "Rho posterior densities")
for(i in 2:length(results_qu)) lines(density(results_qu[[i]]$rho_post), col = "darkgreen")
for(i in 1:length(results_kn)) lines(density(results_kn[[i]]$rho_post), col = "darkgreen")
par(op)
dev.off()

png("plots/r2_density.png", width = 1200, height = 600)
plot(density(c(sapply(results_qu, function(x) x$res_other[1, 2]),
               sapply(results_kn, function(x) x$res_other[1, 2]))), 
     main = "R2 density")
dev.off()
