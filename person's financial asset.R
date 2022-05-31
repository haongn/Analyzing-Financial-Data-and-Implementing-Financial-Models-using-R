assets = seq(0, 1200, .1)
pdf.27 = dlnorm(assets, 4.2, .8)
pdf.35 = dlnorm(assets, 5.7, .8)

plot(assets, pdf.27, type="l", yaxs="i", ylim = c(0, 1.2*max(pdf.27)),
ylab = "p(y|x)", xlab = "assets, y (thousands)", cex.axis=0.8,
cex.lab=0.8)

points(assets, pdf.35, type="l", lty=3)
legend("topright", inset=0.05, c("age=27", "age=35"), lty = c(1,2),
cex = 0.9)

