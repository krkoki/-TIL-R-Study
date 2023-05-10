df <- data.frame(x=seq(-5, 5, length.out = 100))
df
df$y <- exp(df$x)/(1+exp(df$x))
win.graph(); plot(df,type='l')
