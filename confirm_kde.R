x = rnorm(1000)
y = rnorm(1000, mean = 4, sd = 14)

dx = density(x, bw = "nrd")
dx$y = dx$y / max(dx$y)
dy = density(y, bw = "nrd")
dy$y = dy$y / max(dy$y)

xy = kde2d(x, y)

# normalizing
z = xy$z / sum(xy$z)

dyx = colSums(z)
dyx = dyx / max(dyx)
dxy = rowSums(z)
dxy = dxy / max(dxy)

plot(dx)
lines(xy$x, dxy, col = "red")

plot(dy)
lines(xy$y, dyx, col = "red")


