BEGIN {
    for (i = 0; i <= 1000; i++) {
        x = i / 1000.0 * 30.0
        y = exp(-x/10)*cos(x)
        printf "%g\t%g\n", x, y
    }
}
