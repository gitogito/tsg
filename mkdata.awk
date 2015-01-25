BEGIN {
    for (i = 0; i <= 1000; i++) {
        x = i / 1000.0 * 30.0
        printf "%g", x
        for (j = 0; j < 16; j++) {
            y = (1.0 + j / 10) * exp(-x/10)*cos(x)
            printf "\t%g", y
        }
        printf "\n"
    }
}
