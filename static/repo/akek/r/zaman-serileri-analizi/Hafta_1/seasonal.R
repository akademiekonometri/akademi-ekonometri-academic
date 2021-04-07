#====================================
Load.Install("ggseas")
library(ggseas)

AirPassengers
ap_df <- tsdf(AirPassengers)
head(ap_df)

ggplot(ap_df, aes(x = x, y = y)) +
    geom_line(colour = "grey75")

ggplot(ap_df, aes(x = x, y = y)) +
    stat_index(index.ref = 1)

ggplot(ap_df, aes(x = x, y = y)) +
    stat_index(index.ref = 120, index.basis = 1000)

ggplot(ap_df, aes(x = x, y = y)) +
    geom_line(colour = "darkblue", size = 1) +
    stat_rollapplyr(width = 12, FUN = mean, align = "right", colour = "orange", size = 0.5) +
    labs(x = "", y = "Number of US Air Passengers\n(rolling average and original)")

ggplot(ap_df, aes(x = x, y = y)) +
    geom_line(colour = "grey50") +
    stat_decomp(type = "multiplicative", colour = "darkgreen") + ## Mevsimsellik ayarlamasi: klasik dekompozizasyon ile.
    stat_stl(s.window = 7, colour = "red") ## Mevsimsellik ayarlamasi: STL methodu ile.

ggsdc(ap_df, aes(x = x, y = y), method = "decompose") +
    geom_line()
