args <- commandArgs()
library(ggplot2)
h <- read.csv(file = "C:\\Users\\Luke\\Desktop\\parser\\h.csv")
graf <- ggplot(h, aes(x=Year, y=H)) + geom_bar(stat="identity", fill = "#001F3F", colour="#2ECC40") + 
  labs(x="Year", y="H-Index", title = args[7])
png(filename = "C://Users//Luke//Desktop//plot.png")
plot(graf)
dev.off()





