# Plot life expectancy curves from Social Security tables

lifeTable <- data.frame(read.csv("~/CourseraR/Retirement 2/SS Period Life Table 2011.csv",header=TRUE))

lifeTable[,10] <- NULL
lifeTable[,8] <- NULL
lifeTable[,7] <- NULL
lifeTable[,5] <- NULL
lifeTable[,4] <- NULL
lifeTable[,2] <- NULL
#lifeTable[,2] <- as.numeric(lifeTable[,2])
#lifeTable[,3] <- as.numeric(lifeTable[,3])
#lifeTable[,4] <- as.numeric(lifeTable[,4])

colnames(lifeTable) <- c("Age","Male","Female","Couple")
lives <- melt(lifeTable,id.vars="Age")

# lives$value <- as.numeric(as.character(lives$value))

le <- ggplot(data = lives, aes(x = Age, y = value, color=variable)) +
  geom_line() +
  theme_gray() +
  theme_set(theme_gray(base_size = 12)) + 
  theme(text=element_text(family="Times")) +
  ggtitle(paste("Expected Survival Rates for 10,000\n Retirees Age 65",sep="")) +
  ylab("Survivors") + xlim(c(65,115)) +
  theme(legend.title=element_blank())

print (le)

ggsave(paste(filename=figureWD,"Figure 2.png",sep=""),dpi=600)
