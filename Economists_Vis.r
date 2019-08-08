#env setting
library(ggplot2)
library(data.table)

#reading the data
ec.data <- as.data.table(read.csv("Economist_Data.csv"))

#removing the first column
ec.data[, X:=NULL]

#data and aesthetic layer
data_aes.layer <- ggplot(ec.data, aes(x = CPI, y = HDI))

#scatter plot
pl <- data_aes.layer + geom_point(aes(color = Region), shape = 1, size = 4) 

#adding a trend line
pl2 <- pl + geom_smooth(aes(group = 1, linetype ="R"), method = lm, se = FALSE, formula = y ~ log(x), color = "red") +
            scale_linetype_discrete(name = NULL, label = expression(R^2 == 56))

#creating relevant label points
labelPoints <-  c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                     "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                     "India", "Italy", "China", "South Africa", "Spane",
                     "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                     "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                     "New Zealand", "Singapore")

#adding the label point texts
pl2 <- pl2 + geom_text(aes(label = Country), check_overlap = TRUE, data = subset(ec.data, Country %in% labelPoints))

# axis details
pl3 <- pl2 + scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)", limits = c(1,10), 
                                breaks = 1:10) + 
             scale_y_continuous(name = "Human Development Index, 2011 (1=bestt)", limits = c(0.2,1), 
                     breaks = seq(0.2, by = 0.2))

#theme and title
pl3 <- pl3 + theme_bw() + ggtitle("Corruption and human development") 

#legend postion and other detail
pl4 <- pl3 + theme(legend.position="top", legend.box = "horizontal") + theme(legend.title=element_blank())

print(pl4)

ggsave("Economists_dupl.png", plot = last_plot(), dpi = 300)