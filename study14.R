#깜신14탄.
install.packages("moonBook")
library(moonBook)
data(acs)

mytable(Dx~LDLC, data=acs)
Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
mytable(Dx~LDLC+DM+TG, data=acs)
mytable(Dx~., data=acs)
help(Sys.setlocale)
