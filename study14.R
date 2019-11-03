#깜신14탄.문북을 이용해 table 사용.
install.packages("moonBook")
library(moonBook)
data(acs)

mytable(Dx~LDLC, data=acs)
Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
mytable(Dx~LDLC+DM+TG, data=acs)
mytable(Dx~., data=acs)
help(Sys.setlocale)

