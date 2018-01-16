summary(loan$avg.CreditScore)
summary(loan$CreditGrade)

loan$listingcreation.Year = as.factor(loan$listingcreation.Year)
loan$LoanOriginationQuarter.rev <- as.yearqtr(as.Date(loan$LoanOriginationDate))

loan$LoanOriginationQuarter.rev = as.factor(loan$LoanOriginationQuarter.rev)

#summary(loan$listingcreation.Year)

loan$listingcreation.Year = format(as.Date(loan$ListingCreationDate),"%Y")

summary(loan$LoanOriginationQuarter.rev)

qplot(loan$MonthlyLoanPayment, binwidth= 50)+
  scale_x_continuous(breaks = seq(0,1000,100), limits = c(0,600))

loan$avg.CreditScore = (loan$CreditScoreRangeLower+
                          loan$CreditScoreRangeUpper)/2

suppressWarnings(qplot(loan$avg.CreditScore, binwidth =15)+
                   scale_x_continuous(breaks = seq(500,900,50), limits = c(500,900)))

loan2 <- subset(loan,avg.CreditScore>500&BorrowerState=="CO")

qplot(loan2$CreditScoreRangeLower,loan2$BorrowerAPR, data = loan2)

ggplot(aes(avg.CreditScore,BorrowerAPR, color=listingcreation.Year), data= subset(loan, avg.CreditScore>500))+
  geom_line(stat = "summary", fun.y= mean)+
  stat_bin(geom="text",aes(label=..count..),vjust=-1)

ggplot(aes(LoanOriginationQuarter.rev,BorrowerAPR,group=1), data= subset(loan,BorrowerAPR>0))+
  geom_point(stat= "summary", fun.y= mean)+
  stat_summary(fun.y = mean, geom="line")

The category of the listing that the borrower selected when posting their listing: "0- Not Available",
"Debt Consolidation","Home Improvement", "Business", "Personal Loan", "5 - Student Use", 
"6 - Auto", "7- Other", "8 - Baby&Adoption", "9 - Boat", "10 - Cosmetic Procedure", "11 - Engagement Ring", 
"12 - Green Loans", 
"13 - Household Expenses", "14 - Large Purchases", 
"15 - Medical/Dental", "16 - Motorcycle", "17 - RV"," 18 - Taxes", "19 - Vacation", "20 - Wedding Loans"

head(loan2)