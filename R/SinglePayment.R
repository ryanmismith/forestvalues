#SinglePayment(Flow, Rate, Time, Present)

#Compounding
     #FutureVal <- Payment*((1 + InterestRate)^Time

      CompoundingValue <- function(Principle, InterestRate, PeriodLength, Years){
           Val <- Principle*((1 + InterestRate/PeriodLength)^(PeriodLength*Years))
           return(Val)
      }



    #Depreciating:

         #PresentVal = Payment/(1+DiscountRate)^Time
