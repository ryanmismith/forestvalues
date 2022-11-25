# Land Expected Value Uneven Aged Forest

LandExVal <- SingleRotationAge <- function(Age, Stumpage, Volume, Cost, Discount){

  Cost <- ifelse(Cost < 0, Cost*-1, Cost)

  val <- ((Volume*Stumpage) - Cost*((1+Discount)^Age))/(((1+Discount)^Age) - 1)
  return(val)


}
