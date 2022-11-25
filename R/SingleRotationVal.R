# Value At Age for Single Rotation
SingleRotationVal <- function(Age, Stumpage, Volume, Cost, Discount){

  val <- ((Volume*Stumpage) - Cost*((1+Discount)^Age))/((1+Discount)^Age)
  return(val)
}
