import scala.collection.immutable._

object Exercise{

  def countWays(n: Int): Int = {
    1+n/5+n/10+n/25
  }

  def makeChange(amount: Int, denoms: List[Int], index: Int): Int = {
    if(index > denoms.length - 1){ return 1 }
    val denomAmount = denoms(index)
    var ways = 0
    var i = 0
    while((i*denomAmount) <= amount){
      val amountRemaining = amount - (i*denomAmount)
      println("amount: "+amount+" amountRemaining: "+amountRemaining + " coin: "+denomAmount)
      ways += makeChange(amountRemaining, denoms, index + 1)
      i+=1
    }
    ways
  }

}