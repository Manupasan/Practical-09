import scala.io.StdIn._
@main def main()=
  println("========== Question 01 ==========")
  println("Enter the amount: ")
  val amount = readInt()
  interest(amount)

  println("========== Question 02 ==========")
  println("Enter a number: ")
  val num = readInt()
  filterNumber(num)

  println("========== Question 03 ==========")
  println(toUpper("Benny"));
  println(toLower("Niroshan"));
  println(formatNames("Saman")(toUpper));
  println(formatNames("Kumara")(toLower));


def interest(amount : Int)=
  amount match
    case amount if amount <= 20000 => println("Interest is : " + amount * 0.02)
    case amount if amount <= 200000 => println("Interest is : " + amount * 0.04)
    case amount if amount <= 2000000 => println("Interest is : " + amount * 0.035)
    case amount if amount > 2000000=> println("Interest is : " + amount * 0.065)

def filterNumber(num : Int)=
  num match
    case x if x <= 0 => println("Negative/Zero")
    case x if x%2 == 0 => println("Even number")
    case x if x%2 != 0 => println("Odd number")

def toUpper(str: String): String = str match {
  case "" => ""
  case s =>
    if s.head >= 'a' & s.head <= 'z' then
      (s.head - 32).toChar +: toUpper(s.tail)
    else
      s.head +: toUpper(s.tail)
}

def toLower(str: String): String = str match {
  case "" => ""
  case s =>
    if s.head >= 'A' & s.head <= 'Z' then
      (s.head + 32).toChar +: toLower(s.tail)
    else
      s.head +: toLower(s.tail)
}

def formatNames(name: String)(f: (String) => String): String = {
  f(name)
}
