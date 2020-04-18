package edu.kit.iti.hilbert

import edu.kit.iti.hilbert.parser.HilbertParsers
import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks, TableDrivenPropertyChecks}

class PrettyPrintSpec extends FlatSpec with TableDrivenPropertyChecks with Matchers {

  val strings = Seq("[a + b]p", "<a + b>p", "a -> b -> c", "(a -> b) -> c",
    "[a + b + c]p", "[a + (b + c)]p", "[a* + (b + c)*]p",
    "[a + b ; c]p", "[(a + b) ; c]p",
    "[a ; b + c]p", "[a ; (b + c)]p",
    "[a](p -> q)", "[a]p -> q", "[a]<b>-(p -> q)", "-[a]-q -> <a>q")

  for(s <- strings) {
    it must "be correct for " + s in {
      Fact(HilbertParsers.parseFormula(s)).toString.substring(4) should be(s)
    }
  }

}
