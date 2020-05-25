package prologz.resolution

import org.scalatest.flatspec.AnyFlatSpec
import prologz.dsl.{AtomImpl, FactImpl, RuleImpl, StructImpl, Variable, VariableImpl}
import prologz.resolution.Implicits.{RichFact, RichFactList, RichRule, RichTermList}
import prologz.utils.TestUtils

class ImplicitsSpec extends AnyFlatSpec with TestUtils {

  private val variablesToRename: Set[Variable] = Set(VariableImpl("X"), VariableImpl("Y"),  VariableImpl("Z"))
  private val substitution = Substitution(VariableImpl("X") -> AtomImpl(1), VariableImpl("Y") -> StructImpl("s", List(VariableImpl("Z"))), VariableImpl("Z") -> VariableImpl("Y'"))

  private val relTheoryVariables = List(Set(VariableImpl("X"), VariableImpl("Y")), Set(), Set(), Set(), Set(), Set())
  private val mulTheoryVariables = List(Set(VariableImpl("X")), Set(VariableImpl("X"), VariableImpl("Y"), VariableImpl("Z")),
    Set(VariableImpl("X")), Set(VariableImpl("X"), VariableImpl("Y"), VariableImpl("Z"), VariableImpl("W")))

  private val relTheoryRenamed = List(RuleImpl(FactImpl("son", List(VariableImpl("X'"), VariableImpl("Y'"))),
    List(FactImpl("father", List(VariableImpl("Y'"), VariableImpl("X'"))), FactImpl("male", List(VariableImpl("X'"))))),
    FactImpl("father", List(AtomImpl("abraham"), AtomImpl("isaac"))), FactImpl("father", List(AtomImpl("terach"), AtomImpl("abraham"))),
    FactImpl("male", List(AtomImpl("isaac"))), FactImpl("male", List(AtomImpl("abraham"))), FactImpl("male", List(AtomImpl("terach"))))
  private val mulTheoryRenamed = List(FactImpl("sum", List(VariableImpl("X'"), AtomImpl(0), VariableImpl("X'"))),
    RuleImpl(FactImpl("sum", List(VariableImpl("X'"), StructImpl("s", List(VariableImpl("Y'"))), StructImpl("s", List(VariableImpl("Z'"))))),
      List(FactImpl("sum", List(VariableImpl("X'"), VariableImpl("Y'"), VariableImpl("Z'"))))),
    FactImpl("mul", List(VariableImpl("X'"), AtomImpl(0), AtomImpl(0))),
    RuleImpl(FactImpl("mul", List(VariableImpl("X'"), StructImpl("s", List(VariableImpl("Y'"))), VariableImpl("Z'"))),
      List(FactImpl("mul", List(VariableImpl("X'"), VariableImpl("Y'"), VariableImpl("W"))), FactImpl("sum", List(VariableImpl("X'"), VariableImpl("W"), VariableImpl("Z'"))))))

  private val relTheorySubstituted = List(RuleImpl(FactImpl("son", List(AtomImpl(1), StructImpl("s", List(VariableImpl("Y'"))))),
    List(FactImpl("father", List(StructImpl("s", List(VariableImpl("Y'"))), AtomImpl(1))), FactImpl("male", List(AtomImpl(1))))),
    FactImpl("father", List(AtomImpl("abraham"), AtomImpl("isaac"))), FactImpl("father", List(AtomImpl("terach"), AtomImpl("abraham"))),
    FactImpl("male", List(AtomImpl("isaac"))), FactImpl("male", List(AtomImpl("abraham"))), FactImpl("male", List(AtomImpl("terach"))))
  private val mulTheorySubstituted = List(FactImpl("sum", List(AtomImpl(1), AtomImpl(0), AtomImpl(1))),
    RuleImpl(FactImpl("sum", List(AtomImpl(1), StructImpl("s", List(StructImpl("s", List(VariableImpl("Y'"))))), StructImpl("s", List(VariableImpl("Y'"))))),
      List(FactImpl("sum", List(AtomImpl(1), StructImpl("s", List(VariableImpl("Y'"))), VariableImpl("Y'"))))),
    FactImpl("mul", List(AtomImpl(1), AtomImpl(0), AtomImpl(0))),
    RuleImpl(FactImpl("mul", List(AtomImpl(1), StructImpl("s", List(StructImpl("s", List(VariableImpl("Y'"))))), VariableImpl("Y'"))),
      List(FactImpl("mul", List(AtomImpl(1), StructImpl("s", List(VariableImpl("Y'"))), VariableImpl("W"))), FactImpl("sum", List(AtomImpl(1), VariableImpl("W"), VariableImpl("Y'"))))))


  "A term list" should "retrieve all the variables it contains" in {
    assert(termList.getVariables == termListVariables)
  }

  it should "perform a renaming generating a correctly updated term list" in {
    assert(termList.rename(variablesToRename) == termListRenamed)
  }

  it should "perform a substitution generating a correctly updated term list" in {
    assert(termList.substitute(substitution) == termListSubstituted)
  }

  "A fact" should "retrieve all the variables it contains" in {
    assert(fact.getVariables == factVariables)
  }

  it should "perform a renaming generating a correctly updated fact" in {
    assert(fact.rename(variablesToRename) == factRenamed)
  }

  it should "perform a substitution generating a correctly updated fact" in {
    assert(fact.substitute(substitution) == factSubstituted)
  }

  "A fact list" should "retrieve all the variables it contains" in {
    assert(factList.getVariables == factListVariables)
  }

  it should "perform a renaming generating a correctly updated fact list" in {
    assert(factList.rename(variablesToRename) == factListRenamed)
  }

  it should "perform a substitution generating a correctly updated fact list" in {
    assert(factList.substitute(substitution) == factListSubstituted)
  }

  "A rule" should "retrieve all the variables it contains" in {
    assert(rule.getVariables == ruleVariables)
  }

  it should "perform a renaming generating a correctly updated rule" in {
    assert(rule.rename(variablesToRename) == ruleRenamed)
  }

  it should "perform a substitution generating a correctly updated rule" in {
    assert(rule.substitute(substitution) == ruleSubstituted)
  }

}
