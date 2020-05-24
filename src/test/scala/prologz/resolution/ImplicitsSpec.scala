package prologz.resolution

import org.scalatest.flatspec.AnyFlatSpec
import prologz.resolution.Implicits.{RichFact, RichFactList, RichRule, RichTermList}
import prologz.utils.TestUtils

class ImplicitsSpec extends AnyFlatSpec with TestUtils {

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
