package exercises.action.imperative

import java.time.{Instant, LocalDate}

import exercises.action.imperative.UserCreationExercises._
import exercises.action.DateGenerator._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.imperative.UserCreationExercisesTest
class UserCreationExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("parseYesNo") {
    assert(parseYesNo("Y"))
    assert(!parseYesNo("N"))

    assert(Try(parseYesNo("Nope")).isFailure)
  }

  test("readSubscribeToMailingList example") {
    forAll { (yesNo: Boolean) =>
      val inputs = ListBuffer(formatYesNo(yesNo))
      val outputs = ListBuffer.empty[String]
      val console = Console.mock(inputs, outputs)
      val result = readSubscribeToMailingList(console)

      assert(result == yesNo)
      assert(outputs.toList == List("Would you like to subscribe to our mailing list? [Y/N]"))
    }

  }

  test("readSubscribeToMailingList example failure") {
    val console = Console.mock(ListBuffer("Never"), ListBuffer())
    val result = Try(readSubscribeToMailingList(console))

    assert(result.isFailure)
  }

  test("readDateOfBirth example success") {
    forAll { (dateOfBirth: LocalDate) =>

      val console = Console.mock(ListBuffer(dateOfBirthFormatter.format(dateOfBirth)), ListBuffer())
      val result = readDateOfBirth(console)

      assert(result == dateOfBirth)
    }


  }

  test("readDateOfBirth example failure") {
    val console = Console.mock(ListBuffer("21/07/1986"), ListBuffer())
    val result = Try(readDateOfBirth(console))

    assert(result.isFailure)
  }

  test("readUser example") {
    val inputs = ListBuffer("Eda", "18-03-2001", "Y")
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(inputs, outputs)
    val now = Instant.now()
    val clock = Clock.constant(now)
    val result = readUser(console, clock)
    val expected = User(
      name = "Eda",
      dateOfBirth = LocalDate.of(2001, 3, 18),
      subscribedToMailingList = true,
      createdAt = clock.now()
    )

    assert(result == expected)
  }

  //////////////////////////////////////////////
  // PART 2: Error handling
  //////////////////////////////////////////////

  test("readSubscribeToMailingListRetry negative maxAttempt") {
    val console = Console.mock(ListBuffer.empty[String], ListBuffer.empty[String])
    val result = Try(readSubscribeToMailingListRetry(console, maxAttempt = -1))

    assert(result.isFailure)
  }

  test("readSubscribeToMailingListRetry example success") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("Never", "N"), outputs)
    val result = readSubscribeToMailingListRetry(console, maxAttempt = 2)

    assert(result == false)
    assert(
      outputs.toList == List(
        "Would you like to subscribe to our mailing list? [Y/N]",
        """Incorrect format, enter "Y" for Yes or "N" for "No"""",
        "Would you like to subscribe to our mailing list? [Y/N]"
      )
    )
  }
  test("readSubscribeToMailingListRetry PBT example success") {
    val validMaxAttemptGen = Gen.choose(1, 10)
    val invalidInputGen: Gen[String] = arbitrary[String].filterNot(str => Set("Y", "N").apply(str))
    val invalidInputListGen: Gen[List[String]] = Gen.listOf(invalidInputGen)
    forAll(validMaxAttemptGen, invalidInputListGen, arbitrary[Boolean]) {
      (maxAttempt: Int, invalidInputs: List[String], yesNo: Boolean) =>
        val validInputs = if (yesNo) "Y" else "N"
        val inputs = ListBuffer.from(invalidInputs :+ validInputs)
        val outputs = ListBuffer.empty[String]
        val console = Console.mock(inputs, outputs)
        val res = Try(readSubscribeToMailingListRetry(console, maxAttempt))

        val pairOutput = List(
          "Would you like to subscribe to our mailing list? [Y/N]",
          s"""Incorrect format, enter "Y" for Yes or "N" for "No""""
        )
        val attempts = (invalidInputs.size + 1).min(maxAttempt)
        val pairOutputs = List.fill(attempts)(pairOutput).flatten

        if (invalidInputs.size < maxAttempt) {
          assert(res == Success(yesNo))
          assert(outputs.toList == pairOutputs.dropRight(1))
        } else {
          assert(res.isFailure)
          assert(outputs.toList == pairOutputs)
        }
    }


  }

  test("readSubscribeToMailingListRetry example invalid input") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("Never"), outputs)
    val result = Try(readSubscribeToMailingListRetry(console, maxAttempt = 1))

    assert(result.isFailure)
    assert(
      outputs.toList == List(
        "Would you like to subscribe to our mailing list? [Y/N]",
        """Incorrect format, enter "Y" for Yes or "N" for "No""""
      )
    )

    // check that the error message is the same as `readSubscribeToMailingList`
    val console2 = Console.mock(ListBuffer("Never"), ListBuffer.empty[String])
    val result2 = Try(readSubscribeToMailingList(console2))
    assert(result.failed.get.getMessage == result2.failed.get.getMessage)
  }

  test("readDateOfBirthRetry negative maxAttempt") {
    val console = Console.mock(ListBuffer.empty[String], ListBuffer.empty[String])
    val result = Try(readSubscribeToMailingListRetry(console, maxAttempt = -1))

    assert(result.isFailure)
  }

  test("readDateOfBirthRetry example success") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("July 21st 1986", "21-07-1986"), outputs)
    val result = readDateOfBirthRetry(console, maxAttempt = 2)

    assert(result == LocalDate.of(1986, 7, 21))
    assert(
      outputs.toList == List(
        """What's your date of birth? [dd-mm-yyyy]""",
        """Incorrect format, for example enter "18-03-2001" for 18th of March 2001""",
        """What's your date of birth? [dd-mm-yyyy]"""
      )
    )
  }

  test("readDateOfBirthRetry example failure") {
    val outputs = ListBuffer.empty[String]
    val invalidAttempt = "July 21st 1986"
    val console = Console.mock(ListBuffer(invalidAttempt), outputs)
    val result = Try(readDateOfBirthRetry(console, maxAttempt = 1))

    assert(result.isFailure)
    assert(
      outputs.toList == List(
        """What's your date of birth? [dd-mm-yyyy]""",
        """Incorrect format, for example enter "18-03-2001" for 18th of March 2001"""
      )
    )
  }

}
