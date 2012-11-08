using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.Scanners
{
	[TestFixture]
	public class StringScanning_MatchesAndSubstrings
	{
		const string Input = "This is some input";
		IScanner subject;
		Parser dummy_parser;

		[SetUp]
		public void a_string_scanner_with_an_input_string()
		{
			subject = new ScanStrings(Input);
			dummy_parser = new EmptyMatch();
		}

		[Test]
		[TestCase(0, 0, "")]
		[TestCase(0, 18, "This is some input")]
		[TestCase(10, 0, "")]
		[TestCase(5, 5, "is so")]
		[TestCase(2, 16, "is is some input")]
		[TestCase(17, 1, "t")]
		public void create_match_gives_a_successful_scanner_match_with_a_relevant_substring_and_source_parser(int offset, int length, string expectedSubstring)
		{
			var result = subject.CreateMatch(dummy_parser, offset, length);

			Assert.That(result.SourceParser, Is.EqualTo(dummy_parser));
			Assert.That(result.Offset, Is.EqualTo(offset));
			Assert.That(result.Length, Is.EqualTo(length));
			Assert.That(result.Scanner, Is.EqualTo(subject));
			Assert.That(result.ToString(), Is.EqualTo(expectedSubstring));
		}

		[Test]
		public void empty_matches_are_successful ()
		{
			var result = subject.CreateMatch(dummy_parser, 0, 0);
			Assert.That(result.Success, Is.True);
		}

		[Test]
		public void matches_with_negative_length_are_failures ()
		{
			var result = subject.CreateMatch(dummy_parser, 0, -1);
			Assert.That(result.Success, Is.False);
		}

		[Test]
		public void scanner_NoMatch_returns_an_empty_ParserMatch_with_success_of_false()
		{
			var result = subject.NoMatch;

			Assert.IsFalse(result.Success);
			Assert.IsTrue(result.Empty);
			Assert.That(result.Length, Is.EqualTo(-1), "result.Length");
			Assert.That(result.Offset, Is.EqualTo(0), "result.Offset");
		}

		[Test]
		public void scanner_NoMatch_has_offset_of_zero_regardless_of_scanner_offset ()
		{
			for (int i = 0; i < Input.Length; i++)
			{
				var result = subject.NoMatch;
				Assert.That(result.Offset, Is.EqualTo(0));
				subject.Read();
			}
		}

		[Test]
		public void scanner_EmptyMatch_returns_an_empty_ParserMatch_with_success_of_true ()
		{
			var result = subject.EmptyMatch;

			Assert.IsTrue(result.Success);
			Assert.IsTrue(result.Empty);
			Assert.That(result.Length, Is.EqualTo(0));
		}

		[Test]
		public void scanner_EmptyMatch_has_offset_equal_to_scanner_offset ()
		{
			for (int i = 0; i < Input.Length; i++)
			{
				var result = subject.EmptyMatch;
				Assert.That(result.Offset, Is.EqualTo(i));
				subject.Read();
			}
		}

		[Test]
		[TestCase(0,"This is some input")] [TestCase(4," is some input")] [TestCase(11,"e input")] [TestCase(14,"nput")]
		public void scanner_RemainingData_returns_substring_from_the_current_offset_to_the_end_of_input (int offset, string substring)
		{
			subject.Offset = offset;
			var result = subject.RemainingData();

			Assert.That(result, Is.EqualTo(substring));
		}

		[Test]
		[TestCase(0, 0, 0, "")]
		[TestCase(0, 0, 18, "This is some input")]
		[TestCase(4, 10, 0, "")]
		[TestCase(8, 5, 5, "is so")]
		[TestCase(2, 2, 20, "is is some input")]
		[TestCase(5, 17, 1, "t")]
		public void scanner_substring_gives_requested_substring_regardless_of_current_offset(int scannerOffset, int substringOffset, int substringLength, string substring)
		{
			subject.Offset = scannerOffset;
			var result = subject.Substring(substringOffset, substringLength);

			Assert.That(result, Is.EqualTo(substring));
		}
	}
}
