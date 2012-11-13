using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.TerminalParsers
{
	[TestFixture]
	public class RegularExpressionTests
	{
		[Test]
		[TestCase(@"\s\w+\s", " word another, third", " word ")]
		[TestCase(@"\w+", "word another, third", "word")]
		[TestCase(@".+", "word another, third", "word another, third")]
		[TestCase(@"[^,]*", "word another, third", "word another")]
		[TestCase(@".*", "word\non a new line", "word")]
		[TestCase(@"[ -~]*", "print\tprint", "print")]
		[TestCase(@"[ -~]*", "\tprint", "")]
		public void matching_regexes_result_in_matching_parses_and_contain_the_match_and_advance_the_scanner
			(string regex, string inputString, string match)
		{
			IParser subject = new RegularExpression(regex);
			var scanner = new ScanStrings(inputString);
			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.True, "Match result");
			Assert.That(result.Value, Is.EqualTo(match), "Parser match string");
			Assert.That(scanner.Offset, Is.EqualTo(match.Length), "Scanner offset");
		}
		
		[Test]
		[TestCase(@"^.*$", "word \n on a new line", false)]
		[TestCase(@"^.*$", "word    on a new line", true)]
		public void regexes_are_single_line_setting_by_default
			(string regex, string inputString, bool isMatch)
		{
			IParser subject = new RegularExpression(regex);
			var scanner = new ScanStrings(inputString);
			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.EqualTo(isMatch), "Match result");
		}

		[Test]
		public void regexes_that_can_match_empty_strings_can_return_successful_empty_matches ()
		{
			IParser subject = new RegularExpression(@"\W*");
			var scanner = new ScanStrings("123456");

			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo(""));
		}

		[Test]
		public void regexes_whose_match_does_not_start_at_the_current_scanner_position_are_failures ()
		{
			IParser subject = new RegularExpression(@"\w+");
			var scanner = new ScanStrings("      some words");

			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.False);
		}
		
		[Test] // this is a gotcha of PCRE as implemented in .Net
		public void regex_dot_matches_carriage_return_but_not_line_feed ()
		{
			IParser subject = new RegularExpression(@".*");
			var scanner = new ScanStrings("line one\r\nline two");
			var result = subject.TryMatch(scanner);

			Assert.That(result.Value, Contains.Substring("\r"));
		}


		[Test]
		[TestCase(@"\s\w+\s", "word another, third")]
		public void non_matching_regexes_result_in_failing_parses_and_do_not_advance_the_scanner
			(string regex, string inputString)
		{
			IParser subject = new RegularExpression(regex);
			var scanner = new ScanStrings(inputString);
			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.False, "Match result");
			Assert.That(scanner.Offset, Is.EqualTo(0), "Scanner offset");
		}
	}
}
