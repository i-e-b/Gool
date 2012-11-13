using System.Text.RegularExpressions;
using NUnit.Framework;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.TerminalParsers
{
	[TestFixture]
	public class RegularExpressionsSettingsTests
	{

		[Test]
		[TestCase("one\ntwo", "^.*$", RegexOptions.Multiline, true, "one")]

		[TestCase("one\ntwo", @"^\w*\s\w*$", RegexOptions.None, true, "one\ntwo")]
		[TestCase("one\ntwo", @"^.*$", RegexOptions.None, false, "")]
		
		[TestCase("hello", "HELLO", RegexOptions.IgnoreCase, true, "hello")]

		[TestCase("hello", "h e l l o", RegexOptions.IgnorePatternWhitespace, true, "hello")]
		
		[TestCase("mind\nthe\ngap", @"^.*$", RegexOptions.Singleline, true, "mind\nthe\ngap")]
		[TestCase("one\ntwo", @".+", RegexOptions.Singleline, true, "one\ntwo")]
		[TestCase("one\ntwo", @"^\w$", RegexOptions.Singleline, false, "")]

		public void regular_expression_parser_can_have_Regex_options_set
			(string inputString, string pattern, RegexOptions options, bool success, string match)
		{
			var subject = new RegularExpression(pattern, options);
			var scanner = new ScanStrings(inputString);
			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.EqualTo(success));
			if (success) Assert.That(result.Value, Is.EqualTo(match));
		}
	}
}
