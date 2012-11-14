using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Composite;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.CompositeParsers
{
	[TestFixture]
	public class SequenceParserTests
	{
		ScanStrings scanner;
		const string Input = "this is my input";
		IParser __this__, __wally__, __is_my__, __dr_jones__;

		[SetUp]
		public void a_scanner_and_some_terminals ()
		{
			scanner = new ScanStrings(Input) {SkipWhitespace = true};

			__this__ = new LiteralString("this");
			__is_my__ = new LiteralString("is my");
			__wally__ = new LiteralString("wally");
			__dr_jones__ = new LiteralString("Dr. Jones!");
		}

		[Test]
		public void passing_left_and_failing_right_side_fails ()
		{
			var subject = new Sequence(__this__, __wally__);
			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.False);
		}
		
		[Test]
		public void failing_left_and_passing_right_side_fails ()
		{
			var subject = new Sequence(__wally__, __this__);
			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.False);
		}
		
		[Test]
		public void passing_left_and_passing_right_side_passes_and_captures_both ()
		{
			var subject = new Sequence(__this__, __is_my__);
			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("this is my"));
		}

		[Test]
		public void failing_left_side_and_failing_right_side_fails ()
		{
			var subject = new Sequence(__wally__, __dr_jones__);

			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.False);
		}
	}
}
