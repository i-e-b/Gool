using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Composite;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.CompositeParsers
{
	[TestFixture]
	public class IntersectionParserTests
	{
		private ScanStrings scanner;
		private const string Input = "this is my input";
		private IParser __this__, __wally__, __is__, __dr_jones__;

		[SetUp]
		public void a_scanner_and_some_terminals ()
		{
			scanner = new ScanStrings(Input) { SkipWhitespace = true };
			__this__ = new LiteralString("this");
			__is__ = new LiteralString("is");
			__wally__ = new LiteralString("wally");
			__dr_jones__ = new LiteralString("Dr. Jones!");
		}

		[Test]
		public void left_and_then_right_passes_matching_left_then_right ()
		{
			var subject = new Intersection(__this__, __is__);

			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("this is"));
		}


		[Test]
		public void right_and_then_left_passes_using_left_then_right ()
		{
			var subject = new Intersection(__is__, __this__);

			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("this is"));
		}

		[Test]
		public void passing_left_and_failing_right_side_fails ()
		{
			var subject = new Intersection(__this__, __wally__);

			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.False);
		}
		
		[Test]
		public void failing_left_and_passing_right_side_fails ()
		{
			var subject = new Intersection(__wally__, __this__);

			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.False);
		}

		[Test]
		public void failing_left_side_and_failing_right_side_fails ()
		{
			var subject = new Intersection(__wally__, __dr_jones__);

			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.False);
		}
	}
}
