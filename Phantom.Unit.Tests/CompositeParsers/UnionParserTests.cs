using NSubstitute;
using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Composite;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.CompositeParsers
{
	[TestFixture]
	public class UnionParserTests
	{
		ScanStrings scanner;
		const string Input = "this is my input";
		IParser __this__, __wally__, __this_is__, __dr_jones__;

		[SetUp]
		public void a_scanner_and_some_terminals ()
		{
			scanner = new ScanStrings(Input);
			__this__ = new LiteralString("this");
			__this_is__ = new LiteralString("this is");
			__wally__ = new LiteralString("wally");
			__dr_jones__ = new LiteralString("Dr. Jones!");
		}

		[Test]
		public void union_parser_with_passing_left_and_failing_right_side_passes ()
		{
			var subject = new Union(__this__, __wally__);

			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("this"));
		}
		
		[Test]
		public void union_parser_with_failing_left_and_passing_right_side_passes ()
		{
			var subject = new Union(__wally__, __this__);

			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("this"));
		}
		
		[Test]
		public void union_parser_with_passing_left_and_passing_right_side_passes_using_longest_match ()
		{
			var subject = new Union(__this__, __this_is__);

			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("this is"));
		}

		[Test]
		public void union_parser_with_failing_left_side_and_failing_right_side_fails ()
		{
			var subject = new Union(__wally__, __dr_jones__);

			var result = subject.TryMatch(scanner);

			Assert.That(result.Success, Is.False);
		}

		[Test]
		public void uses_parse_method_of_child_parsers ()
		{
			var parser = Substitute.For<ITerminal>();
			parser.TryMatch(scanner).Returns(scanner.NoMatch);
			parser.Parse(scanner).Returns(scanner.NoMatch);

			var subject = new Union(parser, parser);

			subject.TryMatch(scanner);

			parser.Received().Parse(scanner);
			parser.DidNotReceive().TryMatch(scanner);
		}
	}
}
