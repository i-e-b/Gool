using NSubstitute;
using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Interfaces;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.MutualRecursion
{
	[TestFixture]
	public class HoldingParserTests
	{
		IMatchingParser matching_parser;
		IParser complex_parser;
		IScanner scanner;
		HoldingParser subject;

		[SetUp]
		public void setup()
		{
			scanner = new ScanStrings("hello, world");

			matching_parser = Substitute.For<IMatchingParser>();
			complex_parser = Substitute.For<IParser>();

			matching_parser.TryMatch(scanner).ReturnsForAnyArgs(new ParserMatch(null, scanner, 0, 0));
			complex_parser.Parse(scanner).ReturnsForAnyArgs(new ParserMatch(null, scanner, 0, 0));

			subject = new HoldingParser();
		}

		[Test]
		public void holding_parser_passes_calls_to_held_matching_parser_instance ()
		{
			subject.HeldParser = matching_parser;

			subject.Parse(scanner);
	
			matching_parser.Received().TryMatch(scanner);
		}

		[Test]
		public void holding_parser_passes_calls_to_held_complex_parser_instance ()
		{
			subject.HeldParser = complex_parser;

			subject.Parse(scanner);
	
			complex_parser.Received().Parse(scanner);
		}

		[Test]
		public void holding_parser_protects_itself_from_self_recursion ()
		{
			subject.HeldParser = subject;

			var result = subject.Parse(scanner); // if it didn't, this would cause a stack-overflow.
	
			Assert.That(result.Success, Is.False);
		}
	}
}
