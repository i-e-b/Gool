using System;
using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Interfaces;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.MutualRecursion
{
	[TestFixture]
	public class RecursionParserTests
	{
		IMatchingParser matching_parser;
		IParser complex_parser;
		IScanner scanner;
		Recursion subject;

		[SetUp]
		public void setup()
		{
			scanner = new ScanStrings("hello, world");

			matching_parser = new TestParser_AlwaysGives(()=>new ParserMatch(null, scanner, 0, 0));//Substitute.For<IMatchingParser>();
			complex_parser = new TestParser_AlwaysGives(()=>new ParserMatch(null, scanner, 0, 0));//Substitute.For<IParser>();

			//matching_parser.TryMatch(scanner).ReturnsForAnyArgs(new ParserMatch(null, scanner, 0, 0));
			//complex_parser.Parse(scanner).ReturnsForAnyArgs(new ParserMatch(null, scanner, 0, 0));

			subject = new Recursion();
		}

		[Test]
		public void holding_parser_passes_calls_to_held_matching_parser_instance ()
		{
			subject.Source = matching_parser;

			subject.Parse(scanner);

			Assert.That(((TestParser_AlwaysGives)matching_parser).LastTryMatchScanner, Is.EqualTo(scanner));
			//matching_parser.Received().TryMatch(scanner);
		}

		[Test]
		public void holding_parser_passes_calls_to_held_complex_parser_instance ()
		{
			subject.Source = complex_parser;

			subject.Parse(scanner);
	
			Assert.That(((TestParser_AlwaysGives)complex_parser).LastTryMatchScanner, Is.EqualTo(scanner));
			//complex_parser.Received().Parse(scanner);
		}

		[Test]
		public void holding_parser_protects_itself_from_self_recursion ()
		{
			subject.Source = subject;

			Assert.Throws<Exception>(()=> subject.Parse(scanner)); // if it didn't, this would cause a stack-overflow.
		}
	}
}
