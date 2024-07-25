using System;
using System.Linq;
using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Interfaces;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.MutualRecursion
{
	[TestFixture]
	public class RecursionParserTests
	{
		private IMatchingParser matching_parser;
		private IParser complex_parser;
		private IScanner scanner;
		private Recursion subject;

		[SetUp]
		public void setup()
		{
			scanner = new ScanStrings("hello, world");

			matching_parser = new TestParser_AlwaysGives(()=>new ParserMatch(null, scanner, 0, 0));
			complex_parser = new TestParser_AlwaysGives(()=>new ParserMatch(null, scanner, 0, 0));

			subject = new Recursion();
		}

		[Test]
		public void holding_parser_passes_calls_to_held_matching_parser_instance ()
		{
			subject.Source = matching_parser;

			subject.Parse(scanner);

			Assert.That(((TestParser_AlwaysGives)matching_parser).LastTryMatchScanner, Is.EqualTo(scanner));
		}

		[Test]
		public void holding_parser_passes_calls_to_held_complex_parser_instance ()
		{
			subject.Source = complex_parser;

			subject.Parse(scanner);
	
			Assert.That(((TestParser_AlwaysGives)complex_parser).LastTryMatchScanner, Is.EqualTo(scanner));
		}

		[Test]
		public void holding_parser_protects_itself_from_self_recursion ()
		{
			subject.Source = subject;

			Assert.Throws<Exception>(()=> subject.Parse(scanner)); // if it didn't, this would cause a stack-overflow.
		}
		
		
		[Test]
		public void recursion_parser_protects_itself_from_repeated_zero_length_matches()
		{
			var input = new ScanStrings("<<<w><x>y>z>");
			
			var myParser = BNF.Recursive(tree => !("<" > -(tree | "#[^<>]") > ">")).Result();
			//                                   ^--- this is the problem
			
			var result = myParser.Parse(input);
			
			Assert.That(result.Success, Is.True, result + ": " + result.Value);
			Console.WriteLine(string.Join(", ", result.BottomLevelMatches().Select(x => x.ToString())));
		}
	}
}
