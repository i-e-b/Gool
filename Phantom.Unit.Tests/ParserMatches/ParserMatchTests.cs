using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.ParserMatches
{
	[TestFixture]
	public class ParserMatchTests
	{
		IParser parser;
		IScanner scanner;
		ParserMatch subject;

		[Test]
		public void Creating_a_parser_match_with_a_null_parser_results_in_exception ()
		{
			var ex = Assert.Throws<ArgumentNullException>(()=>
				new ParserMatch(parser, null, 0,0));

			Assert.That(ex.Message, Contains.Substring("Tried to create a match from a null scanner"));
		}

		[Test]
		public void Creating_a_parser_match_with_no_parser_is_accepted ()
		{
			var match = new ParserMatch(null, scanner, 0, 0);
			Assert.That(match.SourceParser, Is.Null);
		}

		[SetUp]
		public void A_parser_match_with_a_parser_and_scanner ()
		{
			parser = new LiteralString("hello");
			scanner = new ScanStrings("Hello world");
			subject = new ParserMatch(parser, scanner, 0, 0);
		}

		[Test]
		public void Should_be_able_to_retrieve_source_scanner ()
		{
			Assert.That(subject.Scanner, Is.EqualTo(scanner));
		}

		[Test]
		public void Should_be_able_to_retrieve_triggering_parser ()
		{
			Assert.That(subject.SourceParser, Is.EqualTo(parser));
		}


		[Test]
		public void not_yet_written ()
		{
			/*
			 * Should check child match trees,
			 * creation, concatenation, empty, adding submatches,
			 * depth first walk, bottom level matches
			 */ 

			Assert.Inconclusive();
		}
	}
}
