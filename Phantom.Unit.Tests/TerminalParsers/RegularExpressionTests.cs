using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.TerminalParsers
{
	[TestFixture]
	public class RegularExpressionTests
	{
		IScanner scanner;
		IParser subject;
		const string Input = "This is my input";

		[SetUp]
		public void a_string_scanner_with_some_text ()
		{
			scanner = new ScanStrings(Input);
			subject = new RegularExpression(@"\s\w+\s");
		}

		[Test]
		public void matches_regexes ()
		{
			// TODO: write me!
			Assert.Inconclusive();
		}
	}
}
