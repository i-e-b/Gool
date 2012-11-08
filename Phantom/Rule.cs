using System;
using Phantom.Parsers;
using Phantom.Scanners;

namespace Phantom
{
	/// <summary>
	/// Superclass of parsers. Provides some convenience routines.
	/// </summary>
	public class Rule
	{
		protected IScanner scanner;
		protected Parser top_level_parser;

		public Rule()
		{
			scanner = null;
			top_level_parser = null;
		}

		public Rule(IScanner InputScanner)
		{
			scanner = InputScanner;
			top_level_parser = null;
		}

		public Rule(IScanner InputScanner, Parser Pattern)
		{
			scanner = InputScanner;
			top_level_parser = Pattern;
		}

		public IScanner ScannerInput
		{
			set { scanner = value; }
		}

		public Parser PatternParser
		{
			set { top_level_parser = value; }
		}

		public ParserMatch Compare()
		{
			if (scanner == null) throw new NullReferenceException("Tried to parse without a valid scanner.");
			if (top_level_parser != null)
			{
				return top_level_parser.Parse(scanner);
			}
			if (this is Parser)
			{
				var p = (Parser) this;
				return p.Parse(scanner);
			}
			throw new ArgumentException("Failed to provide a parser to compare with.");
		}
	}
}