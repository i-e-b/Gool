using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom {
	/// <summary>
	/// Template for Semantic Action event handler
	/// </summary>
	/// <param name="sender">Object sending the message</param>
	/// <param name="args">Parser match details</param>
	public delegate void SemanticAction(Object sender, SemanticActionArgs args);

	/// <summary>
	/// Superclass of parsers. Provides some convenience routines.
	/// </summary>
	public class Rule {
		protected Scanners.IScanner scanner;
		protected Parsers.Parser top_level_parser;

		public Scanners.IScanner ScannerInput {
			set { scanner = value; }
		}

		public Parsers.Parser PatternParser {
			set { top_level_parser = value; }
		}

		public Rule() {
			scanner = null;
			top_level_parser = null;
		}

		public Rule(Scanners.IScanner InputScanner) {
			scanner = InputScanner;
			top_level_parser = null;
		}

		public Rule(Scanners.IScanner InputScanner, Parsers.Parser Pattern) {
			scanner = InputScanner;
			top_level_parser = Pattern;
		}

		public Parsers.ParserMatch Compare() {
			if (scanner == null) throw new NullReferenceException("Tried to parse without a valid scanner.");
			if (top_level_parser != null) {
				return top_level_parser.Parse(scanner);
			} else {
				if (this is Parsers.Parser) {
					Parsers.Parser p = (Parsers.Parser)this;
					return p.Parse(scanner);
				} else {
					throw new ArgumentException("Failed to provide a parser to compare with.");
				}
			}
		}
	}
}
