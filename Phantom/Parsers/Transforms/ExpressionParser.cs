using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers.Transforms {
	public class ExpressionParser : Parser {
		public Parser ChildParser { get; set; }

		/// <summary>
		/// Syntactic sweetener for 'new ExpressionParser()'
		/// </summary>
		/// <param name="child">Parser to extract and re-order atoms from.</param>
		/// <returns>Expression parser</returns>
		public static ExpressionParser Wrap (Parser child) {
			return new ExpressionParser(child);
		}

		/// <summary>
		/// Create a new Expression parser with a child parser to provide atoms.
		/// </summary>
		/// <param name="child"></param>
		public ExpressionParser (Parser child) {
			if (child == null) throw new ArgumentException("child parser null while creating an ExpressionParser");
			ChildParser = child;
		}

		/// <summary>
		/// Parses child parser, picking up the top-most
		/// layer of atoms and re-arranging based on order-of-precedence
		/// </summary>
		public override ParserMatch ParseMain (Phantom.Scanners.IScanner scan) {
			#region Holding code -- replace with proper code later
			scan.Normalise();

			Parsers.ParserMatch m = this.ParseMain(scan);
			return m;
			#endregion
		}
	}
}
