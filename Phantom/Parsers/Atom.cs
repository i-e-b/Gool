using System;
using System.Collections.Generic;

namespace Phantom.Parsers
{
	/// <summary>
	/// Represents the smallest sematically important parser match
	/// chunk in a tree of parsers.
	/// To mark a parser as atomic, use the extension method 'Atomic()'
	/// or set the parser's 'AtomFlag' property.
	/// Note: Each parser should contain a seperate instance of 'Atom'.
	/// </summary>
	/// <remarks>
	/// Sub-parsers of atomic parsers may also be atomic, but this atomicity
	/// will be masked.
	/// This is so, for example, a mathematical expression can have it's parts
	/// marked atomic for an ExpressionParser, and statements containing these 
	/// expressions can also be atomic to allow clean compositing of statements.
	/// </remarks>
	public class Atom
	{
		public static Parser Wrap(Parser p)
		{
			if (p == null) throw new ArgumentException("'Atom' can't wrap a null object", "p");
			p.AtomFlag = new Atom();
			return p;
		}

		public static List<Parser> GetTopLevelAtoms(Parser p)
		{
			var l = new List<Parser>();

			var cp = p as ICompositeParser;
			if (cp == null)
			{
				if (p.AtomFlag != null) l.Add(p);
				return l;
			}

			return null;
		}
	}
}