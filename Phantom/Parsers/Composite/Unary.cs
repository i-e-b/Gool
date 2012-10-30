using System;
using System.Collections.Generic;

namespace Phantom.Parsers.Composite
{
	public abstract class Unary : Parser, ICompositeParser
	{
		protected Parser uParser;

		public Unary(Parser parser)
		{
			Parser = parser;
		}

		public Parser Parser
		{
			get { return uParser; }
			set
			{
				if (value == null)
					throw new ArgumentNullException("Parser may not be null in Unary parser.");
				uParser = value;
			}
		}

		#region ICompositeParser Members

		public List<Parser> ChildParsers()
		{
			var c = new List<Parser>();
			c.Add(uParser);
			return c;
		}

		#endregion
	}
}