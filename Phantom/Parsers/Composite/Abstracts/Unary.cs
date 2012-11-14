using System;
using System.Collections.Generic;
using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers.Composite
{
	public abstract class Unary : Parser, ICompositeParser
	{
		IParser uParser;

		protected Unary(IParser parser)
		{
			Parser = parser;
		}

		public IParser Parser
		{
			get { return uParser; }
			set
			{
				if (value == null)
					throw new NullReferenceException("Parser may not be null in Unary parser.");
				uParser = value;
			}
		}

		public List<IParser> ChildParsers()
		{
			var c = new List<IParser>();
			c.Add(uParser);
			return c;
		}

	}
}