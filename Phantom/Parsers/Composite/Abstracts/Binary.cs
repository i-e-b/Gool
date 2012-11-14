using System;
using System.Collections.Generic;
using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// An abstract template for binary composite parsers
	/// </summary>
	public abstract class Binary : Parser, ICompositeParser
	{
		IParser bLeftParser;
		IParser bRightParser;

		protected Binary(IParser left, IParser right)
		{
			LeftParser = left;
			RightParser = right;
		}

		/// <summary>
		/// Gets the left-side parser of the binary pair
		/// </summary>
		public IParser LeftParser
		{
			get { return bLeftParser; }
			set
			{
				if (value == null)
					throw new NullReferenceException("Left parser may not be null in Binary Parser.");
				bLeftParser = value;
			}
		}

		/// <summary>
		/// Gets the right-side parser of the binary pair
		/// </summary>
		public IParser RightParser
		{
			get { return bRightParser; }
			set
			{
				if (value == null)
					throw new NullReferenceException("Right parser may not be null in Binary Parser.");
				bRightParser = value;
			}
		}

		public List<IParser> ChildParsers()
		{
			var c = new List<IParser>();
			c.Add(bLeftParser);
			c.Add(bRightParser);
			return c;
		}
	}
}