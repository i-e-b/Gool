using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers.Composite {
	/// <summary>
	/// An abstract template for binary composite parsers
	/// </summary>
	public abstract class Binary : Parser, ICompositeParser {
		protected Parser bLeftParser;
		protected Parser bRightParser;

		public Binary(Parser left, Parser right) {
			LeftParser = left;
			RightParser = right;
		}

		/// <summary>
		/// Gets the left-side parser of the binary pair
		/// </summary>
		public Parser LeftParser {
			get {
				return bLeftParser;
			}
			set {
				if (value == null)
					throw new ArgumentNullException("Left parser may not be null in Binary Parser.");
				bLeftParser = value;
			}
		}

		/// <summary>
		/// Gets the right-side parser of the binary pair
		/// </summary>
		public Parser RightParser {
			get {
				return bRightParser;
			}
			set {
				if (value == null)
					throw new ArgumentNullException("Right parser may not be null in Binary Parser.");
				bRightParser = value;
			}
		}

		#region ICompositeParser Members

		public List<Parser> ChildParsers () {
			List<Parser> c = new List<Parser>();
			c.Add(bLeftParser);
			c.Add(bRightParser);
			return c;
		}

		#endregion
	}
}
