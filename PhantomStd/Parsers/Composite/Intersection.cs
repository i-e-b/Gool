using Phantom.Parsers.Composite.Abstracts;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// Create an Intersection parser from two sub-parsers.
	/// Either first-then-second should pass, or second-then-first.
	/// </summary>
	public class Intersection : Binary
	{
		/// <summary>
		/// Create an Intersection parser from two sub-parsers.
		/// Either first-then-second should pass, or second-then-first.
		/// </summary>
		public Intersection(IParser first, IParser second)
			: base(first, second)
		{
		}

		/// <inheritdoc />
		public override ParserMatch TryMatch(IScanner scan)
		{
			int offset = scan.Offset;

			var left = LeftParser.Parse(scan);


			if (left.Success)
			{
				var right = RightParser.Parse(scan);
				if (right.Success)
				{
					return ParserMatch.Concat(this, left, right);
				}
			}
			else
			{
				scan.Seek(offset);
				left = RightParser.Parse(scan);
				if (left.Success)
				{
					var right = LeftParser.Parse(scan);
					if (right.Success)
					{
						return ParserMatch.Concat(this, left, right);
					}
				}
			}

			scan.Seek(offset);
			return scan.NoMatch;
		}

		/// <inheritdoc />
		public override string ToString()
		{
			if (TagValue is null) return LeftParser + " & " + RightParser;
			return LeftParser + " & " + RightParser + " Tag='" + TagValue + "'";
		}
	}
}