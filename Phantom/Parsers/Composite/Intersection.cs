using Phantom.Scanners;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// Create an Intersection parser from two sub-parsers.
	/// </summary>
	public class Intersection : Binary
	{
		public Intersection(IParser first, IParser second)
			: base(first, second)
		{
		}

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

		public override string ToString()
		{
			return LeftParser + "&" + RightParser;
		}
	}
}