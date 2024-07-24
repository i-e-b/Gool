using System;
using Phantom.Parsers.Composite.Abstracts;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// Create a generalised repetition parser from a single subparser
	/// </summary>
	public class Repetition : Unary
	{
		/// <summary>
		/// Create a generalised repetition parser from a single subparser
		/// </summary>
		public Repetition(IParser parser, uint lowerBound, uint upperBound)
			: base(parser)
		{
			SetBounds(lowerBound, upperBound);
		}

		/// <summary>
		/// The lower bound of allowed repeat count
		/// </summary>
		public uint LowerBound { get; private set; }

		/// <summary>
		/// The upper bound of allowed repeat count
		/// </summary>
		public uint UpperBound { get; private set; }

		/// <summary>
		/// Set allowable range of repeats
		/// </summary>
		public void SetBounds(uint lowerBound, uint upperBound)
		{
			if (upperBound < lowerBound)
				throw new ArgumentException("Lower bound must be less than upper bound");
			LowerBound = lowerBound;
			UpperBound = upperBound;
		}

		/// <inheritdoc />
		public override ParserMatch TryMatch(IScanner scan)
		{
			if (Parser == this) return scan.NoMatch;

			// save scanner state
			int offset = scan.Offset;

			var m = new ParserMatch(this, scan, 0, 0); // empty match with this parser

			// execution bound
			int count = 0;

			// lower bound, minimum number of executions
			while (count < LowerBound && !scan.EndOfInput)
			{
				var m_temp = Parser.Parse(scan);
				if (!m_temp.Success) break; // stop if not successful
				count++;
				m.AddSubMatch(m_temp);
			}

			if (count == LowerBound)
			{
				while (count < UpperBound && !scan.EndOfInput)
				{
					var m_temp = Parser.Parse(scan);
					if (!m_temp.Success) break; // stop if not successful
					count++;
					m.AddSubMatch(m_temp);
				}
			}
			else
			{
				m = scan.NoMatch;
			}

			if (m == null) m = scan.NoMatch;

			// restoring parser failed, rewind scanner
			if (!m.Success) scan.Seek(offset);

			return m;
		}

		/// <inheritdoc />
		public override string ToString()
		{
			if (LowerBound == 0 && UpperBound > 1) return Parser + "*";
			if (LowerBound == 0 && UpperBound == 1) return Parser + "?";
			if (LowerBound == 1 && UpperBound > 1) return Parser + "+";
			var desc = "[" + LowerBound + ".." + UpperBound + ":" + Parser + "]";

			if (TagValue is null) return desc;
			return desc + " Tag='" + TagValue + "'";
		}
	}
}