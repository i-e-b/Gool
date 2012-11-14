using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Phantom.Parsers
{
	/// <summary>
	/// Creates and stores parser matches.
	/// </summary>
	public class ParserMatch : IEnumerable<ParserMatch>
	{
		readonly IScanner match_scanner;

		List<ParserMatch> child_matches;
		int match_length;
		int match_offset;

		/// <summary>
		/// Builds a new match
		/// </summary>
		public ParserMatch(IParser source, IScanner scanner, int offset, int length)
		{
			if (scanner == null)
				throw new ArgumentNullException("scanner", "Tried to create a match from a null scanner.");

			SourceParser = source;

			match_scanner = scanner;
			match_offset = offset;
			match_length = length;
		}

		/// <summary>
		/// List of child matches (should you need the AST)
		/// </summary>
		public List<ParserMatch> ChildMatches
		{
			get { return child_matches ?? (child_matches = new List<ParserMatch>()); }
		}

		/// <summary>
		/// The parser that generated this match
		/// </summary>
		public IParser SourceParser { get; private set; }

		/// <summary>
		/// Scanner
		/// </summary>
		public IScanner Scanner
		{
			get { return match_scanner; }
		}

		/// <summary>
		/// Offset
		/// </summary>
		public int Offset
		{
			get { return match_offset; }
		}

		/// <summary>
		/// Length
		/// </summary>
		public int Length
		{
			get { return match_length; }
		}

		/// <summary>
		/// Extracts the match value
		/// </summary>
		public String Value
		{
			get
			{
				if (Length < 0)
					throw new Exception("no match");
				return Scanner.Substring(Offset, Length);
			}
		}

		/// <summary>
		/// True if match successfull
		/// </summary>
		public bool Success
		{
			get { return Length >= 0; }
		}

		/// <summary>
		/// True if match empty
		/// </summary>
		public bool Empty
		{
			get { return Length <= 0; }
		}

		#region IEnumerable Members

		public IEnumerator GetEnumerator()
		{
			if (ChildMatches == null) yield break;
			foreach (var childMatch in child_matches)
			{
				yield return childMatch;
			}
		}
		IEnumerator<ParserMatch> IEnumerable<ParserMatch>.GetEnumerator()
		{
			if (ChildMatches == null) yield break;
			foreach (var childMatch in child_matches)
			{
				yield return childMatch;
			}
		}

		#endregion

		public override string ToString()
		{
			return Value;
		}

		/// <summary>
		/// Create a new match by joining a pair of existing matches
		/// </summary>
		/// <returns>Match covering and containing both left and right</returns>
		public static ParserMatch Concat(Parser source, ParserMatch left, ParserMatch right)
		{
			if (left == null || right == null)
				throw new NullReferenceException("Can't concatenate null match");
			if (!left.Success || !right.Success)
				throw new ArgumentException("Can't concatenate to failure match");
			if (left.Scanner != right.Scanner)
				throw new ArgumentException("Can't concatenate between different scanners");

			var m = new ParserMatch(source, left.Scanner, left.Offset, left.Length);
			m.AddSubmatch(left);
			m.AddSubmatch(right);

			return m;
		}

		/// <summary>
		/// Add a sub-parser match, and include it's coverage in this match's coverage.
		/// </summary>
		/// <param name="m">Match to add</param>
		public void AddSubmatch(ParserMatch m)
		{
			if (m == null)
				throw new ArgumentNullException("m", "Can't add null match.");
			if (!m.Success)
				throw new ArgumentException("Can't add failure match.");

			// Add the child
			ChildMatches.Add(m);

			// extend coverage if needed.
			if (m.Empty)
				return;

			int offset, length;

			if (Empty)
			{
				offset = m.Offset;
				length = m.Length;
			}
			else
			{
				offset = Math.Min(Offset, m.Offset);
				int m_end = m.Offset + m.Length;
				int t_end = Offset + Length;
				int end = Math.Max(m_end, t_end);
				length = end - offset;
			}

			match_offset = offset;
			match_length = length;
		}

		/// <summary>
		/// Walk *every* match in this match tree. This will usually result in
		/// duplicate matches.
		/// </summary>
		public IEnumerable<ParserMatch> DepthFirstWalk()
		{
			return DepthFirstWalk(this);
		}

		IEnumerable<ParserMatch> DepthFirstWalk(ParserMatch node)
		{
			if (node == null) yield break;
			yield return node; // this match
			if (node.ChildMatches == null) yield break;
			foreach (ParserMatch child in node.ChildMatches)
			{
				foreach (ParserMatch m in DepthFirstWalk(child))
					yield return m;
			}
		}


		/// <summary>
		/// Return the bottom-most parser matches, including matches
		/// that aren't atoms, ignoring sub-atomic matches and ignoring this match
		/// even if this is atomic.
		/// </summary>
		public IEnumerable<ParserMatch> BottomLevelMatches()
		{
			foreach (ParserMatch m in this)
			{
				foreach (var m2 in BottomLevelMatches(m)) yield return m2;
			}
		}

		IEnumerable<ParserMatch> BottomLevelMatches(ParserMatch node)
		{
			if (node == null || node.Empty) yield break;

			if (node.ChildMatches == null || node.ChildMatches.Count < 1)
			{
				yield return node; // no children, so yield self (this is the bottom level)
				yield break;
			}
			
			foreach (var m in node.ChildMatches.SelectMany(BottomLevelMatches))
			{
				yield return m;
			}
		}
	}
}