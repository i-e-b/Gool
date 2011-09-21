using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers {
	/// <summary>
	/// Creates and stores parser matches.
	/// </summary>
	public class ParserMatch: IEnumerable  {
		private Scanners.IScanner match_scanner;
		private int match_offset;
		private int match_length;

		private List<ParserMatch> child_matches;

		/// <summary>
		/// List of child matches (should you need the AST)
		/// </summary>
		public List<ParserMatch> ChildMatches {
			get {
				if (child_matches == null) child_matches = new List<ParserMatch>();
				return child_matches;
			}
		}

		/// <summary>
		/// Action arguments passed to and from this match's actions.
		/// </summary>
		public SemanticActionArgs ActionArguments { get; private set; }

		/// <summary>
		/// The parser that generated this match
		/// </summary>
		public Parser SourceParser { get; private set; }

		/// <summary>
		/// Builds a new match
		/// </summary>
		/// <param name="scanner"></param>
		/// <param name="offset"></param>
		/// <param name="length"></param>
		public ParserMatch(Parser source, Scanners.IScanner scanner, int offset, int length) {
			if (scanner == null)
				throw new ArgumentNullException("Tried to create a match from a null scanner.");

			SourceParser = source;

			match_scanner = scanner;
			match_offset = offset;
			match_length = length;
		}

		/// <summary>
		/// Fire all the source parser's actions
		/// </summary>
		public void FireActions () {
			if (SourceParser == null) return;
			ActionArguments = SourceParser.OnAction(this);

			if (SourceParser.AtomFlag != null) {
				SourceParser.AtomFlag.FireActions(this);
			}
		}

		/// <summary>
		/// Fire the source's AtomFlag actions, if any.
		/// </summary>
		public void FireAtomicActions () {
			if (SourceParser.AtomFlag != null) {
				SourceParser.AtomFlag.FireActions(this);
			}
		}

		/// <summary>
		/// Scanner
		/// </summary>
		public Scanners.IScanner Scanner { get { return match_scanner; } }

		/// <summary>
		/// Offset
		/// </summary>
		public int Offset { get { return match_offset; } }

		/// <summary>
		/// Length
		/// </summary>
		public int Length { get { return match_length; } }

		/// <summary>
		/// Extracts the match value
		/// </summary>
		public String Value {
			get {
				if (Length < 0)
					throw new Exception("no match");
				return Scanner.Substring(Offset, Length);
			}
		}

		public override string ToString() {
			return this.Value;
		}

		/// <summary>
		/// True if match successfull
		/// </summary>
		public bool Success { get { return Length >= 0; } }

		/// <summary>
		/// True if match empty
		/// </summary>
		public bool Empty { get { return Length <= 0; } }
		
		/// <summary>
		/// Create a new match by joining a pair of existing matches
		/// </summary>
		/// <param name="left">left-side of the match</param>
		/// <param name="right">right-side of the match</param>
		/// <returns>Match covering and containing both left and right</returns>
		public static ParserMatch Concat(Parser Source, ParserMatch left, ParserMatch right) {
			if (left == null || right == null)
				throw new ArgumentNullException("Can't concatenate null match");
			if (!left.Success || !right.Success)
				throw new ArgumentException("Can't concatenate to failure match");
			if (left.Scanner != right.Scanner)
				throw new ArgumentException("Can't concatenate between different scanners");

			ParserMatch m = new ParserMatch(Source, left.Scanner, left.Offset, left.Length);
			m.AddSubmatch(left);
			m.AddSubmatch(right);

			return m;
		}

		/// <summary>
		/// Add a sub-parser match, and include it's coverage in this match's coverage.
		/// </summary>
		/// <param name="m">Match to add</param>
		public void AddSubmatch (ParserMatch m) {
			if (m == null)
				throw new ArgumentNullException("Can't add null match.");
			if (!m.Success)
				throw new ArgumentException("Can't add failure match.");

			// Add the child
			ChildMatches.Add(m);

			// extend coverage if needed.
			if (m.Empty)
				return;

			int offset, length;

			if (this.Empty) {
				offset = m.Offset;
				length = m.Length;
			} else {
				offset = Math.Min(this.Offset, m.Offset);
				int m_end = m.Offset + m.Length;
				int t_end = this.Offset + this.Length;
				int end = Math.Max(m_end, t_end);
				length = end - offset;
			}

			this.match_offset = offset;
			this.match_length = length;
		}

		#region IEnumerable Members

		/// <summary>
		/// Walk *every* match in this match tree. This will usually result in
		/// duplicate matches.
		/// </summary>
		public IEnumerable<ParserMatch> DepthFirstWalk () {
			return DepthFirstWalk(this);
		}
		private IEnumerable<ParserMatch> DepthFirstWalk (ParserMatch node) {
			if (node == null) yield break;
			yield return node; // this match
			if (node.ChildMatches == null) yield break;
			foreach (ParserMatch child in node.ChildMatches) {
				foreach (ParserMatch m in DepthFirstWalk(child))
					yield return m;
			}
		}

		/// <summary>
		/// Return the top-most atomic parser matches, ignoring matches
		/// that aren't atoms, ignoring sub-atoms and ignoring this match
		/// even if this is atomic
		/// </summary>
		public IEnumerable<ParserMatch> TopLevelAtoms () {
			foreach (ParserMatch m in this) {
				foreach (ParserMatch m2 in TopLevelAtoms(m)) yield return m2;
			}
			yield break;
		}

		private IEnumerable<ParserMatch> TopLevelAtoms (ParserMatch node) {
			if (node == null) yield break;
			if (node.SourceParser.IsAtomic) {
				yield return node; // this match (never yield unless atomic)
				yield break; // is atomic, so hide any sub-results
			}
			if (node.ChildMatches == null) yield break;
			foreach (ParserMatch child in node.ChildMatches) {
				foreach (ParserMatch m in TopLevelAtoms(child))
					yield return m;
			}
		}


		/// <summary>
		/// Return the bottom-most parser matches, including matches
		/// that aren't atoms, ignoring sub-atomic matches and ignoring this match
		/// even if this is atomic.
		/// </summary>
		public IEnumerable<ParserMatch> BottomLevelMatches () {
			foreach (ParserMatch m in this) {
				foreach (ParserMatch m2 in BottomLevelMatches(m)) yield return m2;
			}
			yield break;
		}

		private IEnumerable<ParserMatch> BottomLevelMatches (ParserMatch node) {
			if (node == null || node.Empty) yield break;
			if (node.SourceParser.IsAtomic) {
				yield return node; // this match
				yield break; // is atomic, so hide any sub-results
			}
			if (node.ChildMatches == null || node.ChildMatches.Count < 1) {
				yield return node; // no children, so yield self (this is the bottom level)
				yield break;
			} else {
				foreach (ParserMatch child in node.ChildMatches) {
					foreach (ParserMatch m in BottomLevelMatches(child))
						yield return m;
				}
			}
		}

		public IEnumerator GetEnumerator () {
			if (ChildMatches == null) return null;
			return ChildMatches.GetEnumerator();
		}

		#endregion
	}
}
