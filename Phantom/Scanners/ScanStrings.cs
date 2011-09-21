using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Scanners {
	public class ScanStrings: IScanner {
		private string input_string;
		private int scanner_offset;
		private string right_most_match = null;
		private int right_most_point;
		private ITransform transform;
		private bool skip_whitespace;
		private int max_stack_depth;
		private Dictionary<object, int> parser_points; // Parser => Offset
		private List<ParserPoint> failure_points;

		/// <summary>
		/// Create a new scanner from an input string.
		/// </summary>
		/// <param name="Input">String to scan</param>
		public ScanStrings(string Input) {
			right_most_point = 0;
			input_string = Input;
			scanner_offset = max_stack_depth = 0;
			transform = null;
			skip_whitespace = false;
		}

		/// <summary>
		/// Create a new scanner from an input string with an initial offset
		/// </summary>
		/// <param name="Input">String to scan</param>
		/// <param name="InitialOffset">offset from start of input</param>
		public ScanStrings(String Input, int InitialOffset) {
			right_most_point = 0;
			if (scanner_offset >= input_string.Length)
                throw new ArgumentException("Initial offset beyond string end");
			input_string = Input;
			max_stack_depth = 0;
			scanner_offset = InitialOffset;
			transform = null;
			skip_whitespace = false;
        }

		/// <summary>
		/// Gets or sets a boolean value that controls whitespace skipping.
		/// If set to true, white space will be skipped whenever Normalised() is called.
		/// </summary>
		public bool SkipWhitespace {
			get { return skip_whitespace; }
			set { skip_whitespace = value; }
		}

		/// <summary>
		/// Get the original input string
		/// </summary>
		public String InputString { get { return input_string; } }

		#region IScanner Members

		public string FurthestMatch() {
			return right_most_match;
		}

		public void AddFailure(object tester, int position) {
			if (failure_points == null) failure_points = new List<ParserPoint>();
			failure_points.Add(new ParserPoint(tester, position));
		}

		public void ClearFailures() {
			if (failure_points != null) failure_points.Clear();
		}

		public List<string> ListFailures() {
			List<string> lst = new List<string>();

			foreach (ParserPoint p in failure_points) {
				string chunk = input_string.Substring(p.pos);
				if (chunk.Length > 5) chunk = chunk.Substring(0, 5);
				lst.Add(p.parser.ToString() + " --> " + chunk);
			}

			return lst;
		}

		public string BadPatch(int length) {
			int l = length + (InputString.Length - (right_most_point + length));
			return InputString.Substring(right_most_point, l);
		}

		public int StackStats(int CurrentDepth) {
			if (CurrentDepth > max_stack_depth)
				max_stack_depth = CurrentDepth;

			return max_stack_depth;
		}

		public bool RecursionCheck(object accessor, int offset) {
			if (parser_points == null) parser_points = new Dictionary<object, int>();

			if (parser_points.ContainsKey(accessor))
				if (parser_points[accessor] == offset) {
					return true; /*throw new Exception("recursion loop");*/
				}

			parser_points[accessor] = offset;
			return false;
		}

		public bool EOF {
			get {
				if (input_string == null) return true;
				return scanner_offset >= input_string.Length;
			}
		}

		public bool Read() {
			if (EOF) return false;

			scanner_offset++;

			return !EOF;
		}

		public char Peek() {
			if (transform == null)
				return input_string[scanner_offset];
			else
				return transform.Transform(input_string[scanner_offset]);
		}

		public void Normalise() {
			if (EOF) return;
			if (skip_whitespace)
				while (Char.IsWhiteSpace(Peek())) { if (!Read()) break; };
		}

		public int Offset {
			get {
				return scanner_offset;
			}
			set {
				if (value < 0 || value > input_string.Length)
					throw new ArgumentOutOfRangeException("Scanner offset out of bounds");
				scanner_offset = value;
			}
		}

		public void Seek(int offset) {
			if (offset < 0 || offset > input_string.Length + 1)
				throw new ArgumentOutOfRangeException("Scanner seek offset out of bounds");

			scanner_offset = offset;
		}

		public string Substring(int offset, int length) {
			string str = input_string.Substring(offset, (int)Math.Min(length, input_string.Length - offset));

			if (transform != null)
				str = transform.Transform(str);

			return str;
		}

		public string RemainingData() {
			string rems = input_string.Substring(Offset);

			if (transform != null)
				rems = transform.Transform(rems);

			return rems;
		}

		public ITransform Transform {
			get {
				return transform;
			}
			set {
				transform = value;
			}
		}

		public Phantom.Parsers.ParserMatch NoMatch {
			get { return new Parsers.ParserMatch(null, this, 0, -1); }
		}

		public Phantom.Parsers.ParserMatch EmptyMatch {
			get { return new Parsers.ParserMatch(null, this, 0, 0); }
		}

		public Phantom.Parsers.ParserMatch CreateMatch(Parsers.Parser source, int offset, int length) {
			if ((offset + length) > right_most_point) {
				right_most_point = offset + length;
				right_most_match = InputString.Substring(offset, length);
			}
			return new Parsers.ParserMatch(source, this, offset, length);
		}

		#endregion
	}
}
