using System;
using System.Collections.Generic;
using Phantom.Parsers;

namespace Phantom.Scanners
{
	/// <summary>
	/// Scanner that operates over strings
	/// </summary>
	public class ScanStrings : IScanner
	{
		private readonly string input_string;
		private readonly List<ParserPoint> failure_points;
		private int max_stack_depth;
		private Dictionary<object, int>? parser_points; // Parser => Offset
		private string? right_most_match;
		private int right_most_point;
		private int scanner_offset;
		private bool skip_whitespace;

		/// <summary>
		/// Create a new scanner from an input string.
		/// </summary>
		/// <param name="Input">String to scan</param>
		public ScanStrings(string Input):this(Input, 0) { }

		/// <summary>
		/// Create a new scanner from an input string with an initial offset
		/// </summary>
		/// <param name="Input">String to scan</param>
		/// <param name="InitialOffset">offset from start of input</param>
		public ScanStrings(string Input, int InitialOffset)
		{
			right_most_point = 0;
			input_string = Input;

			if (string.IsNullOrEmpty(Input))
				throw new ArgumentException("Initial input is empty");

			if (scanner_offset >= input_string.Length)
				throw new ArgumentException("Initial offset beyond string end");

			max_stack_depth = 0;
			scanner_offset = InitialOffset;
			Transform = new NoTransform();
			skip_whitespace = false;
			failure_points = new List<ParserPoint>();
		}

		/// <summary>
		/// Gets or sets a boolean value that controls whitespace skipping.
		/// If set to true, white space will be skipped whenever Normalised() is called.
		/// </summary>
		public bool SkipWhitespace
		{
			get { return skip_whitespace; }
			set { skip_whitespace = value; }
		}

		/// <summary>
		/// Get the original input string
		/// </summary>
		public String InputString
		{
			get { return input_string; }
		}

		#region IScanner Members

		/// <inheritdoc />
		public string? FurthestMatch()
		{
			return right_most_match;
		}

		/// <inheritdoc />
		public void AddFailure(object tester, int position)
		{
			failure_points.Add(new ParserPoint(tester, position));
		}

		/// <inheritdoc />
		public void ClearFailures()
		{
			if (failure_points != null) failure_points.Clear();
		}

		/// <inheritdoc />
		public List<string> ListFailures()
		{
			var lst = new List<string>();

			foreach (var p in failure_points)
			{
				var chunk = input_string.Substring(p.Pos);
				var idx = chunk.IndexOfAny(new [] {'\r', '\n'});
				if (idx > 5) chunk = chunk.Substring(0, idx);
				
				lst.Add(chunk + " --> " + ParserStringFrag(p));
			}

			return lst;
		}

		private string ParserStringFrag(ParserPoint p)
		{
			var str = p.Parser.ToString();
			if (str.Length > 100) return str.Substring(0,100);
			return str;
		}

		/// <inheritdoc />
		public string BadPatch(int length)
		{
			int l = Math.Min(InputString.Length, (right_most_point + length)) - right_most_point;
			return InputString.Substring(right_most_point, l);
		}

		/// <inheritdoc />
		public int StackStats(int CurrentDepth)
		{
			if (CurrentDepth > max_stack_depth)
				max_stack_depth = CurrentDepth;

			return max_stack_depth;
		}

		/// <inheritdoc />
		public bool RecursionCheck(object accessor, int offset)
		{
			if (parser_points == null) parser_points = new Dictionary<object, int>();

			if (parser_points.ContainsKey(accessor))
				if (parser_points[accessor] == offset)
				{
					return true; /*throw new Exception("recursion loop");*/
				}

			parser_points[accessor] = offset;
			return false;
		}

		/// <inheritdoc />
		public bool EndOfInput
		{
			get
			{
				if (input_string == null) return true;
				return scanner_offset >= input_string.Length;
			}
		}

		/// <inheritdoc />
		public bool Read()
		{
			if (EndOfInput) return false;

			scanner_offset++;

			return !EndOfInput;
		}

		/// <inheritdoc />
		public char Peek()
		{
			return Transform.Transform(input_string[scanner_offset]);
		}

		/// <summary>
		/// If skip whitespace is set and current position is whitespace,
		/// seek forward until on non-whitespace position or EOF.
		/// </summary>
		public void Normalise()
		{
			if (!skip_whitespace) return;
			if (EndOfInput) return;
			char c = Peek();
			while (Char.IsWhiteSpace(c))
			{
				if (!Read()) break;
				c = Peek();
			}
		}

		/// <inheritdoc />
		public int Offset
		{
			get { return scanner_offset; }
			set
			{
				if (value < 0 || value > input_string.Length)
					throw new Exception("Scanner offset out of bounds");
				scanner_offset = value;
			}
		}

		/// <inheritdoc />
		public void Seek(int offset)
		{
			if (offset < 0 || offset > input_string.Length + 1)
				throw new Exception("Scanner seek offset out of bounds");

			scanner_offset = offset;
		}

		/// <inheritdoc />
		public string Substring(int offset, int length)
		{
			return  Transform.Transform(input_string.Substring(offset, Math.Min(length, input_string.Length - offset)));
		}

		/// <inheritdoc />
		public string RemainingData()
		{
			return Transform.Transform(input_string.Substring(Offset));
		}

		/// <inheritdoc />
		public ITransform Transform { get; set; }

		/// <inheritdoc />
		public ParserMatch NoMatch => new(null, this, 0, -1);

		/// <summary>
		/// 
		/// </summary>
		public ParserMatch EmptyMatch => new(null, this, Offset, 0);

		/// <inheritdoc />
		public ParserMatch CreateMatch(IParser source, int offset, int length)
		{
			if ((offset + length) > right_most_point)
			{
				right_most_point = offset + length;
				right_most_match = InputString.Substring(offset, length);
			}
			return new ParserMatch(source, this, offset, length);
		}

		#endregion
	}
}