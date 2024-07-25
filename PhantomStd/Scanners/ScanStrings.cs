using System;
using System.Collections.Generic;
using System.Linq;
using Phantom.Parsers;

namespace Phantom.Scanners;

/// <summary>
/// Scanner that operates over strings
/// </summary>
public class ScanStrings : IScanner
{
	private readonly List<ParserPoint> failure_points;
	private int max_stack_depth;
	private Dictionary<object, int>? parser_points; // Parser => Offset
	private string? right_most_match;
	private int right_most_point;
	private int scanner_offset;

	/// <summary>
	/// Limit of recursion depth, to protect against faulty parser definitions
	/// </summary>
	public int StackSafetyLimit { get; set; } = 1000;

	/// <summary>
	/// Create a new scanner from an input string.
	/// </summary>
	/// <param name="input">String to scan</param>
	public ScanStrings(string input):this(input, 0) { }

	/// <summary>
	/// Create a new scanner from an input string with an initial offset
	/// </summary>
	/// <param name="input">String to scan</param>
	/// <param name="initialOffset">offset from start of input</param>
	public ScanStrings(string input, int initialOffset)
	{
		right_most_point = 0;
		InputString = input;

		if (string.IsNullOrEmpty(input))
			throw new ArgumentException("Initial input is empty");

		if (scanner_offset >= InputString.Length)
			throw new ArgumentException("Initial offset beyond string end");

		max_stack_depth = 0;
		scanner_offset = initialOffset;
		Transform = new NoTransform();
		SkipWhitespace = false;
		failure_points = new List<ParserPoint>();
	}

	/// <summary>
	/// Gets or sets a boolean value that controls whitespace skipping.
	/// If set to true, white space will be skipped whenever Normalised() is called.
	/// </summary>
	public bool SkipWhitespace { get; set; }

	/// <summary>
	/// Get the original input string
	/// </summary>
	public string InputString { get; }

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
		failure_points.Clear();
	}

	/// <inheritdoc />
	public List<string> ListFailures()
	{
		var lst = new List<string>();

		foreach (var p in failure_points)
		{
			var chunk = InputString.Substring(p.Pos);
			var idx = chunk.IndexOfAny(new [] {'\r', '\n'});
			if (idx > 5) chunk = chunk.Substring(0, idx);
				
			lst.Add(chunk + " --> " + ParserStringFrag(p));
		}

		return lst;
	}

	private static string ParserStringFrag(ParserPoint p)
	{
		var str = p.Parser.ToString();
		//if (str.Length > 100) return str.Substring(0,100);
		return str;
	}

	/// <inheritdoc />
	public string BadPatch(int length)
	{
		int l = Math.Min(InputString.Length, (right_most_point + length)) - right_most_point;
		return InputString.Substring(right_most_point, l);
	}

	/// <inheritdoc />
	public int StackStats(int currentDepth)
	{
		if (currentDepth > StackSafetyLimit) throw new Exception($"Stack protection triggered (see {nameof(ScanStrings)}.{nameof(StackSafetyLimit)})");
		
		if (currentDepth > max_stack_depth)
			max_stack_depth = currentDepth;

		return max_stack_depth;
	}

	/// <inheritdoc />
	public bool RecursionCheck(object accessor, int offset)
	{
		parser_points ??= new Dictionary<object, int>();

		if (parser_points.TryGetValue(accessor, out var point) && point == offset)
		{
			return true; //throw new Exception("recursion loop");
		}

		parser_points[accessor] = offset;
		return false;
	}

	/// <inheritdoc />
	public bool EndOfInput
	{
		get
		{
			if (InputString == null) return true;
			return scanner_offset >= InputString.Length;
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
		return Transform.Transform(InputString[scanner_offset]);
	}

	/// <summary>
	/// If skip whitespace is set and current position is whitespace,
	/// seek forward until on non-whitespace position or EOF.
	/// </summary>
	public void Normalise()
	{
		if (!SkipWhitespace) return;
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
			if (value < 0 || value > InputString.Length)
				throw new Exception("Scanner offset out of bounds");
			scanner_offset = value;
		}
	}

	/// <inheritdoc />
	public void Seek(int offset)
	{
		if (offset < 0 || offset > InputString.Length + 1)
			throw new Exception("Scanner seek offset out of bounds");

		scanner_offset = offset;
	}

	/// <inheritdoc />
	public string Substring(int offset, int length)
	{
		return  Transform.Transform(InputString.Substring(offset, Math.Min(length, InputString.Length - offset)));
	}

	/// <inheritdoc />
	public string RemainingData()
	{
		return Transform.Transform(InputString.Substring(Offset));
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