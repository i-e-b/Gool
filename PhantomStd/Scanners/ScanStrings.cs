using System;
using System.Collections.Generic;
using Phantom.Parsers;
using Phantom.Results;

namespace Phantom.Scanners;

/// <summary>
/// Scanner that operates over strings
/// </summary>
public class ScanStrings : IScanner
{
	private readonly List<ParserPoint> _failurePoints;
	private string? _rightMostMatch;
	private int _rightMostPoint;

	/// <summary>
	/// Create a new scanner from an input string.
	/// </summary>
	/// <param name="input">String to scan</param>
	public ScanStrings(string input)
	{
		_rightMostPoint = 0;
		InputString = input;

		if (string.IsNullOrEmpty(input))
			throw new ArgumentException("Initial input is empty");

		Transform = new NoTransform();
		SkipWhitespace = false;
		_failurePoints = new List<ParserPoint>();
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
		return _rightMostMatch;
	}

	/// <inheritdoc />
	public void AddFailure(object tester, int position)
	{
		_failurePoints.Add(new ParserPoint(tester, position));
	}

	/// <inheritdoc />
	public void ClearFailures()
	{
		_failurePoints.Clear();
	}

	/// <inheritdoc />
	public List<string> ListFailures()
	{
		var lst = new List<string>();

		foreach (var p in _failurePoints)
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
		int l = Math.Min(InputString.Length, (_rightMostPoint + length)) - _rightMostPoint;
		return InputString.Substring(_rightMostPoint, l);
	}

	/// <inheritdoc />
	public bool EndOfInput(int offset)
	{
		return offset >= InputString.Length;
	}

	/// <inheritdoc />
	public bool Read(ref int offset)
	{
		if (EndOfInput(offset)) return false;

		offset++;

		return !EndOfInput(offset);
	}

	/// <inheritdoc />
	public char Peek(int offset)
	{
		if (EndOfInput(offset)) return (char)0;
		return Transform.Transform(InputString[offset]);
	}

	/// <summary>
	/// If skip whitespace is set and current position is whitespace,
	/// seek forward until on non-whitespace position or EOF.
	/// </summary>
	public ParserMatch AutoAdvance(ParserMatch? previous)
	{
		previous ??= NullMatch(null, 0);
		
		if (!SkipWhitespace) return previous;
		if (EndOfInput(previous.Right)) return previous;

		var offset = previous.Right;
		var m = EmptyMatch(null, previous.Right);
		var c = Peek(offset);
		while (char.IsWhiteSpace(c))
		{
			if (!Read(ref offset)) break;
			c = Peek(offset);
			m.ExtendTo(offset);
		}
		
		return m.Length > 0 ? m : previous;
	}


	/// <inheritdoc />
	public string Substring(int offset, int length)
	{
		return  Transform.Transform(InputString.Substring(offset, Math.Min(length, InputString.Length - offset)));
	}

	/// <inheritdoc />
	public string RemainingData(int offset)
	{
		return Transform.Transform(InputString.Substring(offset));
	}

	/// <inheritdoc />
	public ITransform Transform { get; set; }

	/// <inheritdoc />
	public ParserMatch NoMatch => new(null, this, 0, -1);

	/// <inheritdoc />
	public ParserMatch EmptyMatch(IParser? source, int offset)
	{
		return new ParserMatch(source, this, offset, 0);
	}
	
	/// <inheritdoc />
	public ParserMatch NullMatch(IParser? source, int offset)
	{
		return new ParserMatch(source, this, offset, -1);
	}
	
	/// <inheritdoc />
	public ParserMatch CreateMatch(IParser source, int offset, int length)
	{
		if ((offset + length) > _rightMostPoint)
		{
			_rightMostPoint = offset + length;
			_rightMostMatch = InputString.Substring(offset, length);
		}
		return new ParserMatch(source, this, offset, length);
	}

	#endregion
}