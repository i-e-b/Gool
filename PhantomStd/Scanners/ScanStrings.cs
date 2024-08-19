using System;
using System.Collections.Generic;
using Phantom.Parsers.Terminals;
using Phantom.Results;

namespace Phantom.Scanners;

/// <summary>
/// Scanner that operates over strings.
/// </summary>
public class ScanStrings : IScanner
{
	private readonly List<ParserPoint> _failurePoints = new();
	private readonly List<ParserMatch> _matchPaths = new();
	private readonly Dictionary<object, object?> _contexts = new();
	private int _rightMostPoint;
	private bool _completed;

	/// <summary>
	/// Create a new scanner from an input string.
	/// </summary>
	/// <param name="input">String to scan</param>
	public ScanStrings(string input)
	{
		_rightMostPoint = 0;
		_completed = false;
		InputString = input;

		if (string.IsNullOrEmpty(input))
			throw new ArgumentException("Initial input is empty");

		Transform = new NoTransform();
		SkipWhitespace = false;
	}

	/// <summary>
	/// Gets or sets a boolean value that controls whitespace skipping.
	/// If set to true, white space will be skipped whenever Normalised() is called.
	/// </summary>
	public bool SkipWhitespace { get; set; }

	/// <summary>
	/// If <c>true</c>, auto-advanced elements (like white-space skips)
	/// will be added to the result tree.
	/// </summary>
	public bool IncludeSkippedElements { get; set; }

	/// <summary>
	/// Get the original input string
	/// </summary>
	public string InputString { get; }

	#region IScanner Members

	/// <summary>
	/// Add a success path, for diagnostic use
	/// </summary>
	public void AddPath(ParserMatch newMatch)
	{
		_matchPaths.Add(newMatch);
	}

	/// <inheritdoc />
	public void Complete()
	{
		_completed = true;
	}

	/// <inheritdoc />
	public void SetContext(IParser parser, object? context)
	{
		_contexts[parser] = context;
	}

	/// <inheritdoc />
	public object? GetContext(IParser parser)
	{
		if (_contexts.TryGetValue(parser, out var ctx)) return ctx;
		return null;
	}

	/// <inheritdoc />
	public void AddFailure(IParser failedParser, ParserMatch? previousMatch)
	{
		_failurePoints.Add(new ParserPoint(failedParser, previousMatch));
	}

	/// <inheritdoc />
	public void ClearFailures()
	{
		_failurePoints.Clear();
	}

	/// <inheritdoc />
	public List<string> ListFailures(bool includePartialMatches = false)
	{
		var lst = new List<string>();

		foreach (var p in _failurePoints)
		{
			var prev = InputString.Substring(0, p.Position);
			var left = p.Length >= 0 ? InputString.Substring(p.Position, p.Length) : "";
			var right = InputString.Substring(p.Position + p.Length);
				
			lst.Add(prev + "◢" + left + "◣" + right + " --> " + ParserStringFrag(p));
		}

		if (includePartialMatches)
		{
			foreach (var m in _matchPaths)
			{
				lst.Add(" ¿" + m.Description() + "? ");
			}
		}

		return lst;
	}

	private static string ParserStringFrag(ParserPoint p)
	{
		var str = p.Parser.ShortDescription(depth: 7);
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
		if (_completed) throw new Exception("This scanner has been completed");
		if (EndOfInput(offset)) return false;

		offset++;

		return !EndOfInput(offset);
	}

	/// <inheritdoc />
	public char Peek(int offset)
	{
		if (_completed) throw new Exception("This scanner has been completed");
		if (EndOfInput(offset)) return (char)0;
		return Transform.Transform(InputString[offset]);
	}

	/// <summary> Placeholder parser for whitespace skip results </summary>
	private readonly Whitespace _ws = new();


	/// <summary>
	/// If skip whitespace is set and current position is whitespace,
	/// seek forward until on non-whitespace position or EOF.
	/// </summary>
	public ParserMatch AutoAdvance(ParserMatch? previous)
	{
		var ws = NullMatch(null, previous?.Right ?? 0);
		
		if (!SkipWhitespace) return ws;
		if (EndOfInput(ws.Right)) return ws;

		var offset = ws.Right;
		var c = Peek(offset);
		
		while (char.IsWhiteSpace(c))      // if this is whitespace
		{
			ws.ExtendTo(offset+1);         // mark our match up to this character
			if (!Read(ref offset)) break; // try to advance to next character
			c = Peek(offset);             // read that character
		}

		return ws;
	}


	/// <inheritdoc />
	public string Substring(int offset, int length)
	{
		return  Transform.Transform(InputString.Substring(offset, Math.Min(length, InputString.Length - offset)));
	}

	/// <inheritdoc />
	public string UntransformedSubstring(int offset, int length)
	{
		return  InputString.Substring(offset, Math.Min(length, InputString.Length - offset));
	}

	/// <inheritdoc />
	public string RemainingData(int offset)
	{
		return Transform.Transform(InputString.Substring(offset));
	}

	/// <inheritdoc />
	public ITransform Transform { get; set; }

	/// <inheritdoc />
	public ParserMatch NoMatch(IParser? source, ParserMatch? previous)
	{
		return new ParserMatch(source, this, previous?.Offset ?? 0, -1);
	}

	/// <inheritdoc />
	public ParserMatch EmptyMatch(IParser source, int offset)
	{
		return new ParserMatch(source, this, offset, 0);
	}
	
	/// <inheritdoc />
	public ParserMatch NullMatch(IParser? source, int offset)
	{
		return new ParserMatch(source, this, offset, -1);
	}
	
	/// <inheritdoc />
	public ParserMatch CreateMatch(IParser source, int offset, int length, Func<string, string>? mutator = null)
	{
		if ((offset + length) > _rightMostPoint)
		{
			_rightMostPoint = offset + length;
		}
		
		return new ParserMatch(source, this, offset, length, mutator);
	}

	#endregion
}