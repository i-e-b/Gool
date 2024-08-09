using System.Collections.Generic;
using System.Linq;
using Phantom.Parsers.Interfaces;
using Phantom.Results;

namespace Phantom.Parsers.Composite;

/// <summary>
/// Creates a Union (or 'alternative') parser from two sub-parsers.
/// </summary>
public class Union : Parser, IMatchingParser
{
	private readonly List<IParser> _parsers = new();
	
	/// <summary>
	/// Creates a Union (or 'alternative') parser from two sub-parsers.
	/// </summary>
	public Union(IParser left, IParser right)
	{
		if (left is Union leftUnion)
		{
			_parsers.AddRange(leftUnion._parsers);
		}
		else
		{
			_parsers.Add(left);
		}

		_parsers.Add(right);
	}

	/// <inheritdoc />
	public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
	{
		// First match version:
		/*
		foreach (var parser in _parsers)
		{
			var result = parser.Parse(scan, previousMatch);
			if (result is { Success: true, Empty: false }) return result.Through(this);
		}
		return scan.NoMatch(this, previousMatch);
		*/
		
		// Longest match version:
		
		ParserMatch? longestMatch = null;

		foreach (var parser in _parsers)
		{
			var result = parser.Parse(scan, previousMatch);
			if (result.Success && (result.Length > (longestMatch?.Length ?? 0))) longestMatch = result;
		}

		if (longestMatch is not null) return longestMatch.Through(this);
		return scan.NoMatch(this, previousMatch);
	}

	/// <inheritdoc />
	public override string ToString()
	{
		var desc = "{" + string.Join(" | ", _parsers.Select(p => p.ToString())) + "}";
			
		if (Tag is null) return desc;
		return desc + " Tag='" + Tag + "'";
	}
	
	/// <inheritdoc />
	public override string ShortDescription(int depth)
	{
		if (depth < 1) return GetType().Name;
		return "{" + string.Join(" | ", _parsers.Select(p => p.ShortDescription(depth - 1))) + "}";
	}
}