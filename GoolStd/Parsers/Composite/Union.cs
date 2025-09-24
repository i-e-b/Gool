using System.Collections.Generic;
using System.Linq;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Composite;

/// <summary>
/// Creates a Union (or 'alternative') parser from two sub-parsers.
/// This returns the longest match.
/// </summary>
public class Union : Parser
{
	private readonly IParser[] _parsers;
	
	/// <summary>
	/// Creates a Union (or 'alternative') parser from two sub-parsers.
	/// </summary>
	public Union(IParser left, IParser right)
	{
		var parserSet = new List<IParser>();
		if (left is Union leftUnion)
		{
			parserSet.AddRange(leftUnion._parsers);
		}
		else
		{
			parserSet.Add(left);
		}

		parserSet.Add(right);

		_parsers = parserSet.ToArray();
	}

	/// <summary>
	/// Creates a Union (or 'alternative') parser from a set of sub-parsers.
	/// </summary>
	public Union(IEnumerable<IParser> parsers)
	{
		_parsers = parsers.ToArray();
	}

	/// <inheritdoc />
	internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
	{
		ParserMatch? longestMatch = null;

		for (var index = 0; index < _parsers.Length; index++)
		{
			var parser = _parsers[index];
			var result = parser.Parse(scan, previousMatch, allowAutoAdvance);
			if (result.Success && (result.Length > (longestMatch?.Length ?? -1))) longestMatch = result;
		}

		if (longestMatch is not null) return longestMatch.Through(this, previousMatch);
		return scan.NoMatch(this, previousMatch);
	}

	/// <inheritdoc />
	public override IEnumerable<IParser> ChildParsers() => _parsers;

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