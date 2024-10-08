using System.Collections.Generic;
using System.Linq;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Composite;

/// <summary>
/// Creates a Union (or 'alternative') parser from two sub-parsers.
/// </summary>
public class Union : Parser
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

	/// <summary>
	/// Creates a Union (or 'alternative') parser from a set of sub-parsers.
	/// </summary>
	public Union(IEnumerable<IParser> parsers)
	{
		_parsers.AddRange(parsers);
	}

	/// <inheritdoc />
	internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
	{
		ParserMatch? longestMatch = null;

		foreach (var parser in _parsers)
		{
			var result = parser.Parse(scan, previousMatch, allowAutoAdvance);
			if (result.Success && (result.Length > (longestMatch?.Length ?? -1))) longestMatch = result;
		}

		if (longestMatch is not null) return longestMatch.Through(this, previousMatch);
		return scan.NoMatch(this, previousMatch);
	}

	/// <inheritdoc />
	public override IEnumerable<IParser> ChildParsers() => _parsers;

	/// <inheritdoc />
	public override bool IsOptional() => _parsers.All(p => p.IsOptional());

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