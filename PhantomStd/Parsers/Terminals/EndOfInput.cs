using Phantom.Parsers.Interfaces;
using Phantom.Results;

namespace Phantom.Parsers.Terminals;

/// <summary>
/// Parser that matches the end of the scanner data
/// </summary>
public class EndOfInput : Parser, IMatchingParser
{
	/// <inheritdoc />
	public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
	{
		var offset = previousMatch?.Right ?? 0;
		return scan.EndOfInput(offset) ? scan.EmptyMatch(this, offset) : scan.NoMatch;
	}

	/// <inheritdoc />
	public override string ToString()
	{
		var desc = "¦¦";
			
		if (TagValue is null) return desc;
		return desc + " Tag='" + TagValue + "'";
	}
}