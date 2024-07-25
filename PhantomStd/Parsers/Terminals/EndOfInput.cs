using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers.Terminals;

/// <summary>
/// Parser that matches the end of the scanner data
/// </summary>
public class EndOfInput : Parser, IMatchingParser
{
	/// <inheritdoc />
	public ParserMatch TryMatch(IScanner scan)
	{
		return scan.EndOfInput ? scan.EmptyMatch : scan.NoMatch;
	}

	/// <inheritdoc />
	public override string ToString()
	{
		var desc = "¦¦";
			
		if (TagValue is null) return desc;
		return desc + " Tag='" + TagValue + "'";
	}
}