using System.Collections.Generic;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parser that matches an end of line marker, either <c>\r</c>, <c>\n</c>, or <c>\r\n</c>
/// </summary>
public class EndOfLine : Parser
{
	/// <inheritdoc />
	internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
	{
		int start = previousMatch?.Right ?? 0;
		int cursor = start;
		int len = 0;

		if (!scan.EndOfInput(cursor) && scan.Peek(cursor) == '\r') // CR
		{
			scan.Read(ref cursor);
			len++;
		}

		if (!scan.EndOfInput(cursor) && scan.Peek(cursor) == '\n') // LF
		{
			scan.Read(ref cursor);
			len++;
		}

		if (len > 0) return scan.CreateMatch(this, start, len);

		return scan.NoMatch(this, previousMatch);
	}

	/// <inheritdoc />
	public override IEnumerable<IParser> ChildParsers() { yield break; }

	/// <inheritdoc />
	public override bool IsOptional() => false;

	/// <inheritdoc />
	public override string ToString()
	{
		var desc = "¬";
			
		if (Tag is null) return desc;
		return desc + " Tag='" + Tag + "'";
	}
    
	/// <inheritdoc />
	public override string ShortDescription(int depth)
	{
		return ToString();
	}
}