using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers.Terminals;

/// <summary>
/// Parser that matches an exact string sequence
/// </summary>
public class LiteralString : Parser, IMatchingParser
{
	private readonly string _test;

	/// <summary>
	/// Parser that matches an exact string sequence
	/// </summary>
	public LiteralString(string toMatch)
	{
			_test = toMatch;
		}

	/// <summary>
	/// Gets the literal string that this parser test for.
	/// </summary>
	public string MatchLiteral
	{
		get { return _test; }
	}

	/// <inheritdoc />
	public ParserMatch TryMatch(IScanner scan)
	{
			int offset = scan.Offset;

			string compare = scan.Substring(offset, _test.Length);

			if (compare == _test)
			{
				scan.Seek(offset + _test.Length);
				return scan.CreateMatch(this, offset, _test.Length);
			}

			scan.Seek(offset);
			return scan.NoMatch;
		}

	/// <inheritdoc />
	public override string ToString()
	{
			var desc = "\"" + _test + "\"";
			
			if (TagValue is null) return desc;
			return desc + " Tag='" + TagValue + "'";
		}
}