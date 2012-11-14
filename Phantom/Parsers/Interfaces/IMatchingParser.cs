namespace Phantom.Parsers.Interfaces
{
	public interface IMatchingParser: IParser
	{
		ParserMatch TryMatch(IScanner scan);
	}
}