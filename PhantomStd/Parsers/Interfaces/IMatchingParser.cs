namespace PhantomStd.Parsers.Interfaces
{
	/// <summary>
	/// Interface for parsers that can attempt to match their parser patterns against scanner data
	/// </summary>
	public interface IMatchingParser: IParser
	{
		/// <summary>
		/// Try to match scanner data against the contained parser
		/// </summary>
		ParserMatch TryMatch(IScanner scan);
	}
}