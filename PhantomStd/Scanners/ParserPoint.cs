namespace Phantom.Scanners;

/// <summary>
/// Match of parser and scanner location
/// </summary>
public class ParserPoint
{
	/// <summary> Parser </summary>
	public readonly object Parser;
	
	/// <summary> Position </summary>
	public readonly int Position;

	/// <summary>
	/// Length of region. Zero if a single point
	/// </summary>
	public readonly int Length;

	/// <summary>
	/// Match of parser and scanner location
	/// </summary>
	public ParserPoint(object p, int start, int end)
	{
		Parser = p;
		Position = start;
		Length = end - start;
	}
}