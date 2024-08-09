using System;
using Phantom.Results;

namespace Phantom.Scanners;

/// <summary>
/// Match of parser and scanner location
/// </summary>
public class ParserPoint
{
	/// <summary> Parser </summary>
	public readonly IParser Parser;
	
	/// <summary> Previous ParserMatch </summary>
	public readonly ParserMatch? PreviousMatch;
	
	/// <summary> Position </summary>
	public readonly int Position;

	/// <summary>
	/// Length of region. Zero if a single point
	/// </summary>
	public readonly int Length;

	/// <summary>
	/// Match of parser and scanner location
	/// </summary>
	public ParserPoint(IParser p, ParserMatch? prevMatch)
	{
		Parser = p;
		PreviousMatch = prevMatch;
		Position = Math.Max(0, prevMatch?.Offset ?? 0);
		Length = Math.Max(0, prevMatch?.Length ?? 0);
	}
}