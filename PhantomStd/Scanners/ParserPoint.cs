using System;
using Gool.Parsers;
using Gool.Results;

namespace Gool.Scanners;

/// <summary>
/// Match of parser and scanner location
/// </summary>
public class ParserPoint
{
	/// <summary> Parser </summary>
	public readonly IParser Parser;
	
	/// <summary> Previous ParserMatch </summary>
	public readonly ParserMatch? PreviousMatch;

	/// <summary> Scanner </summary>
	public readonly IScanner Scanner;

	/// <summary> Position </summary>
	public readonly int Offset;

	/// <summary>
	/// Length of region. Zero if a single point
	/// </summary>
	public readonly int Length;

	/// <summary>
	/// Match of parser and scanner location
	/// </summary>
	public ParserPoint(IParser p, ParserMatch? prevMatch, IScanner scanner)
	{
		Parser = p;
		PreviousMatch = prevMatch;
		Scanner = scanner;
		Offset = Math.Max(0, prevMatch?.Offset ?? 0);
		Length = Math.Max(0, prevMatch?.Length ?? 0);
	}

	/// <summary>
	/// Describe the parser point
	/// </summary>
	public override string ToString()
	{
		if (PreviousMatch is null) return $"{Offset}:{Length} = [{Scanner.UntransformedSubstring(Offset, Length)}] from [{Parser.ShortDescription(1)}]";
		return $"[[{PreviousMatch}]] ... {Offset}:{Length} = [{Scanner.UntransformedSubstring(Offset, Length)}] from [{Parser.ShortDescription(1)}]";
	}
}