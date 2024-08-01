using Phantom.Parsers;
using Phantom.Scanners;

namespace Phantom;

/// <summary>
/// Scanners provide an interface to the input stream to be parsed
/// </summary>
public interface IScanner: IScanningDiagnostics
{
	/// <summary>Returns true when all input is consumed.</summary>
	bool EndOfInput(int offset);

	/// <summary>Get or set the current input transform.</summary>
	ITransform Transform { get; set; }

	/// <summary>Return a failure match.</summary>
	ParserMatch NoMatch { get; }

	/// <summary>Return an empty success match</summary>
	ParserMatch EmptyMatch(IParser? source, int offset);

	/// <summary> Return an empty failure match </summary>
	ParserMatch NullMatch(IParser? source, int offset);

	/// <summary>Advance one position through the input</summary>
	/// <returns>Returns true while there is unconsumed input remaining</returns>
	bool Read(ref int offset);

	/// <summary>Return the character at the given offset</summary>
	char Peek(int offset);

	/// <summary>
	/// Prepares the scanner for the next token.
	/// This is mainly used for whitespace skipping.
	/// </summary>
	public ParserMatch AutoAdvance(ParserMatch? previous);

	/// <summary>Return a substring from the input.</summary>
	/// <param name="offset">Offset relative to the start of the input.</param>
	/// <param name="length">Length of substring to return.</param>
	string Substring(int offset, int length);

	/// <summary>
	/// Return a string containing all data from the current cursor onwards.
	/// </summary>
	/// <returns>String of remaining data</returns>
	string RemainingData(int offset);

	/// <summary>Return a match from a substring of the input</summary>
	ParserMatch CreateMatch(IParser source, int offset, int length);
}