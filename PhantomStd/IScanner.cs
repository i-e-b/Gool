using System.Collections.Generic;
using Phantom.Results;
using Phantom.Scanners;

namespace Phantom;

/// <summary>
/// Scanners provide an interface to the input stream to be parsed,
/// and holds state and context for the application of a parser tree
/// to an input.
/// <p/>
/// A new scanner should be created for each application
/// </summary>
public interface IScanner: IScanningDiagnostics
{
	/// <summary>Returns true when all input is consumed.</summary>
	bool EndOfInput(int offset);

	/// <summary>Get or set the current input transform.</summary>
	ITransform Transform { get; set; }
	
	/// <summary>
	/// If <c>true</c>, auto-advanced elements (like white-space skips) will be added to the result tree.
	/// </summary>
	public bool IncludeSkippedElements { get; set; }

	/// <summary>Return a failure match.</summary>
	ParserMatch NoMatch(IParser? source, ParserMatch? previous);

	/// <summary>Return an empty success match</summary>
	ParserMatch EmptyMatch(IParser source, int offset);

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

	/// <summary>
	/// Return a substring from the input.
	///	Any transformer on the scanner will be applied
	/// </summary>
	/// <param name="offset">Offset relative to the start of the input.</param>
	/// <param name="length">Length of substring to return.</param>
	string Substring(int offset, int length);

	/// <summary>
	/// Return a substring from the input.
	///	No transformers on the scanner are applied
	/// </summary>
	/// <param name="offset">Offset relative to the start of the input.</param>
	/// <param name="length">Length of substring to return.</param>
	string UntransformedSubstring(int offset, int length);

	/// <summary>
	/// Return a string containing all data from the current cursor onwards.
	/// </summary>
	/// <returns>String of remaining data</returns>
	string RemainingData(int offset);

	/// <summary>Return a match from a substring of the input</summary>
	ParserMatch CreateMatch(IParser source, int offset, int length);

	/// <summary>
	/// Add a success path, for diagnostic use
	/// </summary>
	void AddPath(ParserMatch newMatch);

	/// <summary>
	/// Set a context object for a parser.
	/// This will replace any existing context object.
	/// </summary>
	void SetContext(IParser parser, object? context);

	/// <summary>
	/// Get the context object for this parser, if one has been set.
	/// </summary>
	object? GetContext(IParser parser);
}