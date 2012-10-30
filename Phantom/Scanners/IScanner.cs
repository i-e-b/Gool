using System.Collections.Generic;
using Phantom.Parsers;

namespace Phantom.Scanners
{
	/// <summary>
	/// Scanners provide an interface to the input stream to be parsed
	/// </summary>
	public interface IScanner
	{
		/// <summary>Returns true when all input is consumed.</summary>
		bool EOF { get; }

		/// <summary>Get or set the position of the cursor relative to the start of the input.</summary>
		int Offset { get; set; }

		/// <summary>Get or set the current input transform.</summary>
		ITransform Transform { get; set; }

		/// <summary>Return a failure match.</summary>
		ParserMatch NoMatch { get; }

		/// <summary>Return an empty success match</summary>
		ParserMatch EmptyMatch { get; }

		/// <summary>Advance one position through the input</summary>
		/// <returns>Returns true while there is unconsumed input remaining</returns>
		bool Read();

		/// <summary>Returns the character at the current position</summary>
		char Peek();

		/// <summary>
		/// Prepares the scanner for the next token.
		/// This is mainly used for whitespace skipping.
		/// </summary>
		void Normalise();

		/// <summary>Set the position of the cursor relative to the start of the input.</summary>
		void Seek(int offset);

		/// <summary>Return a substring from the input.</summary>
		/// <param name="offset">Offset relative to the start of the input.</param>
		/// <param name="length">Length of substring to return.</param>
		string Substring(int offset, int length);

		/// <summary>
		/// Return a string containing all data from the current cursor onwards.
		/// </summary>
		/// <returns>String of remaining data</returns>
		string RemainingData();

		/// <summary>Return a match from a substring of the input</summary>
		ParserMatch CreateMatch(Parser source, int offset, int length);

		/// <summary>
		/// Stores the combination of 'accessor' and 'offset'
		/// Returns true if the offset was the last accessed by the given accessor.
		/// </summary>
		bool RecursionCheck(object accessor, int offset);

		#region Debug Functions

		/// <summary>
		/// Returns a string of the match that had the highest value of (offset+length)
		/// </summary>
		string FurthestMatch();

		/// <summary>
		/// Set a point at which a parser failed
		/// </summary>
		/// <param name="tester">the parser that failed</param>
		/// <param name="position">position in scanner where it failed.</param>
		void AddFailure(object tester, int position);

		/// <summary>
		/// Output a list of all fail points since the last success.
		/// </summary>
		List<string> ListFailures();

		/// <summary>
		/// Clear the stored list of failures. Should be called whenever a parser succeeds
		/// </summary>
		void ClearFailures();

		/// <summary>
		/// Returns a sample string from after the scanner stops.
		/// </summary>
		string BadPatch(int length);

		/// <summary>
		/// Stores and returns the maximum stack depth reached
		/// </summary>
		/// <param name="CurrentDepth">Current stack depth</param>
		/// <returns>Max Stack depth</returns>
		int StackStats(int CurrentDepth);

		#endregion
	}
}