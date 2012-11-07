using Phantom.Parsers;

namespace Phantom.Scanners
{
	/// <summary>
	/// Scanners provide an interface to the input stream to be parsed
	/// </summary>
	public interface IScanner: IScanningDiagnostics
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

	}
}