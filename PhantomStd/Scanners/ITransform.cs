namespace PhantomStd.Scanners
{
	/// <summary>
	/// Interface for textual transforms (like to lower case or remove punctuation)
	/// </summary>
	public interface ITransform
	{
		/// <summary>
		/// Convert one representation of a string to another.
		/// </summary>
		/// <param name="s">Input form</param>
		/// <returns>Output form</returns>
		string Transform(string s);

		/// <summary>
		/// Convert one representation of a character to another.
		/// </summary>
		/// <param name="c">Input character</param>
		/// <returns>Output character</returns>
		char Transform(char c);
	}
}