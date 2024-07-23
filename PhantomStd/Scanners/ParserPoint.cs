namespace PhantomStd.Scanners
{
	/// <summary>
	/// Match of parser and scanner location
	/// </summary>
	public class ParserPoint
	{
		/// <summary> Parser </summary>
		public readonly object Parser;
		/// <summary> Position </summary>
		public readonly int Pos;

		/// <summary>
		/// Match of parser and scanner location
		/// </summary>
		public ParserPoint(object p, int position)
		{
			Parser = p;
			Pos = position;
		}
	}
}